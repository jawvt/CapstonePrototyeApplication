############################################
#
# Landscape Through Time
# Alex, Ben Emily
# Code started 2/18/26
# 
# Project Description:
#
# Interactive application where users are able to view historical art located in desired regions,
# and have the ability to find the physical locations of the paintings using an interactive map-based design.
# 
#
############################################

library(DBI)
library(RPostgres)
library(shiny)
library(bslib)
library(leaflet)
library(htmltools)
library(DT)
library(shinyjs)

############################################
# Supabase Database Connection
# Connects to a PostgreSQL database hosted on Supabase.
# Replaces the old local .rds file storage with a remote database.
#
# Tables used:
#   - paintings: all Bierstadt painting data (replaces BPaintings.csv)
#   - submissions: user photo submissions with approval_status column
#                  (replaces both submissions.rds and approved.rds)
#
# IMPORTANT: Requires a .Renviron file in the app directory with:
#   SUPABASE_HOST=your-project.supabase.co
#   SUPABASE_DB=postgres
#   SUPABASE_USER=postgres.your-project
#   SUPABASE_PASSWORD=your-password
############################################

readRenviron(".Renviron")

# Helper: create a fresh database connection.
# Called whenever we need to read or write. Each operation opens a connection,
# does its work, and closes it — avoids stale/dropped connection issues.
get_db_con <- function() {
  dbConnect(
    Postgres(),
    host     = Sys.getenv("SUPABASE_HOST"),
    port     = 5432,
    dbname   = Sys.getenv("SUPABASE_DB"),
    user     = Sys.getenv("SUPABASE_USER"),
    password = Sys.getenv("SUPABASE_PASSWORD"),
    sslmode  = "require"
  )
}

# Helper: fetch all submissions from the database.
# Returns a data frame with the same columns as the old .rds files.
# If the table is empty, returns an empty data frame with correct columns.
db_load_submissions <- function() {
  con <- get_db_con()
  on.exit(dbDisconnect(con))
  result <- dbGetQuery(con, "SELECT * FROM submissions ORDER BY submission_date DESC")
  if (nrow(result) == 0) {
    return(data.frame(
      submission_id = character(), name = character(), email = character(),
      painting_id = integer(), photo_url = character(),
      latitude = numeric(), longitude = numeric(),
      observations = character(), submission_date = character(),
      approval_status = character(),
      submission_type = character(),    # "landscape", "museum_photo", or "user_painting"
      painting_title = character(),     # only for user_painting submissions
      artist_name = character(),        # only for user_painting submissions
      painting_year = character(),      # only for user_painting submissions
      painting_context = character(),   # only for user_painting submissions
      stringsAsFactors = FALSE
    ))
  }
  result
}

# Helper: insert a single new submission row into the database.
db_insert_submission <- function(sub_df) {
  con <- get_db_con()
  on.exit(dbDisconnect(con))
  dbExecute(con, 
            "INSERT INTO submissions (submission_id, name, email, painting_id, photo_url,
     latitude, longitude, observations, submission_date, approval_status,
     submission_type, painting_title, artist_name, painting_year, painting_context)
     VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15)",
            params = list(
              sub_df$submission_id,
              sub_df$name,
              sub_df$email,
              sub_df$painting_id,
              sub_df$photo_url,
              sub_df$latitude,
              sub_df$longitude,
              sub_df$observations,
              sub_df$submission_date,
              sub_df$approval_status,
              sub_df$submission_type,
              sub_df$painting_title,
              sub_df$artist_name,
              sub_df$painting_year,
              sub_df$painting_context
            )
  )
}

# Helper: update the approval_status of a submission by its submission_id.
db_update_status <- function(submission_id, new_status) {
  con <- get_db_con()
  on.exit(dbDisconnect(con))
  dbExecute(con,
            "UPDATE submissions SET approval_status = $1 WHERE submission_id = $2",
            params = list(new_status, submission_id)
  )
}

# Helper: delete a submission by its submission_id.
db_delete_submission <- function(submission_id) {
  con <- get_db_con()
  on.exit(dbDisconnect(con))
  dbExecute(con,
            "DELETE FROM submissions WHERE submission_id = $1",
            params = list(submission_id)
  )
}

############################################
# Painting Data
# Loads painting data from Supabase instead of local CSV.
# This runs once when the app starts.
############################################

con <- get_db_con()
paintings_data <- dbGetQuery(con, "SELECT * FROM paintings ORDER BY id")
dbDisconnect(con)

############################################
# Custom CSS
# Claude AI provided support
# Builds the website
#
# TABLE OF CONTENTS:
#
# Root
# Reset and Base
# Navbar Overrides
# Tab Content Wrapper
# Hero/Home Tab
# Button Styles
# Section Headers
# Painting Cards
# Map
# Comparisons
# Lightbox (Paintings)
# Lightbox (Comparisons)
# Admin
# Responsive (mobile)
# Alerts
# Shiny Specific Overrides



############################################

app_css <- "
/* ==============================================
   GLASSMORPHISM THEME
   Landscape Through Time

   KEY CONCEPT: Frosted-glass panels floating over
   rich landscape photography. Translucent layers,
   soft blurs, light refractions, and luminous accents.

   TABLE OF CONTENTS:
   - CSS Variables
   - Reset & Base
   - Navbar Overrides
   - Tab Content Wrapper
   - Hero / Home Tab
   - Button Styles
   - Section Headers
   - Painting Cards
   - Map (Split Layout)
   - Form
   - Comparisons
   - Lightbox (Paintings)
   - Lightbox (Comparisons)
   - Admin
   - Alerts
   - Responsive
   - Shiny Specific Overrides
   ============================================== */

/* ==============================================
   CSS VARIABLES - GLASSMORPHISM PALETTE
   ============================================== */
:root {
  /* Glass effect values */
  --glass-bg: rgba(255, 255, 255, 0.12);
  --glass-bg-strong: rgba(255, 255, 255, 0.18);
  --glass-bg-light: rgba(255, 255, 255, 0.06);
  --glass-border: rgba(255, 255, 255, 0.25);
  --glass-border-subtle: rgba(255, 255, 255, 0.12);
  --glass-blur: 20px;
  --glass-blur-heavy: 40px;

  /* Accent colors - luminous versions of the original earthy tones */
  --terra: #E8976B;
  --terra-dark: #C4724E;
  --terra-light: #F2B896;
  --terra-glow: rgba(232, 151, 107, 0.4);
  --sage: #7FA88A;
  --sage-dark: #5F8868;
  --sage-light: #A0C4A8;
  --sage-glow: rgba(127, 168, 138, 0.35);
  --amber: #E2B94C;
  --amber-light: #EDD07A;
  --amber-glow: rgba(226, 185, 76, 0.3);

  /* Surfaces */
  --surface-dark: #0f1a14;
  --surface-dark-mid: #152420;
  --surface-card: rgba(255, 255, 255, 0.08);

  /* Text */
  --text-primary: #FFFFFF;
  --text-secondary: rgba(255, 255, 255, 0.7);
  --text-muted: rgba(255, 255, 255, 0.45);

  /* Layout */
  --radius-sm: 12px;
  --radius-md: 20px;
  --radius-lg: 28px;
  --radius-xl: 36px;

  /* Shadows */
  --shadow-glass: 0 8px 32px rgba(0, 0, 0, 0.25);
  --shadow-glass-lg: 0 16px 48px rgba(0, 0, 0, 0.3);
  --shadow-glow: 0 0 40px rgba(232, 151, 107, 0.15);
  --ease: cubic-bezier(0.4, 0, 0.2, 1);
}

/* ==============================================
   RESET & BASE
   ============================================== */
* { margin: 0; padding: 0; box-sizing: border-box; }
html { scroll-behavior: smooth; }

body {
  font-family: 'DM Sans', -apple-system, BlinkMacSystemFont, sans-serif;
  background: var(--surface-dark);
  color: var(--text-primary);
  line-height: 1.6;
  overflow-x: hidden;
}

/* ==============================================
   NAVBAR OVERRIDES
   Translucent glass bar with blur effect
   ============================================== */
.navbar {
  background: rgba(15, 26, 20, 0.6) !important;
  backdrop-filter: blur(var(--glass-blur-heavy));
  -webkit-backdrop-filter: blur(var(--glass-blur-heavy));
  border-bottom: 1px solid var(--glass-border-subtle) !important;
  padding: 0 !important;
  min-height: 64px;
  box-shadow: 0 4px 30px rgba(0, 0, 0, 0.3);
  position: sticky;
  top: 0;
  z-index: 999;
}

.navbar-nav .nav-link {
  font-family: 'DM Sans', sans-serif !important;
  font-weight: 600 !important;
  font-size: 13px !important;
  color: var(--text-secondary) !important;
  padding: 22px 24px !important;
  letter-spacing: 0.8px;
  text-transform: uppercase;
  transition: all 0.3s var(--ease);
  border-bottom: 2px solid transparent;
  margin-bottom: -1px;
  position: relative;
}

.navbar-nav .nav-link:hover {
  color: var(--text-primary) !important;
  background: rgba(255, 255, 255, 0.05);
}

.navbar-nav .nav-link.active,
.navbar-nav .nav-item.active .nav-link,
.navbar-nav .nav-link[aria-selected='true'] {
  color: var(--terra-light) !important;
  border-bottom-color: var(--terra) !important;
  background: rgba(232, 151, 107, 0.08);
}

.navbar-toggler {
  border-color: rgba(255, 255, 255, 0.2) !important;
  color: var(--text-secondary) !important;
  margin-right: 16px;
}

.navbar-toggler-icon {
  filter: invert(1);
}

/* ==============================================
   TAB CONTENT WRAPPER
   ============================================== */
.tab-content {
  background: var(--surface-dark);
}

.tab-pane {
  animation: tabFadeIn 0.4s var(--ease);
  padding: 0 !important;
  margin: 0 !important;
}

@keyframes tabFadeIn {
  from { opacity: 0; transform: translateY(12px); }
  to { opacity: 1; transform: translateY(0); }
}

/* ==============================================
   HERO / HOME TAB
   Glass card floating over landscape background
   ============================================== */
.hero-banner {
  position: relative;
  background:
    linear-gradient(180deg, rgba(15,26,20,0.3) 0%, rgba(15,26,20,0.7) 100%),
    url('https://upload.wikimedia.org/wikipedia/commons/thumb/4/4a/Albert_Bierstadt_-_Among_the_Sierra_Nevada%2C_California_-_Google_Art_Project.jpg/2560px-Albert_Bierstadt_-_Among_the_Sierra_Nevada%2C_California_-_Google_Art_Project.jpg');
  background-size: cover;
  background-position: center;
  min-height: calc(100vh - 64px);
  display: flex;
  align-items: center;
  justify-content: center;
  overflow: hidden;
}

.hero-bg-pattern {
  position: absolute;
  inset: 0;
}

.hero-glow {
  position: absolute;
  border-radius: 50%;
  filter: blur(100px);
  pointer-events: none;
}

.hero-glow-1 {
  background: var(--terra-glow);
  width: 600px; height: 600px;
  top: -100px; right: -50px;
  opacity: 0.5;
}

.hero-glow-2 {
  background: var(--sage-glow);
  width: 500px; height: 500px;
  bottom: -100px; left: -50px;
  opacity: 0.4;
}

.hero-glow-3 {
  background: var(--amber-glow);
  width: 300px; height: 300px;
  top: 50%; left: 50%;
  transform: translate(-50%, -50%);
  opacity: 0.3;
}

/* Hero content sits inside a frosted glass card */
.hero-inner {
  position: relative;
  z-index: 2;
  text-align: center;
  max-width: 820px;
  padding: 60px 48px;
  background: var(--glass-bg);
  backdrop-filter: blur(var(--glass-blur-heavy));
  -webkit-backdrop-filter: blur(var(--glass-blur-heavy));
  border: 1px solid var(--glass-border);
  border-radius: var(--radius-xl);
  box-shadow: var(--shadow-glass), var(--shadow-glow);
}

.hero-badge {
  display: inline-block;
  background: rgba(232, 151, 107, 0.15);
  border: 1px solid rgba(232, 151, 107, 0.35);
  color: var(--terra-light);
  font-size: 11px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 2.5px;
  padding: 8px 24px;
  border-radius: 50px;
  margin-bottom: 28px;
  animation: fadeUp 0.8s var(--ease) 0.1s both;
}

.hero-title {
  font-family: 'DM Serif Display', Georgia, serif;
  font-size: clamp(38px, 6vw, 72px);
  font-weight: 400;
  color: var(--text-primary);
  line-height: 1.1;
  margin-bottom: 20px;
  animation: fadeUp 0.8s var(--ease) 0.25s both;
}

.hero-title span {
  background: linear-gradient(135deg, var(--terra-light), var(--amber));
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
}

.hero-subtitle {
  font-size: clamp(15px, 2vw, 18px);
  color: var(--text-secondary);
  max-width: 540px;
  margin: 0 auto 36px;
  line-height: 1.7;
  animation: fadeUp 0.8s var(--ease) 0.4s both;
}

.hero-actions {
  display: flex;
  gap: 16px;
  justify-content: center;
  flex-wrap: wrap;
  animation: fadeUp 0.8s var(--ease) 0.55s both;
}

@keyframes fadeUp {
  from { opacity: 0; transform: translateY(20px); }
  to { opacity: 1; transform: translateY(0); }
}

/* Stats strip: single centered glass box (paintings only) */
.stats-strip {
  display: flex;
  justify-content: center;
  gap: 12px;
  margin-top: 48px;
  animation: fadeUp 0.8s var(--ease) 0.7s both;
}

.stat-item {
  padding: 24px 16px;
  text-align: center;
  background: var(--glass-bg-light);
  border: 1px solid var(--glass-border-subtle);
  border-radius: var(--radius-md);
  transition: all 0.3s var(--ease);
}

.stat-item:hover {
  background: var(--glass-bg);
  border-color: var(--glass-border);
  transform: translateY(-2px);
}

.stat-value {
  font-family: 'DM Serif Display', Georgia, serif;
  font-size: 36px;
  color: var(--amber);
  line-height: 1;
  margin-bottom: 4px;
}

.stat-label {
  font-size: 10px;
  text-transform: uppercase;
  letter-spacing: 2px;
  color: var(--text-muted);
  font-weight: 600;
}

/* ==============================================
   BUTTON STYLES
   Glass-styled buttons with glow effects
   ============================================== */
.btn-terra {
  background: linear-gradient(135deg, var(--terra) 0%, var(--terra-dark) 100%);
  color: var(--text-primary) !important;
  border: none;
  padding: 15px 36px;
  font-size: 14px;
  font-weight: 700;
  border-radius: 50px;
  cursor: pointer;
  transition: all 0.3s var(--ease);
  text-decoration: none;
  display: inline-flex;
  align-items: center;
  gap: 8px;
  box-shadow: 0 4px 20px var(--terra-glow);
}

.btn-terra:hover {
  transform: translateY(-2px) scale(1.03);
  box-shadow: 0 8px 30px var(--terra-glow);
}

.btn-terra:active {
  transform: translateY(0) scale(0.98);
}

.btn-sage {
  background: var(--glass-bg);
  color: var(--sage-light) !important;
  border: 1px solid var(--sage);
  backdrop-filter: blur(10px);
  -webkit-backdrop-filter: blur(10px);
  padding: 14px 34px;
  font-size: 14px;
  font-weight: 700;
  border-radius: 50px;
  cursor: pointer;
  transition: all 0.3s var(--ease);
}

.btn-sage:hover {
  background: rgba(127, 168, 138, 0.15);
  border-color: var(--sage-light);
  transform: translateY(-2px);
  box-shadow: 0 4px 20px var(--sage-glow);
}

.btn-submit {
  background: linear-gradient(135deg, var(--terra) 0%, var(--terra-dark) 100%);
  color: var(--text-primary) !important;
  border: none;
  padding: 16px 40px;
  font-size: 15px;
  font-weight: 700;
  border-radius: var(--radius-md);
  cursor: pointer;
  transition: all 0.3s var(--ease);
  width: 100%;
  box-shadow: 0 4px 20px var(--terra-glow);
}

.btn-submit:hover {
  transform: translateY(-2px);
  box-shadow: 0 8px 30px var(--terra-glow);
}

/* ==============================================
   SECTION HEADERS
   Light text on dark background with glowing accent
   ============================================== */
.section-header {
  text-align: center;
  padding: 80px 24px 48px;
}

.section-header h2 {
  font-family: 'DM Serif Display', Georgia, serif;
  font-size: clamp(30px, 5vw, 48px);
  color: var(--text-primary);
  margin-bottom: 12px;
  line-height: 1.15;
}

.section-header p {
  font-size: 16px;
  color: var(--text-secondary);
  max-width: 520px;
  margin: 0 auto;
  line-height: 1.6;
}

.section-header .accent-line {
  width: 80px;
  height: 3px;
  background: var(--terra);
  border-radius: 2px;
  margin: 20px auto 0;
  box-shadow: 0 0 20px var(--terra-glow);
}

/* ==============================================
   PAINTING CARDS
   Frosted glass cards with glowing borders on hover
   ============================================== */
.gallery-wrap {
  padding: 0 24px 80px;
  max-width: 1400px;
  margin: 0 auto;
}

.paintings-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(360px, 1fr));
  gap: 28px;
}

.painting-card {
  position: relative;
  cursor: pointer;
  border-radius: var(--radius-lg);
  overflow: hidden;
  background: var(--glass-bg-strong);
  backdrop-filter: blur(var(--glass-blur));
  -webkit-backdrop-filter: blur(var(--glass-blur));
  border: 1px solid var(--glass-border-subtle);
  box-shadow: var(--shadow-glass);
  transition: all 0.4s var(--ease);
  transform-style: preserve-3d;
  display: flex;
  flex-direction: column;
}

.painting-card:hover {
  transform: translateY(-8px);
  border-color: var(--glass-border);
  box-shadow: var(--shadow-glass), 0 0 30px rgba(232, 151, 107, 0.1);
}

.painting-card-img-wrap {
  position: relative;
  overflow: hidden;
  aspect-ratio: 16 / 10;
}

.painting-card-img-wrap::after {
  content: '';
  position: absolute;
  inset: 0;
  background: linear-gradient(to top, rgba(15,26,20,0.5) 0%, transparent 40%);
  transition: opacity 0.4s;
}

.painting-card:hover .painting-card-img-wrap::after {
  opacity: 1;
}

.painting-image {
  width: 100%;
  height: 100%;
  object-fit: cover;
  display: block;
  transition: transform 0.6s var(--ease);
}

.painting-card:hover .painting-image {
  transform: scale(1.05);
}

.painting-card-badge {
  position: absolute;
  top: 14px;
  right: 14px;
  background: rgba(15, 26, 20, 0.5);
  backdrop-filter: blur(12px);
  -webkit-backdrop-filter: blur(12px);
  border: 1px solid var(--glass-border-subtle);
  color: var(--amber);
  font-size: 11px;
  font-weight: 700;
  padding: 5px 14px;
  border-radius: 20px;
  z-index: 2;
}

.painting-info {
  padding: 24px;
  flex: 1;
  display: flex;
  flex-direction: column;
}

.painting-title {
  font-family: 'DM Serif Display', Georgia, serif;
  font-size: 20px;
  color: var(--text-primary);
  margin-bottom: 6px;
  line-height: 1.3;
}

.painting-metadata {
  color: var(--terra-light);
  font-size: 12px;
  font-weight: 600;
  margin-bottom: 10px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

.painting-context {
  color: var(--text-secondary);
  font-size: 13px;
  line-height: 1.7;
}

.painting-card-footer {
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-top: auto;
  padding-top: 14px;
  gap: 10px;
}

.painting-card-cta {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  color: var(--terra);
  font-weight: 700;
  font-size: 12px;
  text-transform: uppercase;
  letter-spacing: 1px;
  transition: gap 0.3s;
  cursor: pointer;
  white-space: nowrap;
}

.painting-card:hover .painting-card-cta {
  gap: 10px;
}

/* ==============================================
   MAP - SPLIT LAYOUT
   Glass info panel, dark map container
   ============================================== */
.map-split-layout {
  display: grid;
  grid-template-columns: 3fr 1fr;
  gap: 24px;
  padding: 0 24px 60px;
  max-width: 1400px;
  margin: 0 auto;
  align-items: start;
}

.map-container {
  border-radius: var(--radius-lg);
  overflow: hidden;
  box-shadow: var(--shadow-glass-lg);
  height: 650px;
  width: 100%;
  border: 1px solid var(--glass-border-subtle);
}

.leaflet-container {
  height: 100%;
  border-radius: var(--radius-lg);
}

/* Map Info Panel - frosted glass */
.map-info-panel {
  background: var(--glass-bg-strong);
  backdrop-filter: blur(var(--glass-blur-heavy));
  -webkit-backdrop-filter: blur(var(--glass-blur-heavy));
  border-radius: var(--radius-lg);
  box-shadow: var(--shadow-glass);
  border: 1px solid var(--glass-border);
  padding: 32px;
  height: 650px;
  overflow-y: auto;
  display: flex;
  flex-direction: column;
}

.map-info-placeholder {
  flex: 1;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  text-align: center;
  color: var(--text-secondary);
}

.map-info-placeholder .placeholder-icon {
  font-size: 48px;
  margin-bottom: 16px;
  opacity: 0.3;
}

.map-info-placeholder p {
  font-size: 15px;
  line-height: 1.6;
  max-width: 280px;
  color: var(--text-secondary);
}

.map-info-header {
  display: flex;
  align-items: center;
  gap: 10px;
  margin-bottom: 20px;
  padding-bottom: 16px;
  border-bottom: 1px solid var(--glass-border-subtle);
}

.map-info-dot {
  width: 14px;
  height: 14px;
  border-radius: 50%;
  flex-shrink: 0;
}

.map-info-dot.painting { background: var(--terra); box-shadow: 0 0 8px var(--terra-glow); }
.map-info-dot.submission { background: #60A5FA; box-shadow: 0 0 8px rgba(96,165,250,0.4); }
.map-info-dot.museum { background: #DC3545; box-shadow: 0 0 8px rgba(220,53,69,0.4); }

.map-info-type-label {
  font-size: 11px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 1.5px;
  color: var(--text-muted);
}

.map-info-title {
  font-family: 'DM Serif Display', Georgia, serif;
  font-size: 24px;
  color: var(--text-primary);
  margin-bottom: 8px;
  line-height: 1.3;
}

.map-info-meta {
  color: var(--terra-light);
  font-size: 13px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  margin-bottom: 20px;
}

.map-info-image {
  width: 100%;
  border-radius: var(--radius-sm);
  margin-bottom: 20px;
  box-shadow: var(--shadow-glass);
  max-height: 220px;
  object-fit: cover;
}

.map-info-context {
  color: var(--text-secondary);
  font-size: 14px;
  line-height: 1.7;
  margin-bottom: 20px;
}

.map-info-coords {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 12px;
  margin-top: auto;
}

.coord-box {
  background: var(--glass-bg-light);
  border: 1px solid var(--glass-border-subtle);
  border-radius: var(--radius-sm);
  padding: 12px 14px;
  text-align: center;
}

.coord-label {
  font-size: 10px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 1.5px;
  color: var(--text-muted);
  margin-bottom: 4px;
}

.coord-value {
  font-family: 'DM Serif Display', Georgia, serif;
  font-size: 18px;
  color: var(--text-primary);
}

/* Map Filter Bar */
.map-filter-bar {
  display: flex;
  gap: 8px;
  justify-content: center;
  margin-bottom: 24px;
}

.map-filter-btn {
  display: inline-flex;
  align-items: center;
  gap: 8px;
  font-size: 13px;
  font-weight: 600;
  color: var(--text-secondary);
  padding: 8px 20px;
  border-radius: 50px;
  cursor: pointer;
  border: 1px solid var(--glass-border-subtle);
  background: var(--glass-bg-light);
  transition: all 0.3s var(--ease);
}

.map-filter-btn:hover {
  background: var(--glass-bg);
  border-color: var(--glass-border);
  color: var(--text-primary);
}

.map-filter-btn.active {
  background: var(--glass-bg-strong);
  border-color: var(--terra);
  color: var(--text-primary);
  box-shadow: 0 0 12px var(--terra-glow);
}

.legend-dot {
  width: 10px;
  height: 10px;
  border-radius: 50%;
  display: inline-block;
  flex-shrink: 0;
}

.legend-dot.red { background: var(--terra); }
.legend-dot.blue { background: #60A5FA; }

/* Locate Me button */
.locate-me-btn.tracking {
  border-color: #34D399 !important;
  color: #34D399 !important;
  background: rgba(52, 211, 153, 0.12) !important;
  box-shadow: 0 0 12px rgba(52, 211, 153, 0.3);
}

/* Artist filter dropdown in map filter bar */
.artist-filter-wrap {
  display: inline-flex;
  align-items: center;
}

.artist-filter-wrap .shiny-input-container {
  margin-bottom: 0 !important;
  width: 180px !important;
}

.artist-filter-wrap .selectize-input {
  padding: 7px 14px !important;
  min-height: 0 !important;
  font-size: 13px !important;
  font-weight: 600 !important;
  border-radius: 50px !important;
  background: var(--glass-bg-light) !important;
  border: 1px solid var(--glass-border-subtle) !important;
  color: var(--text-secondary) !important;
  line-height: 1.4 !important;
}

.artist-filter-wrap .selectize-input.focus {
  border-color: var(--terra) !important;
  box-shadow: 0 0 12px var(--terra-glow) !important;
  background: var(--glass-bg-strong) !important;
  color: var(--text-primary) !important;
}

.artist-filter-wrap .selectize-dropdown {
  border-radius: var(--radius-sm) !important;
  margin-top: 4px !important;
}


/* Submission info extras */
.map-info-observations {
  background: var(--glass-bg-light);
  border-left: 3px solid #60A5FA;
  padding: 14px 16px;
  border-radius: 0 var(--radius-sm) var(--radius-sm) 0;
  margin-bottom: 16px;
  font-size: 14px;
  color: var(--text-secondary);
  line-height: 1.6;
  font-style: italic;
}

.map-info-submitter {
  font-size: 13px;
  color: var(--text-muted);
  margin-bottom: 16px;
}

/* View Comparison button inside the map info panel */
.map-info-cta {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  background: var(--glass-bg-light);
  border: 1px solid var(--terra);
  color: var(--terra);
  font-weight: 700;
  font-size: 12px;
  text-transform: uppercase;
  letter-spacing: 1px;
  padding: 10px 20px;
  border-radius: 50px;
  cursor: pointer;
  transition: all 0.3s var(--ease);
  margin-bottom: 20px;
}

.map-info-cta:hover {
  background: rgba(232, 151, 107, 0.15);
  transform: translateY(-2px);
  box-shadow: 0 4px 16px var(--terra-glow);
}

.map-info-cta.museum {
  border-color: #DC3545;
  color: #E25563;
}

.map-info-cta.museum:hover {
  background: rgba(220, 53, 69, 0.15);
  box-shadow: 0 4px 16px rgba(220, 53, 69, 0.3);
}

/* Responsive override for map layout */
@media (max-width: 1024px) {
  .map-split-layout { grid-template-columns: 1fr; }
  .map-info-panel { height: auto; max-height: 500px; }
}

/* ==============================================
   FORM
   Glass card with translucent inputs
   ============================================== */
.form-wrap {
  padding: 0 24px 60px;
  max-width: 660px;
  margin: 0 auto;
}

.form-card {
  background: var(--glass-bg-strong);
  backdrop-filter: blur(var(--glass-blur-heavy));
  -webkit-backdrop-filter: blur(var(--glass-blur-heavy));
  padding: 48px;
  border-radius: var(--radius-lg);
  border: 1px solid var(--glass-border);
  box-shadow: var(--shadow-glass);
}

.form-card .form-group {
  margin-bottom: 24px;
}

.form-card label,
.form-card .control-label {
  font-weight: 600;
  color: var(--text-secondary);
  font-size: 13px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  margin-bottom: 8px;
  display: block;
}

.form-card .form-control,
.form-card .shiny-input-container input[type='text'],
.form-card .shiny-input-container input[type='number'],
.form-card .shiny-input-container select,
.form-card .shiny-input-container textarea {
  width: 100%;
  padding: 14px 16px;
  border: 1px solid var(--glass-border-subtle);
  border-radius: var(--radius-sm);
  font-size: 15px;
  font-family: 'DM Sans', sans-serif;
  transition: all 0.3s var(--ease);
  background: rgba(255, 255, 255, 0.06);
  color: var(--text-primary);
}

.form-card .form-control:focus,
.form-card input:focus,
.form-card select:focus,
.form-card textarea:focus {
  outline: none;
  border-color: var(--sage);
  box-shadow: 0 0 0 4px var(--sage-glow);
  background: rgba(255, 255, 255, 0.1);
}

.upload-zone {
  border: 3px dashed var(--glass-border-subtle);
  border-radius: var(--radius-md);
  padding: 40px 24px;
  text-align: center;
  transition: all 0.3s var(--ease);
  background: var(--glass-bg-light);
  cursor: pointer;
  color: var(--text-secondary);
}

.upload-zone:hover {
  border-color: var(--sage);
  background: rgba(127, 168, 138, 0.08);
}

.upload-icon {
  font-size: 36px;
  margin-bottom: 8px;
}

/* ==============================================
   COMPARISONS
   Glass-bordered thumbnails
   ============================================== */
.comparison-wrap {
  padding: 0 24px 60px;
  max-width: 1400px;
  margin: 0 auto;
}

.comparison-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(320px, 1fr));
  gap: 24px;
}

.comparison-thumb {
  position: relative;
  aspect-ratio: 16 / 10;
  border-radius: var(--radius-md);
  overflow: hidden;
  cursor: pointer;
  border: 1px solid var(--glass-border-subtle);
  box-shadow: var(--shadow-glass);
  transition: all 0.4s var(--ease);
}

.comparison-thumb:hover {
  transform: translateY(-6px) scale(1.02);
  border-color: var(--glass-border);
  box-shadow: var(--shadow-glass), 0 0 30px rgba(232,151,107,0.1);
}

.comparison-thumb img {
  width: 100%;
  height: 100%;
  object-fit: cover;
  transition: transform 0.5s var(--ease);
}

.comparison-thumb:hover img {
  transform: scale(1.06);
}

.comparison-thumb-overlay {
  position: absolute;
  inset: 0;
  background: linear-gradient(to top, rgba(15,26,20,0.8) 0%, transparent 50%);
  display: flex;
  align-items: flex-end;
  padding: 20px;
  opacity: 0;
  transition: opacity 0.3s;
}

.comparison-thumb:hover .comparison-thumb-overlay {
  opacity: 1;
}

.comparison-thumb-label {
  color: var(--text-primary);
  font-weight: 700;
  font-size: 13px;
  display: flex;
  align-items: center;
  gap: 6px;
  background: var(--glass-bg);
  backdrop-filter: blur(10px);
  -webkit-backdrop-filter: blur(10px);
  padding: 6px 14px;
  border-radius: 20px;
  border: 1px solid var(--glass-border-subtle);
}

/* Submitter name badge in top-left of comparison thumbnails */
.comparison-thumb-submitter {
  position: absolute;
  top: 12px;
  left: 12px;
  z-index: 2;
  background: rgba(15, 26, 20, 0.55);
  backdrop-filter: blur(12px);
  -webkit-backdrop-filter: blur(12px);
  border: 1px solid var(--glass-border-subtle);
  color: var(--text-primary);
  font-size: 11px;
  font-weight: 600;
  padding: 5px 12px;
  border-radius: 20px;
  letter-spacing: 0.3px;
}

.no-comparisons {
  text-align: center;
  padding: 80px 24px;
  color: var(--text-secondary);
  font-size: 18px;
}

/* Filter banner shown when Compare tab is filtered to a specific painting */
.compare-filter-banner {
  display: flex;
  align-items: center;
  justify-content: space-between;
  flex-wrap: wrap;
  gap: 12px;
  background: var(--glass-bg-strong);
  backdrop-filter: blur(var(--glass-blur));
  -webkit-backdrop-filter: blur(var(--glass-blur));
  border: 1px solid var(--glass-border);
  border-radius: var(--radius-md);
  padding: 16px 24px;
  margin-bottom: 28px;
  max-width: 1400px;
  margin-left: auto;
  margin-right: auto;
}

.compare-filter-text {
  color: var(--text-secondary);
  font-size: 14px;
}

.compare-filter-text strong {
  color: var(--text-primary);
}

.compare-filter-see-all {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  background: var(--glass-bg-light);
  border: 1px solid var(--terra);
  color: var(--terra);
  font-weight: 700;
  font-size: 12px;
  text-transform: uppercase;
  letter-spacing: 1px;
  padding: 8px 20px;
  border-radius: 50px;
  cursor: pointer;
  transition: all 0.3s var(--ease);
}

.compare-filter-see-all:hover {
  background: rgba(232, 151, 107, 0.15);
  transform: translateY(-2px);
  box-shadow: 0 4px 16px var(--terra-glow);
}

/* ==============================================
   LIGHTBOX CLOSE BUTTON
   Used by the comparison lightbox
   ============================================== */
.lightbox-close {
  position: absolute;
  top: 24px;
  right: 24px;
  border-radius: 50%;
  cursor: pointer;
  transition: all 0.3s;
  z-index: 10;
  color: var(--text-primary);
  font-size: 28px;
  width: 48px;
  height: 48px;
  display: flex;
  align-items: center;
  justify-content: center;
  background: var(--glass-bg);
  backdrop-filter: blur(12px);
  -webkit-backdrop-filter: blur(12px);
  border: 1px solid var(--glass-border-subtle);
}

.lightbox-close:hover {
  transform: rotate(90deg) scale(1.1);
  background: var(--glass-bg-strong);
}

/* ==============================================
   COMPARISON LIGHTBOX (Side-by-side)
   Glass labels over dark split view
   ============================================== */
#comparison-lightbox {
  display: none;
  position: fixed;
  inset: 0;
  background: rgba(8, 12, 10, 0.97);
  z-index: 10001;
}

#comparison-lightbox.active {
  display: flex;
}

.comparison-container {
  width: 100%;
  height: 100%;
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 4px;
  padding: 70px 32px;
}

.comparison-side {
  position: relative;
  overflow: hidden;
  background: #000;
  border-radius: var(--radius-sm);
  border: 1px solid var(--glass-border-subtle);
}

.comparison-side img {
  width: 100%;
  height: 100%;
  object-fit: contain;
  transition: transform 0.1s ease;
}

.comparison-label {
  position: absolute;
  top: 16px;
  left: 16px;
  background: var(--glass-bg-strong);
  backdrop-filter: blur(12px);
  -webkit-backdrop-filter: blur(12px);
  padding: 8px 18px;
  border-radius: 20px;
  font-size: 13px;
  font-weight: 700;
  color: var(--amber);
  border: 1px solid var(--glass-border-subtle);
}

/* ==============================================
   ADMIN
   Glass login card and admin controls
   ============================================== */
.admin-wrap {
  max-width: 1200px;
  margin: 0 auto;
  padding: 0 24px 60px;
}

.admin-login-card {
  max-width: 400px;
  margin: 0 auto;
  background: var(--glass-bg-strong);
  backdrop-filter: blur(var(--glass-blur-heavy));
  -webkit-backdrop-filter: blur(var(--glass-blur-heavy));
  padding: 48px;
  border-radius: var(--radius-lg);
  border: 1px solid var(--glass-border);
  box-shadow: var(--shadow-glass);
  text-align: center;
}

.admin-login-card h3 {
  font-family: 'DM Serif Display', Georgia, serif;
  color: var(--text-primary);
  font-size: 28px;
}

.admin-login-card p {
  color: var(--text-secondary);
  font-size: 14px;
}

.admin-login-card .form-control {
  padding: 14px 16px;
  border: 1px solid var(--glass-border-subtle);
  border-radius: var(--radius-sm);
  width: 100%;
  transition: all 0.3s;
  background: rgba(255, 255, 255, 0.06);
  color: var(--text-primary);
}

.admin-login-card .form-control:focus {
  border-color: var(--sage);
  box-shadow: 0 0 0 4px var(--sage-glow);
  outline: none;
  background: rgba(255, 255, 255, 0.1);
}

/* Admin Toolbar */
.admin-toolbar {
  display: flex;
  gap: 12px;
  flex-wrap: wrap;
  margin-bottom: 24px;
}

.admin-toolbar .btn {
  border-radius: var(--radius-sm);
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  cursor: pointer;
  transition: all 0.3s;
  border: 1px solid;
  backdrop-filter: blur(10px);
  -webkit-backdrop-filter: blur(10px);
}

.admin-toolbar .btn-approve {
  background: rgba(127, 168, 138, 0.2);
  border-color: var(--sage);
  color: var(--sage-light);
}

.admin-toolbar .btn-approve:hover {
  background: rgba(127, 168, 138, 0.35);
}

.admin-toolbar .btn-reject {
  background: rgba(232, 151, 107, 0.1);
  border-color: var(--terra);
  color: var(--terra-light);
}

.admin-toolbar .btn-reject:hover {
  background: rgba(232, 151, 107, 0.2);
}

.admin-toolbar .btn-refresh {
  background: var(--glass-bg-light);
  border-color: var(--glass-border);
  color: var(--text-secondary);
}

.admin-toolbar .btn-refresh:hover {
  background: var(--glass-bg);
}

/* DataTable Overrides - glass-styled table */
.dataTables_wrapper {
  background: var(--glass-bg-strong);
  backdrop-filter: blur(var(--glass-blur));
  -webkit-backdrop-filter: blur(var(--glass-blur));
  border: 1px solid var(--glass-border-subtle);
  border-radius: var(--radius-md);
  padding: 24px;
  box-shadow: var(--shadow-glass);
  color: var(--text-primary);
}

table.dataTable thead th {
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 1px;
  color: var(--text-secondary);
  border-bottom: 1px solid var(--glass-border-subtle) !important;
}

table.dataTable tbody td {
  color: var(--text-secondary);
  border-bottom: 1px solid var(--glass-border-subtle) !important;
}

table.dataTable tbody tr:hover {
  background: var(--glass-bg-light) !important;
}

table.dataTable tbody tr.selected {
  background: rgba(232, 151, 107, 0.15) !important;
}

.dataTables_wrapper .dataTables_length,
.dataTables_wrapper .dataTables_filter,
.dataTables_wrapper .dataTables_info,
.dataTables_wrapper .dataTables_paginate {
  color: var(--text-muted) !important;
}

.dataTables_wrapper .dataTables_filter input {
  background: rgba(255,255,255,0.06);
  border: 1px solid var(--glass-border-subtle);
  color: var(--text-primary);
  border-radius: var(--radius-sm);
  padding: 6px 12px;
}

.dataTables_wrapper .dataTables_paginate .paginate_button {
  color: var(--text-secondary) !important;
  border: 1px solid var(--glass-border-subtle) !important;
  background: var(--glass-bg-light) !important;
  border-radius: 6px !important;
}

.dataTables_wrapper .dataTables_paginate .paginate_button.current {
  background: var(--glass-bg-strong) !important;
  border-color: var(--terra) !important;
  color: var(--terra-light) !important;
}

/* ==============================================
   ALERTS
   Glass-styled success and error alerts
   ============================================== */
.alert-success-custom {
  background: rgba(127, 168, 138, 0.15);
  border: 1px solid var(--sage);
  color: var(--sage-light);
  padding: 16px 20px;
  border-radius: var(--radius-sm);
  margin-bottom: 24px;
  font-weight: 600;
  backdrop-filter: blur(10px);
  -webkit-backdrop-filter: blur(10px);
}

.alert-error-custom {
  background: rgba(232, 151, 107, 0.15);
  border: 1px solid var(--terra);
  color: var(--terra-light);
  padding: 16px 20px;
  border-radius: var(--radius-sm);
  margin-bottom: 24px;
  font-weight: 600;
  backdrop-filter: blur(10px);
  -webkit-backdrop-filter: blur(10px);
}

/* ==============================================
   RESPONSIVE
   ============================================== */
@media (max-width: 768px) {
  .paintings-grid { grid-template-columns: 1fr; }
  .comparison-grid { grid-template-columns: 1fr; }
  .stats-strip { gap: 8px; }
  .comparison-container { grid-template-columns: 1fr; }
  .form-card { padding: 32px 24px; }
  .hero-inner { padding: 40px 24px; margin: 0 16px; }
  .section-header { padding: 40px 20px 30px; }
}

/* ==============================================
   SHINY SPECIFIC OVERRIDES
   ============================================== */
.shiny-input-container {
  width: 100% !important;
}

.selectize-input {
  border: 1px solid var(--glass-border-subtle) !important;
  border-radius: var(--radius-sm) !important;
  padding: 10px 14px !important;
  background: rgba(255, 255, 255, 0.06) !important;
  color: var(--text-primary) !important;
}

.selectize-input.focus {
  border-color: var(--sage) !important;
  box-shadow: 0 0 0 4px var(--sage-glow) !important;
  background: rgba(255, 255, 255, 0.1) !important;
}

.selectize-dropdown {
  background: var(--surface-dark-mid) !important;
  border: 1px solid var(--glass-border-subtle) !important;
  border-radius: var(--radius-sm) !important;
}

.selectize-dropdown .option {
  color: var(--text-secondary) !important;
}

.selectize-dropdown .option.active {
  background: var(--glass-bg) !important;
  color: var(--text-primary) !important;
}

.selectize-input .item {
  color: var(--text-primary) !important;
}

/* Override Bootstrap nav-pills for dark theme */
.nav-pills .nav-link {
  color: var(--text-secondary) !important;
}

.nav-pills .nav-link.active {
  background: var(--glass-bg) !important;
  color: var(--text-primary) !important;
}

/* Scrollbar styling for dark theme */
::-webkit-scrollbar {
  width: 8px;
}
::-webkit-scrollbar-track {
  background: var(--surface-dark);
}
::-webkit-scrollbar-thumb {
  background: rgba(255,255,255,0.15);
  border-radius: 4px;
}
::-webkit-scrollbar-thumb:hover {
  background: rgba(255,255,255,0.25);
}

/* ==============================================
   LIGHT/DARK MODE TOGGLE BUTTON
   ============================================== */
.theme-toggle {
  background: var(--glass-bg);
  backdrop-filter: blur(12px);
  -webkit-backdrop-filter: blur(12px);
  border: 1px solid var(--glass-border-subtle);
  color: var(--text-secondary);
  width: 40px;
  height: 40px;
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  cursor: pointer;
  transition: all 0.3s var(--ease);
  font-size: 18px;
  margin: 12px 16px 12px 0;
  flex-shrink: 0;
}

.theme-toggle:hover {
  background: var(--glass-bg-strong);
  border-color: var(--glass-border);
  color: var(--amber);
  transform: scale(1.1);
}

/* ==============================================
   LIGHT MODE OVERRIDES
   ============================================== */
body.light-mode {
  --glass-bg: rgba(0, 0, 0, 0.04);
  --glass-bg-strong: rgba(0, 0, 0, 0.07);
  --glass-bg-light: rgba(0, 0, 0, 0.02);
  --glass-border: rgba(0, 0, 0, 0.12);
  --glass-border-subtle: rgba(0, 0, 0, 0.06);
  --surface-dark: #F5F0EB;
  --surface-dark-mid: #EDE7E0;
  --surface-card: rgba(0, 0, 0, 0.04);
  --text-primary: #2D2D2D;
  --text-secondary: rgba(45, 45, 45, 0.7);
  --text-muted: rgba(45, 45, 45, 0.45);
  --terra: #C2714F;
  --terra-dark: #A85D3F;
  --terra-light: #9E5A3C;
  --terra-glow: rgba(194, 113, 79, 0.25);
  --sage: #5F8868;
  --sage-dark: #4A6F52;
  --sage-light: #4A6F52;
  --sage-glow: rgba(95, 136, 104, 0.2);
  --amber: #B8942A;
  --amber-light: #A07E1F;
  --amber-glow: rgba(184, 148, 42, 0.2);
  --shadow-glass: 0 4px 20px rgba(0, 0, 0, 0.08);
  --shadow-glass-lg: 0 8px 32px rgba(0, 0, 0, 0.1);
  --shadow-glow: 0 0 30px rgba(194, 113, 79, 0.08);
}

body.light-mode .navbar {
  background: rgba(245, 240, 235, 0.8) !important;
  border-bottom-color: rgba(0, 0, 0, 0.08) !important;
  box-shadow: 0 2px 20px rgba(0, 0, 0, 0.06);
}

body.light-mode .hero-banner {
  background:
    linear-gradient(180deg, rgba(245,240,235,0.2) 0%, rgba(245,240,235,0.55) 100%),
    url('https://upload.wikimedia.org/wikipedia/commons/thumb/4/4a/Albert_Bierstadt_-_Among_the_Sierra_Nevada%2C_California_-_Google_Art_Project.jpg/2560px-Albert_Bierstadt_-_Among_the_Sierra_Nevada%2C_California_-_Google_Art_Project.jpg');
  background-size: cover;
  background-position: center;
}

body.light-mode .hero-inner {
  background: rgba(255, 255, 255, 0.65);
  border-color: rgba(0, 0, 0, 0.1);
}

body.light-mode .painting-card-badge {
  background: rgba(255, 255, 255, 0.75);
  border-color: rgba(0, 0, 0, 0.08);
}

body.light-mode .lightbox-close {
  background: rgba(255, 255, 255, 0.7);
  border-color: rgba(0, 0, 0, 0.1);
  color: var(--text-primary);
}

body.light-mode .dataTables_wrapper .dataTables_filter input {
  background: rgba(0, 0, 0, 0.03);
  border-color: rgba(0, 0, 0, 0.1);
  color: var(--text-primary);
}

body.light-mode .selectize-input {
  background: rgba(0, 0, 0, 0.03) !important;
  border-color: rgba(0, 0, 0, 0.1) !important;
  color: var(--text-primary) !important;
}

body.light-mode .selectize-dropdown {
  background: #F5F0EB !important;
  border-color: rgba(0, 0, 0, 0.1) !important;
}

body.light-mode .form-card .form-control,
body.light-mode .form-card input,
body.light-mode .form-card select,
body.light-mode .form-card textarea {
  background: rgba(0, 0, 0, 0.03);
  color: var(--text-primary);
}

body.light-mode .admin-login-card .form-control {
  background: rgba(0, 0, 0, 0.03);
  color: var(--text-primary);
}

body.light-mode ::-webkit-scrollbar-track {
  background: #F5F0EB;
}
body.light-mode ::-webkit-scrollbar-thumb {
  background: rgba(0, 0, 0, 0.15);
}

/* ==============================================
   SUBMISSION COUNT BADGE ON GALLERY CARDS
   ============================================== */
.submission-count-badge {
  display: inline-flex;
  align-items: center;
  gap: 5px;
  background: var(--glass-bg);
  backdrop-filter: blur(10px);
  -webkit-backdrop-filter: blur(10px);
  border: 1px solid var(--glass-border-subtle);
  padding: 4px 12px;
  border-radius: 20px;
  font-size: 11px;
  font-weight: 700;
  color: var(--sage-light);
}

/* ==============================================
   MUSEUM PHOTO BADGE ON GALLERY CARDS
   ============================================== */
.museum-photo-badge {
  position: absolute;
  bottom: 14px;
  left: 14px;
  z-index: 2;
  display: inline-flex;
  align-items: center;
  gap: 5px;
  background: rgba(15, 26, 20, 0.55);
  backdrop-filter: blur(12px);
  -webkit-backdrop-filter: blur(12px);
  border: 1px solid var(--amber-glow);
  color: var(--amber);
  font-size: 11px;
  font-weight: 700;
  padding: 6px 14px;
  border-radius: 20px;
  cursor: pointer;
  transition: all 0.3s var(--ease);
  letter-spacing: 0.3px;
}

.museum-photo-badge:hover {
  background: rgba(226, 185, 76, 0.2);
  border-color: var(--amber);
  transform: translateY(-2px);
  box-shadow: 0 4px 16px var(--amber-glow);
}
"

################################################################################
# UI SECTION
################################################################################

ui <- page_navbar(
  
  useShinyjs(),
  # useShinyjs() activates the shinyjs package, which lets the server send
  # JavaScript commands to the browser.
  
  title = NULL,
  # Setting title to NULL hides the default app title in the navbar.
  
  id = "main_tabs",
  # Gives the navbar an ID so the server can detect which tab is currently active.
  # In the server you'll see: input$main_tabs
  
  theme = bs_theme(
    # bs_theme() customizes the visual appearance using Bootstrap 5.
    # GLASSMORPHISM THEME: Dark background with luminous accents.
    
    version = 5,
    # Use Bootstrap version 5.
    
    bg = "#0f1a14",    # Dark forest background (was cream)
    fg = "#FFFFFF",    # White foreground text (was charcoal)
    primary = "#E8976B",   # Luminous terra accent (was #C2714F)
    secondary = "#7FA88A", # Luminous sage accent (was #6B8F71)
    success = "#7FA88A",   # Sage green for success alerts
    info = "#E2B94C",      # Bright amber for info alerts
    
    base_font = font_google("DM Sans"),
    # Loads the "DM Sans" font from Google Fonts for body text.
    
    heading_font = font_google("DM Serif Display")
    # Loads "DM Serif Display" from Google Fonts for headings/titles.
  ),
  
  header = tags$head(
    # tags$head() injects content into the HTML <head> tag.
    # This is where you load external resources and styles.
    
    tags$link(href = "https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700&family=DM+Serif+Display&display=swap", rel = "stylesheet"),
    # Loads the Google Fonts stylesheet so DM Sans and DM Serif Display
    # are available in the browser.
    
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    # This meta tag makes the app responsive on mobile devices --
    # it tells the browser to scale the page to the device width.
    
    tags$style(HTML(app_css))
    # Injects the custom CSS string (defined earlier as app_css) into the page.
  ),
  
  
  # -- TAB 1: MAP ----------------------------------------------------------
  # Split layout with map on the left and an info panel on the right.
  # Red circle markers for Bierstadt paintings, blue circles for user submissions.
  # Clicking any marker populates the info panel with details.
  nav_panel(
    title = "Map",
    icon = icon("map-location-dot"),
    
    tags$div(class = "section-header",
             tags$h2("Explore Locations"),
             tags$p("See historical paintings across America. Click a marker for details."),
             tags$div(class = "accent-line")
    ),
    
    # Map filter buttons — let users toggle which markers are visible.
    # "All" shows both, "Paintings" shows only red, "Submissions" shows only blue.
    tags$div(class = "map-filter-bar",
             tags$div(class = "map-filter-btn active", id = "map_filter_all",
                      onclick = "Shiny.setInputValue('set_map_filter', 'all');",
                      "All"
             ),
             tags$div(class = "map-filter-btn", id = "map_filter_paintings",
                      onclick = "Shiny.setInputValue('set_map_filter', 'paintings');",
                      tags$span(class = "legend-dot red"),
                      "Paintings"
             ),
             tags$div(class = "map-filter-btn", id = "map_filter_submissions",
                      onclick = "Shiny.setInputValue('set_map_filter', 'submissions');",
                      tags$span(class = "legend-dot blue"),
                      "Submissions"
             ),
             
             tags$div(class = "map-filter-btn", id = "map_filter_museums",
                      onclick = "Shiny.setInputValue('set_map_filter', 'museums');",
                      tags$span(class = "legend-dot", style = "background: #DC3545;"),
                      "Museums"
             ),
             tags$div(class = "map-filter-btn locate-me-btn", id = "locate_me_btn",
                      onclick = "startGeolocation();",
                      HTML("&#9678; My Location")
             ),
             # Artist filter dropdown — styled inline with the filter buttons
             tags$div(class = "artist-filter-wrap",
                      selectInput("map_artist_filter", NULL,
                                  choices = c("All Artists" = "", sort(unique(paintings_data$artist))),
                                  selected = "",
                                  width = "180px")
             )
    ),
    
    # Two-column grid layout -- map on left, info panel on right.
    tags$div(class = "map-split-layout",
             
             # LEFT: Interactive Leaflet map
             tags$div(class = "map-container",
                      leafletOutput("main_map", height = "100%")
             ),
             
             # RIGHT: Info panel -- populated when a marker is clicked
             tags$div(class = "map-info-panel",
                      uiOutput("map_info_content")
             )
    )
  ),
  
  
  # -- TAB 2: GALLERY ------------------------------------------------------
  # Displays all paintings as clickable cards with optional museum photo badges.
  nav_panel(
    title = "Gallery",
    icon = icon("images"),
    
    tags$div(class = "section-header",
             tags$h2("The Collection"),
             tags$div(class = "accent-line")
    ),
    
    # Gallery filter bar
    tags$div(class = "map-filter-bar",
             tags$div(class = "map-filter-btn active", id = "gallery_filter_all",
                      onclick = "Shiny.setInputValue('set_gallery_filter', 'all');",
                      "All Paintings"
             ),
             tags$div(class = "map-filter-btn", id = "gallery_filter_museum",
                      onclick = "Shiny.setInputValue('set_gallery_filter', 'museum_photos');",
                      HTML("&#127963; Museum Photos")
             )
    ),
    
    tags$div(class = "gallery-wrap",
             tags$div(id = "paintings-container", class = "paintings-grid",
                      uiOutput("painting_cards")
             )
    )
  ),
  
  
  # -- TAB 4: SUBMIT ------------------------------------------------------
  # A form where visitors upload a modern photo from a Bierstadt location.
  # They can optionally add their name, email, GPS coordinates, and observations.
  nav_panel(
    title = "Contribute",
    icon = icon("camera"),
    
    tags$div(class = "section-header",
             tags$h2("Contribute"),
             tags$p("Share landscape photos, museum visits, or upload historical paintings to expand the collection."),
             tags$div(class = "accent-line")
    ),
    
    tags$div(class = "form-wrap",
             tags$div(class = "form-card",
                      
                      uiOutput("submit_message"),
                      # Placeholder for success or error messages shown after form submission.
                      
                      # -- SUBMISSION TYPE SELECTOR --
                      # Three radio buttons that control which fields are visible below.
                      tags$div(class = "form-group",
                               radioButtons("submit_type", "What are you submitting?",
                                            choices = c(
                                              "Landscape Photo (visit a painting's location)" = "landscape",
                                              "Museum Photo (photo of painting in a museum)" = "museum_photo",
                                              "Upload a Painting (add a new historical painting)" = "user_painting"
                                            ),
                                            selected = "landscape"
                               )
                      ),
                      
                      tags$div(class = "form-group",
                               textInput("submit_name", "Your Name (optional)", placeholder = "Jane Doe")
                      ),
                      
                      tags$div(class = "form-group",
                               textInput("submit_email", "Email (optional)", placeholder = "jane@university.edu")
                      ),
                      
                      tags$div(class = "form-group",
                               radioButtons(
                                 inputId = "submit_source_type",
                                 label = "Where are you uploading this photo from?",
                                 choices = c(
                                   "I visited the actual painting location (in the field)" = "field",
                                   "I took this photo in a museum (of the painting itself)" = "museum"
                                 ),
                                 selected = NULL,
                                 inline = FALSE
                               )
                      ),
                      
                      # === Conditional: Painting Site (Field Location) ===
                      conditionalPanel(
                        condition = "input.submit_source_type == 'field'",
                        tags$div(class = "form-group",
                                 selectInput(
                                   "submit_painting",
                                   "Which Bierstadt painting location did you visit?",
                                   choices = c("Select a location..." = "", 
                                               setNames(paintings_data$id, paintings_data$title))
                                 )
                        )
                      ),
                      
                      # === Conditional: Museum ===
                      conditionalPanel(
                        condition = "input.submit_source_type == 'museum'",
                        tags$div(class = "form-group",
                                 selectInput(
                                   "submit_museum",
                                   "Which museum did you visit?",
                                   choices = c(
                                     "Select a museum..." = "",
                                     "Metropolitan Museum of Art (New York)" = "met",
                                     "National Gallery of Art (Washington, D.C.)" = "nga",
                                     "Smithsonian American Art Museum" = "saam",
                                     "Denver Art Museum" = "denver",
                                     "Museum of Fine Arts, Boston" = "mfa_boston",
                                     "Other (please specify below)" = "other"
                                   )
                                 )
                        )
                      ),
                      
                      # Optional: If user selects "Other" museum, show a text field
                      conditionalPanel(
                        condition = "input.submit_source_type == 'museum' && input.submit_museum == 'other'",
                        tags$div(class = "form-group",
                                 textInput("submit_museum_other", "Please specify the museum name", 
                                           placeholder = "e.g. Yale University Art Gallery")
                        )
                      ),
                      
                      tags$div(class = "form-group",
                               tags$label("Upload Your Photo"),
                               tags$div(class = "upload-zone",
                                        tags$div(class = "upload-icon", HTML("&#128247;")),
                                        tags$p(style = "color: var(--text-secondary); font-size: 14px;", "Drag & drop or click to browse"),
                                        fileInput("submit_photo", NULL, accept = c("image/png", "image/jpeg", "image/jpg"))
                               )
                      ),
                      
                      # -- GPS COORDINATES with "Use My Location" button --
                      tags$div(class = "form-group",
                               tags$div(style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 8px;",
                                        tags$label("GPS Coordinates", style = "margin-bottom: 0;"),
                                        tags$button(
                                          id = "use_my_location_btn",
                                          class = "map-filter-btn",
                                          style = "padding: 6px 16px; font-size: 12px;",
                                          onclick = "getFormLocation();",
                                          HTML("&#9678; Use My Location")
                                        )
                               ),
                               tags$div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 16px;",
                                        tags$div(class = "form-group", style = "margin-bottom: 0;",
                                                 numericInput("submit_latitude", NULL, value = NA, step = 0.0001)
                                        ),
                                        tags$div(class = "form-group", style = "margin-bottom: 0;",
                                                 numericInput("submit_longitude", NULL, value = NA, step = 0.0001)
                                        )
                               ),
                               tags$div(id = "location_status", style = "font-size: 12px; color: var(--text-muted); margin-top: 6px;")
                      ),
                      
                      tags$div(class = "form-group",
                               textAreaInput("submit_observations", "Observations (optional)", rows = 3,
                                             placeholder = "What did you notice about how the landscape has changed?")
                      ),
                      
                      actionButton("submit_button", HTML("Submit &rarr;"), class = "btn-submit")
             )
    )
  ),
  
  
  # -- TAB 5: COMPARE ----------------------------------------------------
  # Shows a grid of approved submissions as thumbnails. Clicking one opens
  # a side-by-side lightbox comparing the original painting with the modern photo.
  nav_panel(
    title = "Compare",
    icon = icon("arrows-left-right"),
    
    tags$div(class = "section-header",
             tags$h2("Past vs Present"),
             tags$p("See how these landscapes have transformed over 150 years. Click to open side-by-side."),
             tags$div(class = "accent-line")
    ),
    
    tags$div(class = "comparison-wrap",
             uiOutput("comparison_gallery")
             # Placeholder filled by the server. If no approved submissions exist,
             # it shows a "be the first to contribute" message. Otherwise it
             # generates a grid of clickable thumbnail cards.
    )
  ),
  
  
  # -- SPACER --------------------------------------------------------------
  nav_spacer(),
  # Pushes everything after it to the far right of the navbar.
  
  
  # -- THEME TOGGLE --------------------------------------------------------
  # A sun/moon button in the navbar that switches between dark and light mode.
  nav_item(
    tags$button(
      id = "theme_toggle",
      class = "theme-toggle",
      title = "Toggle light/dark mode",
      onclick = "toggleTheme()",
      HTML("&#9788;")
    )
  ),
  
  
  # -- TAB 6: LOGIN ------------------------------------------------------
  # Admin-only tab. Shows a password form by default. On correct login,
  # shows a data table of all submissions with approve/reject buttons.
  nav_panel(
    title = "Admin Login",
    icon = icon("right-to-bracket"),
    
    tags$div(class = "section-header",
             tags$h2("Admin Login"),
             tags$p("Sign in to review and manage community submissions."),
             tags$div(class = "accent-line")
    ),
    
    tags$div(class = "admin-wrap",
             
             conditionalPanel(
               condition = "output.admin_authenticated == false",
               # conditionalPanel() shows/hides its contents based on a JavaScript condition.
               # Here: show the login form only when the user is NOT authenticated.
               # output.admin_authenticated is a reactive value set by the server.
               
               tags$div(class = "admin-login-card",
                        tags$h3("Sign In"),
                        tags$p("Enter admin credentials to manage submissions."),
                        passwordInput("admin_password", NULL, placeholder = "Password"),
                        # passwordInput() is like textInput but hides the characters as dots.
                        actionButton("admin_login", "Sign In", class = "btn-submit")
                        # When clicked, the server checks if the password matches "admin123".
               )
             ),
             
             conditionalPanel(
               condition = "output.admin_authenticated == true",
               # Show the admin panel only when the user IS authenticated.
               
               tags$div(class = "admin-toolbar",
                        actionButton("refresh_admin", "Refresh", class = "btn btn-refresh"),
                        actionButton("approve_submission", "Approve Selected", class = "btn btn-approve"),
                        actionButton("reject_submission", "Reject Selected", class = "btn btn-reject"),
                        actionButton("delete_submission", "Delete Selected", class = "btn btn-reject",
                                     style = "background: #DC3545; border-color: #DC3545; color: white;"),
                        # Submission type filter dropdown
                        tags$div(class = "artist-filter-wrap",
                                 selectInput("admin_type_filter", NULL,
                                             choices = c("All Types" = "",
                                                         "Landscape Photos" = "landscape",
                                                         "Museum Photos" = "museum_photo",
                                                         "User Paintings" = "user_painting"),
                                             selected = "",
                                             width = "180px")
                        ),
                        # Approval status filter dropdown
                        tags$div(class = "artist-filter-wrap",
                                 selectInput("admin_status_filter", NULL,
                                             choices = c("All Statuses" = "",
                                                         "Pending" = "Pending",
                                                         "Approved" = "Approved",
                                                         "Rejected" = "Rejected"),
                                             selected = "",
                                             width = "160px")
                        )
               ),
               
               DTOutput("admin_table")
               # DTOutput() is a placeholder for a DataTables interactive table.
               # The server renders it with renderDT(), showing all submissions.
             )
    )
  ),
  
  
  # -- LIGHTBOXES AND JAVASCRIPT ------------------------------------------
  # These elements are always present in the HTML but hidden by default.
  # The JavaScript functions below control when they appear.
  footer = tagList(
    
    # -- COMPARISON LIGHTBOX --------------------------------------------
    # A full-screen side-by-side view. Left = historical painting, Right = modern photo.
    tags$div(id = "comparison-lightbox",
             tags$div(class = "lightbox-close", onclick = "closeComparisonLightbox()",
                      style = "position: fixed; top: 24px; right: 24px; z-index: 10002;", HTML("&times;")),
             
             tags$div(class = "comparison-container",
                      tags$div(class = "comparison-side",
                               tags$div(class = "comparison-label", "Historical"),
                               tags$img(id = "comp-historical", src = "", draggable = "false")
                               # draggable = "false" prevents the browser's default image drag behavior.
                      ),
                      tags$div(class = "comparison-side",
                               tags$div(class = "comparison-label", "Present Day"),
                               tags$img(id = "comp-modern", src = "", draggable = "false")
                      )
             )
    ),
    
    # -- MUSEUM PHOTO LIGHTBOX ------------------------------------------
    # Full-screen overlay showing a user's photo of a painting in a museum.
    # Opened from the museum photo badge on gallery cards.
    tags$div(id = "museum-photo-lightbox",
             style = "display:none; position:fixed; inset:0; background:rgba(8,12,10,0.97); z-index:10001; align-items:center; justify-content:center; flex-direction:column; padding:60px 32px;",
             tags$div(class = "lightbox-close", onclick = "closeMuseumLightbox()",
                      style = "position:fixed; top:24px; right:24px; z-index:10002;", HTML("&times;")),
             tags$div(style = "text-align:center; max-width:900px; width:100%;",
                      tags$div(id = "museum-lb-label",
                               style = "background:var(--glass-bg-strong); backdrop-filter:blur(12px); -webkit-backdrop-filter:blur(12px); padding:8px 18px; border-radius:20px; font-size:13px; font-weight:700; color:var(--amber); border:1px solid var(--glass-border-subtle); display:inline-block; margin-bottom:20px;",
                               "Museum Photo"
                      ),
                      tags$img(id = "museum-lb-img", src = "", style = "max-width:100%; max-height:70vh; border-radius:var(--radius-md); box-shadow:var(--shadow-glass-lg); object-fit:contain;"),
                      tags$div(id = "museum-lb-info", style = "margin-top:16px; color:var(--text-secondary); font-size:14px;")
             )
    ),
    
    # -- JAVASCRIPT ----------------------------------------------------
    # All the client-side interactivity: lightbox open/close, 3D card tilt,
    # tab switching, and fixing the Leaflet map rendering bug in tabs.
    tags$script(HTML(paste0("

      // -- LIGHT/DARK MODE TOGGLE ----------------------------------------
      window.toggleTheme = function() {
        document.body.classList.toggle('light-mode');
        var btn = document.getElementById('theme_toggle');
        if (document.body.classList.contains('light-mode')) {
          btn.innerHTML = '&#9789;';
        } else {
          btn.innerHTML = '&#9788;';
        }
      };

      // paintingsData is the CSV data injected from R into JavaScript as JSON.
      var paintingsData = ", jsonlite::toJSON(paintings_data, auto_unbox = TRUE), ";

      // Opens the comparison lightbox with two images side by side.
      window.openComparisonLightbox = function(historicalUrl, modernUrl) {
        document.getElementById('comp-historical').src = historicalUrl;
        document.getElementById('comp-modern').src = modernUrl;
        document.getElementById('comparison-lightbox').classList.add('active');
        document.body.style.overflow = 'hidden';

        // Adds scroll-to-zoom behavior on both images simultaneously.
        var sides = document.querySelectorAll('.comparison-side img');
        sides.forEach(function(img) {
          img.addEventListener('wheel', function(e) {
            e.preventDefault();
            // Prevents the page from scrolling when the user scrolls over the image.
            var current = parseFloat(img.style.transform.replace('scale(', '').replace(')', '') || 1);
            var delta = e.deltaY * -0.01;
            var scale = Math.max(1, Math.min(3, current + delta));
            // Clamp zoom between 1x (normal) and 3x (maximum zoom).
            sides.forEach(function(s) { s.style.transform = 'scale(' + scale + ')'; });
            // Apply the same zoom level to BOTH images so they stay in sync.
          });
        });
      };

      window.closeComparisonLightbox = function() {
        document.getElementById('comparison-lightbox').classList.remove('active');
        document.body.style.overflow = '';
      };

      // -- MUSEUM PHOTO LIGHTBOX -----------------------------------------
      window.openMuseumLightbox = function(imgSrc, title, submitter) {
        document.getElementById('museum-lb-img').src = imgSrc;
        document.getElementById('museum-lb-info').innerHTML = '<strong>' + title + '</strong><br>Photographed by ' + submitter;
        var lb = document.getElementById('museum-photo-lightbox');
        lb.style.display = 'flex';
        document.body.style.overflow = 'hidden';
      };

      window.closeMuseumLightbox = function() {
        document.getElementById('museum-photo-lightbox').style.display = 'none';
        document.body.style.overflow = '';
      };

      // Pressing Escape closes any open lightbox.
      document.addEventListener('keydown', function(e) {
        if (e.key === 'Escape') {
          closeComparisonLightbox();
          closeMuseumLightbox();
        }
      });

      // -- 3D CARD TILT EFFECT ------------------------------------------
      // As the mouse moves over a painting card, it tilts in 3D toward the cursor.
      function initTilt() {
        document.querySelectorAll('.painting-card').forEach(function(card) {
          card.addEventListener('mousemove', function(e) {
            var rect = card.getBoundingClientRect();
            // getBoundingClientRect() returns the card's position and size on screen.
            var x = e.clientX - rect.left;
            var y = e.clientY - rect.top;
            // x and y = mouse position relative to the top-left of the card.
            var rotX = (y - rect.height / 2) / 25;
            var rotY = (rect.width / 2 - x) / 25;
            // Calculate rotation angles. Dividing by 25 keeps the tilt subtle.
            card.style.transform = 'perspective(800px) rotateX(' + rotX + 'deg) rotateY(' + rotY + 'deg) translateY(-6px)';
            // Apply the 3D transform. perspective(800px) sets how dramatic the 3D effect looks.
          });
          card.addEventListener('mouseleave', function() {
            card.style.transform = '';
            // Reset the card to its normal flat state when the mouse leaves.
          });
        });
      }

      // Re-run initTilt() after Shiny re-renders the painting cards,
      // because new DOM elements won't have the event listeners yet.
      $(document).on('shiny:value', function(e) {
        if (e.name === 'painting_cards') {
          setTimeout(initTilt, 100);
          // 100ms delay gives the DOM time to finish rendering before we attach listeners.
        }
      });
      setTimeout(initTilt, 500);
      // Also run once 500ms after page load for the initial render.

      // -- TAB SWITCHING FROM R ----------------------------------------
      // This handler listens for messages sent from the R server via
      // session$sendCustomMessage('switchTab', 'Gallery').
      // It then simulates a click on the matching navbar tab link.
      Shiny.addCustomMessageHandler('switchTab', function(tab) {
        var tabLink = document.querySelector('a.nav-link[data-value=\"' + tab + '\"]');
        if (tabLink) tabLink.click();
      });

      // -- LIVE GEOLOCATION ------------------------------------------------
      // Uses the browser Geolocation API with watchPosition to continuously
      // track the user and send lat/lng updates to Shiny.
      var geoWatchId = null;

      window.startGeolocation = function() {
        var btn = document.getElementById('locate_me_btn');

        // If already tracking, stop
        if (geoWatchId !== null) {
          navigator.geolocation.clearWatch(geoWatchId);
          geoWatchId = null;
          btn.classList.remove('tracking');
          Shiny.setInputValue('user_location', null);
          return;
        }

        if (!navigator.geolocation) {
          alert('Geolocation is not supported by your browser.');
          return;
        }

        btn.classList.add('tracking');

        geoWatchId = navigator.geolocation.watchPosition(
          function(pos) {
            Shiny.setInputValue('user_location', {
              lat: pos.coords.latitude,
              lng: pos.coords.longitude,
              acc: pos.coords.accuracy,
              t: Date.now()
            });
          },
          function(err) {
            console.warn('Geolocation error:', err.message);
            btn.classList.remove('tracking');
            geoWatchId = null;
            if (err.code === 1) {
              alert('Location access was denied. Please allow location access in your browser settings.');
            } else {
              alert('Unable to retrieve your location.');
            }
          },
          { enableHighAccuracy: true, maximumAge: 5000, timeout: 15000 }
        );
      };

      // -- FORM GEOLOCATION (one-shot) ------------------------------------
      // Uses getCurrentPosition to fill in the lat/lng fields on the
      // Contribute form. Unlike the map's watchPosition, this fires once.
      window.getFormLocation = function() {
        var statusEl = document.getElementById('location_status');
        var btn = document.getElementById('use_my_location_btn');

        if (!navigator.geolocation) {
          if (statusEl) statusEl.textContent = 'Geolocation not supported by your browser.';
          return;
        }

        if (statusEl) statusEl.textContent = 'Getting your location...';
        btn.style.opacity = '0.5';
        btn.style.pointerEvents = 'none';

        navigator.geolocation.getCurrentPosition(
          function(pos) {
            Shiny.setInputValue('submit_latitude', pos.coords.latitude);
            Shiny.setInputValue('submit_longitude', pos.coords.longitude);
            // Also update the visible numeric input fields
            $('#submit_latitude').val(pos.coords.latitude.toFixed(4));
            $('#submit_longitude').val(pos.coords.longitude.toFixed(4));
            if (statusEl) statusEl.textContent = 'Location set (' + pos.coords.latitude.toFixed(4) + ', ' + pos.coords.longitude.toFixed(4) + ')';
            btn.style.opacity = '1';
            btn.style.pointerEvents = 'auto';
          },
          function(err) {
            if (statusEl) statusEl.textContent = err.code === 1 ? 'Location access denied.' : 'Unable to get location.';
            btn.style.opacity = '1';
            btn.style.pointerEvents = 'auto';
          },
          { enableHighAccuracy: true, timeout: 15000 }
        );
      };

      // -- LEAFLET MAP FIX ----------------------------------------------
      // Leaflet maps don't render correctly when inside a hidden tab because
      // they calculate their size when the tab isn't visible yet (size = 0).
      // This fix fires a window resize event after the Map tab becomes visible,
      // which tells Leaflet to recalculate and correctly fill the container.
      $(document).on('shown.bs.tab', function(e) {
        if (e.target && e.target.getAttribute('data-value') === 'Map') {
          setTimeout(function() {
            window.dispatchEvent(new Event('resize'));
          }, 250);
        }
      });
    ")))
  )
)


################################################################################
# SERVER SECTION
# The server is the "back end" -- it contains all the logic that responds to
# user actions, reads/writes data, and generates dynamic content.
# It receives three arguments automatically from Shiny:
#   input  -- all current values from UI widgets (buttons, text fields, etc.)
#   output -- where you attach rendered content to send back to the UI
#   session -- the connection to the current user's browser session
################################################################################

server <- function(input, output, session) {
  
  # -- REACTIVE VALUES ------------------------------------------------------
  # reactiveValues() creates a shared "state" object that any part of the server
  # can read or write. Whenever a value changes, anything that depends on it
  # automatically re-runs (like a spreadsheet cell formula updating).
  rv <- reactiveValues(
    admin_auth = FALSE,
    submission_success = FALSE,
    submission_error = NULL,
    submissions = db_load_submissions(),
    # Single source of truth — approved items are filtered with
    # rv$submissions[rv$submissions$approval_status == "Approved", ]
    approved_trigger = 0,
    selected_marker = NULL,   # stores the ID of the currently clicked map marker
    selected_type = NULL,     # "painting" or "submission" to distinguish marker types
    filter_painting_id = NULL, # when set, the Compare tab only shows comparisons for this painting
    map_filter = "all",        # controls which markers are visible: "all", "paintings", or "submissions"
    gallery_filter = "all"     # controls gallery view: "all" or "museum_photos"
  )
  
  # UPDATED: Tracks which basemap is currently displayed to avoid unnecessary redraws
  current_basemap <- reactiveVal("minimal")
  
  
  # -- GALLERY / MAP "VIEW COMPARISONS" NAVIGATION ---------------------------
  # When a "View Comparisons" link is clicked from a gallery card or map panel,
  # this handler stores the painting ID to filter the Compare tab, then navigates.
  observeEvent(input$go_compare_painting, {
    val <- input$go_compare_painting
    # From gallery cards: a list with $id (painting ID) and $t (timestamp).
    # From map info panels: a raw numeric from Math.random().
    
    # Extract the painting ID depending on the format
    if (is.list(val) && !is.null(val$id)) {
      painting_id <- as.integer(val$id)
    } else if (is.numeric(val)) {
      painting_id <- val
    } else {
      painting_id <- NULL
    }
    
    # Only set the filter if it matches a real painting ID.
    # Otherwise, clear any existing filter so Compare shows all.
    if (!is.null(painting_id) && painting_id %in% paintings_data$id) {
      rv$filter_painting_id <- as.integer(painting_id)
    } else {
      rv$filter_painting_id <- NULL
    }
    
    session$sendCustomMessage("switchTab", "Compare")
  })
  
  # -- "SEE ALL" COMPARISONS RESET ------------------------------------------
  # Clears the painting filter so the Compare tab shows all comparisons again.
  observeEvent(input$clear_compare_filter, {
    rv$filter_painting_id <- NULL
  })
  
  # -- CONTRIBUTE BUTTON FROM GALLERY CARD ----------------------------------
  # When "+ Contribute" is clicked on a gallery card, pre-select that painting
  # in the Submit form dropdown and navigate to the Contribute tab.
  observeEvent(input$contribute_for_painting, {
    val <- input$contribute_for_painting
    if (!is.null(val$id)) {
      updateSelectInput(session, "submit_painting", selected = as.character(val$id))
    }
    session$sendCustomMessage("switchTab", "Contribute")
  })
  
  # -- STATS DISPLAY --------------------------------------------------------
  # These render the live submission counts shown in the hero stats strip.
  output$stat_submissions <- renderText({ as.character(nrow(rv$submissions)) })
  # nrow() counts how many rows are in the submissions data frame.
  # as.character() converts the number to a string for display.
  # This automatically re-runs whenever rv$submissions changes.
  
  output$stat_approved <- renderText({ as.character(nrow(rv$submissions[rv$submissions$approval_status == "Approved", ])) })
  
  
  # -- PAINTING CARDS --------------------------------------------------------
  # Dynamically generates one HTML card per painting from the CSV data.
  # Shows submission count per painting and a "View Comparisons" link
  # when approved comparisons exist for that painting.
  output$painting_cards <- renderUI({
    
    all_subs <- rv$submissions
    gallery_filter <- rv$gallery_filter
    
    sub_counts <- if (nrow(all_subs) > 0) {
      as.data.frame(table(all_subs$painting_id), stringsAsFactors = FALSE)
    } else {
      data.frame(Var1 = character(), Freq = integer(), stringsAsFactors = FALSE)
    }
    
    # Count APPROVED landscape submissions per painting (for "View Comparisons").
    approved_subs <- rv$submissions[rv$submissions$approval_status == "Approved" &
                                      (is.na(rv$submissions$submission_type) | rv$submissions$submission_type == "landscape"), ]
    approved_counts <- if (nrow(approved_subs) > 0) {
      as.data.frame(table(approved_subs$painting_id), stringsAsFactors = FALSE)
    } else {
      data.frame(Var1 = character(), Freq = integer(), stringsAsFactors = FALSE)
    }
    
    # Find approved MUSEUM PHOTO submissions per painting.
    museum_subs <- rv$submissions[rv$submissions$approval_status == "Approved" &
                                    !is.na(rv$submissions$submission_type) &
                                    rv$submissions$submission_type == "museum_photo", ]
    
    # Build a lookup: painting_id -> first approved museum photo submission
    museum_photo_lookup <- list()
    if (nrow(museum_subs) > 0) {
      for (j in 1:nrow(museum_subs)) {
        pid_char <- as.character(museum_subs[j, "painting_id"])
        if (is.null(museum_photo_lookup[[pid_char]])) {
          museum_photo_lookup[[pid_char]] <- museum_subs[j, ]
        }
      }
    }
    
    # Determine which paintings to show based on gallery filter.
    if (gallery_filter == "museum_photos") {
      # Only paintings that have at least one approved museum photo
      painting_ids_with_museum <- as.integer(names(museum_photo_lookup))
      display_data <- paintings_data[paintings_data$id %in% painting_ids_with_museum, ]
    } else {
      display_data <- paintings_data
    }
    
    if (nrow(display_data) == 0) {
      return(tags$div(class = "no-comparisons",
                      HTML("No paintings with museum photos yet. Be the first to contribute one!")
      ))
    }
    
    cards <- lapply(1:nrow(display_data), function(i) {
      p <- display_data[i, ]
      
      # Look up how many total submissions exist for this painting.
      count_match <- sub_counts[sub_counts$Var1 == as.character(p$id), "Freq"]
      sub_count <- if (length(count_match) > 0) count_match[1] else 0
      
      # Look up how many APPROVED landscape comparisons exist.
      approved_match <- approved_counts[approved_counts$Var1 == as.character(p$id), "Freq"]
      approved_count <- if (length(approved_match) > 0) approved_match[1] else 0
      
      # Check if a museum photo exists for this painting.
      museum_sub <- museum_photo_lookup[[as.character(p$id)]]
      has_museum_photo <- !is.null(museum_sub)
      
      tags$div(class = "painting-card",
               
               tags$div(class = "painting-card-img-wrap",
                        tags$img(src = p$image_url, class = "painting-image", alt = p$title),
                        tags$div(class = "painting-card-badge", p$year),
                        # Museum photo badge — clickable icon in bottom-right of image
                        if (has_museum_photo) {
                          tags$div(
                            class = "museum-photo-badge",
                            onclick = sprintf(
                              "event.stopPropagation(); openMuseumLightbox('%s', '%s', '%s');",
                              gsub("'", "\\\\'", museum_sub$photo_url),
                              gsub("'", "\\\\'", p$title),
                              gsub("'", "\\\\'", museum_sub$name)
                            ),
                            title = "View museum photo",
                            HTML("&#127963; Museum Photo")
                          )
                        }
               ),
               
               tags$div(class = "painting-info",
                        tags$h3(class = "painting-title", p$title),
                        tags$div(class = "painting-meta", paste0(p$artist)),
                        tags$p(class = "painting-context", p$context),
                        tags$div(class = "painting-card-footer",
                                 tags$div(class = "submission-count-badge",
                                          paste0(approved_count, " comparison", ifelse(approved_count != 1, "s", ""))
                                 ),
                                 tags$div(style = "display: flex; align-items: center; gap: 12px;",
                                          if (approved_count > 0) {
                                            tags$div(class = "painting-card-cta",
                                                     onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('go_compare_painting', {id: %d, t: Date.now()});", p$id),
                                                     HTML(paste0("View Comparison", ifelse(approved_count != 1, "s", ""), " &rarr;"))
                                            )
                                          },
                                          tags$div(class = "painting-card-cta",
                                                   style = "color: var(--sage-light);",
                                                   onclick = sprintf(
                                                     "event.stopPropagation(); Shiny.setInputValue('contribute_for_painting', {id: %d, t: Date.now()});",
                                                     p$id
                                                   ),
                                                   HTML("&#43; Contribute")
                                          )
                                 )
                        )
               )
      )
    })
    
    tagList(cards)
  })
  
  
  # -- MAP (UPDATED) --------------------------------------------------------
  # UPDATED: Replaced default pin markers with red/blue circle markers.
  # Changed base tiles to CartoDB.Positron for a cleaner look.
  # Added split layout with info panel, marker click detection, and
  # dynamic blue markers for approved user submissions.
  # Red circle markers for Bierstadt paintings, blue for approved submissions.
  # Clicking a marker updates the info panel on the right side of the layout.
  
  output$main_map <- renderLeaflet({
    # Base map only — markers are added reactively via leafletProxy
    # so they can be toggled by the filter buttons.
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "minimal") %>%
      setView(lng = -98.5, lat = 39.8, zoom = 4)
  })
  
  # -- MAP MARKER OBSERVER ---------------------------------------------------
  # Reactively draws painting markers (red) and submission markers (blue)
  # based on rv$map_filter. Uses leafletProxy to add/remove markers without
  # re-rendering the entire map.
  observe({
    approved <- rv$submissions[rv$submissions$approval_status == "Approved", ]
    rv$approved_trigger
    filter <- rv$map_filter
    artist_filter <- input$map_artist_filter  # "" means all artists
    
    # Filter paintings by artist if one is selected
    filtered_paintings <- if (!is.null(artist_filter) && artist_filter != "") {
      paintings_data[paintings_data$artist == artist_filter, ]
    } else {
      paintings_data
    }
    
    proxy <- leafletProxy("main_map")
    
    # -- PAINTING MARKERS (red) --
    proxy %>% clearGroup("paintings")
    if (filter %in% c("all", "paintings") && nrow(filtered_paintings) > 0) {
      proxy %>% addCircleMarkers(
        data = filtered_paintings,
        lng = ~longitude, lat = ~latitude,
        radius = 8,
        color = "#A85D3F",
        fillColor = "#C2714F",
        fillOpacity = 0.85,
        weight = 2,
        stroke = TRUE,
        group = "paintings",
        layerId = ~paste0("painting_", id),
        label = ~title,
        labelOptions = labelOptions(
          style = list("font-weight" = "600", "font-family" = "DM Sans, sans-serif"),
          textsize = "13px",
          direction = "top",
          offset = c(0, -12)
        )
      )
    }
    
    # -- SUBMISSION MARKERS (blue) --
    proxy %>% clearGroup("submissions")
    if (filter %in% c("all", "submissions") && nrow(approved) > 0) {
      valid_subs <- approved[!is.na(approved$latitude) & !is.na(approved$longitude), ]
      
      # If an artist filter is active, only show submissions linked to that artist's paintings
      if (!is.null(artist_filter) && artist_filter != "" && nrow(valid_subs) > 0) {
        artist_painting_ids <- filtered_paintings$id
        valid_subs <- valid_subs[valid_subs$painting_id %in% artist_painting_ids, ]
      }
      
      if (nrow(valid_subs) > 0) {
        valid_subs$painting_title <- sapply(valid_subs$painting_id, function(pid) {
          match_row <- paintings_data[paintings_data$id == pid, ]
          if (nrow(match_row) > 0) match_row$title[1] else "Unknown Location"
        })
        
        proxy %>% addCircleMarkers(
          data = valid_subs,
          lng = ~longitude, lat = ~latitude,
          radius = 8,
          color = "#2563EB",
          fillColor = "#3B82F6",
          fillOpacity = 0.85,
          weight = 2,
          stroke = TRUE,
          group = "submissions",
          layerId = ~paste0("submission_", submission_id),
          label = ~paste0(painting_title, " (", name, ")"),
          labelOptions = labelOptions(
            style = list("font-weight" = "600", "font-family" = "DM Sans, sans-serif"),
            textsize = "13px",
            direction = "top",
            offset = c(0, -10)
          )
        )
      }
    }
    
    # -- MUSEUM MARKERS (dark red) --
    proxy %>% clearGroup("museums")
    if (filter %in% c("all", "museums")) {
      museum_data <- filtered_paintings[!is.na(filtered_paintings$museum_latitude) & !is.na(filtered_paintings$museum_longitude), ]
      if (nrow(museum_data) > 0) {
        proxy %>% addCircleMarkers(
          data = museum_data,
          lng = ~museum_longitude, lat = ~museum_latitude,
          radius = 8,
          color = "#DC3545",
          fillColor = "#E25563",
          fillOpacity = 0.85,
          weight = 2,
          stroke = TRUE,
          group = "museums",
          layerId = ~paste0("museum_", id),
          label = ~paste0(title, " \u2014 ", museum_name),
          labelOptions = labelOptions(
            style = list("font-weight" = "600", "font-family" = "DM Sans, sans-serif"),
            textsize = "13px",
            direction = "top",
            offset = c(0, -10)
          )
        )
      }
    }
  })
  
  # -- MAP FILTER TOGGLE -----------------------------------------------------
  # Handles clicks on the filter buttons above the map.
  # Updates rv$map_filter which triggers the marker observer to redraw,
  # and sends JS to swap the active class on the buttons.
  observeEvent(input$set_map_filter, {
    new_filter <- input$set_map_filter
    if (new_filter %in% c("all", "paintings", "submissions", "museums")) {
      rv$map_filter <- new_filter
      # Update the active button styling via JS
      shinyjs::runjs(sprintf("
        document.querySelectorAll('.map-filter-btn').forEach(function(btn) { btn.classList.remove('active'); });
        document.getElementById('map_filter_%s').classList.add('active');
      ", new_filter))
    }
  })
  
  # -- GALLERY FILTER TOGGLE -----------------------------------------------
  # Handles clicks on the gallery filter buttons (All / Museum Photos).
  # Updates rv$gallery_filter and swaps the active class on the buttons.
  observeEvent(input$set_gallery_filter, {
    new_filter <- input$set_gallery_filter
    if (new_filter %in% c("all", "museum_photos")) {
      rv$gallery_filter <- new_filter
      shinyjs::runjs(sprintf("
        document.querySelectorAll('#gallery_filter_all, #gallery_filter_museum').forEach(function(btn) { btn.classList.remove('active'); });
        document.getElementById('%s').classList.add('active');
      ", ifelse(new_filter == "all", "gallery_filter_all", "gallery_filter_museum")))
    }
  })
  
  # -- USER LIVE LOCATION MARKER --------------------------------------------
  # Draws a pulsing blue dot on the map at the user's current GPS position.
  # Uses a custom divIcon with CSS animation. The watchPosition JS sends
  # updated coordinates continuously, and this observer redraws the marker.
  observeEvent(input$user_location, {
    loc <- input$user_location
    proxy <- leafletProxy("main_map")
    
    if (is.null(loc)) {
      # User toggled tracking off — remove the marker
      proxy %>% clearGroup("user_location")
      return()
    }
    
    proxy %>%
      clearGroup("user_location") %>%
      addMarkers(
        lng = loc$lng, lat = loc$lat,
        group = "user_location",
        icon = makeIcon(
          iconUrl = NULL,
          iconWidth = 18, iconHeight = 18,
          iconAnchorX = 9, iconAnchorY = 9
        ),
        options = markerOptions(interactive = FALSE)
      )
    
    # Replace the default marker with a pulsing custom div via JS
    shinyjs::runjs("
      (function() {
        var markers = document.querySelectorAll('.leaflet-marker-icon');
        // Find the user location marker (last added, no src)
        for (var i = markers.length - 1; i >= 0; i--) {
          var m = markers[i];
          if (!m.src || m.src === '' || m.src === window.location.href) {
            m.style.background = 'none';
            m.style.border = 'none';
            m.style.boxShadow = 'none';
            m.style.width = '40px';
            m.style.height = '40px';
            m.style.marginLeft = '-20px';
            m.style.marginTop = '-20px';
            m.innerHTML = '<div class=\"user-location-pulse\"><div class=\"ring\"></div><div class=\"dot\"></div></div>';
            break;
          }
        }
      })();
    ")
  })
  
  # -- MARKER CLICK -> INFO PANEL (UPDATED) ----------------------------------
  # UPDATED: New observeEvent() that detects marker clicks on the map.
  # Parses the layerId prefix to determine if a painting or submission
  # was clicked, then stores the selection in rv$selected_marker and
  # rv$selected_type so the info panel re-renders.
  observeEvent(input$main_map_marker_click, {
    click <- input$main_map_marker_click
    if (is.null(click) || is.null(click$id)) return()
    
    marker_id <- click$id
    
    if (grepl("^painting_", marker_id)) {
      pid <- as.integer(sub("painting_", "", marker_id))
      rv$selected_marker <- pid
      rv$selected_type <- "painting"
    } else if (grepl("^submission_", marker_id)) {
      sid <- sub("submission_", "", marker_id)
      rv$selected_marker <- sid
      rv$selected_type <- "submission"
    } else if (grepl("^museum_", marker_id)) {
      pid <- as.integer(sub("museum_", "", marker_id))
      rv$selected_marker <- pid
      rv$selected_type <- "museum"
    }
    
    leafletProxy("main_map") %>%
      flyTo(lng = click$lng, lat = click$lat, zoom = max(input$main_map_zoom, 8))
  })
  
  
  # -- GO TO MUSEUM OBSERVER -------------------------------------------------
  # When the user clicks "View Museum" in a painting's info panel, this flies
  # the map to the museum marker and switches the info panel to museum mode.
  observeEvent(input$go_to_museum, {
    pid <- input$go_to_museum$id
    if (is.null(pid)) return()
    
    p <- paintings_data[paintings_data$id == pid, ]
    if (nrow(p) == 0) return()
    p <- p[1, ]
    
    if (is.na(p$museum_latitude) || is.na(p$museum_longitude)) return()
    
    # Ensure museums are visible on the map
    if (!rv$map_filter %in% c("all", "museums")) {
      rv$map_filter <- "all"
      shinyjs::runjs("
        document.querySelectorAll('.map-filter-btn').forEach(function(btn) { btn.classList.remove('active'); });
        document.getElementById('map_filter_all').classList.add('active');
      ")
    }
    
    # Switch info panel to museum mode for this painting
    rv$selected_marker <- pid
    rv$selected_type <- "museum"
    
    # Fly to the museum location
    leafletProxy("main_map") %>%
      flyTo(lng = p$museum_longitude, lat = p$museum_latitude, zoom = max(input$main_map_zoom, 8))
  })
  
  # -- GO TO PAINTING OBSERVER -----------------------------------------------
  # When the user clicks "View Painting" in a museum's info panel, this flies
  # the map to the painting's landscape location and switches the info panel
  # to painting mode. Mirror of the go_to_museum observer above.
  observeEvent(input$go_to_painting, {
    pid <- input$go_to_painting$id
    if (is.null(pid)) return()
    
    p <- paintings_data[paintings_data$id == pid, ]
    if (nrow(p) == 0) return()
    p <- p[1, ]
    
    if (is.na(p$latitude) || is.na(p$longitude)) return()
    
    # Ensure paintings are visible on the map
    if (!rv$map_filter %in% c("all", "paintings")) {
      rv$map_filter <- "all"
      shinyjs::runjs("
        document.querySelectorAll('.map-filter-btn').forEach(function(btn) { btn.classList.remove('active'); });
        document.getElementById('map_filter_all').classList.add('active');
      ")
    }
    
    # Switch info panel to painting mode
    rv$selected_marker <- pid
    rv$selected_type <- "painting"
    
    # Fly to the painting's landscape location
    leafletProxy("main_map") %>%
      flyTo(lng = p$longitude, lat = p$latitude, zoom = max(input$main_map_zoom, 8))
  })
  
  # -- INFO PANEL CONTENT (UPDATED) ------------------------------------------
  # UPDATED: New renderUI() that populates the right-side info panel.
  # Four states:
  #   1. No marker selected: shows a placeholder with instructions.
  #   2. Painting marker: shows title, artist, year, image, context, coords.
  #   3. Submission marker: shows painting title, submitter, photo, observations, coords.
  #   4. Museum marker: shows museum name, painting housed there, image, coords.
  output$map_info_content <- renderUI({
    if (is.null(rv$selected_marker) || is.null(rv$selected_type)) {
      return(tags$div(class = "map-info-placeholder",
                      tags$div(class = "placeholder-icon", HTML("&#128205;")),
                      tags$p("Click a marker on the map to view location details.")
      ))
    }
    
    if (rv$selected_type == "painting") {
      p <- paintings_data[paintings_data$id == rv$selected_marker, ]
      if (nrow(p) == 0) return(NULL)
      p <- p[1, ]
      
      # Check if approved comparisons exist for this painting
      approved_for_painting <- rv$submissions[rv$submissions$approval_status == "Approved" & rv$submissions$painting_id == p$id, ]
      ap_count <- nrow(approved_for_painting)
      
      tagList(
        tags$div(class = "map-info-header",
                 tags$div(class = "map-info-dot painting"),
                 tags$span(class = "map-info-type-label", "Bierstadt Painting")
        ),
        tags$h3(class = "map-info-title", p$title),
        tags$div(class = "map-info-meta", paste0(p$artist, " | ", p$year)),
        tags$img(class = "map-info-image", src = p$image_url, alt = p$title),
        tags$p(class = "map-info-context", p$context),
        # Show "View Comparison(s)" button if approved comparisons exist
        if (ap_count > 0) {
          tags$div(class = "map-info-cta",
                   onclick = "Shiny.setInputValue('go_compare_painting', Math.random());",
                   HTML(paste0("View Comparison", ifelse(ap_count != 1, "s", ""), " &rarr;"))
          )
        },
        # Show "View Museum" button if museum location exists for this painting
        if (!is.null(p$museum_latitude) && !is.na(p$museum_latitude) &&
            !is.null(p$museum_longitude) && !is.na(p$museum_longitude)) {
          tags$div(class = "map-info-cta museum",
                   onclick = sprintf("Shiny.setInputValue('go_to_museum', {id: %d, t: Date.now()});", p$id),
                   HTML("View Museum &rarr;")
          )
        },
        tags$div(class = "map-info-coords",
                 tags$div(class = "coord-box",
                          tags$div(class = "coord-label", "Latitude"),
                          tags$div(class = "coord-value", round(p$latitude, 4))
                 ),
                 tags$div(class = "coord-box",
                          tags$div(class = "coord-label", "Longitude"),
                          tags$div(class = "coord-value", round(p$longitude, 4))
                 )
        )
      )
      
    } else if (rv$selected_type == "submission") {
      sub <- rv$submissions[rv$submissions$submission_id == rv$selected_marker & rv$submissions$approval_status == "Approved", ]
      if (nrow(sub) == 0) return(NULL)
      sub <- sub[1, ]
      
      painting <- paintings_data[paintings_data$id == sub$painting_id, ]
      painting_title <- if (nrow(painting) > 0) painting$title[1] else "Unknown Location"
      
      tagList(
        tags$div(class = "map-info-header",
                 tags$div(class = "map-info-dot submission"),
                 tags$span(class = "map-info-type-label", "Community Submission")
        ),
        tags$h3(class = "map-info-title", painting_title),
        tags$div(class = "map-info-meta", paste0("Submitted by ", sub$name, " | ", sub$submission_date)),
        tags$img(class = "map-info-image", src = sub$photo_url, alt = painting_title),
        if (!is.null(sub$observations) && sub$observations != "") {
          tags$div(class = "map-info-observations", sub$observations)
        },
        # Always show View Comparison since this IS an approved submission
        tags$div(class = "map-info-cta",
                 onclick = "Shiny.setInputValue('go_compare_painting', Math.random());",
                 HTML("View Comparison &rarr;")
        ),
        tags$div(class = "map-info-coords",
                 tags$div(class = "coord-box",
                          tags$div(class = "coord-label", "Latitude"),
                          tags$div(class = "coord-value", round(sub$latitude, 4))
                 ),
                 tags$div(class = "coord-box",
                          tags$div(class = "coord-label", "Longitude"),
                          tags$div(class = "coord-value", round(sub$longitude, 4))
                 )
        )
      )
    } else if (rv$selected_type == "museum") {
      p <- paintings_data[paintings_data$id == rv$selected_marker, ]
      if (nrow(p) == 0) return(NULL)
      p <- p[1, ]
      
      tagList(
        tags$div(class = "map-info-header",
                 tags$div(class = "map-info-dot museum"),
                 tags$span(class = "map-info-type-label", "Museum / Collection")
        ),
        tags$h3(class = "map-info-title", ifelse(!is.null(p$museum_name) && p$museum_name != "", p$museum_name, "Unknown Museum")),
        tags$div(class = "map-info-meta", paste0("Houses: ", p$title)),
        tags$img(class = "map-info-image",
                 src = if (!is.null(p$museum_image_url) && !is.na(p$museum_image_url) && p$museum_image_url != "") {
                   p$museum_image_url
                 } else {
                   p$image_url
                 },
                 alt = ifelse(!is.null(p$museum_name) && !is.na(p$museum_name), p$museum_name, p$title)),
        tags$p(class = "map-info-context", paste0("This museum or collection currently holds \"", p$title, "\" by ", p$artist, " (", p$year, ").")),
        # "View Painting" button — flies map to the painting's landscape location
        tags$div(class = "map-info-cta museum",
                 onclick = sprintf("Shiny.setInputValue('go_to_painting', {id: %d, t: Date.now()});", p$id),
                 HTML("View Painting &rarr;")
        ),
        tags$div(class = "map-info-coords",
                 tags$div(class = "coord-box",
                          tags$div(class = "coord-label", "Latitude"),
                          tags$div(class = "coord-value", round(p$museum_latitude, 4))
                 ),
                 tags$div(class = "coord-box",
                          tags$div(class = "coord-label", "Longitude"),
                          tags$div(class = "coord-value", round(p$museum_longitude, 4))
                 )
        )
      )
    }
  })
  
  outputOptions(output, "main_map", suspendWhenHidden = FALSE)
  # Keeps the map rendered even when the Map tab isn't visible
  
  # -- BASEMAP AUTO-SWITCH ------------------------------------------
  # UPDATED: Switches from minimal CartoDB tiles to Esri satellite imagery
  # when the user zooms in past level 8, and back when they zoom out.
  # Uses leafletProxy to swap tile layers without re-rendering the map.
  observeEvent(input$main_map_zoom, {
    zoom <- input$main_map_zoom
    if (is.null(zoom)) return()
    
    proxy <- leafletProxy("main_map")
    
    if (zoom >= 8 && current_basemap() != "satellite") {
      # Zoomed in -- switch to satellite imagery
      proxy %>%
        clearGroup("minimal") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "satellite")
      current_basemap("satellite")
      
    } else if (zoom < 8 && current_basemap() != "minimal") {
      # Zoomed out -- switch back to minimal tiles
      proxy %>%
        clearGroup("satellite") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "minimal")
      current_basemap("minimal")
    }
  })
  
  # Triggers a window resize event when the Map tab is opened
  observeEvent(input$main_tabs, {
    if (input$main_tabs == "Map") {
      shinyjs::delay(200, {
        shinyjs::runjs("window.dispatchEvent(new Event('resize'));")
      })
    }
    
    # Clear the comparison filter whenever the user navigates AWAY from Compare.
    # This way, returning to Compare later always shows all comparisons,
    # unless the user clicks "View Comparisons" from a gallery card again.
    if (input$main_tabs != "Compare") {
      rv$filter_painting_id <- NULL
    }
  })
  
  # -- SUBMISSION FORM MESSAGES --------------------------------------------
  # Renders a success or error alert above the form after a submission attempt.
  output$submit_message <- renderUI({
    if (rv$submission_success) {
      tags$div(class = "alert-success-custom",
               HTML("&#10003; Photo submitted successfully! It's pending admin review.")
               # &#10003; is the HTML code for the " checkmark character.
      )
    } else if (!is.null(rv$submission_error)) {
      # !is.null() checks that there IS an error message stored.
      tags$div(class = "alert-error-custom",
               HTML(paste0("&#10007; ", rv$submission_error))
               # &#10007; is the  cross/X character. paste0 appends the error message.
      )
    }
    # If neither condition is true, renderUI returns nothing (no message shown).
  })
  
  
  # -- FORM SUBMISSION HANDLER ----------------------------------------------
  # Runs when the Submit button is clicked. Validates all fields, then saves
  # the submission to disk if everything is valid.
  observeEvent(input$submit_button, {
    
    # Reset any previous success/error state before processing.
    rv$submission_success <- FALSE
    rv$submission_error <- NULL
    
    # -- VALIDATION ------------------------------------------------------
    # Validate based on submission type.
    sub_type <- input$submit_type
    
    # For landscape and museum_photo, an existing painting must be selected.
    if (sub_type %in% c("landscape", "museum_photo") && input$submit_painting == "") {
      rv$submission_error <- "Please select a painting."
      return()
    }
    # For user_painting, title and artist are required.
    if (sub_type == "user_painting") {
      if (is.null(input$submit_painting_title) || trimws(input$submit_painting_title) == "") {
        rv$submission_error <- "Please enter the painting title."
        return()
      }
      if (is.null(input$submit_artist_name) || trimws(input$submit_artist_name) == "") {
        rv$submission_error <- "Please enter the artist name."
        return()
      }
    }
    if (is.null(input$submit_photo)) {
      rv$submission_error <- "Please upload a photo."
      return()
    }
    if (is.na(input$submit_latitude) || is.na(input$submit_longitude)) {
      rv$submission_error <- "Please enter GPS coordinates or use the location button."
      return()
    }
    if (input$submit_photo$size > 5 * 1024 * 1024) {
      rv$submission_error <- "File must be less than 5MB."
      return()
    }
    
    # -- SAVE SUBMISSION --------------------------------------------------
    tryCatch({
      
      file_data <- readBin(input$submit_photo$datapath, "raw", file.info(input$submit_photo$datapath)$size)
      base64_image <- paste0("data:image/jpeg;base64,", base64enc::base64encode(file_data))
      
      # Build the painting_id: for landscape/museum_photo it comes from the dropdown;
      # for user_painting it's NA (no existing painting to link to).
      pid <- if (sub_type %in% c("landscape", "museum_photo")) {
        as.integer(input$submit_painting)
      } else {
        NA_integer_
      }
      
      new_submission <- data.frame(
        submission_id = as.character(as.integer(Sys.time())),
        name = ifelse(input$submit_name == "", "Anonymous", input$submit_name),
        email = input$submit_email,
        painting_id = pid,
        photo_url = base64_image,
        latitude = input$submit_latitude,
        longitude = input$submit_longitude,
        observations = input$submit_observations,
        submission_date = as.character(Sys.Date()),
        approval_status = "Pending",
        submission_type = sub_type,
        painting_title = if (sub_type == "user_painting") trimws(input$submit_painting_title) else NA_character_,
        artist_name = if (sub_type == "user_painting") trimws(input$submit_artist_name) else NA_character_,
        painting_year = if (sub_type == "user_painting") trimws(input$submit_painting_year) else NA_character_,
        painting_context = if (sub_type == "user_painting") trimws(input$submit_painting_context) else NA_character_,
        stringsAsFactors = FALSE
      )
      
      rv$submissions <- rbind(rv$submissions, new_submission)
      db_insert_submission(new_submission)
      # Writes the new submission to Supabase.
      
      rv$submission_success <- TRUE
      # Triggers the success message to appear above the form.
      
      # -- RESET FORM FIELDS --------------------------------------------
      # Clear all inputs back to their default/empty state after successful submit.
      updateTextInput(session, "submit_name", value = "")
      updateTextInput(session, "submit_email", value = "")
      updateSelectInput(session, "submit_painting", selected = "")
      updateNumericInput(session, "submit_latitude", value = NA)
      updateNumericInput(session, "submit_longitude", value = NA)
      updateTextAreaInput(session, "submit_observations", value = "")
      
    }, error = function(e) {
      # If anything above throws an error, this runs instead of crashing the app.
      rv$submission_error <- paste("Failed:", e$message)
      # e$message contains the technical error description from R.
    })
  })
  
  
  # -- COMPARISON GALLERY --------------------------------------------------
  # Generates the grid of approved photo thumbnails shown on the Compare tab.
  # When rv$filter_painting_id is set (from clicking "View Comparisons" on a
  # gallery card), only shows comparisons for that specific painting, along
  # with a banner showing which painting is being filtered and a "See All" button.
  output$comparison_gallery <- renderUI({
    rv$approved_trigger
    approved <- rv$submissions[rv$submissions$approval_status == "Approved", ]
    filter_id <- rv$filter_painting_id
    
    if (nrow(approved) == 0) {
      return(tags$div(class = "no-comparisons",
                      HTML("No approved comparisons yet. Be the first to contribute!")
      ))
    }
    
    # Apply filter if set
    if (!is.null(filter_id)) {
      filtered <- approved[approved$painting_id == filter_id, ]
      filter_painting <- paintings_data[paintings_data$id == filter_id, ]
      filter_name <- if (nrow(filter_painting) > 0) filter_painting$title[1] else "Unknown"
    } else {
      filtered <- approved
    }
    
    if (nrow(filtered) == 0) {
      # Filter is active but no comparisons match (shouldn't normally happen)
      return(tagList(
        tags$div(class = "compare-filter-banner",
                 tags$span(paste0("No comparisons found for this painting.")),
                 tags$div(class = "compare-filter-see-all",
                          onclick = "Shiny.setInputValue('clear_compare_filter', Math.random());",
                          HTML("See All Comparisons &rarr;"))
        )
      ))
    }
    
    cards <- lapply(1:nrow(filtered), function(i) {
      sub <- filtered[i, ]
      painting <- paintings_data[paintings_data$id == sub$painting_id, ]
      
      tags$div(class = "comparison-thumb",
               onclick = sprintf("openComparisonLightbox('%s', '%s')", painting$image_url, sub$photo_url),
               tags$div(class = "comparison-thumb-submitter", sub$name),
               # Submitter's name shown in the top-left corner of the thumbnail.
               tags$img(src = painting$image_url, alt = painting$title),
               tags$div(class = "comparison-thumb-overlay",
                        tags$div(class = "comparison-thumb-label", HTML("&#8644; Compare"))
               )
      )
    })
    
    # Build the final output: filter banner (if filtered) + grid
    tagList(
      # Show the filter banner only when a specific painting is being viewed
      if (!is.null(filter_id)) {
        tags$div(class = "compare-filter-banner",
                 tags$span(class = "compare-filter-text",
                           HTML(paste0("Showing comparisons for <strong>", htmltools::htmlEscape(filter_name), "</strong>"))
                 ),
                 tags$div(class = "compare-filter-see-all",
                          onclick = "Shiny.setInputValue('clear_compare_filter', Math.random());",
                          HTML("See All Comparisons &rarr;"))
        )
      },
      tags$div(class = "comparison-grid", cards)
    )
  })
  
  
  # -- ADMIN AUTHENTICATION ------------------------------------------------
  observeEvent(input$admin_login, {
    # Runs when the "Sign In" button on the Login tab is clicked.
    if (input$admin_password == "admin123") rv$admin_auth <- TRUE
    # Simple hardcoded password check. Sets admin_auth to TRUE on success.
    # NOTE: This is not secure for production -- password should be hashed/stored safely.
  })
  
  output$admin_authenticated <- reactive({ rv$admin_auth })
  # Exposes the authentication state to the UI so conditionalPanel() can use it.
  # reactive() creates a reactive expression that re-evaluates when rv$admin_auth changes.
  
  outputOptions(output, "admin_authenticated", suspendWhenHidden = FALSE)
  # Same as with the map -- prevents Shiny from pausing this output when the tab is hidden,
  # since conditionalPanel needs it available even when Login tab isn't active.
  
  
  # -- ADMIN TABLE ----------------------------------------------------------
  # Renders the interactive data table of all submissions for the admin to review.
  # -- ADMIN FILTERED DATA --------------------------------------------------
  # Reactive expression that filters submissions based on admin dropdown selections.
  # Used by both the table and the approve/reject/delete handlers to ensure
  # row indices always match the displayed table.
  admin_filtered <- reactive({
    input$refresh_admin
    subs <- rv$submissions
    if (nrow(subs) == 0) return(subs)
    
    # Filter by submission type
    type_filter <- input$admin_type_filter
    if (!is.null(type_filter) && type_filter != "") {
      subs <- subs[!is.na(subs$submission_type) & subs$submission_type == type_filter, ]
    }
    
    # Filter by approval status
    status_filter <- input$admin_status_filter
    if (!is.null(status_filter) && status_filter != "") {
      subs <- subs[subs$approval_status == status_filter, ]
    }
    
    subs
  })
  
  output$admin_table <- renderDT({
    filtered <- admin_filtered()
    
    if (nrow(filtered) == 0) return(datatable(data.frame(Message = "No submissions match the current filters."), rownames = FALSE))
    
    # For user_painting submissions, show painting_title instead of painting_id
    display <- filtered[, c("submission_id", "name", "submission_type", "painting_id", "latitude", "longitude", "submission_date", "approval_status")]
    
    # Replace painting_id with painting title for readability
    display$painting_id <- sapply(display$painting_id, function(pid) {
      if (is.na(pid)) return("(user painting)")
      match_row <- paintings_data[paintings_data$id == pid, ]
      if (nrow(match_row) > 0) match_row$title[1] else as.character(pid)
    })
    names(display)[names(display) == "painting_id"] <- "painting"
    
    datatable(
      display,
      options = list(
        pageLength = 25,
        order = list(list(6, 'desc'))
        # Sort by submission_date descending (column index 6, 0-based).
      ),
      rownames = FALSE,
      selection = 'single'
    )
  })
  
  
  # -- APPROVE SUBMISSION --------------------------------------------------
  # Uses admin_filtered() to map the selected table row back to the correct
  # submission_id, since filters may cause row indices to differ from rv$submissions.
  observeEvent(input$approve_submission, {
    if (length(input$admin_table_rows_selected) > 0) {
      idx <- input$admin_table_rows_selected
      filtered <- admin_filtered()
      if (idx > nrow(filtered)) return()
      sid <- filtered[idx, "submission_id"]
      
      # Update in Supabase
      db_update_status(sid, "Approved")
      
      # Update in-memory by matching submission_id
      rv$submissions[rv$submissions$submission_id == sid, "approval_status"] <- "Approved"
      rv$approved_trigger <- rv$approved_trigger + 1
      
      showNotification("Approved!", type = "message")
    }
  })
  
  
  # -- REJECT SUBMISSION ------------------------------------------------------
  observeEvent(input$reject_submission, {
    if (length(input$admin_table_rows_selected) > 0) {
      idx <- input$admin_table_rows_selected
      filtered <- admin_filtered()
      if (idx > nrow(filtered)) return()
      sid <- filtered[idx, "submission_id"]
      
      db_update_status(sid, "Rejected")
      rv$submissions[rv$submissions$submission_id == sid, "approval_status"] <- "Rejected"
      
      showNotification("Rejected.", type = "warning")
    }
  })
  
  # -- DELETE SUBMISSION ------------------------------------------------------
  observeEvent(input$delete_submission, {
    if (length(input$admin_table_rows_selected) > 0) {
      idx <- input$admin_table_rows_selected
      filtered <- admin_filtered()
      if (idx > nrow(filtered)) return()
      sid <- filtered[idx, "submission_id"]
      
      # Delete from Supabase
      db_delete_submission(sid)
      
      # Remove from in-memory data frame by submission_id
      rv$submissions <- rv$submissions[rv$submissions$submission_id != sid, ]
      rv$approved_trigger <- rv$approved_trigger + 1
      
      showNotification("Deleted.", type = "error")
    }
  })
  
  
  # -- REFRESH ADMIN DATA --------------------------------------------------
  observeEvent(input$refresh_admin, {
    # Re-reads submissions from Supabase.
    # Useful if data was modified externally or by another admin session.
    rv$submissions <- db_load_submissions()
    rv$approved_trigger <- rv$approved_trigger + 1
    showNotification("Data refreshed from database!", type = "message")
  })
}


################################################################################
# LAUNCH THE APP
# shinyApp() wires the UI and server together and starts the application.
# When you click "Run App" in RStudio, this is the line that actually
# launches the web server and opens the browser.
################################################################################
shinyApp(ui = ui, server = server)