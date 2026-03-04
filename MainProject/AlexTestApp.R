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

library(shiny)
library(bslib)
library(leaflet)
library(htmltools)
library(DT)
library(shinyjs)

############################################
# User Inupt Data Storage
# This code manages local storage for the app:
# 1. Sets up a local folder "app_data" to store data files.
# 2. Defines paths for submissions and approved data (.rds files).
# 3. Creates the folder if it doesn't exist.
# 4. Provides functions to load data from an .rds file (or create an empty
#    data frame if the file doesn't exist) and to save data back to an .rds file.
#
# Non local data storage for final application?
############################################

LocalDataDirectory <- "app_data"
SUBMISSIONS_FILE <- file.path(LocalDataDirectory, "submissions.rds")
APPROVED_FILE <- file.path(LocalDataDirectory, "approved.rds")

if (!dir.exists(LocalDataDirectory)) dir.create(LocalDataDirectory, recursive = TRUE)

load_data <- function(file_path) {
  if (file.exists(file_path)) {
    readRDS(file_path)
  } else {
    data.frame(
      submission_id = character(), name = character(), email = character(),
      painting_id = integer(), photo_url = character(),
      latitude = numeric(), longitude = numeric(),
      observations = character(), submission_date = character(),
      approval_status = character(), stringsAsFactors = FALSE
    )
  }
}

save_data <- function(data, file_path) saveRDS(data, file_path)

############################################
# Painting Data
# Imports painting data csv locally
# reads CSV and creates data frame
# How can we make this dynamic?
############################################

paintings_csv <- "BPaintings.csv"
paintings_data <- read.csv(paintings_csv, stringsAsFactors = FALSE)

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
  margin-top: 14px;
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
"

################################################################################
# UI SECTION
################################################################################

ui <- page_navbar(
  useShinyjs(),
  title = NULL,
  id = "main_tabs",
  theme = bs_theme(
    version = 5,
    bg = "#0f1a14",
    fg = "#FFFFFF",
    primary = "#E8976B",
    secondary = "#7FA88A",
    success = "#7FA88A",
    info = "#E2B94C",
    base_font = font_google("DM Sans"),
    heading_font = font_google("DM Serif Display")
  ),
  header = tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700&family=DM+Serif+Display&display=swap", rel = "stylesheet"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML(app_css))
  ),
  
  # -- TAB 1: HOME --
  nav_panel(
    title = "Home",
    icon = icon("mountain-sun"),
    tags$div(class = "hero-banner",
             tags$div(class = "hero-bg-pattern"),
             tags$div(class = "hero-glow hero-glow-1"),
             tags$div(class = "hero-glow hero-glow-2"),
             tags$div(class = "hero-glow hero-glow-3"),
             tags$div(class = "hero-inner",
                      tags$h1(class = "hero-title", HTML("Landscape<br>Through Time")),
                      tags$p(class = "hero-subtitle", "Explore Albert Bierstadt's iconic paintings."),
                      tags$div(class = "hero-actions",
                               actionButton("go_gallery", HTML("View the Collection &rarr;"), class = "btn-terra"),
                               actionButton("go_submit", "Contribute a Photo", class = "btn-sage")
                      ),
                      tags$div(class = "stats-strip",
                               tags$div(class = "stat-item",
                                        tags$div(class = "stat-value", as.character(nrow(paintings_data))),
                                        tags$div(class = "stat-label", "Paintings")
                               )
                      )
             )
    )
  ),
  
  # -- TAB 2: GALLERY --
  nav_panel(
    title = "Gallery",
    icon = icon("images"),
    tags$div(class = "section-header",
             tags$h2("The Collection"),
             tags$div(class = "accent-line")
    ),
    tags$div(class = "gallery-wrap",
             tags$div(id = "paintings-container", class = "paintings-grid",
                      uiOutput("painting_cards")
             )
    )
  ),
  
  # -- TAB 3: MAP --
  nav_panel(
    title = "Map",
    icon = icon("map-location-dot"),
    tags$div(class = "section-header",
             tags$h2("Explore Locations"),
             tags$p("See where Bierstadt set up his easel across America. Click a marker for details."),
             tags$div(class = "accent-line")
    ),
    tags$div(class = "map-filter-bar",
             tags$div(class = "map-filter-btn active", id = "map_filter_all",
                      onclick = "Shiny.setInputValue('set_map_filter', 'all');", "All"),
             tags$div(class = "map-filter-btn", id = "map_filter_paintings",
                      onclick = "Shiny.setInputValue('set_map_filter', 'paintings');",
                      tags$span(class = "legend-dot red"), "Paintings"),
             tags$div(class = "map-filter-btn", id = "map_filter_submissions",
                      onclick = "Shiny.setInputValue('set_map_filter', 'submissions');",
                      tags$span(class = "legend-dot blue"), "Submissions")
    ),
    tags$div(class = "map-split-layout",
             tags$div(class = "map-container",
                      leafletOutput("main_map", height = "100%")
             ),
             tags$div(class = "map-info-panel",
                      uiOutput("map_info_content")
             )
    )
  ),
  
  # -- TAB 4: SUBMIT --
  nav_panel(
    title = "Submit",
    icon = icon("camera"),
    tags$div(class = "section-header",
             tags$h2("Contribute Your Photo"),
             tags$p("Visit a Bierstadt painting location and share what it looks like today."),
             tags$div(class = "accent-line")
    ),
    tags$div(class = "form-wrap",
             tags$div(class = "form-card",
                      uiOutput("submit_message"),
                      tags$div(class = "form-group",
                               textInput("submit_name", "Your Name (optional)", placeholder = "Jane Doe")
                      ),
                      tags$div(class = "form-group",
                               textInput("submit_email", "Email (optional)", placeholder = "jane@university.edu")
                      ),
                      tags$div(class = "form-group",
                               selectInput("submit_painting", "Which location did you visit?",
                                           choices = c("Select a location..." = "", setNames(paintings_data$id, paintings_data$title)))
                      ),
                      tags$div(class = "form-group",
                               tags$label("Upload Your Photo"),
                               tags$div(class = "upload-zone",
                                        tags$div(class = "upload-icon", HTML("&#128247;")),
                                        tags$p(style = "...", "Drag & drop or click to browse"),
                                        fileInput("submit_photo", NULL, accept = c("image/png", "image/jpeg", "image/jpg"))
                               )
                      ),
                      tags$div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 16px;",
                               tags$div(class = "form-group",
                                        numericInput("submit_latitude", "Latitude", value = NA, step = 0.0001)
                               ),
                               tags$div(class = "form-group",
                                        numericInput("submit_longitude", "Longitude", value = NA, step = 0.0001)
                               )
                      ),
                      tags$div(class = "form-group",
                               textAreaInput("submit_observations", "Observations (optional)", rows = 3,
                                             placeholder = "What did you notice about how the landscape has changed?")
                      ),
                      actionButton("submit_button", HTML("Submit Photo &rarr;"), class = "btn-submit")
             )
    )
  ),
  
  # -- TAB 5: COMPARE --
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
    )
  ),
  
  nav_spacer(),
  
  # -- THEME TOGGLE --
  nav_item(
    tags$button(
      id = "theme_toggle", class = "theme-toggle",
      title = "Toggle light/dark mode", onclick = "toggleTheme()",
      HTML("&#9788;")
    )
  ),
  
  # -- TAB 6: ADMIN LOGIN --
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
               tags$div(class = "admin-login-card",
                        tags$h3("Sign In"),
                        tags$p("Enter admin credentials to manage submissions."),
                        passwordInput("admin_password", NULL, placeholder = "Password"),
                        actionButton("admin_login", "Sign In", class = "btn-submit")
               )
             ),
             conditionalPanel(
               condition = "output.admin_authenticated == true",
               tags$div(class = "admin-toolbar",
                        actionButton("refresh_admin", "Refresh", class = "btn btn-refresh"),
                        actionButton("approve_submission", "Approve Selected", class = "btn btn-approve"),
                        actionButton("reject_submission", "Reject Selected", class = "btn btn-reject"),
                        actionButton("delete_submission", "Delete Selected", class = "btn btn-reject",
                                     style = "background: #8B0000; border-color: #8B0000; color: white;")
               ),
               DTOutput("admin_table")
             )
    )
  ),
  
  # -- LIGHTBOXES AND JAVASCRIPT --
  footer = tagList(
    tags$div(id = "comparison-lightbox",
             tags$div(class = "lightbox-close", onclick = "closeComparisonLightbox()",
                      style = "position: fixed; top: 24px; right: 24px; z-index: 10002;", HTML("&times;")),
             tags$div(class = "comparison-container",
                      tags$div(class = "comparison-side",
                               tags$div(class = "comparison-label", "Historical"),
                               tags$img(id = "comp-historical", src = "", draggable = "false")
                      ),
                      tags$div(class = "comparison-side",
                               tags$div(class = "comparison-label", "Present Day"),
                               tags$img(id = "comp-modern", src = "", draggable = "false")
                      )
             )
    ),
    tags$script(HTML(paste0("
      window.toggleTheme = function() {
        document.body.classList.toggle('light-mode');
        var btn = document.getElementById('theme_toggle');
        if (document.body.classList.contains('light-mode')) {
          btn.innerHTML = '&#9789;';
        } else {
          btn.innerHTML = '&#9788;';
        }
      };
      var paintingsData = ", jsonlite::toJSON(paintings_data, auto_unbox = TRUE), ";
      window.openComparisonLightbox = function(historicalUrl, modernUrl) {
        document.getElementById('comp-historical').src = historicalUrl;
        document.getElementById('comp-modern').src = modernUrl;
        document.getElementById('comparison-lightbox').classList.add('active');
        document.body.style.overflow = 'hidden';
        var sides = document.querySelectorAll('.comparison-side img');
        sides.forEach(function(img) {
          img.addEventListener('wheel', function(e) {
            e.preventDefault();
            var current = parseFloat(img.style.transform.replace('scale(', '').replace(')', '') || 1);
            var delta = e.deltaY * -0.01;
            var scale = Math.max(1, Math.min(3, current + delta));
            sides.forEach(function(s) { s.style.transform = 'scale(' + scale + ')'; });
          });
        });
      };
      window.closeComparisonLightbox = function() {
        document.getElementById('comparison-lightbox').classList.remove('active');
        document.body.style.overflow = '';
      };
      document.addEventListener('keydown', function(e) {
        if (e.key === 'Escape') { closeComparisonLightbox(); }
      });
      function initTilt() {
        document.querySelectorAll('.painting-card').forEach(function(card) {
          card.addEventListener('mousemove', function(e) {
            var rect = card.getBoundingClientRect();
            var x = e.clientX - rect.left;
            var y = e.clientY - rect.top;
            var rotX = (y - rect.height / 2) / 25;
            var rotY = (rect.width / 2 - x) / 25;
            card.style.transform = 'perspective(800px) rotateX(' + rotX + 'deg) rotateY(' + rotY + 'deg) translateY(-6px)';
          });
          card.addEventListener('mouseleave', function() {
            card.style.transform = '';
          });
        });
      }
      $(document).on('shiny:value', function(e) {
        if (e.name === 'painting_cards') { setTimeout(initTilt, 100); }
      });
      setTimeout(initTilt, 500);
      Shiny.addCustomMessageHandler('switchTab', function(tab) {
        var tabLink = document.querySelector('a.nav-link[data-value=\"' + tab + '\"]');
        if (tabLink) tabLink.click();
      });
      $(document).on('shown.bs.tab', function(e) {
        if (e.target && e.target.getAttribute('data-value') === 'Map') {
          setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 250);
        }
      });
    ")))
  )
)

################################################################################
# SERVER SECTION
################################################################################

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    admin_auth = FALSE,
    submission_success = FALSE,
    submission_error = NULL,
    submissions = load_data(SUBMISSIONS_FILE),
    approved = load_data(APPROVED_FILE),
    approved_trigger = 0,
    selected_marker = NULL,
    selected_type = NULL,
    filter_painting_id = NULL,
    map_filter = "all"
  )
  
  current_basemap <- reactiveVal("minimal")
  
  observeEvent(input$go_gallery, { session$sendCustomMessage("switchTab", "Gallery") })
  observeEvent(input$go_submit, { session$sendCustomMessage("switchTab", "Submit") })
  
  observeEvent(input$go_compare_painting, {
    val <- input$go_compare_painting
    if (is.list(val) && !is.null(val$id)) {
      painting_id <- as.integer(val$id)
    } else if (is.numeric(val)) {
      painting_id <- val
    } else {
      painting_id <- NULL
    }
    if (!is.null(painting_id) && painting_id %in% paintings_data$id) {
      rv$filter_painting_id <- as.integer(painting_id)
    } else {
      rv$filter_painting_id <- NULL
    }
    session$sendCustomMessage("switchTab", "Compare")
  })
  
  # -- NEW: CONTRIBUTE PHOTO BUTTON HANDLER --
  observeEvent(input$go_contribute_painting, {
    val <- input$go_contribute_painting
    if (is.list(val) && !is.null(val$id)) {
      updateSelectInput(session, "submit_painting", selected = as.character(val$id))
    }
    session$sendCustomMessage("switchTab", "Submit")
  })
  
  observeEvent(input$clear_compare_filter, { rv$filter_painting_id <- NULL })
  
  output$stat_submissions <- renderText({ as.character(nrow(rv$submissions)) })
  output$stat_approved <- renderText({ as.character(nrow(rv$approved)) })
  
  # -- PAINTING CARDS --
  output$painting_cards <- renderUI({
    all_subs <- rv$submissions
    sub_counts <- if (nrow(all_subs) > 0) {
      as.data.frame(table(all_subs$painting_id), stringsAsFactors = FALSE)
    } else {
      data.frame(Var1 = character(), Freq = integer(), stringsAsFactors = FALSE)
    }
    approved_subs <- rv$approved
    approved_counts <- if (nrow(approved_subs) > 0) {
      as.data.frame(table(approved_subs$painting_id), stringsAsFactors = FALSE)
    } else {
      data.frame(Var1 = character(), Freq = integer(), stringsAsFactors = FALSE)
    }
    
    cards <- lapply(1:nrow(paintings_data), function(i) {
      p <- paintings_data[i, ]
      count_match <- sub_counts[sub_counts$Var1 == as.character(p$id), "Freq"]
      sub_count <- if (length(count_match) > 0) count_match[1] else 0
      approved_match <- approved_counts[approved_counts$Var1 == as.character(p$id), "Freq"]
      approved_count <- if (length(approved_match) > 0) approved_match[1] else 0
      
      tags$div(class = "painting-card",
               tags$div(class = "painting-card-img-wrap",
                        tags$img(src = p$image_url, class = "painting-image", alt = p$title),
                        tags$div(class = "painting-card-badge", p$year)
               ),
               tags$div(class = "painting-info",
                        tags$h3(class = "painting-title", p$title),
                        tags$div(class = "painting-meta", paste0(p$artist, " \u2022 ", p$year)),
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
                                                   onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('go_contribute_painting', {id: %d, t: Date.now()});", p$id),
                                                   HTML("&#43; Contribute Photo")
                                          )
                                 )
                        )
               )
      )
    })
    
    tagList(cards)
  })
  
  # -- MAP --
  output$main_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "minimal") %>%
      setView(lng = -98.5, lat = 39.8, zoom = 4)
  })
  
  observe({
    approved <- rv$approved
    rv$approved_trigger
    filter <- rv$map_filter
    proxy <- leafletProxy("main_map")
    proxy %>% clearGroup("paintings")
    if (filter %in% c("all", "paintings")) {
      proxy %>% addCircleMarkers(
        data = paintings_data, lng = ~longitude, lat = ~latitude,
        radius = 10, color = "#A85D3F", fillColor = "#C2714F", fillOpacity = 0.85,
        weight = 2, stroke = TRUE, group = "paintings",
        layerId = ~paste0("painting_", id), label = ~title,
        labelOptions = labelOptions(
          style = list("font-weight" = "600", "font-family" = "DM Sans, sans-serif"),
          textsize = "13px", direction = "top", offset = c(0, -12)
        )
      )
    }
    proxy %>% clearGroup("submissions")
    if (filter %in% c("all", "submissions") && nrow(approved) > 0) {
      valid_subs <- approved[!is.na(approved$latitude) & !is.na(approved$longitude), ]
      if (nrow(valid_subs) > 0) {
        valid_subs$painting_title <- sapply(valid_subs$painting_id, function(pid) {
          match_row <- paintings_data[paintings_data$id == pid, ]
          if (nrow(match_row) > 0) match_row$title[1] else "Unknown Location"
        })
        proxy %>% addCircleMarkers(
          data = valid_subs, lng = ~longitude, lat = ~latitude,
          radius = 8, color = "#2563EB", fillColor = "#3B82F6", fillOpacity = 0.85,
          weight = 2, stroke = TRUE, group = "submissions",
          layerId = ~paste0("submission_", submission_id),
          label = ~paste0(painting_title, " (", name, ")"),
          labelOptions = labelOptions(
            style = list("font-weight" = "600", "font-family" = "DM Sans, sans-serif"),
            textsize = "13px", direction = "top", offset = c(0, -10)
          )
        )
      }
    }
  })
  
  observeEvent(input$set_map_filter, {
    new_filter <- input$set_map_filter
    if (new_filter %in% c("all", "paintings", "submissions")) {
      rv$map_filter <- new_filter
      shinyjs::runjs(sprintf("
        document.querySelectorAll('.map-filter-btn').forEach(function(btn) { btn.classList.remove('active'); });
        document.getElementById('map_filter_%s').classList.add('active');
      ", new_filter))
    }
  })
  
  observeEvent(input$main_map_marker_click, {
    click <- input$main_map_marker_click
    if (is.null(click) || is.null(click$id)) return()
    marker_id <- click$id
    if (grepl("^painting_", marker_id)) {
      rv$selected_marker <- as.integer(sub("painting_", "", marker_id))
      rv$selected_type <- "painting"
    } else if (grepl("^submission_", marker_id)) {
      rv$selected_marker <- sub("submission_", "", marker_id)
      rv$selected_type <- "submission"
    }
  })
  
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
      approved_for_painting <- rv$approved[rv$approved$painting_id == p$id, ]
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
        if (ap_count > 0) {
          tags$div(class = "map-info-cta",
                   onclick = "Shiny.setInputValue('go_compare_painting', Math.random());",
                   HTML(paste0("View Comparison", ifelse(ap_count != 1, "s", ""), " &rarr;"))
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
      sub <- rv$approved[rv$approved$submission_id == rv$selected_marker, ]
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
    }
  })
  
  outputOptions(output, "main_map", suspendWhenHidden = FALSE)
  
  observeEvent(input$main_map_zoom, {
    zoom <- input$main_map_zoom
    if (is.null(zoom)) return()
    proxy <- leafletProxy("main_map")
    if (zoom >= 8 && current_basemap() != "satellite") {
      proxy %>% clearGroup("minimal") %>% addProviderTiles(providers$Esri.WorldImagery, group = "satellite")
      current_basemap("satellite")
    } else if (zoom < 8 && current_basemap() != "minimal") {
      proxy %>% clearGroup("satellite") %>% addProviderTiles(providers$CartoDB.Positron, group = "minimal")
      current_basemap("minimal")
    }
  })
  
  observeEvent(input$main_tabs, {
    if (input$main_tabs == "Map") {
      shinyjs::delay(200, { shinyjs::runjs("window.dispatchEvent(new Event('resize'));") })
    }
    if (input$main_tabs != "Compare") { rv$filter_painting_id <- NULL }
  })
  
  output$submit_message <- renderUI({
    if (rv$submission_success) {
      tags$div(class = "alert-success-custom",
               HTML("&#10003; Photo submitted successfully! It's pending admin review."))
    } else if (!is.null(rv$submission_error)) {
      tags$div(class = "alert-error-custom",
               HTML(paste0("&#10007; ", rv$submission_error)))
    }
  })
  
  observeEvent(input$submit_button, {
    rv$submission_success <- FALSE
    rv$submission_error <- NULL
    if (input$submit_painting == "") { rv$submission_error <- "Please select a location."; return() }
    if (is.null(input$submit_photo)) { rv$submission_error <- "Please upload a photo."; return() }
    if (is.na(input$submit_latitude) || is.na(input$submit_longitude)) { rv$submission_error <- "Please enter GPS coordinates."; return() }
    if (input$submit_photo$size > 5 * 1024 * 1024) { rv$submission_error <- "File must be less than 5MB."; return() }
    tryCatch({
      file_data <- readBin(input$submit_photo$datapath, "raw", file.info(input$submit_photo$datapath)$size)
      base64_image <- paste0("data:image/jpeg;base64,", base64enc::base64encode(file_data))
      new_submission <- data.frame(
        submission_id = as.character(as.integer(Sys.time())),
        name = ifelse(input$submit_name == "", "Anonymous", input$submit_name),
        email = input$submit_email,
        painting_id = as.integer(input$submit_painting),
        photo_url = base64_image,
        latitude = input$submit_latitude,
        longitude = input$submit_longitude,
        observations = input$submit_observations,
        submission_date = as.character(Sys.Date()),
        approval_status = "Pending",
        stringsAsFactors = FALSE
      )
      rv$submissions <- rbind(rv$submissions, new_submission)
      save_data(rv$submissions, SUBMISSIONS_FILE)
      rv$submission_success <- TRUE
      updateTextInput(session, "submit_name", value = "")
      updateTextInput(session, "submit_email", value = "")
      updateSelectInput(session, "submit_painting", selected = "")
      updateNumericInput(session, "submit_latitude", value = NA)
      updateNumericInput(session, "submit_longitude", value = NA)
      updateTextAreaInput(session, "submit_observations", value = "")
    }, error = function(e) {
      rv$submission_error <- paste("Failed:", e$message)
    })
  })
  
  output$comparison_gallery <- renderUI({
    rv$approved_trigger
    approved <- rv$approved
    filter_id <- rv$filter_painting_id
    if (nrow(approved) == 0) {
      return(tags$div(class = "no-comparisons", HTML("No approved comparisons yet. Be the first to contribute!")))
    }
    if (!is.null(filter_id)) {
      filtered <- approved[approved$painting_id == filter_id, ]
      filter_painting <- paintings_data[paintings_data$id == filter_id, ]
      filter_name <- if (nrow(filter_painting) > 0) filter_painting$title[1] else "Unknown"
    } else {
      filtered <- approved
    }
    if (nrow(filtered) == 0) {
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
               tags$img(src = painting$image_url, alt = painting$title),
               tags$div(class = "comparison-thumb-overlay",
                        tags$div(class = "comparison-thumb-label", HTML("&#8644; Compare"))
               )
      )
    })
    tagList(
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
  
  observeEvent(input$admin_login, {
    if (input$admin_password == "admin123") rv$admin_auth <- TRUE
  })
  
  output$admin_authenticated <- reactive({ rv$admin_auth })
  outputOptions(output, "admin_authenticated", suspendWhenHidden = FALSE)
  
  output$admin_table <- renderDT({
    input$refresh_admin
    if (nrow(rv$submissions) == 0) return(data.frame(Message = "No submissions yet"))
    datatable(
      rv$submissions[, c("submission_id", "name", "painting_id", "latitude", "longitude", "submission_date", "approval_status")],
      options = list(pageLength = 25, order = list(list(6, 'desc'))),
      rownames = FALSE, selection = 'single'
    )
  })
  
  observeEvent(input$approve_submission, {
    if (length(input$admin_table_rows_selected) > 0) {
      idx <- input$admin_table_rows_selected
      sub <- rv$submissions[idx, ]
      sub$approval_status <- "Approved"
      rv$approved <- rbind(rv$approved, sub)
      save_data(rv$approved, APPROVED_FILE)
      rv$submissions[idx, "approval_status"] <- "Approved"
      save_data(rv$submissions, SUBMISSIONS_FILE)
      showNotification("Approved!", type = "message")
    }
  })
  
  observeEvent(input$reject_submission, {
    if (length(input$admin_table_rows_selected) > 0) {
      idx <- input$admin_table_rows_selected
      rv$submissions[idx, "approval_status"] <- "Rejected"
      save_data(rv$submissions, SUBMISSIONS_FILE)
      showNotification("Rejected.", type = "warning")
    }
  })
  
  observeEvent(input$delete_submission, {
    if (length(input$admin_table_rows_selected) > 0) {
      idx <- input$admin_table_rows_selected
      deleted_id <- rv$submissions[idx, "submission_id"]
      rv$submissions <- rv$submissions[-idx, ]
      save_data(rv$submissions, SUBMISSIONS_FILE)
      rv$approved <- rv$approved[rv$approved$submission_id != deleted_id, ]
      save_data(rv$approved, APPROVED_FILE)
      rv$approved_trigger <- rv$approved_trigger + 1
      showNotification("Deleted.", type = "error")
    }
  })
  
  observeEvent(input$refresh_admin, {
    rv$submissions <- load_data(SUBMISSIONS_FILE)
    rv$approved <- load_data(APPROVED_FILE)
    showNotification("Data refreshed!", type = "message")
  })
}

shinyApp(ui = ui, server = server)
