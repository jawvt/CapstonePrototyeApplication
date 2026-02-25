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

paintings_csv <- "/Users/jawvt/Documents/Capstone/CapstonePrototyeApplication/MainProject/BPaintings.csv"
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
/* #########################################
   CSS VARIABLES - EARTHY PALETTE
     - Colors, shades, and tints
     - Border radii for rounded corners
     - Shadow styles for consistent elevation
     - Easing function for smooth transitions
     - Makes it easy to maintain a consistent look and update styles site-wide.
   ######################################### */
:root {
  --terra: #C2714F;
  --terra-dark: #A85D3F;
  --terra-light: #D4917A;
  --sage: #6B8F71;
  --sage-dark: #567360;
  --sage-light: #8FB396;
  --amber: #D4A843;
  --amber-light: #EBC76D;
  --forest: #1E3328;
  --forest-mid: #2A4A38;
  --sand: #F5EDE0;
  --sand-dark: #E8DCCB;
  --cream: #FBF8F3;
  --charcoal: #2D2D2D;
  --charcoal-light: #4A4A4A;
  --white: #FFFFFF;
  --radius-sm: 10px;
  --radius-md: 16px;
  --radius-lg: 24px;
  --shadow-sm: 0 2px 8px rgba(30, 51, 40, 0.08);
  --shadow-md: 0 8px 30px rgba(30, 51, 40, 0.12);
  --shadow-lg: 0 16px 50px rgba(30, 51, 40, 0.18);
  --ease: cubic-bezier(0.4, 0, 0.2, 1);
}

/* #########################################
   RESET & BASE
   ######################################### */
* { margin: 0; padding: 0; box-sizing: border-box; }

html { scroll-behavior: smooth; }

body {
  font-family: 'DM Sans', -apple-system, BlinkMacSystemFont, sans-serif;
  background: var(--cream);
  color: var(--charcoal);
  line-height: 1.6;
  overflow-x: hidden;
}

/* #########################################
   NAVBAR OVERRIDES
   Custom styles to override default Bootstrap navbar
   ######################################### */

.navbar {
  background: var(--forest) !important;
  /* Dark forest background color for navbar */

  border-bottom: 4px solid var(--white) !important;
  /* white-colored bottom border for accent */

  padding: 0 !important;
  /* Remove default padding */

  min-height: 64px;
  /* Set a consistent minimum height */

  box-shadow: 0 4px 20px rgba(30, 51, 40, 0.25);
  /* Add subtle shadow for depth */

  position: sticky;
  /* Keep navbar fixed at the top when scrolling */

  top: 2;
  /* Stick to the top edge */

  z-index: 999;
  /* Ensure navbar stays above other elements */
}

.navbar-nav .nav-link {
  font-family: 'DM Sans', sans-serif !important;
  /* Use custom font for navbar links */

  font-weight: 600 !important;
  /* Bold text for readability */

  font-size: 14px !important;
  /* Standard font size for links */

  color: rgba(245, 237, 224, 0.75) !important;
  /* Light text color */

  padding: 20px 20px !important;
  /* Spacing around links */

  letter-spacing: 0.5px;
  /* Small spacing between letters */

  text-transform: uppercase;
  /* Uppercase text for style consistency */

  transition: all 0.3s var(--ease);
  /* Smooth hover and active transitions */

  border-bottom: 3px solid transparent;
  /* Space for active link underline */ 

  margin-bottom: -3px;
  /* Align underline with navbar bottom */

  position: relative;
  /* Needed for pseudo-elements or active underline positioning */
}

.navbar-nav .nav-link:hover {
  color: var(--sand) !important;
  /* Change text color on hover */

  background: rgba(255,255,255,0.1);
  /* Subtle background highlight on hover */
}

.navbar-nav .nav-link.active,
.navbar-nav .nav-item.active .nav-link,
.navbar-nav .nav-link[aria-selected='true'] {
  color: var(--white) !important;
  /* White text for active link */

  border-bottom-color: var(--white) !important;
  /* Terra-colored underline for active link */

  background: rgba(194, 113, 79, 0.1);
  /* Slight highlight behind active link */
}

/* Hamburger toggle for mobile */
.navbar-toggler {
  border-color: rgba(245, 237, 224, 0.3) !important;
  /* Light border around hamburger button */

  color: var(--sand) !important;
  /* Hamburger icon color */

  margin-right: 16px;
  /* Space from the right edge */
}

.navbar-toggler-icon {
  filter: invert(1);
  /* Invert icon color so it's visible on dark background */
}


/* #########################################
   TAB CONTENT WRAPPER
   ######################################### */

.tab-content {
  background: var(--cream);
  /* Sets the background color of the tab content area using the --cream variable */
  
}

.tab-pane {
  animation: tabFadeIn 0.4s var(--ease);
  /* Applies the tabFadeIn animation over 0.4 seconds using the custom easing curve --ease */
  padding: 0 !important;
  margin: 0 !important;
}
}

@keyframes tabFadeIn {
  from { 
    opacity: 0; 
    /* Start fully transparent */
    transform: translateY(12px); 
    /* Start slightly below final position */
  }
  to { 
    opacity: 1; 
    /* End fully visible */
    transform: translateY(0); 
    /* End at original position */
  }
  /* Creates a smooth fade-in and upward slide effect for tab panes */
}

/* #########################################
   HERO / HOME TAB
   ######################################### */

.hero-banner {
  position: relative;
  /* Position relative to contain absolute children like glows */

  background: linear-gradient(135deg, var(--forest) 100%, var(--forest-mid) 50%, var(--forest) 100%);
  /* Gradient background from forest to terra-dark */

  min-height: calc(100vh - 64px);
  /* Full viewport height minus navbar height */

  display: flex;
  /* Use flexbox for centering content */

  align-items: center;
  /* Vertically center content */

  justify-content: center;
  /* Horizontally center content */

  overflow: hidden;
  /* Hide anything that overflows hero boundaries */
}

.hero-bg-pattern {
  position: absolute;
  /* Positioned over the hero-banner but behind content */

  inset: 0;
  /* Stretch to fill the parent container */

}

.hero-glow {
  position: absolute;
  /* Positioned behind hero content */

  width: 1600px;
  height: 1600px;
  /* Size of glow */

  border-radius: 50%;
  /* Make it a circle */

  filter: blur(120px);
  /* Blur for soft glow effect */

  opacity: 0.15;
  /* Low opacity for subtle effect */

  pointer-events: none;
  /* Glow doesn't block clicks */
}

.hero-glow-1 {
  background: var(--white);
  /* Terra-colored glow */

  top: -200px;
  right: -100px;
  /* Positioned top-right outside hero */
}

.hero-glow-2 {
  background: var(--sage);
  /* Sage-colored glow */

  bottom: -200px;
  left: -100px;
  /* Positioned bottom-left outside hero */
}

.hero-glow-3 {
  background: var(--amber);
  /* Amber-colored glow */

  top: 50%;
  left: 50%;
  /* Centered in hero */

  transform: translate(-50%, -50%);
  /* Offset to perfectly center */

  width: 400px;
  height: 400px;
  /* Smaller glow */

  opacity: 0.08;
  /* Very subtle */
}

.hero-inner {
  position: relative;
  /* So z-index works for stacking */

  z-index: 2;
  /* Bring content above glows and patterns */

  text-align: center;
  /* Center text inside */

  max-width: 800px;
  /* Limit width for readability */

  padding: 60px 32px;
  /* Inner spacing */
}

.hero-badge {
  display: inline-block;
  /* Shrink to content width */

  background: rgba(194, 113, 79, 0.2);
  /* Semi-transparent background */

  border: 1px solid rgba(194, 113, 79, 0.4);
  /* Subtle border matching background */

  color: var(--terra-light);
  /* Text color */

  font-size: 12px;
  /* Small text */

  font-weight: 700;
  /* Bold */

  text-transform: uppercase;
  /* Uppercase letters */

  letter-spacing: 2.5px;
  /* Space between letters */

  padding: 8px 22px;
  /* Padding inside badge */

  border-radius: 50px;
  /* Rounded edges */

  margin-bottom: 32px;
  /* Space below badge */

  animation: fadeUp 0.8s var(--ease) 0.1s both;
  /* Slide-up fade animation with delay */
}

.hero-title {
  font-family: 'DM Serif Display', Georgia, serif;
  /* Serif font for hero title */

  font-size: clamp(40px, 7vw, 80px);
  /* Responsive font size between 40px and 80px */

  font-weight: 400;
  /* Normal weight */

  color: var(--white);
  /* White text */

  line-height: 1.1;
  /* Tight line spacing */

  margin-bottom: 24px;
  /* Space below title */

  animation: fadeUp 0.8s var(--ease) 0.25s both;
  /* Slide-up fade animation with slight delay */
}

.hero-title span {
  background: linear-gradient(135deg, var(--terra-light) 0%, var(--amber-light) 100%);
  /* Gradient for highlighted text */

  -webkit-background-clip: text;
  /* Clip background to text for webkit browsers */

  -webkit-text-fill-color: transparent;
  /* Make text itself transparent so background shows */

  background-clip: text;
  /* Standard property for gradient text */
}

.hero-subtitle {
  font-size: clamp(16px, 2vw, 20px);
  /* Responsive subtitle size */

  color: rgba(245, 237, 224, 1);
  /* Slightly transparent text */

  max-width: 600px;
  /* Limit width for readability */

  margin: 0 auto 44px;
  /* Center horizontally with bottom margin */

  line-height: 1.7;
  /* Taller line spacing for readability */

  animation: fadeUp 0.8s var(--ease) 0.4s both;
  /* Fade-up animation with delay */
}

.hero-actions {
  display: flex;
  /* Flex layout for buttons/links */

  gap: 16px;
  /* Space between buttons */

  justify-content: center;
  /* Center horizontally */

  flex-wrap: wrap;
  /* Wrap on smaller screens */

  animation: fadeUp 0.8s var(--ease) 0.55s both;
  /* Fade-up animation with delay */
}

@keyframes fadeUp {
  from { opacity: 0; transform: translateY(24px); }
  /* Start transparent and slightly below */

  to { opacity: 1; transform: translateY(0); }
  /* End fully visible at normal position */
}

/* Stats strip */
.stats-strip {
  display: grid;
  /* Use grid for equal-width stats */

  grid-template-columns: repeat(3, 1fr);
  /* Three columns */

  gap: 2px;
  /* Small gap between stats */

  margin-top: 70px;
  /* Space above stats */

  background: rgba(255,255,255,0.05);
  /* Slightly visible background */

  border-radius: var(--radius-lg);
  /* Rounded corners */

  overflow: hidden;
  /* Clip children if overflow */

  animation: fadeUp 0.8s var(--ease) 0.7s both;
  /* Fade-up animation */
}

.stat-item {
  padding: 28px 20px;
  /* Padding inside each stat box */

  text-align: center;
  /* Center text */

  backdrop-filter: blur(10px);
  /* Glass effect blur */

  background: rgba(255,255,255,0.03);
  /* Slight translucent background */

  transition: background 0.3s;
  /* Smooth hover transition */
}

.stat-item:hover {
  background: rgba(255,255,255,0.06);
  /* Slightly brighter background on hover */
}

.stat-value {
  font-family: 'DM Serif Display', Georgia, serif;
  /* Serif font for number */

  font-size: 42px;
  /* Large number */

  color: var(--amber-light);
  /* Bright color for emphasis */

  line-height: 1;
  /* Tight line spacing */

  margin-bottom: 6px;
  /* Space below number */
}

.stat-label {
  font-size: 11px;
  /* Small label */

  text-transform: uppercase;
  /* Uppercase letters */

  letter-spacing: 2px;
  /* Space between letters */

  color: rgba(245, 237, 224, 1);
  /* Semi-transparent label text */

  font-weight: 600;
  /* Slightly bold */
}

/* #########################################
   BUTTON STYLES
  
   This section styles custom buttons with different color themes:
   - .btn-terra: bold terra gradient buttons
   - .btn-sage: outlined sage buttons
   - .btn-submit: full-width forest-themed submit buttons
   Includes hover and active effects for interactivity.
   ######################################### */


/* ===== Terra Gradient Button (used for primary actions in hero) ===== */

.btn-terra {
  background: linear-gradient(135deg, var(--terra) 0%, var(--terra-dark) 100%);
  /* Primary gradient background */

  color: var(--white) !important;
  /* White text for contrast */

  border: none;
  /* No border */

  padding: 16px 36px;
  /* Button spacing */

  font-size: 15px;
  font-weight: 700;
  border-radius: 50px;
  /* Pill-shaped button */

  cursor: pointer;
  transition: all 0.3s var(--ease);
  text-decoration: none;
  display: inline-flex;
  align-items: center;
  gap: 8px;
  box-shadow: 0 4px 15px rgba(194, 113, 79, 0.3);
  /* Subtle shadow for depth */
}

.btn-terra:hover {
  transform: translateY(-2px) scale(1.03);
  box-shadow: 0 8px 25px rgba(194, 113, 79, 0.4);
  /* Hover lift + stronger shadow */
}

.btn-terra:active {
  transform: translateY(0) scale(0.98);
  /* Pressed effect */
}


/* ===== Sage Outline Button (used for secondary actions in hero) ===== */

.btn-sage {
  background: transparent;
  color: var(--sage-light) !important;
  border: 2px solid var(--sage);
  padding: 14px 34px;
  font-size: 15px;
  font-weight: 700;
  border-radius: 50px;
  cursor: pointer;
  transition: all 0.3s var(--ease);
  /* Simple outlined button with hover effect */
}

.btn-sage:hover {
  background: rgba(107, 143, 113, 0.15);
  border-color: var(--sage-light);
  transform: translateY(-2px);
  /* Lift + subtle background on hover */
}


/* ===== Submit Button (used in forms, not hero) ===== */


.btn-submit {
  background: linear-gradient(135deg, var(--forest-mid) 0%, var(--forest) 100%);
  color: var(--white) !important;
  border: none;
  padding: 16px 40px;
  font-size: 16px;
  font-weight: 700;
  border-radius: var(--radius-md);
  cursor: pointer;
  transition: all 0.3s var(--ease);
  width: 100%;
  box-shadow: 0 4px 15px rgba(30, 51, 40, 0.25);
  /* Full-width submit button for forms */
}

.btn-submit:hover {
  transform: translateY(-2px);
  box-shadow: 0 8px 25px rgba(30, 51, 40, 0.35);
  /* Hover lift + stronger shadow */
}


/* ######################################### 

   SECTION HEADERS
   
   Styles section headers with centered titles, subtitles, and a small accent line.
   
   ######################################### */

.section-header {
  text-align: center;
  /* Center all content inside the header */

  padding: 20px 24px 40px;
  /* Vertical and horizontal spacing around the section header */
}

.section-header h2 {
  font-family: 'DM Serif Display', Georgia, serif;
  /* Serif font for the main title */

  font-size: clamp(32px, 5vw, 52px);
  /* Responsive font size between 32px and 52px */

  color: var(--forest);
  /* Dark green title color */

  margin-bottom: 12px;
  /* Space below the title */

  line-height: 1.15;
  /* Tight line spacing for compact title */
}

.section-header p {
  font-size: 17px;
  /* Subtitle / description text size */

  color: var(--charcoal-light);
  /* Lighter text color for contrast */

  max-width: 560px;
  /* Limit width for readability */

  margin: 0 auto;
  /* Center subtitle horizontally */

  line-height: 1.6;
  /* Comfortable line spacing */
}

.section-header .accent-line {
  width: 200px;
  /* Width of the small line under the header */

  height: 5px;
  /* Thickness of the accent line */

  background: linear-gradient(90deg, var(--terra), var(--terra));
  /* Gradient color for visual emphasis */

  border-radius: 2px;
  /* Slightly rounded ends */

  margin: 16px auto 0;
  /* Center the line with spacing above */
}


/* ######################################### 
   PAINTING CARDS

   Styles a responsive gallery of painting cards with hover effects, image zoom,
   overlay gradient, badges, and textual info.
   ######################################### */

.gallery-wrap {
  padding: 0 24px 60px;
  /* Space around the gallery */

  max-width: 1400px;
  /* Limit gallery width */

  margin: 0 auto;
  /* Center gallery */
}

.paintings-grid {
  display: grid;
  /* Grid layout for cards */

  grid-template-columns: repeat(auto-fill, minmax(380px, 1fr));
  /* Responsive columns with minimum 380px width */

  gap: 32px;
  /* Space between cards */
}

.painting-card {
  position: relative;
  cursor: pointer;
  border-radius: var(--radius-lg);
  overflow: hidden;
  background: var(--white);
  box-shadow: var(--shadow-md);
  transition: all 0.4s var(--ease);
  transform-style: preserve-3d;
  /* Basic card container with rounded edges and shadow */
}

.painting-card:hover {
  transform: translateY(-6px);
  box-shadow: var(--shadow-lg);
  /* Hover lift + stronger shadow */
}

.painting-card-img-wrap {
  position: relative;
  overflow: hidden;
  aspect-ratio: 16 / 10;
  /* Image container maintains ratio */
}

.painting-card-img-wrap::after {
  content: '';
  position: absolute;
  inset: 0;
  background: linear-gradient(to top, rgba(30,51,40,0.6) 0%, transparent 50%);
  opacity: 0;
  transition: opacity 0.4s;
  /* Overlay gradient for hover effect */
}

.painting-card:hover .painting-card-img-wrap::after {
  opacity: 1;
  /* Show gradient on hover */
}

.painting-image {
  width: 100%;
  height: 100%;
  object-fit: cover;
  display: block;
  transition: transform 0.6s var(--ease);
  /* Image fills container and transitions on hover */
}

.painting-card:hover .painting-image {
  transform: scale(1.06);
  /* Slight zoom on hover */
}

.painting-card-badge {
  position: absolute;
  top: 16px;
  right: 16px;
  background: rgba(30, 51, 40, 0.8);
  backdrop-filter: blur(8px);
  color: var(--amber-light);
  font-size: 12px;
  font-weight: 700;
  padding: 6px 14px;
  border-radius: 20px;
  z-index: 2;
  /* Small badge on top-right of card */
}

.painting-info {
  padding: 24px;
  /* Space around text content */
}

.painting-title {
  font-family: 'DM Serif Display', Georgia, serif;
  font-size: 22px;
  color: var(--forest);
  margin-bottom: 6px;
  line-height: 1.3;
  /* Painting title styling */
}

.painting-metadata {
  color: var(--terra);
  font-size: 13px;
  font-weight: 600;
  margin-bottom: 12px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  /* Metadata (like year/artist) styling */
}

.painting-context {
  color: var(--charcoal-light);
  font-size: 14px;
  line-height: 1.65;
  /* Description text styling */
}

.painting-card-cta {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  color: var(--terra);
  font-weight: 700;
  font-size: 13px;
  text-transform: uppercase;
  letter-spacing: 1px;
  margin-top: 14px;
  transition: gap 0.3s;
  /* Call-to-action link styling */
}

.painting-card:hover .painting-card-cta {
  gap: 10px;
  /* Slight animation on hover */
}

/* ######################################### 
   MAP - SPLIT LAYOUT (UPDATED)
   
   UPDATED: Replaced single centered map with a two-column grid layout.
   Map on the left with red circle markers for paintings and blue circles
   for user submissions. Info panel on the right shows details when a
   marker is clicked. Includes a color legend and responsive breakpoint.
   ######################################### */

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
  border-radius: 24px;
  overflow: hidden;
  box-shadow: 0 20px 60px rgba(0, 0, 0, 0.3);
  height: 650px;
  width: 100%;
}

.leaflet-container {
  height: 100%;
  border-radius: 24px;
}

/* --- Map Info Panel (right side) --- */
.map-info-panel {
  background: var(--white);
  border-radius: var(--radius-lg);
  box-shadow: var(--shadow-md);
  border: 1px solid var(--sand-dark);
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
  color: var(--charcoal-light);
}

.map-info-placeholder .placeholder-icon {
  font-size: 48px;
  margin-bottom: 16px;
  opacity: 0.4;
}

.map-info-placeholder p {
  font-size: 16px;
  line-height: 1.6;
  max-width: 280px;
}

.map-info-header {
  display: flex;
  align-items: center;
  gap: 10px;
  margin-bottom: 20px;
  padding-bottom: 16px;
  border-bottom: 2px solid var(--sand-dark);
}

.map-info-dot {
  width: 14px;
  height: 14px;
  border-radius: 50%;
  flex-shrink: 0;
}

.map-info-dot.painting { background: var(--terra); }
.map-info-dot.submission { background: #3B82F6; }

.map-info-type-label {
  font-size: 11px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 1.5px;
  color: var(--charcoal-light);
}

.map-info-title {
  font-family: 'DM Serif Display', Georgia, serif;
  font-size: 24px;
  color: var(--forest);
  margin-bottom: 8px;
  line-height: 1.3;
}

.map-info-meta {
  color: var(--terra);
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
  box-shadow: var(--shadow-sm);
  max-height: 220px;
  object-fit: cover;
}

.map-info-context {
  color: var(--charcoal-light);
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
  background: var(--cream);
  border: 1px solid var(--sand-dark);
  border-radius: var(--radius-sm);
  padding: 12px 14px;
  text-align: center;
}

.coord-label {
  font-size: 10px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 1.5px;
  color: var(--charcoal-light);
  margin-bottom: 4px;
}

.coord-value {
  font-family: 'DM Serif Display', Georgia, serif;
  font-size: 18px;
  color: var(--forest);
}

/* --- Map Legend --- */
.map-legend {
  display: flex;
  gap: 24px;
  justify-content: center;
  margin-bottom: 24px;
}

.legend-item {
  display: flex;
  align-items: center;
  gap: 8px;
  font-size: 13px;
  font-weight: 600;
  color: var(--charcoal-light);
}

.legend-dot {
  width: 12px;
  height: 12px;
  border-radius: 50%;
  border: 2px solid rgba(0,0,0,0.15);
}

.legend-dot.red { background: var(--terra); }
.legend-dot.blue { background: #3B82F6; }

/* --- Submission info extras --- */
.map-info-observations {
  background: var(--cream);
  border-left: 3px solid #3B82F6;
  padding: 14px 16px;
  border-radius: 0 var(--radius-sm) var(--radius-sm) 0;
  margin-bottom: 16px;
  font-size: 14px;
  color: var(--charcoal);
  line-height: 1.6;
  font-style: italic;
}

.map-info-submitter {
  font-size: 13px;
  color: var(--charcoal-light);
  margin-bottom: 16px;
}

/* --- Responsive override for map layout --- */
@media (max-width: 1024px) {
  .map-split-layout {
    grid-template-columns: 1fr;
  }
  .map-info-panel {
    height: auto;
    max-height: 500px;
  }
}


/* ######################################### 
   FORM
   
   Styles forms with a centered card layout, input fields, labels, and upload zones.
   Includes hover/focus effects for better usability and visual feedback.
   
   ######################################### */

.form-wrap {
  padding: 0 24px 60px;
  /* Space around the form */

  max-width: 660px;
  /* Limit form width */

  margin: 0 auto;
  /* Center form horizontally */
}

.form-card {
  background: var(--white);
  /* Card background */

  padding: 48px;
  /* Inner spacing */

  border-radius: var(--radius-lg);
  /* Rounded edges */

  box-shadow: var(--shadow-md);
  /* Card shadow */

  border: 1px solid var(--sand-dark);
  /* Subtle border */
}

.form-card .form-group {
  margin-bottom: 24px;
  /* Space between input groups */
}

.form-card label,
.form-card .control-label {
  font-weight: 600;
  color: var(--forest);
  font-size: 14px;
  margin-bottom: 8px;
  display: block;
  /* Label styling */
}

.form-card .form-control,
.form-card .shiny-input-container input[type='text'],
.form-card .shiny-input-container input[type='number'],
.form-card .shiny-input-container select,
.form-card .shiny-input-container textarea {
  width: 100%;
  padding: 14px 16px;
  border: 2px solid var(--sand-dark);
  border-radius: var(--radius-sm);
  font-size: 15px;
  font-family: 'DM Sans', sans-serif;
  transition: all 0.3s var(--ease);
  background: var(--cream);
  color: var(--charcoal);
  /* Input field styling */
}

.form-card .form-control:focus,
.form-card input:focus,
.form-card select:focus,
.form-card textarea:focus {
  outline: none;
  border-color: var(--sage);
  box-shadow: 0 0 0 4px rgba(107, 143, 113, 0.15);
  background: var(--white);
  /* Focus effect for inputs */
}

.upload-zone {
  border: 3px dashed var(--sand-dark);
  border-radius: var(--radius-md);
  padding: 40px 24px;
  text-align: center;
  transition: all 0.3s var(--ease);
  background: var(--cream);
  cursor: pointer;
  /* Drag-and-drop upload area styling */
}

.upload-zone:hover {
  border-color: var(--sage);
  background: rgba(107, 143, 113, 0.04);
  /* Hover effect for upload area */
}

.upload-icon {
  font-size: 36px;
  margin-bottom: 8px;
  /* Icon inside upload zone */
}

/* #########################################
   COMPARISONS
   
   Styles a responsive grid of comparison thumbnails with hover zoom, overlay,
   and labels. Also styles a message when no comparisons are available.
   
   ######################################### */

.comparison-wrap {
  padding: 0 24px 60px;
  /* Space around the comparison section */

  max-width: 1400px;
  /* Limit section width */

  margin: 0 auto;
  /* Center section */
}

.comparison-grid {
  display: grid;
  /* Grid layout for thumbnails */

  grid-template-columns: repeat(auto-fill, minmax(320px, 1fr));
  /* Responsive columns with minimum width 320px */

  gap: 24px;
  /* Space between thumbnails */
}

.comparison-thumb {
  position: relative;
  aspect-ratio: 16 / 10;
  border-radius: var(--radius-md);
  overflow: hidden;
  cursor: pointer;
  box-shadow: var(--shadow-sm);
  transition: all 0.4s var(--ease);
  /* Thumbnail container with rounded edges, shadow, and hover animation */
}

.comparison-thumb:hover {
  transform: translateY(-6px) scale(1.02);
  box-shadow: var(--shadow-lg);
  /* Hover lift and stronger shadow */
}

.comparison-thumb img {
  width: 100%;
  height: 100%;
  object-fit: cover;
  transition: transform 0.5s var(--ease);
  /* Image fills container and transitions on hover */
}

.comparison-thumb:hover img {
  transform: scale(1.08);
  /* Slight zoom effect on hover */
}

.comparison-thumb-overlay {
  position: absolute;
  inset: 0;
  background: linear-gradient(to top, rgba(30,51,40,0.7) 0%, transparent 60%);
  display: flex;
  align-items: flex-end;
  padding: 20px;
  opacity: 0;
  transition: opacity 0.3s;
  /* Overlay with gradient and bottom-aligned content, hidden by default */
}

.comparison-thumb:hover .comparison-thumb-overlay {
  opacity: 1;
  /* Show overlay on hover */
}

.comparison-thumb-label {
  color: var(--white);
  font-weight: 700;
  font-size: 14px;
  display: flex;
  align-items: center;
  gap: 6px;
  /* Label text on overlay */
}

.no-comparisons {
  text-align: center;
  padding: 80px 24px;
  color: var(--charcoal-light);
  font-size: 18px;
  /* Message when no comparisons exist */
}

 
 /* #########################################
   LIGHTBOX (Paintings)
   
   This CSS creates a full-screen lightbox overlay for displaying paintings.
   It includes a fade-in dark overlay, centered animated image, interactive
   close button, and contextual metadata panel that animates into view.
   Only necessary lines are explained below.

   ######################################### */

#lightbox {
  display: none; 
  /* Keeps the lightbox hidden until activated */

  position: fixed; 
  /* Locks the lightbox to the viewport so it stays in place while scrolling */

  inset: 0; 
  /* Shorthand for top/right/bottom/left: 0 -- makes it cover the full screen */

  background: rgba(10, 15, 12, 0.97); 
  /* Creates a nearly opaque dark overlay behind the image */

  z-index: 10000; 
  /* Ensures the lightbox appears above all other page elements */

  opacity: 0; 
  /* Starts transparent so it can fade in smoothly */

  transition: opacity 0.4s; 
  /* Animates fade-in and fade-out effect */
}

#lightbox.active {
  display: flex; 
  /* Makes the container visible and enables flex layout */

  opacity: 1; 
  /* Fades the overlay into view */
}

.lightbox-content {
  height: 100%; 
  /* Allows vertical centering within the full viewport */

  display: flex;

  align-items: center; 
  /* Vertically centers the image */

  justify-content: center; 
  /* Horizontally centers the image */

  position: relative; 
  /* Allows absolutely positioned children (close button, info panel) */
}

.lightbox-image-container {
  max-width: 90%; 
  /* Prevents the image from exceeding viewport width */

  max-height: 80%; 
  /* Prevents the image from exceeding viewport height */

  overflow: hidden; 
  /* Hides image edges during scaling animation */

  border-radius: var(--radius-md); 
  /* Applies consistent rounded corners */

  box-shadow: 0 30px 80px rgba(0,0,0,0.5); 
  /* Adds depth so the image feels elevated */
}

.lightbox-image {
  max-width: 100%;
  max-height: 100%; 
  /* Ensures the image scales responsively inside its container */

  animation: kenBurns 20s ease-in-out infinite alternate; 
  /* Applies a slow cinematic zoom and pan animation */
}

@keyframes kenBurns {
  100% { 
    transform: scale(1.08) translate(-1.5%, -1.5%); 
    /* Gradually zooms and slightly shifts the image for subtle motion */
  }
}

.lightbox-close {
  position: absolute; 
  /* Positions close button relative to the lightbox content */

  top: 24px;
  right: 24px; 
  /* Places close button in the top-right corner */

  border-radius: 50%; 
  /* Makes the button circular */

  cursor: pointer; 
  /* Shows pointer cursor to indicate interactivity */

  transition: all 0.3s; 
  /* Smooth hover animation */

  z-index: 10; 
  /* Keeps button above image and info panel */
}

.lightbox-close:hover {
  transform: rotate(90deg) scale(1.1); 
  /* Rotates and slightly enlarges button for interactive feedback */
}

.lightbox-info {
  position: absolute; 
  /* Anchors metadata panel within lightbox */

  bottom: 32px;
  left: 32px;
  right: 32px; 
  /* Positions info panel near bottom with side margins */

  backdrop-filter: blur(20px); 
  /* Adds glass-like blur effect behind the panel */

  opacity: 0; 
  /* Starts hidden for animation */

  transform: translateY(16px); 
  /* Slightly shifts downward before animating upward */

  transition: all 0.5s var(--ease) 0.2s; 
  /* Smooth delayed entrance animation */
}

#lightbox.active .lightbox-info {
  opacity: 1; 
  /* Fades metadata panel into view */

  transform: translateY(0); 
  /* Slides panel upward into final position */
}

/* #########################################
   COMPARISON LIGHTBOX (Side-by-side)
   SUMMARY:
   This CSS creates a full-screen comparison lightbox for displaying two
   paintings side-by-side. It uses CSS Grid for equal split layout,
   rounded containers for a polished look, and styled overlay labels
   with a subtle glassmorphism effect for context.

   ######################################### */
   
#comparison-lightbox {
  display: none; 
  /* Keeps comparison lightbox hidden until activated */

  position: fixed; 
  /* Locks it to the viewport */

  inset: 0; 
  /* Covers the entire screen */

  background: rgba(10, 15, 12, 0.97); 
  /* Dark cinematic overlay */

  z-index: 10001; 
  /* Ensures it appears above the standard lightbox */
}

#comparison-lightbox.active {
  display: flex; 
  /* Makes the lightbox visible when active */
}

.comparison-container {
  width: 100%;
  height: 100%; 
  /* Allows full viewport usage */

  display: grid; 
  /* Enables side-by-side layout */

  grid-template-columns: 1fr 1fr; 
  /* Creates two equal-width columns */

  gap: 4px; 
  /* Small divider between images */

  padding: 70px 32px; 
  /* Prevents content from touching screen edges */
}

.comparison-side {
  position: relative; 
  /* Allows labels to be positioned inside */

  overflow: hidden; 
  /* Prevents zoomed images from spilling out */

  background: #000; 
  /* Neutral background behind images */

  border-radius: var(--radius-sm); 
  /* Gives both image panels rounded corners */
}

.comparison-side img {
  width: 100%;
  height: 100%; 
  /* Forces image to fill its side container */

  object-fit: contain; 
  /* Ensures full image is visible without cropping */

  transition: transform 0.1s ease; 
  /* Smooth micro-interactions (zoom/pan effects if applied) */
}

.comparison-label {
  position: absolute; 
  /* Anchors label inside image container */

  top: 16px;
  left: 16px; 
  /* Places label in upper-left corner */

  background: rgba(30, 51, 40, 0.8); 
  /* Semi-transparent dark overlay */

  backdrop-filter: blur(8px); 
  /* Creates glass-like blur effect */

  padding: 8px 18px; 
  /* Adds breathing room inside label */

  border-radius: 20px; 
  /* Creates pill-shaped label */

  font-size: 13px;
  font-weight: 700; 
  /* Makes label text bold and readable */

  color: var(--amber-light); 
  /* Accent color for visual hierarchy */

  border: 1px solid rgba(107, 143, 113, 0.2); 
  /* Subtle outline for depth */
}

/* #########################################
   ADMIN
   
   This CSS styles the Admin interface, including the login card,
   admin action toolbar, and DataTable overrides. It creates a clean,
   centered layout with strong typography, rounded cards, elevated
   shadows, and clearly differentiated action buttons (approve, reject,
   refresh) with interactive hover feedback.

   ######################################### */

.admin-wrap {
  max-width: 1200px;
  /* Prevents admin content from stretching too wide on large screens */

  margin: 0 auto;
  /* Centers the admin layout horizontally */

  padding: 0 24px 60px;
  /* Adds side spacing and bottom breathing room */
}

.admin-login-card {
  max-width: 400px;
  /* Keeps login form compact and focused */

  margin: 0 auto;
  /* Centers login card */

  background: var(--white);
  /* Clean card background */

  padding: 48px;
  /* Generous spacing for premium feel */

  border-radius: var(--radius-lg);
  /* Large rounded corners for modern UI */

  box-shadow: var(--shadow-md);
  /* Elevates card from background */

  text-align: center;
  /* Centers login text and inputs */
}

.admin-login-card h3 {
  font-family: 'DM Serif Display', Georgia, serif;
  /* Elegant headline typography */

  color: var(--forest);
  /* Strong brand heading color */

  font-size: 28px;
}

.admin-login-card p {
  color: var(--charcoal-light);
  /* Softer secondary text */

  font-size: 14px;
}

.admin-login-card .form-control {
  padding: 14px 16px;
  /* Comfortable click/tap target size */

  border: 2px solid var(--sand-dark);
  /* Subtle input boundary */

  border-radius: var(--radius-sm);
  /* Consistent rounded inputs */

  width: 100%;
  /* Full-width inside card */

  transition: all 0.3s;
  /* Smooth focus animation */
}

.admin-login-card .form-control:focus {
  border-color: var(--sage);
  /* Highlights active input */

  box-shadow: 0 0 0 4px rgba(107,143,113,0.15);
  /* Accessible focus ring */

  outline: none;
  /* Removes default browser outline */
}


/* =========================
   ADMIN TOOLBAR
   ========================= */

.admin-toolbar {
  display: flex;
  /* Horizontal button layout */

  gap: 12px;
  /* Even spacing between buttons */

  flex-wrap: wrap;
  /* Prevents overflow on small screens */
}

.admin-toolbar .btn {
  border-radius: var(--radius-sm);
  /* Rounded action buttons */

  font-weight: 700;
  /* Strong action emphasis */

  text-transform: uppercase;
  /* Creates administrative tone */

  letter-spacing: 0.5px;
  /* Improves readability of uppercase text */

  cursor: pointer;
  /* Indicates clickability */

  transition: all 0.3s;
  /* Smooth hover effects */

  border: 2px solid;
  /* Gives structure to transparent buttons */
}

.admin-toolbar .btn-approve {
  background: var(--sage);
  /* Positive action color */

  border-color: var(--sage);

  color: white;
}

.admin-toolbar .btn-approve:hover {
  background: var(--sage-dark);
  /* Darkens to reinforce interaction */
}

.admin-toolbar .btn-reject {
  background: transparent;
  /* Neutral base */

  border-color: var(--terra);
  /* Warning/destructive tone */

  color: var(--terra);
}

.admin-toolbar .btn-reject:hover {
  background: rgba(194,113,79,0.1);
  /* Light tint feedback on hover */
}

.admin-toolbar .btn-refresh {
  background: transparent;

  border-color: var(--charcoal-light);

  color: var(--charcoal-light);
  /* Neutral utility action */
}

.admin-toolbar .btn-refresh:hover {
  background: rgba(0,0,0,0.04);
  /* Subtle hover feedback */
}


/* =========================
   DataTable Overrides
   ========================= */

.dataTables_wrapper {
  background: var(--white);
  /* Makes table area card-like */

  border-radius: var(--radius-md);
  /* Rounded table container */

  padding: 24px;
  /* Inner spacing */

  box-shadow: var(--shadow-sm);
  /* Slight elevation */
}

table.dataTable thead th {
  font-weight: 700;
  /* Strong header hierarchy */

  text-transform: uppercase;
  /* Administrative styling */

  letter-spacing: 1px;
  /* Improves legibility */

  color: var(--forest);
  /* Brand-aligned header color */

  border-bottom: 2px solid var(--sand-dark) !important;
  /* Strong visual divider under headers */
}

/* #########################################
   ALERTS
   
   This CSS defines custom success and error alert components.
   Each alert uses soft tinted backgrounds, colored borders for clear
   status indication, rounded corners for consistency with the design
   system, and bold typography to ensure visibility and readability.
   
   ######################################### */

.alert-success-custom {
  background: rgba(107, 143, 113, 0.1);
  /* Light sage tint to indicate positive/success state */

  border: 1px solid var(--sage);
  /* Stronger sage border for visual definition */

  color: var(--sage-dark);
  /* Darker green text for readable contrast */

  padding: 16px 20px;
  /* Comfortable internal spacing */

  border-radius: var(--radius-sm);
  /* Rounded corners for design consistency */

  margin-bottom: 24px;
  /* Spacing below alert */

  font-weight: 600;
  /* Slight emphasis for important message */
}

.alert-error-custom {
  background: rgba(194, 113, 79, 0.1);
  /* Light terra tint to indicate warning/error */

  border: 1px solid var(--terra);
  /* Strong terra border for clarity */

  color: var(--terra-dark);
  /* Darker red tone for readable contrast */

  padding: 16px 20px;
  /* Consistent internal spacing */

  border-radius: var(--radius-sm);
  /* Rounded corners for cohesive UI */

  margin-bottom: 24px;
  /* Spacing below alert */

  font-weight: 600;
  /* Emphasizes message importance */
}



/* #########################################
   RESPONSIVE
   
   Makes media mobile friendly
   ######################################### */
   
@media (max-width: 768px) {
  .paintings-grid { grid-template-columns: 1fr; }
  /* Changes the paintings grid to a single column instead of multiple columns */

  .comparison-grid { grid-template-columns: 1fr; }
  /* Makes side-by-side comparison images stack vertically */

  .stats-strip { grid-template-columns: 1fr; }
  /* Stacks any stats or metrics strip vertically for mobile */

  .comparison-container { grid-template-columns: 1fr; }
  /* Stacks the comparison lightbox images vertically instead of side-by-side */

  .form-card { padding: 32px 24px; }
  /* Reduces padding on forms for smaller screens */

  .hero-inner { padding: 40px 20px; }
  /* Adjusts padding inside hero sections to fit mobile screens */

  .lightbox-content { padding: 20px; }
  /* Reduces lightbox content padding to maximize image space on mobile */

  .lightbox-info { left: 16px; right: 16px; bottom: 16px; padding: 20px; }
  /* Moves the lightbox info panel closer to edges and reduces padding for small screens */

  .section-header { padding: 40px 20px 30px; }
  /* Adjusts section header spacing for mobile screens */
}

/* #########################################
   SHINY SPECIFIC OVERRIDES
   ######################################### */


.shiny-input-container { 
  width: 100% !important; 
  /* Forces all Shiny inputs to fill the full width of their container, overriding Shiny defaults */
}

.selectize-input { 
  border: 2px solid var(--sand-dark) !important; 
  /* Gives dropdown inputs a consistent border matching your design */
  
  border-radius: var(--radius-sm) !important; 
  /* Adds rounded corners to dropdowns */
  
  padding: 10px 14px !important; 
  /* Adds comfortable spacing inside the input */
}

.selectize-input.focus { 
  border-color: var(--sage) !important; 
  /* Changes border to the sage color when the input is focused */
  
  box-shadow: 0 0 0 4px rgba(107,143,113,0.15) !important; 
  /* Adds a subtle focus ring to indicate active state */
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
    
    version = 5,
    # Use Bootstrap version 5.
    
    bg = "#FBF8F3",  # Background color (cream/off-white)
    fg = "#2D2D2D",  # Foreground/text color (dark charcoal)
    primary = "#C2714F",   # Primary accent color (terra/orange-brown)
    secondary = "#6B8F71", # Secondary accent color (sage green)
    success = "#6B8F71",   # Color used for "success" alerts (sage green)
    info = "#D4A843",      # Color used for "info" alerts (amber/gold)
    
    base_font = font_google("DM Sans"),
    # Loads the "DM Sans" font from Google Fonts for body text.
    
    heading_font = font_google("DM Serif Display")
    # Loads "DM Serif Display" from Google Fonts for headings/titles.
  ),
  
  header = tags$head(
    # tags$head() injects content into the HTML <head> tag.
    # This is where you load external resources and styles.
    
    tags$link(href = "https://fonts.googleapis.com/...", rel = "stylesheet"),
    # Loads the Google Fonts stylesheet so DM Sans and DM Serif Display
    # are available in the browser.
    
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    # This meta tag makes the app responsive on mobile devices --
    # it tells the browser to scale the page to the device width.
    
    tags$style(HTML(app_css))
    # Injects the custom CSS string (defined earlier as app_css) into the page.
  ),
  
  
  # -- TAB 1: HOME ----------------------------------------------------------
  # The landing/hero page with a large banner, a tagline, two call-to-action
  # buttons, and a stats strip showing live submission counts.
  nav_panel(
    title = "Home",
    icon = icon("mountain-sun"),
    # icon() uses Font Awesome icons. "mountain-sun" shows a mountain with sun icon.
    
    tags$div(class = "hero-banner",
             # A full-screen banner section styled with the CSS class "hero-banner".
             
             tags$div(class = "hero-bg-pattern"),
             # A decorative dot pattern layered behind the content (pure CSS, no content).
             
             tags$div(class = "hero-glow hero-glow-1"),
             tags$div(class = "hero-glow hero-glow-2"),
             tags$div(class = "hero-glow hero-glow-3"),
             # Three decorative colored blurs (glows) for visual depth. Pure CSS effects.
             
             tags$div(class = "hero-inner",
                      # The centered content area inside the banner.
                      
                      tags$h1(class = "hero-title", HTML("Landscape<br>Through Time")),
                      # The main title. HTML() lets us use raw HTML tags like <br> and <span>.
                      # The <span> around "Time" gets a gradient color treatment via CSS.
                      
                      tags$p(class = "hero-subtitle",
                             "Explore Albert Bierstadt's iconic paintings."
                      ),
                      # Subtitle/description text below the main title.
                      
                      tags$div(class = "hero-actions",
                               actionButton("go_gallery", HTML("View the Collection &rarr;"), class = "btn-terra"),
                               # A button with ID "go_gallery". When clicked, the server detects
                               # input$go_gallery and navigates to the Gallery tab.
                               # &rarr; is the HTML code for a right arrow '.
                               
                               actionButton("go_submit", "Contribute a Photo", class = "btn-sage")
                               # A second button that navigates to the Submit tab.
                      ),
                      
                      tags$div(class = "stats-strip",
                               # A three-column strip at the bottom of the hero showing live stats.
                               
                               tags$div(class = "stat-item",
                                        tags$div(class = "stat-value", as.character(nrow(paintings_data))),
                                        
                                        tags$div(class = "stat-label", "Locations")
                               ),
                               
                               tags$div(class = "stat-item",
                                        tags$div(class = "stat-value", textOutput("stat_submissions", inline = TRUE)),
                                        # textOutput() displays a reactive value from the server.
                                        # "stat_submissions" is updated live whenever a new submission is added.
                                        # inline = TRUE means it renders as inline text, not a block.
                                        tags$div(class = "stat-label", "Submissions")
                               ),
                               
                               tags$div(class = "stat-item",
                                        tags$div(class = "stat-value", textOutput("stat_approved", inline = TRUE)),
                                        # Same as above but for approved comparisons.
                                        tags$div(class = "stat-label", "Comparisons")
                               )
                      )
             )
    )
  ),
  
  
  # -- TAB 2: GALLERY ------------------------------------------------------
  # Displays all Bierstadt paintings as clickable cards. Clicking opens a
  # fullscreen lightbox with a Ken Burns zoom animation.
  nav_panel(
    title = "Gallery",
    icon = icon("images"),
    
    tags$div(class = "section-header",
             tags$h2("The Collection"),
             tags$div(class = "accent-line")
             # accent-line is a small decorative horizontal line rendered via CSS.
    ),
    
    tags$div(class = "gallery-wrap",
             tags$div(id = "paintings-container", class = "paintings-grid",
                      uiOutput("painting_cards")
                      # uiOutput() is a placeholder that gets filled in by the server.
                      # The server generates each painting card dynamically from the CSV data.
             )
    )
  ),
  
  
  # -- TAB 3: MAP ----------------------------------------------------------
  # UPDATED: Split layout with map on the left and an info panel on the right.
  # Red circle markers for Bierstadt paintings, blue circles for user submissions.
  # Clicking any marker populates the info panel with details.
  nav_panel(
    title = "Map",
    icon = icon("map-location-dot"),
    
    tags$div(class = "section-header",
             tags$h2("Explore Locations"),
             tags$p("See where Bierstadt set up his easel across America. Click a marker for details."),
             tags$div(class = "accent-line")
    ),
    
    # Legend showing what each marker color means
    # UPDATED: Added color legend above the map
    tags$div(class = "map-legend",
             tags$div(class = "legend-item",
                      tags$div(class = "legend-dot red"),
                      "Bierstadt Paintings"
             ),
             tags$div(class = "legend-item",
                      tags$div(class = "legend-dot blue"),
                      "Community Submissions"
             )
    ),
    
    # UPDATED: Two-column grid layout -- map on left, info panel on right.
    # Uses CSS class .map-split-layout for the grid.
    tags$div(class = "map-split-layout",
             
             # LEFT: Interactive Leaflet map
             tags$div(class = "map-container",
                      leafletOutput("main_map", height = "100%")
                      # leafletOutput() is the placeholder for the interactive map.
                      # The actual map is rendered by the server using renderLeaflet().
                      # height = "100%" makes it fill the map-container div.
             ),
             
             # RIGHT: Info panel -- populated when a marker is clicked
             # UPDATED: New panel that shows painting/submission details
             tags$div(class = "map-info-panel",
                      uiOutput("map_info_content")
                      # uiOutput() is a placeholder filled by the server.
                      # Shows a placeholder message until a marker is clicked,
                      # then displays the title, image, coordinates, and context.
             )
    )
  ),
  
  
  # -- TAB 4: SUBMIT ------------------------------------------------------
  # A form where visitors upload a modern photo from a Bierstadt location.
  # They can optionally add their name, email, GPS coordinates, and observations.
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
                      # Placeholder for success or error messages shown after form submission.
                      # The server controls whether to show a green success or red error alert.
                      
                      tags$div(class = "form-group",
                               textInput("submit_name", "Your Name (optional)", placeholder = "Jane Doe")
                               # textInput() creates a text field. First arg is the input ID,
                               # second is the label shown above the field, third is the greyed-out
                               # placeholder text inside the field.
                      ),
                      
                      tags$div(class = "form-group",
                               textInput("submit_email", "Email (optional)", placeholder = "jane@university.edu")
                      ),
                      
                      tags$div(class = "form-group",
                               selectInput("submit_painting", "Which location did you visit?",
                                           choices = c("Select a location..." = "", setNames(paintings_data$id, paintings_data$title)))
                               # selectInput() creates a dropdown menu.
                               # setNames(paintings_data$id, paintings_data$title) pairs each painting's
                               # ID (the value sent to the server) with its title (the text shown to users).
                               # The empty string -- is the default "please select" option.
                      ),
                      
                      tags$div(class = "form-group",
                               tags$label("Upload Your Photo"),
                               tags$div(class = "upload-zone",
                                        # upload-zone is a styled dashed box for the file upload area.
                                        tags$div(class = "upload-icon", HTML("&#128247;")),
                                        # &#128247; is the HTML code for the "* camera emoji.
                                        tags$p(style = "...", "Drag & drop or click to browse"),
                                        fileInput("submit_photo", NULL, accept = c("image/png", "image/jpeg", "image/jpg"))
                                        # fileInput() creates the file upload control.
                                        # accept limits which file types the browser's file picker shows.
                                        # NULL for the label means no label text -- it's handled by the tags above.
                               )
                      ),
                      
                      tags$div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 16px;",
                               # An inline CSS grid to place Latitude and Longitude fields side-by-side.
                               
                               tags$div(class = "form-group",
                                        numericInput("submit_latitude", "Latitude", value = NA, step = 0.0001)
                                        # numericInput() creates a number field. value = NA starts it empty.
                                        # step = 0.0001 sets how much the value changes when using arrow keys.
                               ),
                               tags$div(class = "form-group",
                                        numericInput("submit_longitude", "Longitude", value = NA, step = 0.0001)
                               )
                      ),
                      
                      tags$div(class = "form-group",
                               textAreaInput("submit_observations", "Observations (optional)", rows = 3,
                                             placeholder = "What did you notice about how the landscape has changed?")
                               # textAreaInput() is like textInput but multi-line. rows = 3 sets the height.
                      ),
                      
                      actionButton("submit_button", HTML("Submit Photo &rarr;"), class = "btn-submit")
                      # The submit button. When clicked, the server's observeEvent(input$submit_button)
                      # block runs to validate the form and save the data.
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
  # Pushes everything after it (the Login tab) to the far right of the navbar.
  
  
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
                                     style = "background: #8B0000; border-color: #8B0000; color: white;")
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
    
    # -- PAINTING LIGHTBOX ----------------------------------------------
    # A full-screen overlay that appears when a painting card is clicked.
    tags$div(id = "lightbox",
             tags$div(class = "lightbox-content",
                      
                      tags$div(class = "lightbox-close", onclick = "closeLightbox()", HTML("&times;")),
                      # The  close button. onclick calls the JS closeLightbox() function.
                      # &times; is the HTML code for the  symbol.
                      
                      tags$div(class = "lightbox-image-container",
                               tags$img(id = "lightbox-img", class = "lightbox-image", src = "")
                               # The image starts with an empty src=--. The JS openLightbox() function
                               # fills in the correct image URL when a card is clicked.
                      ),
                      
                      tags$div(class = "lightbox-info",
                               tags$h3(id = "lightbox-title"),
                               # These empty elements get filled by JS with the painting's title,
                               # artist/year metadata, and descriptive context text.
                               tags$p(id = "lightbox-meta", class = "meta"),
                               tags$p(id = "lightbox-context", class = "context")
                      )
             )
    ),
    
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
    
    # -- JAVASCRIPT ----------------------------------------------------
    # All the client-side interactivity: lightbox open/close, 3D card tilt,
    # tab switching, and fixing the Leaflet map rendering bug in tabs.
    tags$script(HTML(paste0("

      // paintingsData is the CSV data injected from R into JavaScript as JSON.
      // This lets JS look up painting details (image URL, title, etc.) by ID.
      var paintingsData = ", jsonlite::toJSON(paintings_data, auto_unbox = TRUE), ";

      // Opens the painting lightbox for the painting with the given ID.
      window.openLightbox = function(id) {
        var p = paintingsData.find(function(x) { return x.id === id; });
        // .find() searches the array for the painting whose id matches.
        if (!p) return;
        // If no match found, do nothing.

        document.getElementById('lightbox-img').src = p.image_url;
        // Sets the lightbox image to this painting's URL.
        document.getElementById('lightbox-title').textContent = p.title;
        document.getElementById('lightbox-meta').textContent = p.artist + ' \u2022 ' + p.year;
        // \u2022 is the Unicode bullet point character
        document.getElementById('lightbox-context').textContent = p.context;
        document.getElementById('lightbox').classList.add('active');
        // Adding the 'active' class triggers the CSS to show the lightbox.
        document.body.style.overflow = 'hidden';
        // Prevents the page from scrolling while the lightbox is open.
      };

      window.closeLightbox = function() {
        document.getElementById('lightbox').classList.remove('active');
        // Removing 'active' hides the lightbox again.
        document.body.style.overflow = '';
        // Restores normal page scrolling.
      };

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

      // Pressing Escape closes whichever lightbox is open.
      document.addEventListener('keydown', function(e) {
        if (e.key === 'Escape') { closeLightbox(); closeComparisonLightbox(); }
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
    submissions = load_data(SUBMISSIONS_FILE),
    approved = load_data(APPROVED_FILE),
    approved_trigger = 0,
    selected_marker = NULL,   # UPDATED: stores the ID of the currently clicked map marker
    selected_type = NULL      # UPDATED: "painting" or "submission" to distinguish marker types
  )
  
  # UPDATED: Tracks which basemap is currently displayed to avoid unnecessary redraws
  current_basemap <- reactiveVal("minimal")
  
  
  # -- HERO BUTTON NAVIGATION ----------------------------------------------
  # These two blocks listen for clicks on the hero buttons and tell the browser
  # to switch to the appropriate tab using a custom JavaScript message.
  observeEvent(input$go_gallery, {
    # observeEvent() runs its code block whenever the specified input changes/fires.
    # input$go_gallery is triggered when the "View the Collection" button is clicked.
    session$sendCustomMessage("switchTab", "Gallery")
    # Sends a message to the JS handler named "switchTab", passing "Gallery" as the value.
  })
  observeEvent(input$go_submit, {
    session$sendCustomMessage("switchTab", "Submit")
  })
  
  
  # -- STATS DISPLAY --------------------------------------------------------
  # These render the live submission counts shown in the hero stats strip.
  output$stat_submissions <- renderText({ as.character(nrow(rv$submissions)) })
  # nrow() counts how many rows are in the submissions data frame.
  # as.character() converts the number to a string for display.
  # This automatically re-runs whenever rv$submissions changes.
  
  output$stat_approved <- renderText({ as.character(nrow(rv$approved)) })
  
  
  # -- PAINTING CARDS --------------------------------------------------------
  # Dynamically generates one HTML card per painting from the CSV data.
  output$painting_cards <- renderUI({
    # renderUI() generates HTML content dynamically. The result fills the
    # uiOutput("painting_cards") placeholder in the UI.
    
    cards <- lapply(1:nrow(paintings_data), function(i) {
      # lapply() loops over each row index and returns a list of HTML elements.
      p <- paintings_data[i, ]
      # p is a single row from the paintings data frame.
      
      tags$div(class = "painting-card", onclick = sprintf("openLightbox(%d)", p$id),
               # sprintf() builds a JavaScript call string like: openLightbox(3)
               # So clicking this card calls the JS function with this painting's ID.
               
               tags$div(class = "painting-card-img-wrap",
                        tags$img(src = p$image_url, class = "painting-image", alt = p$title),
                        # The painting image. src comes from the CSV's image_url column.
                        # alt text is used by screen readers and shown if the image fails to load.
                        tags$div(class = "painting-card-badge", p$year)
                        # A small badge showing the year, positioned over the image corner via CSS.
               ),
               
               tags$div(class = "painting-info",
                        tags$h3(class = "painting-title", p$title),
                        tags$div(class = "painting-meta", paste0(p$artist, " \u2022 ", p$year)),
                        # paste0() joins strings with no separator. \u2022 is the bullet character.
                        tags$p(class = "painting-context", p$context),
                        tags$div(class = "painting-card-cta", HTML("View Full &rarr;"))
                        # &rarr; is HTML for the ' right arrow character.
               )
      )
    })
    
    tagList(cards)
    # tagList() combines the list of card elements into a single HTML fragment
    # that Shiny can render as one unit.
  })
  
  
  # -- MAP (UPDATED) --------------------------------------------------------
  # UPDATED: Replaced default pin markers with red/blue circle markers.
  # Changed base tiles to CartoDB.Positron for a cleaner look.
  # Added split layout with info panel, marker click detection, and
  # dynamic blue markers for approved user submissions.
  # Red circle markers for Bierstadt paintings, blue for approved submissions.
  # Clicking a marker updates the info panel on the right side of the layout.
  
  output$main_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "minimal") %>%
      # UPDATED: Added group = "minimal" so tiles can be swapped on zoom.
      # CartoDB.Positron provides a cleaner, modern base map
      
      addCircleMarkers(
        data = paintings_data,
        lng = ~longitude, lat = ~latitude,
        radius = 10,
        color = "#A85D3F",        # terra-dark border
        fillColor = "#C2714F",    # terra fill (red circles)
        fillOpacity = 0.85,
        weight = 2,
        stroke = TRUE,
        layerId = ~paste0("painting_", id),
        # layerId uniquely identifies each marker so we can detect clicks
        label = ~title,
        labelOptions = labelOptions(
          style = list("font-weight" = "600", "font-family" = "DM Sans, sans-serif"),
          textsize = "13px",
          direction = "top",
          offset = c(0, -12)
        )
      ) %>%
      
      # UPDATED: setView centers on the US at zoom 4 instead of fitBounds
      setView(lng = -98.5, lat = 39.8, zoom = 4)
  })
  
  # -- SUBMISSION MARKERS (BLUE CIRCLES) (UPDATED) ----------------------------
  # UPDATED: New observe() block that reactively adds/updates blue circle
  # markers on the map for approved community submissions.
  # Uses leafletProxy() to add markers without re-rendering the entire map.
  observe({
    approved <- rv$approved
    rv$approved_trigger
    
    proxy <- leafletProxy("main_map")
    proxy %>% clearGroup("submissions")
    
    if (nrow(approved) > 0) {
      valid_subs <- approved[!is.na(approved$latitude) & !is.na(approved$longitude), ]
      if (nrow(valid_subs) > 0) {
        valid_subs$painting_title <- sapply(valid_subs$painting_id, function(pid) {
          match_row <- paintings_data[paintings_data$id == pid, ]
          if (nrow(match_row) > 0) match_row$title[1] else "Unknown Location"
        })
        
        proxy %>% addCircleMarkers(
          data = valid_subs,
          lng = ~longitude, lat = ~latitude,
          radius = 8,
          color = "#2563EB",        # blue border
          fillColor = "#3B82F6",    # blue fill
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
    }
  })
  
  # -- INFO PANEL CONTENT (UPDATED) ------------------------------------------
  # UPDATED: New renderUI() that populates the right-side info panel.
  # Three states:
  #   1. No marker selected: shows a placeholder with instructions.
  #   2. Painting marker: shows title, artist, year, image, context, coords.
  #   3. Submission marker: shows painting title, submitter, photo, observations, coords.
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
      
      tagList(
        tags$div(class = "map-info-header",
                 tags$div(class = "map-info-dot painting"),
                 tags$span(class = "map-info-type-label", "Bierstadt Painting")
        ),
        tags$h3(class = "map-info-title", p$title),
        tags$div(class = "map-info-meta", paste0(p$artist, " | ", p$year)),
        tags$img(class = "map-info-image", src = p$image_url, alt = p$title),
        tags$p(class = "map-info-context", p$context),
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
    # Check each required field. If invalid, set an error message and stop.
    if (input$submit_painting == "") {
      rv$submission_error <- "Please select a location."
      return()
      # return() exits the observeEvent block early -- nothing below this runs.
    }
    if (is.null(input$submit_photo)) {
      rv$submission_error <- "Please upload a photo."
      return()
    }
    if (is.na(input$submit_latitude) || is.na(input$submit_longitude)) {
      # is.na() checks if a value is missing/empty.
      rv$submission_error <- "Please enter GPS coordinates."
      return()
    }
    if (input$submit_photo$size > 5 * 1024 * 1024) {
      # File size is in bytes. 5 * 1024 * 1024 = 5MB in bytes.
      rv$submission_error <- "File must be less than 5MB."
      return()
    }
    
    # -- SAVE SUBMISSION --------------------------------------------------
    tryCatch({
      # tryCatch() runs the code inside and catches any errors so the app
      # doesn't crash -- it runs the error function instead.
      
      file_data <- readBin(input$submit_photo$datapath, "raw", file.info(input$submit_photo$datapath)$size)
      # readBin() reads the uploaded file as raw binary data.
      # input$submit_photo$datapath is the temporary file path Shiny saved it to.
      
      base64_image <- paste0("data:image/jpeg;base64,", base64enc::base64encode(file_data))
      # base64encode() converts the binary image into a Base64 text string.
      # This format (data:image/jpeg;base64,...) can be stored as text and
      # displayed directly in an <img> src attribute without a file URL.
      
      new_submission <- data.frame(
        # Creates a one-row data frame with all the submission details.
        submission_id = as.character(as.integer(Sys.time())),
        # Uses the current Unix timestamp as a unique ID. as.integer() removes decimals.
        name = ifelse(input$submit_name == "", "Anonymous", input$submit_name),
        # ifelse() picks "Anonymous" if the name field is blank.
        email = input$submit_email,
        painting_id = as.integer(input$submit_painting),
        # The dropdown sends the painting's ID as a string; convert it to integer.
        photo_url = base64_image,
        latitude = input$submit_latitude,
        longitude = input$submit_longitude,
        observations = input$submit_observations,
        submission_date = as.character(Sys.Date()),
        # Sys.Date() gets today's date. as.character() converts it to "YYYY-MM-DD".
        approval_status = "Pending",
        # All new submissions start as "Pending" until an admin approves or rejects.
        stringsAsFactors = FALSE
        # Prevents R from converting text columns to factor type (old R behavior).
      )
      
      rv$submissions <- rbind(rv$submissions, new_submission)
      # rbind() appends the new row to the bottom of the existing submissions table.
      
      save_data(rv$submissions, SUBMISSIONS_FILE)
      # Writes the updated submissions data frame to disk as an .rds file.
      
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
  output$comparison_gallery <- renderUI({
    rv$approved_trigger
    # Reading this value here means the output will re-render whenever
    # approved_trigger is incremented, even if rv$approved change isn't
    # detected automatically.
    approved <- rv$approved
    
    if (nrow(approved) == 0) {
      # If no submissions have been approved yet, show a placeholder message.
      return(tags$div(class = "no-comparisons",
                      HTML("No approved comparisons yet. Be the first to contribute!")
                      
      ))
    }
    
    cards <- lapply(1:nrow(approved), function(i) {
      sub <- approved[i, ]
      # sub = one approved submission row.
      
      painting <- paintings_data[paintings_data$id == sub$painting_id, ]
      # Look up the matching painting from the CSV using the stored painting_id.
      
      tags$div(class = "comparison-thumb",
               onclick = sprintf("openComparisonLightbox('%s', '%s')", painting$image_url, sub$photo_url),
               # When clicked, opens the side-by-side lightbox with the historical
               # painting URL and the user's modern photo URL.
               
               tags$img(src = painting$image_url, alt = painting$title),
               # Shows the historical painting as the thumbnail preview.
               
               tags$div(class = "comparison-thumb-overlay",
                        tags$div(class = "comparison-thumb-label", HTML("&#8644; Compare"))
                        # &#8644; is the  compare arrows character. Shown on hover via CSS.
               )
      )
    })
    
    tags$div(class = "comparison-grid", cards)
    # Wraps all thumbnails in the grid container.
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
  output$admin_table <- renderDT({
    input$refresh_admin
    # Reading this input here means the table will re-render whenever the
    # Refresh button is clicked (even though we don't use its value directly).
    
    if (nrow(rv$submissions) == 0) return(data.frame(Message = "No submissions yet"))
    # If there's nothing to show, return a simple placeholder table.
    
    datatable(
      rv$submissions[, c("submission_id", "name", "painting_id", "latitude", "longitude", "submission_date", "approval_status")],
      # Select only the relevant columns to display (excludes the base64 photo data,
      # which would be enormous and unreadable in a table).
      
      options = list(
        pageLength = 25,
        # Show 25 rows per page before pagination kicks in.
        order = list(list(6, 'desc'))
        # Default sort: column index 6 (submission_date) descending (newest first).
      ),
      rownames = FALSE,
      # Don't show R's row numbers as a column.
      selection = 'single'
      # Only allow one row to be selected at a time (for approve/reject actions).
    )
  })
  
  
  # -- APPROVE SUBMISSION --------------------------------------------------
  observeEvent(input$approve_submission, {
    if (length(input$admin_table_rows_selected) > 0) {
      # Only act if a row is actually selected in the table.
      idx <- input$admin_table_rows_selected
      # idx is the row number of the selected submission.
      
      sub <- rv$submissions[idx, ]
      # Extract that specific submission row.
      
      sub$approval_status <- "Approved"
      # Update its status.
      
      rv$approved <- rbind(rv$approved, sub)
      # Add it to the approved data frame, making it appear on the Compare tab.
      
      save_data(rv$approved, APPROVED_FILE)
      # Persist the approved data to disk.
      
      rv$submissions[idx, "approval_status"] <- "Approved"
      # Also update the status in the main submissions table.
      
      save_data(rv$submissions, SUBMISSIONS_FILE)
      # Persist the updated submissions to disk.
      
      showNotification("Approved!", type = "message")
      # Shows a small toast notification in the corner of the browser.
    }
  })
  
  
  # -- REJECT SUBMISSION ------------------------------------------------------
  observeEvent(input$reject_submission, {
    if (length(input$admin_table_rows_selected) > 0) {
      idx <- input$admin_table_rows_selected
      rv$submissions[idx, "approval_status"] <- "Rejected"
      save_data(rv$submissions, SUBMISSIONS_FILE)
      showNotification("Rejected.", type = "warning")
    }
  })
  
  # -- DELETE SUBMISSION ------------------------------------------------------
  observeEvent(input$delete_submission, {
    if (length(input$admin_table_rows_selected) > 0) {
      idx <- input$admin_table_rows_selected
      # Grab the submission_id of the row being deleted before removing it.
      # This is used to find and remove the matching row in the approved data.
      deleted_id <- rv$submissions[idx, "submission_id"]
      
      rv$submissions <- rv$submissions[-idx, ]
      # Remove the selected row from the submissions data frame entirely.
      save_data(rv$submissions, SUBMISSIONS_FILE)
      # Persist the updated submissions to disk.
      
      rv$approved <- rv$approved[rv$approved$submission_id != deleted_id, ]
      save_data(rv$approved, APPROVED_FILE)
      rv$approved_trigger <- rv$approved_trigger + 1
      # Increment the trigger counter to force comparison_gallery and any
      # other outputs watching approved_trigger to immediately re-render.
      
      showNotification("Deleted.", type = "error")
    }
  })
  
  
  # -- REFRESH ADMIN DATA --------------------------------------------------
  observeEvent(input$refresh_admin, {
    # Re-reads both data files from disk and updates the reactive values.
    # Useful if data was modified externally or by another session.
    rv$submissions <- load_data(SUBMISSIONS_FILE)
    rv$approved <- load_data(APPROVED_FILE)
    showNotification("Data refreshed!", type = "message")
  })
}


################################################################################
# LAUNCH THE APP
# shinyApp() wires the UI and server together and starts the application.
# When you click "Run App" in RStudio, this is the line that actually
# launches the web server and opens the browser.
################################################################################
shinyApp(ui = ui, server = server)