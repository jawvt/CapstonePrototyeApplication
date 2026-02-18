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

paintings_csv <- "/Users/jawvt/Documents/Capstone/CapstonePrototyeApplication/AlexTestApplication4/BeirdstadtPaintings.csv"
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

  border-bottom: 3px solid var(--terra) !important;
  /* Terra-colored bottom border for accent */

  padding: 0 !important;
  /* Remove default padding */

  min-height: 64px;
  /* Set a consistent minimum height */

  box-shadow: 0 4px 20px rgba(30, 51, 40, 0.25);
  /* Add subtle shadow for depth */

  position: sticky;
  /* Keep navbar fixed at the top when scrolling */

  top: 0;
  /* Stick to the top edge */

  z-index: 999;
  /* Ensure navbar stays above other elements */
}

.navbar-brand {
  display: none !important;
  /* Hide the brand/logo for a cleaner look */
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

  background: rgba(255,255,255,0.05);
  /* Subtle background highlight on hover */
}

.navbar-nav .nav-link.active,
.navbar-nav .nav-item.active .nav-link,
.navbar-nav .nav-link[aria-selected='true'] {
  color: var(--white) !important;
  /* White text for active link */

  border-bottom-color: var(--terra) !important;
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
  /* Invert icon color so it’s visible on dark background */
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

  background: linear-gradient(135deg, var(--forest) 0%, var(--forest-mid) 40%, var(--terra-dark) 100%);
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

  opacity: 0.06;
  /* Very faint pattern */

  background-image:
    radial-gradient(circle at 20% 80%, var(--amber) 1px, transparent 1px),
    radial-gradient(circle at 80% 20%, var(--terra-light) 1px, transparent 1px),
    radial-gradient(circle at 50% 50%, var(--sage-light) 0.5px, transparent 0.5px);
  /* Layered dot patterns for texture */

  background-size: 60px 60px, 80px 80px, 40px 40px;
  /* Size of each dot pattern layer */
}

.hero-glow {
  position: absolute;
  /* Positioned behind hero content */

  width: 600px;
  height: 600px;
  /* Size of glow */

  border-radius: 50%;
  /* Make it a circle */

  filter: blur(120px);
  /* Blur for soft glow effect */

  opacity: 0.15;
  /* Low opacity for subtle effect */

  pointer-events: none;
  /* Glow doesn’t block clicks */
}

.hero-glow-1 {
  background: var(--terra);
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

  color: rgba(245, 237, 224, 0.7);
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

  color: rgba(245, 237, 224, 0.5);
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

  padding: 60px 24px 40px;
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
  width: 50px;
  /* Width of the small line under the header */

  height: 4px;
  /* Thickness of the accent line */

  background: linear-gradient(90deg, var(--terra), var(--amber));
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
   MAP
   ######################################### 
   Styles the map container with rounded corners, shadow, and fixed height.
   Ensures the Leaflet map fills the container.
*/

.map-container {
  border-radius: 24px;
  /* Rounded corners for map container */

  overflow: hidden;
  /* Clip map content to container shape */

  box-shadow: 0 20px 60px rgba(0, 0, 0, 0.3);
  /* Drop shadow for depth */

  height: 600px;
  /* Fixed map height */
}

.leaflet-container {
  height: 100%;
  /* Make map fill its container */

  border-radius: px;
  /* Rounded corners (note: 'px' is incomplete, should be a number like 24px) */
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
  /* Shorthand for top/right/bottom/left: 0 — makes it cover the full screen */

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

