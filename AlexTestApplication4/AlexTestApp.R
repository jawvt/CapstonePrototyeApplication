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
Submissions <- file.path(LocalDataDirectory, "submissions.rds")
Approved <- file.path(LocalDataDirectory, "approved.rds")

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





