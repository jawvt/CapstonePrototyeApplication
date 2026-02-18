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
############################################

paintings_csv <- "/Users/jawvt/Documents/Capstone/CapstonePrototyeApplication/AlexTestApplication4/BeirdstadtPaintings.csv"
paintings_data <- read.csv(paintings_csv, stringsAsFactors = FALSE)

############################################
# Custom CSS
############################################

app_css <- "
/* ======================================================================
   CSS VARIABLES - EARTHY PALETTE
     - Colors, shades, and tints
     - Border radii for rounded corners
     - Shadow styles for consistent elevation
     - Easing function for smooth transitions
     - Makes it easy to maintain a consistent look and update styles site-wide.
   ====================================================================== */
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





