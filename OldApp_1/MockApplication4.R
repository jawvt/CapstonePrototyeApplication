################################################################################
# LANDSCAPE THROUGH TIME - R SHINY APPLICATION
# A geocache-style data collection tool for comparing historical Western U.S.
# landscape paintings by Albert Bierstadt with present-day photographs
################################################################################

# INSTALLATION INSTRUCTIONS:
# Run this command first to install all required packages:
# install.packages(c("shiny", "bslib", "leaflet", "htmltools", "DT", "shinyjs", "base64enc"))

################################################################################
# DATA STORAGE - FILE-BASED (SIMPLE & PERSISTENT)
################################################################################
# This app uses local RDS files to store data - much simpler than Google Sheets!
# 
# HOW IT WORKS:
# - Submissions are saved to: app_data/submissions.rds
# - Approved photos are saved to: app_data/approved.rds
# - Data persists between app restarts
# - No authentication or external services needed
# - Works locally and on shinyapps.io
#
# IMPORTANT FOR SHINYAPPS.IO DEPLOYMENT:
# - The app_data folder will be created automatically
# - Data persists during the app session
# - Data is lost when app is redeployed or instance restarts
# - For truly persistent cloud storage, consider:
#   * Google Sheets (requires setup)
#   * Database (PostgreSQL, MySQL)
#   * Cloud storage (AWS S3, Google Cloud Storage)
#
################################################################################

# Load required libraries
library(shiny)
library(bslib)
library(leaflet)
library(htmltools)
library(DT)
library(shinyjs)

# Optional: Only needed if using Google Sheets backend
# library(googlesheets4)
# library(googledrive)

################################################################################
# CONFIGURATION - PERSISTENT FILE STORAGE
################################################################################

# Data storage directory (creates if doesn't exist)
DATA_DIR <- "app_data"
SUBMISSIONS_FILE <- file.path(DATA_DIR, "submissions.rds")
APPROVED_FILE <- file.path(DATA_DIR, "approved.rds")

# Create data directory if it doesn't exist
if (!dir.exists(DATA_DIR)) {
  dir.create(DATA_DIR, recursive = TRUE)
}

# Helper functions for file storage
load_data <- function(file_path) {
  if (file.exists(file_path)) {
    readRDS(file_path)
  } else {
    # Return empty data frame with correct structure
    data.frame(
      submission_id = character(),
      name = character(),
      email = character(),
      painting_id = integer(),
      photo_url = character(),
      latitude = numeric(),
      longitude = numeric(),
      observations = character(),
      submission_date = character(),
      approval_status = character(),
      stringsAsFactors = FALSE
    )
  }
}

save_data <- function(data, file_path) {
  saveRDS(data, file_path)
}

################################################################################
# DATA: PAINTING DATASET
################################################################################

paintings_data <- data.frame(
  id = 1:5,
  title = c(
    "The Rocky Mountains, Lander's Peak",
    "Among the Sierra Nevada, California",
    "A Storm in the Rocky Mountains, Mt. Rosalie",
    "Valley of the Yosemite",
    "Emigrants Crossing the Plains"
  ),
  artist = rep("Albert Bierstadt", 5),
  year = c(1863, 1868, 1866, 1864, 1867),
  latitude = c(40.7489, 37.8651, 39.5501, 37.7459, 41.3114),
  longitude = c(-109.5596, -119.5383, -105.7821, -119.5332, -105.5886),
  image_url = c(
    "https://upload.wikimedia.org/wikipedia/commons/thumb/4/45/Albert_Bierstadt_-_The_Rocky_Mountains%2C_Lander%27s_Peak.jpg/800px-Albert_Bierstadt_-_The_Rocky_Mountains%2C_Lander%27s_Peak.jpg",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5f/Bierstadt_-_Among_the_Sierra_Nevada_Mountains_-_1868.jpg/800px-Bierstadt_-_Among_the_Sierra_Nevada_Mountains_-_1868.jpg",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d0/Albert_Bierstadt_-_A_Storm_in_the_Rocky_Mountains%2C_Mt._Rosalie_-_Google_Art_Project.jpg/800px-Albert_Bierstadt_-_A_Storm_in_the_Rocky_Mountains%2C_Mt._Rosalie_-_Google_Art_Project.jpg",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e6/Albert_Bierstadt_-_Valley_of_the_Yosemite_-_Google_Art_Project.jpg/800px-Albert_Bierstadt_-_Valley_of_the_Yosemite_-_Google_Art_Project.jpg",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0d/Emigrants_Crossing_the_Plains%2C_or_The_Oregon_Trail_%28Albert_Bierstadt%29%2C_1869.jpg/800px-Emigrants_Crossing_the_Plains%2C_or_The_Oregon_Trail_%28Albert_Bierstadt%29%2C_1869.jpg"
  ),
  context = c(
    "Painted during Bierstadt's first major expedition to the American West in 1859, this monumental work depicts the Wind River Range in Wyoming Territory. The painting exemplifies the Hudson River School's romantic vision of the American frontier, combining meticulous geological detail with dramatic atmospheric effects.",
    "Created following Bierstadt's 1863 journey to California, this luminous canvas captures the grandeur of the Sierra Nevada range. The painting's golden light and sublime scale reflect both the artist's German Romantic training and the era's belief in Manifest Destiny and westward expansion.",
    "Commissioned by publisher James McHenry, this dramatic composition showcases Bierstadt's mastery of atmospheric perspective and theatrical lighting. The tempestuous sky and rugged terrain embody the period's fascination with nature's raw power and the sublime experience of wilderness.",
    "Painted after Bierstadt's first visit to Yosemite Valley in 1863, this work helped introduce Eastern audiences to California's natural wonders. The composition's balanced symmetry and luminous quality influenced early conservation efforts and the eventual establishment of Yosemite National Park.",
    "This work captures the arduous journey of westward migration across the Great Plains. Bierstadt's treatment of light and vast open space reflects both the hope and hardship of frontier expansion, documenting a pivotal moment in American territorial expansion."
  ),
  stringsAsFactors = FALSE
)

################################################################################
# USER INTERFACE
################################################################################

ui <- fluidPage(
  useShinyjs(),
  theme = bslib::bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#2c3e50",
    primary = "#2d5f2d",
    base_font = bslib::font_google("Source Sans Pro"),
    heading_font = bslib::font_google("Playfair Display")
  ),
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      /* Global styles */
      body {
        background-color: #f8f9fa;
      }
      
      /* Navigation bar */
      .navbar {
        background: linear-gradient(135deg, #2d5f2d 0%, #1f4a1f 100%) !important;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        padding: 15px 0;
        margin-bottom: 0;
      }
      
      .navbar-brand {
        font-family: 'Playfair Display', serif;
        font-size: 28px;
        font-weight: 700;
        color: white !important;
      }
      
      .nav-tabs {
        border-bottom: none;
        background-color: #2d5f2d;
        padding: 0 20px;
      }
      
      .nav-tabs .nav-link,
      .nav-tabs .nav-link a,
      .nav-tabs .nav-item .nav-link,
      .navbar-nav .nav-link {
        color: #ffffff !important;
        font-weight: 600;
        font-size: 16px;
        border: none;
        padding: 18px 30px;
        margin: 0 5px;
        transition: all 0.3s ease;
        border-radius: 0;
        opacity: 0.85;
        text-shadow: 1px 1px 2px rgba(0,0,0,0.2);
      }
      
      .nav-tabs .nav-link:hover,
      .nav-tabs .nav-link a:hover,
      .nav-tabs .nav-item .nav-link:hover,
      .navbar-nav .nav-link:hover {
        color: #ffffff !important;
        background-color: rgba(255,255,255,0.15);
        opacity: 1;
      }
      
      .nav-tabs .nav-link.active,
      .nav-tabs .nav-link.active a,
      .nav-tabs .nav-item .nav-link.active,
      .navbar-nav .nav-link.active {
        color: #ffffff !important;
        background-color: rgba(255,255,255,0.25);
        border: none;
        border-bottom: 4px solid #9caf88;
        opacity: 1;
        font-weight: 700;
      }
      
      /* Hero section */
      .hero-section {
        background: linear-gradient(135deg, #2d5f2d 0%, #4a7c59 50%, #9caf88 100%);
        color: white;
        padding: 100px 40px;
        text-align: center;
        position: relative;
        overflow: hidden;
      }
      
      .hero-section::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        background: url('data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 1200 120\"><path d=\"M0,0 L1200,0 L1200,120 C800,80 400,100 0,120 Z\" fill=\"rgba(255,255,255,0.05)\"/></svg>') no-repeat bottom;
        opacity: 0.3;
      }
      
      .hero-section .content {
        position: relative;
        z-index: 1;
      }
      
      .hero-section h1 {
        color: white;
        font-size: 56px;
        margin-bottom: 20px;
        font-weight: 700;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.2);
      }
      
      .hero-section .subtitle {
        font-size: 22px;
        max-width: 900px;
        margin: 0 auto 40px;
        line-height: 1.7;
        opacity: 0.95;
      }
      
      /* Buttons - WHITE TEXT ALWAYS */
      .btn-primary, 
      .btn-primary:hover, 
      .btn-primary:focus, 
      .btn-primary:active,
      .btn-primary:visited {
        background-color: #2d5f2d;
        border: none;
        color: white !important;
        padding: 14px 35px;
        font-size: 16px;
        font-weight: 600;
        border-radius: 50px;
        transition: all 0.3s ease;
        box-shadow: 0 4px 15px rgba(45, 95, 45, 0.3);
        text-decoration: none;
      }
      
      .btn-primary:hover {
        background-color: #1f4a1f;
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(45, 95, 45, 0.4);
      }
      
      .btn-lg {
        padding: 16px 45px;
        font-size: 18px;
      }
      
      /* Content sections */
      .content-section {
        padding: 60px 0;
        max-width: 1200px;
        margin: 0 auto;
      }
      
      .section-header {
        text-align: center;
        margin-bottom: 50px;
      }
      
      .section-header h2 {
        color: #2d5f2d;
        font-size: 42px;
        font-weight: 700;
        margin-bottom: 15px;
      }
      
      .section-header p {
        color: #666;
        font-size: 18px;
        max-width: 700px;
        margin: 0 auto;
      }
      
      /* Stat boxes */
      .stat-box {
        background: white;
        padding: 40px 20px;
        border-radius: 12px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.08);
        text-align: center;
        margin: 15px;
        transition: all 0.3s ease;
      }
      
      .stat-box:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 30px rgba(0,0,0,0.12);
      }
      
      .stat-number {
        font-size: 54px;
        font-weight: bold;
        color: #2d5f2d;
        margin-bottom: 10px;
      }
      
      .stat-label {
        font-size: 16px;
        color: #666;
        text-transform: uppercase;
        letter-spacing: 1px;
      }
      
      /* Feature cards */
      .feature-card {
        background: white;
        border-radius: 12px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.08);
        padding: 30px;
        margin-bottom: 30px;
        transition: all 0.3s ease;
      }
      
      .feature-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 30px rgba(0,0,0,0.12);
      }
      
      .feature-card h3 {
        color: #2d5f2d;
        font-size: 24px;
        margin-bottom: 15px;
      }
      
      .feature-card p {
        color: #555;
        line-height: 1.7;
      }
      
      /* Painting cards */
      .painting-card {
        background: white;
        border-radius: 12px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.08);
        padding: 0;
        margin: 0 auto 40px auto;
        overflow: hidden;
        transition: all 0.3s ease;
        max-width: 550px;
      }
      
      .painting-card:hover {
        transform: translateY(-8px);
        box-shadow: 0 12px 35px rgba(0,0,0,0.15);
      }
      
      .painting-card img {
        width: 100%;
        height: 300px;
        object-fit: cover;
      }
      
      .painting-card-body {
        padding: 30px;
      }
      
      .painting-card h3 {
        margin-top: 0;
        font-size: 24px;
        color: #2d5f2d;
        margin-bottom: 12px;
        line-height: 1.3;
      }
      
      .painting-meta {
        color: #999;
        font-size: 14px;
        margin-bottom: 15px;
      }
      
      .painting-context {
        line-height: 1.8;
        color: #555;
        margin-bottom: 25px;
        font-size: 15px;
      }
      
      /* Gallery grid improvements */
      .gallery-grid {
        display: flex;
        flex-wrap: wrap;
        justify-content: center;
        gap: 30px;
        padding: 0 20px;
      }
      
      .gallery-grid .painting-card {
        flex: 0 1 calc(50% - 15px);
        max-width: 550px;
      }
      
      @media (max-width: 992px) {
        .gallery-grid .painting-card {
          flex: 0 1 100%;
          max-width: 600px;
        }
      }
      
      /* Form styling */
      .form-section {
        background: white;
        padding: 50px;
        border-radius: 12px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.08);
        max-width: 800px;
        margin: 0 auto;
      }
      
      .form-group label {
        font-weight: 600;
        color: #2d5f2d;
        margin-bottom: 8px;
        font-size: 15px;
      }
      
      .form-control {
        border: 2px solid #e0e0e0;
        border-radius: 8px;
        padding: 12px;
        font-size: 15px;
        transition: border-color 0.3s ease;
      }
      
      .form-control:focus {
        border-color: #2d5f2d;
        box-shadow: 0 0 0 3px rgba(45, 95, 45, 0.1);
        outline: none;
      }
      
      /* Comparison cards */
      .comparison-card {
        background: white;
        border-radius: 12px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.08);
        padding: 30px;
        margin-bottom: 40px;
      }
      
      .comparison-images {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 20px;
        margin-bottom: 25px;
      }
      
      .comparison-images img {
        width: 100%;
        border-radius: 8px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
      }
      
      .image-label {
        text-align: center;
        font-weight: 600;
        color: #2d5f2d;
        margin-top: 10px;
        font-size: 14px;
        text-transform: uppercase;
        letter-spacing: 1px;
      }
      
      /* Leaflet map */
      .leaflet-container {
        border-radius: 12px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.1);
      }
      
      /* Alerts */
      .alert {
        border-radius: 8px;
        padding: 18px;
        margin-bottom: 25px;
        border: none;
      }
      
      .alert-success {
        background-color: #d4edda;
        color: #155724;
      }
      
      .alert-danger {
        background-color: #f8d7da;
        color: #721c24;
      }
      
      /* Info box */
      .info-box {
        background: linear-gradient(135deg, #e8f4f8 0%, #d1e7ee 100%);
        border-left: 5px solid #87ceeb;
        padding: 25px;
        border-radius: 8px;
        margin-bottom: 30px;
      }
      
      /* Tab content padding */
      .tab-content {
        padding: 40px 20px;
      }
      
      /* Responsive */
      @media (max-width: 768px) {
        .hero-section h1 {
          font-size: 36px;
        }
        .hero-section .subtitle {
          font-size: 18px;
        }
        .comparison-images {
          grid-template-columns: 1fr;
        }
      }
    "))
  ),
  
  # Navigation bar
  div(class = "navbar",
      div(class = "container-fluid",
          div(class = "navbar-brand", "Landscape Through Time")
      )
  ),
  
  # Tabbed navigation
  navbarPage(
    title = NULL,
    id = "main_tabs",
    windowTitle = "Landscape Through Time",
    
    # HOME TAB
    tabPanel(
      "Home",
      value = "home",
      
      # Hero section
      div(class = "hero-section",
          div(class = "content",
              h1("Landscape Through Time"),
              p(class = "subtitle", 
                "Explore historical Western landscapes through Albert Bierstadt's iconic paintings and contribute your own modern-day photographs from the same locations. Help us document how these magnificent vistas have changed over 150 years."
              ),
              actionButton("goto_map", "Explore the Map", 
                           class = "btn btn-primary btn-lg", 
                           icon = icon("map-marked-alt"))
          )
      ),
      
      # Statistics
      div(class = "content-section",
          fluidRow(
            column(4,
                   div(class = "stat-box",
                       div(class = "stat-number", "5"),
                       div(class = "stat-label", "Historic Locations")
                   )
            ),
            column(4,
                   div(class = "stat-box",
                       div(class = "stat-number", textOutput("total_submissions")),
                       div(class = "stat-label", "Photo Submissions")
                   )
            ),
            column(4,
                   div(class = "stat-box",
                       div(class = "stat-number", textOutput("approved_count")),
                       div(class = "stat-label", "Approved Comparisons")
                   )
            )
          )
      ),
      
      # About section
      div(class = "content-section",
          div(class = "section-header",
              h2("About This Project")
          ),
          fluidRow(
            column(12,
                   div(class = "feature-card",
                       p("Albert Bierstadt (1830-1902) was one of America's most celebrated landscape painters, known for his dramatic depictions of the American West. This interactive project invites you to visit the locations where Bierstadt set up his easel and document how these landscapes have transformed over time."),
                       h3("How to Participate"),
                       tags$ol(
                         tags$li(strong("Explore"), " the map to find painting locations near you"),
                         tags$li(strong("Visit"), " the actual location and take a photo from a similar vantage point"),
                         tags$li(strong("Submit"), " your photo along with GPS coordinates and observations"),
                         tags$li(strong("View"), " approved comparisons in our gallery to see landscape changes over time")
                       ),
                       p("Your contributions help researchers understand landscape transformation, ecological change, and the enduring cultural significance of these iconic Western vistas.")
                   )
            )
          )
      )
    ),
    
    # EXPLORE MAP TAB
    tabPanel(
      "Explore Map",
      value = "map",
      div(class = "content-section",
          div(class = "section-header",
              h2("Explore Painting Locations"),
              p("Click on any marker to view painting details and location information")
          ),
          leafletOutput("main_map", height = 600)
      )
    ),
    
    # PAINTING GALLERY TAB
    tabPanel(
      "Painting Gallery",
      value = "gallery",
      div(class = "content-section",
          div(class = "section-header",
              h2("Painting Gallery"),
              p("Explore all five Bierstadt paintings in our collection")
          ),
          uiOutput("painting_cards")
      )
    ),
    
    # SUBMIT PHOTO TAB
    tabPanel(
      "Submit Photo",
      value = "submit",
      div(class = "content-section",
          div(class = "section-header",
              h2("Submit Your Photo")
          ),
          div(class = "form-section",
              div(class = "info-box",
                  icon("camera"),
                  strong(" Instructions: "),
                  "Visit one of Bierstadt's painting locations, take a photo from a similar vantage point, and fill out the form below. Your submission will be saved automatically and reviewed by an admin."
              ),
              
              uiOutput("submit_message"),
              
              textInput("submit_name", "Your Name (optional)", 
                        placeholder = "e.g., John Smith"),
              
              textInput("submit_email", "Email (optional)", 
                        placeholder = "e.g., john@example.com"),
              
              selectInput("submit_painting", "Which painting location did you visit?",
                          choices = c("Select a location..." = "", 
                                      setNames(paintings_data$id, paintings_data$title))),
              
              fileInput("submit_photo", "Upload Your Photo",
                        accept = c("image/png", "image/jpeg", "image/jpg"),
                        buttonLabel = "Browse...",
                        placeholder = "No file selected"),
              
              numericInput("submit_latitude", "Latitude (e.g., 40.7489)",
                           value = NA),
              
              numericInput("submit_longitude", "Longitude (e.g., -109.5596)",
                           value = NA),
              
              textAreaInput("submit_observations", "Observations (optional)",
                            rows = 4,
                            placeholder = "Describe what you noticed about the landscape, weather conditions, changes from the painting, etc."),
              
              actionButton("submit_button", "Submit Photo", 
                           class = "btn btn-primary btn-lg",
                           icon = icon("upload"))
          )
      )
    ),
    
    # PAST VS PRESENT GALLERY TAB
    tabPanel(
      "Past vs Present",
      value = "comparisons",
      div(class = "content-section",
          div(class = "section-header",
              h2("Past vs Present Gallery"),
              p("View approved photo submissions comparing historical paintings with modern photographs")
          ),
          
          fluidRow(
            column(4,
                   selectInput("filter_painting", "Filter by Location",
                               choices = c("All Locations" = "", 
                                           setNames(paintings_data$id, paintings_data$title)))
            )
          ),
          
          uiOutput("comparison_gallery")
      )
    ),
    
    # ADMIN TAB
    tabPanel(
      "Admin",
      value = "admin",
      div(class = "content-section",
          div(class = "section-header",
              h2("Admin Panel")
          ),
          
          conditionalPanel(
            condition = "output.admin_authenticated == false",
            div(class = "form-section",
                h3("Authentication Required"),
                passwordInput("admin_password", "Enter Admin Password"),
                actionButton("admin_login", "Login", class = "btn btn-primary")
            )
          ),
          
          conditionalPanel(
            condition = "output.admin_authenticated == true",
            div(class = "info-box",
                icon("info-circle"),
                " Select a submission from the table below and click 'Approve' or 'Reject'. Approved submissions will appear in the Past vs Present gallery."
            ),
            
            fluidRow(
              column(12,
                     actionButton("refresh_admin", "Refresh Data", icon = icon("sync"), class = "btn btn-primary"),
                     actionButton("approve_submission", "Approve Selected", icon = icon("check"), class = "btn btn-primary", style = "margin-left: 10px;"),
                     actionButton("reject_submission", "Reject Selected", icon = icon("times"), class = "btn btn-primary", style = "margin-left: 10px;")
              )
            ),
            br(), br(),
            
            DTOutput("admin_table")
          )
      )
    )
  )
)

################################################################################
# SERVER LOGIC
################################################################################

server <- function(input, output, session) {
  
  # Reactive values - load from files
  rv <- reactiveValues(
    admin_auth = FALSE,
    submission_success = FALSE,
    submission_error = NULL,
    submissions = load_data(SUBMISSIONS_FILE),
    approved = load_data(APPROVED_FILE)
  )
  
  ############################################################################
  # HELPER FUNCTIONS
  ############################################################################
  
  # Function to get statistics
  get_stats <- reactive({
    list(
      total_submissions = nrow(rv$submissions),
      approved_count = nrow(rv$approved)
    )
  })
  
  ############################################################################
  # HOME TAB OUTPUTS
  ############################################################################
  
  output$total_submissions <- renderText({
    stats <- get_stats()
    as.character(stats$total_submissions)
  })
  
  output$approved_count <- renderText({
    stats <- get_stats()
    as.character(stats$approved_count)
  })
  
  # Navigate to map button
  observeEvent(input$goto_map, {
    updateNavbarPage(session, "main_tabs", selected = "map")
  })
  
  # Navigate to submit tab when clicking visit button
  observeEvent(input$goto_submit, {
    updateNavbarPage(session, "main_tabs", selected = "submit")
  })
  
  ############################################################################
  # EXPLORE MAP TAB
  ############################################################################
  
  output$main_map <- renderLeaflet({
    # Create popup content for each painting
    paintings_data$popup_html <- sapply(1:nrow(paintings_data), function(i) {
      painting <- paintings_data[i, ]
      HTML(paste0(
        '<div style="width:300px;">',
        '<img src="', painting$image_url, '" style="width:100%; border-radius:6px; margin-bottom:10px;">',
        '<h4 style="margin:0 0 5px 0; color:#2d5f2d;">', painting$title, '</h4>',
        '<p style="margin:0; color:#666; font-size:12px;">', 
        painting$artist, ', ', painting$year, '</p>',
        '<p style="margin:10px 0; font-size:13px; line-height:1.4;">',
        substr(painting$context, 1, 150), '...</p>',
        '<p style="margin:0; font-size:12px;"><strong>Location:</strong> ',
        round(painting$latitude, 4), ', ', round(painting$longitude, 4), '</p>',
        '</div>'
      ))
    })
    
    # Create the map
    leaflet(paintings_data) %>%
      addTiles() %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~popup_html,
        label = ~title
      ) %>%
      fitBounds(
        lng1 = min(paintings_data$longitude) - 2,
        lat1 = min(paintings_data$latitude) - 2,
        lng2 = max(paintings_data$longitude) + 2,
        lat2 = max(paintings_data$latitude) + 2
      )
  })
  
  ############################################################################
  # PAINTING GALLERY TAB
  ############################################################################
  
  output$painting_cards <- renderUI({
    cards <- lapply(1:nrow(paintings_data), function(i) {
      painting <- paintings_data[i, ]
      
      div(class = "painting-card",
          img(src = painting$image_url, alt = painting$title),
          div(class = "painting-card-body",
              h3(painting$title),
              div(class = "painting-meta",
                  paste0(painting$artist, " • ", painting$year)
              ),
              div(class = "painting-meta",
                  icon("map-marker-alt"),
                  paste0(" ", round(painting$latitude, 4), ", ", round(painting$longitude, 4))
              ),
              p(class = "painting-context", painting$context),
              actionButton(
                inputId = paste0("visit_", painting$id),
                label = "I Visited This Location",
                class = "btn btn-primary",
                icon = icon("camera"),
                onclick = 'Shiny.setInputValue("goto_submit", Math.random())'
              )
          )
      )
    })
    
    div(class = "gallery-grid", cards)
  })
  
  ############################################################################
  # SUBMIT PHOTO TAB
  ############################################################################
  
  output$submit_message <- renderUI({
    if (rv$submission_success) {
      div(class = "alert alert-success",
          icon("check-circle"),
          " Thank you! Your photo has been submitted successfully and is pending review."
      )
    } else if (!is.null(rv$submission_error)) {
      div(class = "alert alert-danger",
          icon("exclamation-circle"),
          " Error: ", rv$submission_error
      )
    }
  })
  
  observeEvent(input$submit_button, {
    # Reset messages
    rv$submission_success <- FALSE
    rv$submission_error <- NULL
    
    # Validation
    if (input$submit_painting == "") {
      rv$submission_error <- "Please select a painting location."
      return()
    }
    
    if (is.null(input$submit_photo)) {
      rv$submission_error <- "Please upload a photo."
      return()
    }
    
    if (is.na(input$submit_latitude) || is.na(input$submit_longitude)) {
      rv$submission_error <- "Please enter valid GPS coordinates."
      return()
    }
    
    # Check file size (5MB limit)
    if (input$submit_photo$size > 5 * 1024 * 1024) {
      rv$submission_error <- "File size must be less than 5MB."
      return()
    }
    
    # Prepare submission data
    tryCatch({
      # Read the uploaded file and convert to base64
      file_path <- input$submit_photo$datapath
      file_data <- readBin(file_path, "raw", file.info(file_path)$size)
      base64_image <- paste0("data:image/jpeg;base64,", base64enc::base64encode(file_data))
      
      # Create submission record
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
      
      # Add to submissions
      rv$submissions <- rbind(rv$submissions, new_submission)
      
      # Save to file for persistence
      save_data(rv$submissions, SUBMISSIONS_FILE)
      
      # Success!
      rv$submission_success <- TRUE
      
      # Clear form
      updateTextInput(session, "submit_name", value = "")
      updateTextInput(session, "submit_email", value = "")
      updateSelectInput(session, "submit_painting", selected = "")
      updateNumericInput(session, "submit_latitude", value = NA)
      updateNumericInput(session, "submit_longitude", value = NA)
      updateTextAreaInput(session, "submit_observations", value = "")
      
    }, error = function(e) {
      rv$submission_error <- paste("Failed to submit:", e$message)
    })
  })
  
  ############################################################################
  # PAST VS PRESENT GALLERY TAB
  ############################################################################
  
  output$comparison_gallery <- renderUI({
    approved <- rv$approved
    
    if (nrow(approved) == 0) {
      return(
        div(class = "info-box",
            h3("No Approved Submissions Yet"),
            p("Be the first to contribute! Visit a painting location and submit your photo. Submissions are automatically saved and will persist between sessions.")
        )
      )
    }
    
    # Filter by painting if selected
    if (input$filter_painting != "") {
      approved <- approved[approved$painting_id == as.integer(input$filter_painting), ]
    }
    
    if (nrow(approved) == 0) {
      return(
        div(class = "info-box",
            p("No approved submissions for this location yet.")
        )
      )
    }
    
    # Create comparison cards
    cards <- lapply(1:nrow(approved), function(i) {
      submission <- approved[i, ]
      painting <- paintings_data[paintings_data$id == submission$painting_id, ]
      
      column(12,
             div(class = "comparison-card",
                 h3(painting$title),
                 div(class = "comparison-images",
                     div(
                       img(src = painting$image_url, alt = "Historical painting"),
                       div(class = "image-label", paste0("Historical (", painting$year, ")"))
                     ),
                     div(
                       img(src = submission$photo_url, alt = "Modern photo"),
                       div(class = "image-label", paste0("Present Day (", submission$submission_date, ")"))
                     )
                 ),
                 p(strong("Photographer: "), submission$name),
                 p(strong("Location: "), 
                   round(submission$latitude, 4), ", ", 
                   round(submission$longitude, 4)),
                 if (!is.na(submission$observations) && submission$observations != "") {
                   p(strong("Observations: "), submission$observations)
                 }
             )
      )
    })
    
    do.call(fluidRow, cards)
  })
  
  ############################################################################
  # ADMIN TAB
  ############################################################################
  
  # Admin authentication (simple password - replace with your own)
  observeEvent(input$admin_login, {
    if (input$admin_password == "admin123") {  # TODO: Change this password!
      rv$admin_auth <- TRUE
    }
  })
  
  output$admin_authenticated <- reactive({
    rv$admin_auth
  })
  outputOptions(output, "admin_authenticated", suspendWhenHidden = FALSE)
  
  # Admin table
  output$admin_table <- renderDT({
    input$refresh_admin  # Dependency for refresh button
    
    submissions <- rv$submissions
    
    if (nrow(submissions) == 0) {
      return(data.frame(Message = "No submissions yet."))
    }
    
    # Select relevant columns for display
    display_data <- submissions[, c("submission_id", "name", "painting_id", 
                                    "latitude", "longitude", "submission_date", 
                                    "approval_status")]
    
    datatable(
      display_data,
      options = list(
        pageLength = 25,
        order = list(list(6, 'desc'))  # Sort by submission date
      ),
      rownames = FALSE,
      selection = 'single'
    )
  })
  
  # Add approve and reject buttons in admin panel
  observeEvent(input$admin_table_rows_selected, {
    if (length(input$admin_table_rows_selected) > 0) {
      # Show approve/reject buttons
      shinyjs::show("approve_btn")
      shinyjs::show("reject_btn")
    }
  })
  
  # Approve submission
  observeEvent(input$approve_submission, {
    if (length(input$admin_table_rows_selected) > 0) {
      selected_row <- input$admin_table_rows_selected
      
      # Get the submission
      submission <- rv$submissions[selected_row, ]
      submission$approval_status <- "Approved"
      
      # Add to approved list
      rv$approved <- rbind(rv$approved, submission)
      save_data(rv$approved, APPROVED_FILE)
      
      # Update submissions list
      rv$submissions[selected_row, "approval_status"] <- "Approved"
      save_data(rv$submissions, SUBMISSIONS_FILE)
      
      showNotification("Submission approved!", type = "message")
    }
  })
  
  # Reject submission
  observeEvent(input$reject_submission, {
    if (length(input$admin_table_rows_selected) > 0) {
      selected_row <- input$admin_table_rows_selected
      
      # Update status
      rv$submissions[selected_row, "approval_status"] <- "Rejected"
      save_data(rv$submissions, SUBMISSIONS_FILE)
      
      showNotification("Submission rejected.", type = "warning")
    }
  })
  
  observeEvent(input$refresh_admin, {
    # Reload data from files
    rv$submissions <- load_data(SUBMISSIONS_FILE)
    rv$approved <- load_data(APPROVED_FILE)
    showNotification("Data refreshed!", type = "message")
  })
}

################################################################################
# RUN THE APPLICATION
################################################################################

shinyApp(ui = ui, server = server)

################################################################################
# DEPLOYMENT INSTRUCTIONS FOR SHINYAPPS.IO
################################################################################
#
# 1. Install rsconnect package:
#    install.packages("rsconnect")
#
# 2. Set up your shinyapps.io account:
#    rsconnect::setAccountInfo(name='YOUR_ACCOUNT',
#                              token='YOUR_TOKEN',
#                              secret='YOUR_SECRET')
#
# 3. Before deploying:
#    - Test the app locally first
#    - Change the admin password (default is "admin123")
#
# 4. Deploy:
#    rsconnect::deployApp()
#
# NOTE: Data is stored in local RDS files (app_data folder). On shinyapps.io,
# this data persists during the session but will be lost when:
# - You redeploy the app
# - The instance restarts
# - The app scales to multiple instances
#
# For production with truly persistent storage across deployments, consider:
# - Setting up a database (PostgreSQL, MySQL)
# - Using cloud storage (AWS S3, Google Cloud Storage)
# - Google Sheets (requires authentication setup)
#
################################################################################