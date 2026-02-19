################################################################################
# LANDSCAPE THROUGH TIME - R SHINY APPLICATION
# A geocache-style data collection tool for comparing historical Western U.S.
# landscape paintings by Albert Bierstadt with present-day photographs
################################################################################

# INSTALLATION INSTRUCTIONS:
# Run this command first to install all required packages:
#install.packages(c("shiny", "shinydashboard", "leaflet", "googlesheets4", 
#                   "googledrive", "htmltools", "DT", "shinyjs"))

################################################################################
# GOOGLE SHEETS SETUP INSTRUCTIONS:
################################################################################
# 1. Create a new Google Sheet with two tabs:
#    - Tab 1: "Submissions" 
#    - Tab 2: "Approved"
#
# 2. In "Submissions" tab, create these column headers in row 1:
#    submission_id | name | email | painting_id | photo_url | latitude | 
#    longitude | observations | submission_date | approval_status
#
# 3. In "Approved" tab, create the same column headers
#
# 4. Get your Google Sheet ID from the URL:
#    https://docs.google.com/spreadsheets/d/YOUR_SHEET_ID_HERE/edit
#
# 5. Replace YOUR_GOOGLE_SHEET_ID below with your actual Sheet ID
#
# 6. For authentication, you have two options:
#    Option A (Recommended for deployment): Use a service account
#      - Create service account in Google Cloud Console
#      - Download JSON key file
#      - Share your Google Sheet with service account email
#      - Set path: Sys.setenv(GS4_AUTH_JSON = "path/to/service-account.json")
#    
#    Option B (For local testing): Use OAuth
#      - Run gs4_auth() interactively first time
#      - Cache token for subsequent uses
################################################################################

# Load required libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(googlesheets4)
library(googledrive)
library(htmltools)
library(DT)
library(shinyjs)

################################################################################
# CONFIGURATION
################################################################################

# TODO: Replace with your Google Sheet ID
GOOGLE_SHEET_ID <- "17h9vh7j-t0gcIcJXc15oA8dLZGR5XHfn7m0b2GYciq8"

# For public access without authentication (if sheet is publicly readable)
# Uncomment this line:
# gs4_deauth()

# For service account authentication (recommended for deployment)
# Uncomment and set path to your service account JSON:
# gs4_auth(path = "path/to/service-account.json")

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

ui <- dashboardPage(
  skin = "green",
  
  # Header
  dashboardHeader(
    title = "Landscape Through Time",
    titleWidth = 300
  ),
  
  # Sidebar (minimal - just for branding)
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Explore Map", tabName = "map", icon = icon("map-marked-alt")),
      menuItem("Painting Gallery", tabName = "gallery", icon = icon("images")),
      menuItem("Submit Photo", tabName = "submit", icon = icon("camera")),
      menuItem("Past vs Present", tabName = "comparisons", icon = icon("th")),
      menuItem("Admin", tabName = "admin", icon = icon("lock"))
    )
  ),
  
  # Main body
  dashboardBody(
    useShinyjs(),
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Playfair+Display:wght@400;700&family=Source+Sans+Pro:wght@300;400;600&display=swap');
        
        /* Overall styling */
        body {
          font-family: 'Source Sans Pro', sans-serif;
        }
        
        h1, h2, h3, h4 {
          font-family: 'Playfair Display', serif;
          color: #2d5f2d;
        }
        
        .content-wrapper {
          background-color: #f9f9f9;
        }
        
        /* Hero section */
        .hero-section {
          background: linear-gradient(135deg, #2d5f2d 0%, #9caf88 100%);
          color: white;
          padding: 60px 40px;
          text-align: center;
          border-radius: 8px;
          margin-bottom: 30px;
        }
        
        .hero-section h1 {
          color: white;
          font-size: 48px;
          margin-bottom: 20px;
          font-weight: 700;
        }
        
        .hero-section p {
          font-size: 20px;
          max-width: 800px;
          margin: 0 auto;
          line-height: 1.6;
        }
        
        /* Stats boxes */
        .stat-box {
          background: white;
          padding: 30px;
          border-radius: 8px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
          text-align: center;
          margin: 10px;
        }
        
        .stat-number {
          font-size: 48px;
          font-weight: bold;
          color: #2d5f2d;
        }
        
        .stat-label {
          font-size: 18px;
          color: #666;
          margin-top: 10px;
        }
        
        /* Painting cards */
        .painting-card {
          background: white;
          border-radius: 8px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.1);
          padding: 20px;
          margin-bottom: 30px;
          transition: transform 0.3s ease, box-shadow 0.3s ease;
        }
        
        .painting-card:hover {
          transform: translateY(-5px);
          box-shadow: 0 8px 20px rgba(0,0,0,0.15);
        }
        
        .painting-card img {
          width: 100%;
          border-radius: 8px;
          margin-bottom: 15px;
        }
        
        .painting-card h3 {
          margin-top: 0;
          font-size: 24px;
        }
        
        .painting-meta {
          color: #666;
          font-size: 14px;
          margin-bottom: 10px;
        }
        
        .painting-context {
          line-height: 1.6;
          color: #444;
          margin-bottom: 15px;
        }
        
        /* Buttons */
        .btn-primary {
          background-color: #2d5f2d;
          border: none;
          padding: 12px 30px;
          font-size: 16px;
          border-radius: 6px;
          transition: background-color 0.3s ease;
        }
        
        .btn-primary:hover {
          background-color: #1f4a1f;
        }
        
        /* Form styling */
        .form-section {
          background: white;
          padding: 40px;
          border-radius: 8px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
          max-width: 800px;
          margin: 0 auto;
        }
        
        .form-group label {
          font-weight: 600;
          color: #2d5f2d;
          margin-bottom: 8px;
        }
        
        .form-control {
          border: 2px solid #d9d9d9;
          border-radius: 6px;
          padding: 10px;
        }
        
        .form-control:focus {
          border-color: #2d5f2d;
          box-shadow: 0 0 0 0.2rem rgba(45, 95, 45, 0.25);
        }
        
        /* Comparison cards */
        .comparison-card {
          background: white;
          border-radius: 8px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.1);
          padding: 20px;
          margin-bottom: 30px;
        }
        
        .comparison-images {
          display: grid;
          grid-template-columns: 1fr 1fr;
          gap: 15px;
          margin-bottom: 15px;
        }
        
        .comparison-images img {
          width: 100%;
          border-radius: 8px;
        }
        
        .image-label {
          text-align: center;
          font-weight: 600;
          color: #2d5f2d;
          margin-top: 8px;
        }
        
        /* Leaflet map */
        .leaflet-container {
          border-radius: 8px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        }
        
        /* Alert messages */
        .alert {
          border-radius: 6px;
          padding: 15px;
          margin-bottom: 20px;
        }
        
        .alert-success {
          background-color: #d4edda;
          border: 1px solid #c3e6cb;
          color: #155724;
        }
        
        .alert-danger {
          background-color: #f8d7da;
          border: 1px solid #f5c6cb;
          color: #721c24;
        }
        
        /* Info box */
        .info-box {
          background: #e8f4f8;
          border-left: 4px solid #87ceeb;
          padding: 20px;
          border-radius: 6px;
          margin-bottom: 20px;
        }
      "))
    ),
    
    # Tab items
    tabItems(
      # HOME TAB
      tabItem(
        tabName = "home",
        
        # Hero section
        div(class = "hero-section",
            h1("Landscape Through Time"),
            p("Explore historical Western landscapes through Albert Bierstadt's iconic paintings and contribute your own modern-day photographs from the same locations. Help us document how these magnificent vistas have changed over 150 years.")
        ),
        
        # Statistics
        fluidRow(
          column(4,
                 div(class = "stat-box",
                     div(class = "stat-number", "5"),
                     div(class = "stat-label", "Historic Painting Locations")
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
        ),
        
        # About section
        fluidRow(
          column(12,
                 div(class = "form-section",
                     h2("About This Project"),
                     p("Albert Bierstadt (1830-1902) was one of America's most celebrated landscape painters, known for his dramatic depictions of the American West. This interactive project invites you to visit the locations where Bierstadt set up his easel and document how these landscapes have transformed over time."),
                     h3("How to Participate"),
                     tags$ol(
                       tags$li(strong("Explore"), " the map to find painting locations near you"),
                       tags$li(strong("Visit"), " the actual location and take a photo from a similar vantage point"),
                       tags$li(strong("Submit"), " your photo along with GPS coordinates and observations"),
                       tags$li(strong("View"), " approved comparisons in our gallery to see landscape changes over time")
                     ),
                     p("Your contributions help researchers understand landscape transformation, ecological change, and the enduring cultural significance of these iconic Western vistas."),
                     br(),
                     actionButton("goto_map", "Explore the Map", class = "btn btn-primary btn-lg", 
                                  icon = icon("map-marked-alt"))
                 )
          )
        )
      ),
      
      # EXPLORE MAP TAB
      tabItem(
        tabName = "map",
        h2("Explore Painting Locations"),
        div(class = "info-box",
            icon("info-circle"),
            " Click on any marker to view painting details and location information."
        ),
        fluidRow(
          column(12,
                 leafletOutput("main_map", height = 600)
          )
        )
      ),
      
      # PAINTING GALLERY TAB
      tabItem(
        tabName = "gallery",
        h2("Painting Gallery"),
        p("Explore all five Bierstadt paintings in our collection. Each location is waiting for your modern photograph!"),
        br(),
        uiOutput("painting_cards")
      ),
      
      # SUBMIT PHOTO TAB
      tabItem(
        tabName = "submit",
        h2("Submit Your Photo"),
        
        div(class = "form-section",
            div(class = "info-box",
                icon("camera"),
                strong(" Instructions: "),
                "Visit one of Bierstadt's painting locations, take a photo from a similar vantage point, and share it with us! Your submission will be reviewed before appearing in the gallery."
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
      ),
      
      # PAST VS PRESENT GALLERY TAB
      tabItem(
        tabName = "comparisons",
        h2("Past vs Present Gallery"),
        p("View approved photo submissions comparing Bierstadt's historical paintings with modern photographs."),
        br(),
        
        fluidRow(
          column(4,
                 selectInput("filter_painting", "Filter by Location",
                             choices = c("All Locations" = "", 
                                         setNames(paintings_data$id, paintings_data$title)))
          )
        ),
        
        uiOutput("comparison_gallery")
      ),
      
      # ADMIN TAB
      tabItem(
        tabName = "admin",
        h2("Admin Panel"),
        
        # Simple password protection
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
              " Review pending submissions below. To approve a submission, manually change its status to 'Approved' in the Google Sheet 'Submissions' tab, or copy the row to the 'Approved' tab."
          ),
          
          actionButton("refresh_admin", "Refresh Data", icon = icon("sync")),
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
  
  # Reactive values
  rv <- reactiveValues(
    admin_auth = FALSE,
    submission_success = FALSE,
    submission_error = NULL
  )
  
  ############################################################################
  # HELPER FUNCTIONS
  ############################################################################
  
  # Function to safely read from Google Sheets
  safe_read_sheet <- function(sheet_name) {
    tryCatch({
      read_sheet(GOOGLE_SHEET_ID, sheet = sheet_name)
    }, error = function(e) {
      return(data.frame())  # Return empty data frame on error
    })
  }
  
  # Function to get statistics
  get_stats <- reactive({
    submissions <- safe_read_sheet("Submissions")
    approved <- safe_read_sheet("Approved")
    
    list(
      total_submissions = nrow(submissions),
      approved_count = nrow(approved)
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
    updateTabItems(session, "sidebar", "map")
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
      
      column(6,
             div(class = "painting-card",
                 img(src = painting$image_url, alt = painting$title),
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
    
    do.call(fluidRow, cards)
  })
  
  # Navigate to submit tab when clicking visit button
  observeEvent(input$goto_submit, {
    updateTabItems(session, "sidebar", "submit")
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
      submission <- data.frame(
        submission_id = as.character(as.integer(Sys.time())),
        name = ifelse(input$submit_name == "", "Anonymous", input$submit_name),
        email = input$submit_email,
        painting_id = as.integer(input$submit_painting),
        photo_url = base64_image,  # Store as base64
        latitude = input$submit_latitude,
        longitude = input$submit_longitude,
        observations = input$submit_observations,
        submission_date = as.character(Sys.Date()),
        approval_status = "Pending",
        stringsAsFactors = FALSE
      )
      
      # Append to Google Sheets
      sheet_append(GOOGLE_SHEET_ID, submission, sheet = "Submissions")
      
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
    approved <- safe_read_sheet("Approved")
    
    if (nrow(approved) == 0) {
      return(
        div(class = "info-box",
            h3("No Approved Submissions Yet"),
            p("Be the first to contribute! Visit a painting location and submit your photo.")
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
    
    submissions <- safe_read_sheet("Submissions")
    
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
      rownames = FALSE
    )
  })
  
  observeEvent(input$refresh_admin, {
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
# 3. Before deploying, make sure:
#    - You've replaced GOOGLE_SHEET_ID with your actual Sheet ID
#    - You've set up Google Sheets authentication (service account recommended)
#    - You've tested the app locally
#
# 4. Deploy:
#    rsconnect::deployApp()
#
# 5. For Google Sheets authentication on shinyapps.io:
#    - Use service account JSON
#    - Upload the JSON file with your app
#    - Reference it in your code: gs4_auth(path = "service-account.json")
#
################################################################################