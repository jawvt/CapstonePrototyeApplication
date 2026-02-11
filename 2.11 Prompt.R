# ==============================================================================
# Historical Landscape Geocache: Bierstadt Western Paintings
# ==============================================================================
# A public-facing R Shiny app for comparing historical Western U.S. landscape 
# paintings with present-day photographs taken at the same locations.
#
# Author: Art History & Historical Ecology Team
# Purpose: Enable public data collection for ecological/cultural analysis
# ==============================================================================

library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(htmltools)
library(leaflet)
#install.packages("leaflet.extras")
library(leaflet.extras)

# ==============================================================================
# DATA SETUP
# ==============================================================================

# Placeholder painting data - replace with your curated dataset
# TODO: Load from CSV or database with real painting metadata
paintings_data <- tibble::tibble(
  id = 1:5,
  title = c(
    "Among the Sierra Nevada, California",
    "Rocky Mountain Landscape",
    "The Oregon Trail",
    "Valley of the Yosemite",
    "Sunset in the Yosemite Valley"
  ),
  artist = rep("Albert Bierstadt", 5),
  year = c(1868, 1870, 1869, 1864, 1868),
  latitude = c(37.7749, 40.3772, 42.8751, 37.7459, 37.7459),
  longitude = c(-119.4194, -105.5217, -108.7496, -119.5332, -119.5332),
  description = c(
    "Bierstadt's romanticized vision of the Sierra Nevada showcases the sublime wilderness that captivated 19th-century Americans, painted during the height of westward expansion.",
    "This monumental canvas exemplifies the Hudson River School's influence on Western landscape painting, emphasizing dramatic light and geological grandeur.",
    "Depicting pioneers on their westward journey, this work reflects both the promise and mythology of Manifest Destiny through Bierstadt's characteristic luminous palette.",
    "One of Bierstadt's earliest Yosemite paintings, created after his 1863 expedition, helping establish the valley as an icon of American natural beauty.",
    "Bierstadt's mastery of atmospheric perspective and golden-hour lighting transforms this Yosemite scene into a meditation on nature's ephemeral beauty."
  ),
  # Placeholder image paths - replace with actual hosted images
  image_url = c(
    "https://via.placeholder.com/800x600?text=Sierra+Nevada+1868",
    "https://via.placeholder.com/800x600?text=Rocky+Mountain+1870",
    "https://via.placeholder.com/800x600?text=Oregon+Trail+1869",
    "https://via.placeholder.com/800x600?text=Yosemite+Valley+1864",
    "https://via.placeholder.com/800x600?text=Yosemite+Sunset+1868"
  )
)

# TODO: Load approved submissions from your database or API
# For now, use placeholder data showing example structure
approved_photos <- tibble::tibble(
  painting_id = c(1, 2),
  photo_url = c(
    "https://via.placeholder.com/800x600?text=Modern+Photo+Sierra+2024",
    "https://via.placeholder.com/800x600?text=Modern+Photo+Rockies+2024"
  ),
  submission_date = as.Date(c("2024-05-15", "2024-06-20")),
  photographer = c("Jane Smith", "John Doe"),
  notes = c(
    "Photographed from western overlook at dawn",
    "Some tree growth visible compared to original"
  )
)

# ==============================================================================
# UI DEFINITION
# ==============================================================================

ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#2c3e50",
    primary = "#3498db",
    base_font = font_google("Lato"),
    heading_font = font_google("Playfair Display")
  ),
  
  # Custom CSS for clean, modern design
  tags$head(
    tags$style(HTML("
      body { font-family: 'Lato', sans-serif; }
      h1, h2, h3, h4, h5 { font-family: 'Playfair Display', serif; }
      
      .navbar-custom { 
        background: linear-gradient(135deg, #2c3e50 0%, #3498db 100%);
        padding: 20px 0;
        margin-bottom: 0;
      }
      .navbar-custom h2 { 
        color: white; 
        margin: 0; 
        font-weight: 600;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.2);
      }
      .navbar-custom p { 
        color: rgba(255,255,255,0.9); 
        margin: 5px 0 0 0; 
      }
      
      .tab-content { 
        height: calc(100vh - 180px); 
        overflow: auto;
      }
      
      .painting-card { 
        border: 1px solid #e0e0e0; 
        border-radius: 8px; 
        padding: 20px; 
        margin-bottom: 20px;
        background: white;
        box-shadow: 0 2px 8px rgba(0,0,0,0.05);
        transition: transform 0.2s, box-shadow 0.2s;
      }
      .painting-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
      }
      
      .painting-img { 
        width: 100%; 
        border-radius: 4px; 
        margin-bottom: 15px;
      }
      
      .leaflet-container { 
        height: 100% !important; 
        border-radius: 8px;
      }
      
      .comparison-container {
        margin-bottom: 30px;
        border: 1px solid #e0e0e0;
        border-radius: 8px;
        padding: 20px;
        background: white;
      }
      
      .comparison-images {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 20px;
        margin-top: 15px;
      }
      
      .comparison-images img {
        width: 100%;
        border-radius: 4px;
        border: 1px solid #ddd;
      }
      
      .metadata-text { 
        font-size: 0.95rem; 
        line-height: 1.6; 
        color: #555;
      }
      
      .location-prompt {
        background: #f8f9fa;
        padding: 15px;
        border-left: 4px solid #3498db;
        border-radius: 4px;
        margin-top: 15px;
      }
      
      .submission-form {
        background: #f8f9fa;
        padding: 30px;
        border-radius: 8px;
        border: 1px solid #e0e0e0;
      }
      
      .info-box {
        background: #e8f4f8;
        border-left: 4px solid #3498db;
        padding: 15px;
        margin-bottom: 20px;
        border-radius: 4px;
      }
      
      .nav-tabs {
        border-bottom: 2px solid #3498db;
      }
      
      .nav-tabs .nav-link {
        color: #2c3e50;
        font-weight: 500;
        border: none;
        padding: 12px 24px;
      }
      
      .nav-tabs .nav-link:hover {
        border: none;
        color: #3498db;
      }
      
      .nav-tabs .nav-link.active {
        color: #3498db;
        border: none;
        border-bottom: 3px solid #3498db;
        background: transparent;
      }
    "))
  ),
  
  useShinyjs(),
  
  # Header
  div(
    class = "navbar-custom",
    div(
      class = "container-fluid",
      h2("Historical Landscape Geocache", style = "text-align: center;"),
      p("Comparing Bierstadt's Western Paintings with Modern Photographs", 
        style = "text-align: center;")
    )
  ),
  
  # Main tabset panel
  div(
    class = "container-fluid mt-3",
    style = "height: calc(100vh - 120px);",
    
    tabsetPanel(
      id = "main_tabs",
      type = "tabs",
      
      # ==============================================================================
      # TAB 1: EXPLORE MAP
      # ==============================================================================
      tabPanel(
        title = "Explore Map",
        icon = icon("map-location-dot"),
        value = "map_tab",
        
        div(
          style = "height: 100%; padding: 20px;",
          
          div(
            class = "info-box",
            p(
              icon("info-circle"), 
              strong(" Click on map markers"),
              "to view historical paintings and location details. Plan your visit to capture a modern photo!"
            )
          ),
          
          div(
            style = "height: calc(100% - 80px);",
            leafletOutput("map", height = "100%")
          )
        )
      ),
      
      # ==============================================================================
      # TAB 2: PAINTING GALLERY
      # ==============================================================================
      tabPanel(
        title = "Painting Gallery",
        icon = icon("palette"),
        value = "gallery_tab",
        
        div(
          class = "tab-content",
          style = "padding: 20px;",
          
          h3("Browse Historical Paintings"),
          p(
            "View the complete collection of Albert Bierstadt's Western landscape paintings.",
            "Each work includes location data and art-historical context.",
            class = "lead mb-4"
          ),
          
          uiOutput("painting_gallery")
        )
      ),
      
      # ==============================================================================
      # TAB 3: SUBMIT PHOTO
      # ==============================================================================
      tabPanel(
        title = "Submit Photo",
        icon = icon("camera"),
        value = "submit_tab",
        
        div(
          class = "tab-content",
          style = "padding: 20px;",
          
          h3("Contribute a Modern Photograph"),
          
          div(
            class = "info-box mb-4",
            h5("How to Participate:"),
            tags$ol(
              tags$li("Visit one of the painting locations shown on the Explore Map"),
              tags$li("Take a photograph from approximately the same vantage point"),
              tags$li("Fill out the submission form below"),
              tags$li("Your photo will be reviewed and added to the comparison gallery")
            ),
            p(
              icon("clock"), strong(" Review Process: "),
              "All submissions are manually reviewed before appearing publicly to ensure quality and authenticity.",
              class = "mt-3 mb-0"
            )
          ),
          
          div(
            class = "submission-form",
            
            selectInput(
              "submit_painting",
              "Select Painting Location:",
              choices = setNames(paintings_data$id, 
                                 paste0(paintings_data$title, " (", paintings_data$year, ")")),
              width = "100%"
            ),
            
            fileInput(
              "submit_photo",
              "Upload Your Photo:",
              accept = c("image/jpeg", "image/jpg", "image/png"),
              width = "100%"
            ),
            
            textInput(
              "submit_name",
              "Your Name:",
              placeholder = "Enter your name",
              width = "100%"
            ),
            
            textAreaInput(
              "submit_notes",
              "Notes (optional):",
              placeholder = "Any observations about changes you noticed, time of day, weather conditions, etc.",
              rows = 4,
              width = "100%"
            ),
            
            div(
              class = "mt-3",
              p(
                icon("location-dot"), 
                strong(" GPS Location: "),
                "Will be automatically captured from your photo's EXIF data, or you can manually enter coordinates.",
                class = "text-muted"
              )
            ),
            
            numericInput(
              "submit_lat",
              "Latitude (if not in photo):",
              value = NULL,
              min = -90,
              max = 90,
              width = "48%",
              step = 0.0001
            ),
            
            numericInput(
              "submit_lon",
              "Longitude (if not in photo):",
              value = NULL,
              min = -180,
              max = 180,
              width = "48%",
              step = 0.0001
            ),
            
            actionButton(
              "submit_button",
              "Submit Photo",
              icon = icon("paper-plane"),
              class = "btn-primary btn-lg mt-3",
              width = "100%"
            ),
            
            uiOutput("submit_message")
          )
        )
      ),
      
      # ==============================================================================
      # TAB 4: COMPARE PAST & PRESENT
      # ==============================================================================
      tabPanel(
        title = "Compare Past & Present",
        icon = icon("images"),
        value = "compare_tab",
        
        div(
          class = "tab-content",
          style = "padding: 20px;",
          
          h3("Visual Comparisons"),
          p(
            "View approved submissions side-by-side with historical paintings.",
            "Observe landscape changes, ecological shifts, and cultural transformations.",
            class = "lead mb-4"
          ),
          
          uiOutput("comparison_gallery")
        )
      ),
      
      # ==============================================================================
      # TAB 5: ABOUT
      # ==============================================================================
      tabPanel(
        title = "About",
        icon = icon("circle-info"),
        value = "about_tab",
        
        div(
          class = "tab-content",
          style = "padding: 20px;",
          
          layout_columns(
            col_widths = c(8, 4),
            
            div(
              h3("Project Overview"),
              
              h4("Purpose", class = "mt-4"),
              p(
                "This geocache-style project invites the public to participate in",
                "historical ecology research by capturing modern photographs at locations",
                "depicted in 19th-century Western landscape paintings."
              ),
              
              h4("About Albert Bierstadt", class = "mt-4"),
              p(
                "Albert Bierstadt (1830-1902) was a German-American painter known for his",
                "vast, dramatic landscapes of the American West. As a member of the Hudson",
                "River School, his works helped shape American perceptions of Western",
                "wilderness during the era of westward expansion."
              ),
              
              h4("Research Value", class = "mt-4"),
              p(
                "By comparing historical artistic representations with contemporary photographs,",
                "researchers can study changes in vegetation, water levels, wildlife, human",
                "development, and climate impacts over 150+ years."
              ),
              
              h4("Data Usage", class = "mt-4"),
              p(
                "All submitted photographs are reviewed before publication. Approved",
                "submissions become part of a public research dataset available for",
                "ecological, cultural, and art-historical analysis."
              )
            ),
            
            div(
              div(
                class = "painting-card",
                h4("Technical Details"),
                tags$ul(
                  tags$li(strong("Platform:"), "R Shiny with Leaflet mapping"),
                  tags$li(strong("Data:"), "100+ Bierstadt paintings with geo-coordinates"),
                  tags$li(strong("Review:"), "Manual approval process for all submissions"),
                  tags$li(strong("Format:"), "Public-facing, accessible interface")
                )
              ),
              
              div(
                class = "painting-card mt-3",
                h4("Contact & Feedback"),
                p("Questions about the project or submissions?"),
                p(
                  icon("envelope"), " ", 
                  tags$a(href = "mailto:project@example.edu", "project@example.edu")
                ),
                p(
                  icon("github"), " ", 
                  tags$a(href = "https://github.com/yourproject", 
                         target = "_blank", "View on GitHub")
                )
              )
            )
          )
        )
      )
    )
  )
)
# ==============================================================================
# SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {
  
  # ==============================================================================
  # REACTIVE VALUES
  # ==============================================================================
  
  selected_painting <- reactiveVal(NULL)
  
  # ==============================================================================
  # MAP TAB: LEAFLET INTERACTIVE MAP
  # ==============================================================================
  
  output$map <- renderLeaflet({
    leaflet(data = paintings_data) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$OpenStreetMap, group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 10,
        color = "#3498db",
        fillColor = "#3498db",
        fillOpacity = 0.7,
        weight = 2,
        popup = ~paste0(
          "<div style='width: 300px;'>",
          "<h5 style='margin-bottom: 10px; color: #2c3e50;'>", title, "</h5>",
          "<img src='", image_url, "' style='width: 100%; border-radius: 4px; margin-bottom: 10px;'>",
          "<p style='margin: 5px 0;'><strong>Artist:</strong> ", artist, "</p>",
          "<p style='margin: 5px 0;'><strong>Year:</strong> ", year, "</p>",
          "<p style='margin: 10px 0 5px 0; font-size: 0.9em;'>", description, "</p>",
          "<div style='background: #f8f9fa; padding: 10px; border-radius: 4px; margin-top: 10px;'>",
          "<strong>📍 Visit this location</strong><br>",
          "Take a photo from this vantage point and contribute to the study!",
          "</div>",
          "</div>"
        ),
        layerId = ~id
      ) %>%
      addLayersControl(
        baseGroups = c("Satellite", "Street Map", "Topographic"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(lng = -110, lat = 40, zoom = 5)
  })
  
  # Handle map marker clicks
  observeEvent(input$map_marker_click, {
    clicked_id <- input$map_marker_click$id
    selected_painting(clicked_id)
  })
  
  # ==============================================================================
  # GALLERY TAB: PAINTING CARDS
  # ==============================================================================
  
  output$painting_gallery <- renderUI({
    lapply(1:nrow(paintings_data), function(i) {
      painting <- paintings_data[i, ]
      
      div(
        class = "painting-card",
        div(
          style = "display: grid; grid-template-columns: 1fr 2fr; gap: 20px;",
          
          # Image column
          div(
            img(src = painting$image_url, class = "painting-img", 
                alt = painting$title)
          ),
          
          # Details column
          div(
            h4(painting$title, style = "margin-top: 0; color: #2c3e50;"),
            p(
              class = "metadata-text",
              strong("Artist: "), painting$artist, br(),
              strong("Year: "), painting$year, br(),
              strong("Location: "), 
              sprintf("%.4f°N, %.4f°W", painting$latitude, abs(painting$longitude))
            ),
            p(class = "metadata-text", painting$description),
            div(
              class = "location-prompt",
              p(
                icon("map-marker-alt"), strong(" Visit This Location"), br(),
                "Coordinates: ", 
                sprintf("%.4f, %.4f", painting$latitude, painting$longitude), br(),
                "Find this spot and capture your own photograph to contribute to the study!"
              )
            )
          )
        )
      )
    })
  })
  
  # ==============================================================================
  # SUBMIT TAB: PHOTO SUBMISSION HANDLING
  # ==============================================================================
  
  # TODO: Connect to your backend database/API for storing submissions
  submissions_list <- reactiveVal(list())
  
  observeEvent(input$submit_button, {
    # Validation
    if (is.null(input$submit_photo)) {
      output$submit_message <- renderUI({
        div(
          class = "alert alert-warning mt-3",
          icon("exclamation-triangle"), " Please upload a photo."
        )
      })
      return()
    }
    
    if (input$submit_name == "" || is.null(input$submit_name)) {
      output$submit_message <- renderUI({
        div(
          class = "alert alert-warning mt-3",
          icon("exclamation-triangle"), " Please enter your name."
        )
      })
      return()
    }
    
    # TODO: In production, save to database
    # Example structure for backend integration:
    # submission_data <- list(
    #   painting_id = input$submit_painting,
    #   photo_path = input$submit_photo$datapath,
    #   photographer = input$submit_name,
    #   notes = input$submit_notes,
    #   latitude = input$submit_lat,
    #   longitude = input$submit_lon,
    #   submission_date = Sys.Date(),
    #   status = "pending"  # Will be reviewed by admin
    # )
    # 
    # save_submission_to_database(submission_data)
    
    # Show success message
    output$submit_message <- renderUI({
      div(
        class = "alert alert-success mt-3",
        icon("check-circle"), strong(" Submission Received!"), br(),
        "Thank you for your contribution. Your photo will be reviewed and",
        "added to the comparison gallery once approved."
      )
    })
    
    # Reset form
    shinyjs::reset("submit_photo")
    shinyjs::reset("submit_name")
    shinyjs::reset("submit_notes")
    shinyjs::reset("submit_lat")
    shinyjs::reset("submit_lon")
  })
  
  # ==============================================================================
  # COMPARE TAB: PAST & PRESENT COMPARISONS
  # ==============================================================================
  
  output$comparison_gallery <- renderUI({
    # Filter approved photos to match with paintings
    if (nrow(approved_photos) == 0) {
      return(
        div(
          class = "info-box",
          style = "text-align: center; padding: 40px;",
          icon("images", style = "font-size: 3em; color: #ccc;"),
          h4("No Comparisons Available Yet", style = "margin-top: 20px;"),
          p("Be the first to contribute a modern photograph!", 
            "Visit the Submit Photo tab to get started.")
        )
      )
    }
    
    # Create comparison cards for each approved photo
    lapply(1:nrow(approved_photos), function(i) {
      photo <- approved_photos[i, ]
      painting <- paintings_data %>% filter(id == photo$painting_id)
      
      if (nrow(painting) == 0) return(NULL)
      
      div(
        class = "comparison-container",
        
        h4(painting$title, style = "color: #2c3e50; margin-bottom: 10px;"),
        p(
          class = "text-muted",
          "Historical: ", painting$year, " | ",
          "Modern: ", format(photo$submission_date, "%B %Y"), " | ",
          "Photographer: ", photo$photographer
        ),
        
        div(
          class = "comparison-images",
          
          # Historical painting
          div(
            h5("Historical (", painting$year, ")", style = "text-align: center; margin-bottom: 10px;"),
            img(src = painting$image_url, alt = paste("Historical", painting$title))
          ),
          
          # Modern photo
          div(
            h5("Modern (", format(photo$submission_date, "%Y"), ")", 
               style = "text-align: center; margin-bottom: 10px;"),
            img(src = photo$photo_url, alt = paste("Modern photo of", painting$title))
          )
        ),
        
        if (!is.na(photo$notes) && photo$notes != "") {
          div(
            class = "mt-3 p-3",
            style = "background: #f8f9fa; border-radius: 4px;",
            strong("Photographer's Notes: "),
            photo$notes
          )
        }
      )
    })
  })
  
}

# ==============================================================================
# RUN THE APP
# ==============================================================================

shinyApp(ui = ui, server = server)

# ==============================================================================
# DEPLOYMENT & INTEGRATION NOTES
# ==============================================================================

# DATABASE INTEGRATION:
# Replace the placeholder data and submission handling with your backend:
# 
# 1. Load paintings from database or CSV:
#    paintings_data <- read.csv("paintings_metadata.csv")
#    OR
#    paintings_data <- dbGetQuery(con, "SELECT * FROM paintings")
#
# 2. Load approved photos from database:
#    approved_photos <- dbGetQuery(con, 
#      "SELECT * FROM submissions WHERE status = 'approved'")
#
# 3. Photo submission handler:
#    - Save uploaded file to server storage or cloud (S3, etc.)
#    - Insert submission record into database with status = 'pending'
#    - Send notification to admin for review
#
# 4. Admin review workflow (separate admin panel recommended):
#    - Create admin interface to review pending submissions
#    - Admin can approve/reject and add to approved_photos table
#    - Approved items automatically appear in comparison gallery
#
# DEPLOYMENT:
# - shinyapps.io: Free tier for public access
# - RStudio Connect: For institutional hosting
# - Self-hosted: shiny-server on Linux VM
#
# REQUIRED PACKAGES:
# install.packages(c("shiny", "shinyjs", "bslib", "dplyr", "htmltools", 
#                    "leaflet", "leaflet.extras"))