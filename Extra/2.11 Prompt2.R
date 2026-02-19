# ==============================================================================
# Historical Landscape Geocache: Bierstadt Western Paintings
# ==============================================================================
# A public-facing R Shiny app for comparing historical Western U.S. landscape 
# paintings with present-day photographs taken at the same locations.
# ==============================================================================

library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(htmltools)
library(leaflet)

# ==============================================================================
# DATA SETUP
# ==============================================================================

# Placeholder painting data with real Wikimedia Commons images
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
  # Using Wikimedia Commons - public domain Bierstadt images
  image_url = c(
    "https://upload.wikimedia.org/wikipedia/commons/thumb/6/60/Bierstadt_Albert_Among_the_Sierra_Nevada_Mountains.jpg/800px-Bierstadt_Albert_Among_the_Sierra_Nevada_Mountains.jpg",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8e/Albert_Bierstadt_-_Rocky_Mountain_Landscape_-_Google_Art_Project.jpg/800px-Albert_Bierstadt_-_Rocky_Mountain_Landscape_-_Google_Art_Project.jpg",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9e/Albert_Bierstadt_-_Emigrants_Crossing_the_Plains_-_Google_Art_Project.jpg/800px-Albert_Bierstadt_-_Emigrants_Crossing_the_Plains_-_Google_Art_Project.jpg",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/3/31/Looking_Down_Yosemite-Valley.jpg/800px-Looking_Down_Yosemite-Valley.jpg",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ae/Bierstadt-sunset-in-yosemite-valley.jpg/800px-Bierstadt-sunset-in-yosemite-valley.jpg"
  )
)

# TODO: Load approved submissions from your database
approved_photos <- tibble::tibble(
  painting_id = c(1, 2),
  photo_url = c(
    "https://upload.wikimedia.org/wikipedia/commons/thumb/1/13/Ansel_Adams_-_National_Archives_79-AAB-02.jpg/1200px-Ansel_Adams_-_National_Archives_79-AAB-02.jpg",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f7/Rocky_Mountain_National_Park_in_September_2011_-_Glacier_Gorge_from_Bear_Lake.JPG/1200px-Rocky_Mountain_National_Park_in_September_2011_-_Glacier_Gorge_from_Bear_Lake.JPG"
  ),
  submission_date = as.Date(c("2024-05-15", "2024-06-20")),
  photographer = c("Jane Smith", "John Doe"),
  notes = c(
    "Photographed from western overlook at dawn. The tree coverage has increased significantly.",
    "Climate change has affected the snow coverage compared to the historical painting."
  )
)

# ==============================================================================
# UI DEFINITION
# ==============================================================================

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#2c3e50",
    primary = "#3498db",
    base_font = font_google("Lato"),
    heading_font = font_google("Playfair Display")
  ),
  
  useShinyjs(),
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      body { 
        font-family: 'Lato', sans-serif; 
        margin: 0;
        padding: 0;
      }
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
      
      .map-container {
        height: calc(100vh - 200px);
        position: relative;
      }
      
      .map-info {
        background: #e8f4f8;
        padding: 10px 20px;
        border-bottom: 1px solid #ddd;
      }
      
      #map {
        height: 100% !important;
        width: 100% !important;
      }
      
      .tab-content { 
        min-height: calc(100vh - 180px);
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
        max-width: 800px;
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
  
  # Main content
  div(
    class = "container-fluid",
    style = "padding-top: 20px;",
    
    tabsetPanel(
      id = "main_tabs",
      type = "tabs",
      
      # MAP TAB
      tabPanel(
        title = "Explore Map",
        icon = icon("map-location-dot"),
        value = "map_tab",
        
        div(
          class = "map-info",
          p(
            icon("info-circle"), 
            strong(" Click on map markers"),
            " to view historical paintings and location details.",
            style = "margin: 0;"
          )
        ),
        div(
          class = "map-container",
          leafletOutput("map", width = "100%", height = "100%")
        )
      ),
      
      # GALLERY TAB
      tabPanel(
        title = "Painting Gallery",
        icon = icon("palette"),
        value = "gallery_tab",
        
        div(
          class = "container",
          style = "padding: 20px;",
          
          h3("Browse Historical Paintings"),
          p(
            "View the complete collection of Albert Bierstadt's Western landscape paintings.",
            class = "lead mb-4"
          ),
          
          uiOutput("painting_gallery")
        )
      ),
      
      # SUBMIT TAB
      tabPanel(
        title = "Submit Photo",
        icon = icon("camera"),
        value = "submit_tab",
        
        div(
          class = "container",
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
              "All submissions are manually reviewed before appearing publicly.",
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
              placeholder = "Any observations about changes you noticed...",
              rows = 4,
              width = "100%"
            ),
            
            div(
              class = "row",
              div(
                class = "col-md-6",
                numericInput(
                  "submit_lat",
                  "Latitude (optional):",
                  value = NULL,
                  min = -90,
                  max = 90,
                  step = 0.0001
                )
              ),
              div(
                class = "col-md-6",
                numericInput(
                  "submit_lon",
                  "Longitude (optional):",
                  value = NULL,
                  min = -180,
                  max = 180,
                  step = 0.0001
                )
              )
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
      
      # COMPARE TAB
      tabPanel(
        title = "Compare Past & Present",
        icon = icon("images"),
        value = "compare_tab",
        
        div(
          class = "container",
          style = "padding: 20px;",
          
          h3("Visual Comparisons"),
          p(
            "View approved submissions side-by-side with historical paintings.",
            class = "lead mb-4"
          ),
          
          uiOutput("comparison_gallery")
        )
      ),
      
      # ABOUT TAB
      tabPanel(
        title = "About",
        icon = icon("circle-info"),
        value = "about_tab",
        
        div(
          class = "container",
          style = "padding: 20px;",
          
          div(
            class = "row",
            div(
              class = "col-md-8",
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
              )
            ),
            
            div(
              class = "col-md-4",
              div(
                class = "painting-card",
                h4("Technical Details"),
                tags$ul(
                  tags$li(strong("Platform:"), " R Shiny with Leaflet"),
                  tags$li(strong("Data:"), " 100+ Bierstadt paintings"),
                  tags$li(strong("Review:"), " Manual approval"),
                  tags$li(strong("Format:"), " Public-facing")
                )
              ),
              
              div(
                class = "painting-card mt-3",
                h4("Contact"),
                p(
                  icon("envelope"), " ",
                  tags$a(href = "mailto:project@example.edu", "project@example.edu")
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
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  # LEAFLET MAP
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
          "Take a photo here and contribute to the study!",
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
  
  # PAINTING GALLERY
  output$painting_gallery <- renderUI({
    lapply(1:nrow(paintings_data), function(i) {
      painting <- paintings_data[i, ]
      
      div(
        class = "painting-card",
        div(
          class = "row",
          div(
            class = "col-md-5",
            img(src = painting$image_url, class = "painting-img", 
                alt = painting$title)
          ),
          div(
            class = "col-md-7",
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
                sprintf("Coordinates: %.4f, %.4f", painting$latitude, painting$longitude)
              )
            )
          )
        )
      )
    })
  })
  
  # SUBMISSION HANDLER
  observeEvent(input$submit_button, {
    if (is.null(input$submit_photo)) {
      output$submit_message <- renderUI({
        div(class = "alert alert-warning mt-3",
            icon("exclamation-triangle"), " Please upload a photo.")
      })
      return()
    }
    
    if (input$submit_name == "") {
      output$submit_message <- renderUI({
        div(class = "alert alert-warning mt-3",
            icon("exclamation-triangle"), " Please enter your name.")
      })
      return()
    }
    
    # TODO: Save to database
    output$submit_message <- renderUI({
      div(
        class = "alert alert-success mt-3",
        icon("check-circle"), strong(" Submission Received!"), br(),
        "Your photo will be reviewed and added to the gallery once approved."
      )
    })
    
    # Reset form
    shinyjs::reset("submit_photo")
    shinyjs::reset("submit_name")
    shinyjs::reset("submit_notes")
  })
  
  # COMPARISON GALLERY
  output$comparison_gallery <- renderUI({
    if (nrow(approved_photos) == 0) {
      return(
        div(
          class = "info-box text-center",
          style = "padding: 40px;",
          icon("images", style = "font-size: 3em; color: #ccc;"),
          h4("No Comparisons Available Yet", class = "mt-3"),
          p("Be the first to contribute!")
        )
      )
    }
    
    lapply(1:nrow(approved_photos), function(i) {
      photo <- approved_photos[i, ]
      painting <- paintings_data %>% filter(id == photo$painting_id)
      
      if (nrow(painting) == 0) return(NULL)
      
      div(
        class = "comparison-container",
        h4(painting$title, style = "color: #2c3e50;"),
        p(
          class = "text-muted",
          sprintf("Historical: %s | Modern: %s | Photographer: %s", 
                  painting$year, 
                  format(photo$submission_date, "%Y"),
                  photo$photographer)
        ),
        div(
          class = "comparison-images",
          div(
            h5(paste("Historical (", painting$year, ")"), 
               style = "text-align: center; margin-bottom: 10px;"),
            img(src = painting$image_url, alt = painting$title)
          ),
          div(
            h5(paste("Modern (", format(photo$submission_date, "%Y"), ")"),
               style = "text-align: center; margin-bottom: 10px;"),
            img(src = photo$photo_url, alt = "Modern photo")
          )
        ),
        if (!is.na(photo$notes) && photo$notes != "") {
          div(
            class = "mt-3 p-3",
            style = "background: #f8f9fa; border-radius: 4px;",
            strong("Photographer's Notes: "), photo$notes
          )
        }
      )
    })
  })
}

shinyApp(ui = ui, server = server)
