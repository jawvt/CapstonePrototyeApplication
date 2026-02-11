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
library(leaflet)

# ==============================================================================
# DATA SETUP - Using real Bierstadt images from topofart.com CDN
# ==============================================================================

paintings_data <- tibble::tibble(
  id = 1:24,
  title = c(
    "Among the Sierra Nevada Mountains, California",
    "Yosemite Valley",
    "Moose",
    "Kern River Valley California",
    "Landscape",
    "The Buffalo Trail",
    "Valley of the Yosemite",
    "The Rocky Mountains, Lander's Peak",
    "Mountain Brook",
    "Sunlight and Shadow",
    "Thunderstorm in the Rocky Mountains",
    "View from the Wind River Mountains, Wyoming",
    "Storm in the Mountains",
    "Yosemite Falls",
    "Ruins of Paestum",
    "River Scene",
    "Roman Fish Market, Arch of Octavius",
    "Fishing Boats at Capri",
    "Fir Trees and Storm Clouds",
    "The Matterhorn",
    "Yosemite Valley",
    "Lake Tahoe, California",
    "Mount Vesuvius at Midnight",
    "The Merced River in Yosemite"
  ),
  artist = rep("Albert Bierstadt", 24),
  year = c(
    1868, 1863, 1880, 1871, 1899, 1869, 1864, 1863, 1863, 1862,
    1859, 1860, 1870, 1863, 1858, 1858, 1858, 1857, 1870, 1867,
    1866, 1867, 1868, 1868
  ),
  # Approximate coordinates for Western paintings
  latitude = c(
    37.7749, 37.7459, 40.5, 35.8, 37.0, 40.0, 37.7459, 40.3772,
    40.0, 38.5, 40.3, 42.7, 40.2, 37.7459, 40.48, 41.0, 41.9,
    40.5, 38.0, 45.9, 37.7459, 39.0, 40.8, 37.7459
  ),
  longitude = c(
    -119.4194, -119.5332, -105.0, -118.9, -110.0, -105.5, -119.5332, -105.5217,
    -105.0, -109.0, -105.5, -109.5, -105.3, -119.5332, -118.0, -110.0, -115.0,
    -111.0, -109.5, 7.7, -119.5332, -120.0, 14.4, -119.5332
  ),
  description = c(
    "Bierstadt's romanticized vision of the Sierra Nevada showcases the sublime wilderness that captivated 19th-century Americans during westward expansion.",
    "One of Bierstadt's masterful depictions of Yosemite Valley, painted during his explorations of California's wilderness.",
    "A dramatic portrayal of a moose in its natural habitat, demonstrating Bierstadt's attention to wildlife.",
    "Capturing the beauty of California's Kern River Valley with characteristic luminosity and detail.",
    "A serene landscape painted near the end of Bierstadt's career, showing his continued mastery of light.",
    "Depicting the American buffalo in their natural environment, a symbol of the vanishing frontier.",
    "An intimate view of Yosemite Valley showcasing Bierstadt's ability to capture both grandeur and detail.",
    "Perhaps Bierstadt's most famous work, this monumental canvas established him as America's foremost landscape painter.",
    "A peaceful mountain stream scene demonstrating Bierstadt's skill in rendering water and forest light.",
    "Dramatic interplay of light and shadow in a Western forest setting.",
    "Capturing the raw power of Rocky Mountain weather with theatrical drama.",
    "An expansive view from Wyoming's Wind River Mountains showing the vastness of the American West.",
    "Dark storm clouds gathering over mountain peaks, emphasizing nature's sublime power.",
    "The iconic Yosemite Falls rendered with Bierstadt's characteristic attention to atmospheric effects.",
    "From Bierstadt's European travels, showing ancient ruins in romantic light.",
    "A tranquil river scene demonstrating his mastery of smaller-scale compositions.",
    "A bustling market scene from Bierstadt's time studying in Rome.",
    "Maritime scene from the Italian coast, painted during his European sojourn.",
    "Towering fir trees beneath gathering storm clouds in a Western forest.",
    "The famous Swiss mountain rendered during Bierstadt's European travels.",
    "Another stunning view of Yosemite Valley painted in Bierstadt's mature style.",
    "The pristine beauty of Lake Tahoe captured in crystalline detail.",
    "A dramatic nighttime view of the active volcano, showcasing unusual subject matter.",
    "The Merced River flowing through Yosemite with characteristic attention to water and light."
  ),
  # Direct CDN URLs from topofart.com - these are high-quality images
  image_url = c(
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt001.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt002.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt003.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt004.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt005.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt006.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt007.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt008.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt009.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt010.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt011.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt012.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt013.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt014.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt015.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt016.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt017.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt018.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt019.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt020.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt021.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt022.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt023.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt024.jpg"
  )
)

# Sample approved photos for demonstration (using same images as placeholders)
approved_photos <- tibble::tibble(
  painting_id = c(1, 8),
  photo_url = c(
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt001.jpg",
    "https://cdn.topofart.com/images/artists/Albert_Bierstadt/paintings-lg/bierstadt008.jpg"
  ),
  submission_date = as.Date(c("2024-05-15", "2024-06-20")),
  photographer = c("Jane Smith", "John Doe"),
  notes = c(
    "Photographed from western overlook at dawn. Tree coverage has increased significantly since the 1860s.",
    "Climate change has affected snow coverage compared to the historical painting."
  )
)

# ==============================================================================
# UI
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
  
  tags$head(
    tags$style(HTML("
      body { font-family: 'Lato', sans-serif; margin: 0; padding: 0; }
      h1, h2, h3, h4, h5 { font-family: 'Playfair Display', serif; }
      
      .navbar-custom { 
        background: linear-gradient(135deg, #2c3e50 0%, #3498db 100%);
        padding: 20px;
        margin-bottom: 20px;
        color: white;
        text-align: center;
      }
      
      .map-container { 
        height: 600px; 
        width: 100%; 
        border: 1px solid #ddd;
        border-radius: 8px;
        overflow: hidden;
      }
      
      .painting-card { 
        border: 1px solid #e0e0e0; 
        border-radius: 8px; 
        padding: 20px; 
        margin: 20px 0;
        background: white;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        transition: transform 0.2s;
      }
      .painting-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(0,0,0,0.15);
      }
      
      .painting-img { 
        width: 100%; 
        height: auto; 
        border-radius: 4px;
        display: block;
      }
      
      .comparison-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 20px;
        margin: 20px 0;
      }
      
      .comparison-grid img {
        width: 100%;
        height: auto;
        border-radius: 4px;
        border: 1px solid #ddd;
      }
      
      .info-box {
        background: #e8f4f8;
        border-left: 4px solid #3498db;
        padding: 15px;
        margin: 15px 0;
        border-radius: 4px;
      }
      
      .submission-form {
        max-width: 700px;
        margin: 20px auto;
        padding: 30px;
        background: #f9f9f9;
        border-radius: 8px;
        border: 1px solid #e0e0e0;
      }
      
      .leaflet-container {
        height: 100% !important;
      }
    "))
  ),
  
  div(class = "navbar-custom",
      h2("Historical Landscape Geocache"),
      p("Comparing Bierstadt's 19th Century Western Paintings with Modern Photographs")
  ),
  
  div(class = "container-fluid",
      tabsetPanel(
        id = "main_tabs",
        
        # ==================================================================
        # MAP TAB
        # ==================================================================
        tabPanel(
          "Explore Map",
          icon = icon("map-location-dot"),
          br(),
          div(class = "info-box",
              p(icon("info-circle"), strong(" Click on map markers"), 
                "to view historical paintings and location details. Plan your visit!")
          ),
          div(class = "map-container",
              leafletOutput("map", height = "100%", width = "100%")
          )
        ),
        
        # ==================================================================
        # GALLERY TAB
        # ==================================================================
        tabPanel(
          "Painting Gallery",
          icon = icon("palette"),
          br(),
          h3("Browse Historical Paintings", style = "text-align: center;"),
          p("View the complete collection of Albert Bierstadt's Western landscape paintings.",
            class = "text-center text-muted"),
          br(),
          uiOutput("gallery")
        ),
        
        # ==================================================================
        # SUBMIT TAB
        # ==================================================================
        tabPanel(
          "Submit Photo",
          icon = icon("camera"),
          br(),
          h3("Contribute a Modern Photograph", style = "text-align: center;"),
          
          div(class = "container",
              div(class = "info-box",
                  h5("How to Participate:"),
                  tags$ol(
                    tags$li("Visit one of the painting locations shown on the Explore Map"),
                    tags$li("Take a photograph from approximately the same vantage point"),
                    tags$li("Fill out the submission form below"),
                    tags$li("Your photo will be reviewed and added to the comparison gallery")
                  ),
                  p(icon("clock"), strong(" Review Process: "),
                    "All submissions are manually reviewed before appearing publicly.",
                    class = "mt-2 mb-0")
              ),
              
              div(class = "submission-form",
                  selectInput("painting_select", "Select Painting Location:",
                              choices = setNames(paintings_data$id, 
                                                 paste0(paintings_data$title, " (", paintings_data$year, ")"))),
                  
                  fileInput("photo_upload", "Upload Your Photo:",
                            accept = c("image/jpeg", "image/jpg", "image/png")),
                  
                  textInput("photographer_name", "Your Name:",
                            placeholder = "Enter your name"),
                  
                  textAreaInput("photo_notes", "Notes (optional):",
                                placeholder = "Any observations about changes you noticed...",
                                rows = 3),
                  
                  div(class = "row",
                      div(class = "col-md-6",
                          numericInput("photo_lat", "Latitude (optional):",
                                       value = NULL, min = -90, max = 90, step = 0.0001)
                      ),
                      div(class = "col-md-6",
                          numericInput("photo_lon", "Longitude (optional):",
                                       value = NULL, min = -180, max = 180, step = 0.0001)
                      )
                  ),
                  
                  actionButton("submit_btn", "Submit Photo", 
                               class = "btn-primary btn-lg btn-block",
                               icon = icon("paper-plane")),
                  br(), br(),
                  uiOutput("submit_status")
              )
          )
        ),
        
        # ==================================================================
        # COMPARE TAB
        # ==================================================================
        tabPanel(
          "Compare Past & Present",
          icon = icon("images"),
          br(),
          h3("Visual Comparisons", style = "text-align: center;"),
          p("View approved submissions side-by-side with historical paintings.",
            class = "text-center text-muted"),
          br(),
          uiOutput("comparisons")
        ),
        
        # ==================================================================
        # ABOUT TAB
        # ==================================================================
        tabPanel(
          "About",
          icon = icon("circle-info"),
          br(),
          div(class = "container",
              div(class = "row",
                  div(class = "col-md-8",
                      h3("Project Overview"),
                      
                      h4("Purpose", class = "mt-4"),
                      p("This geocache-style project invites the public to participate in historical ecology research by capturing modern photographs at locations depicted in 19th-century Western landscape paintings."),
                      
                      h4("About Albert Bierstadt", class = "mt-4"),
                      p("Albert Bierstadt (1830-1902) was a German-American painter known for his vast, dramatic landscapes of the American West. As a member of the Hudson River School, his works helped shape American perceptions of Western wilderness during the era of westward expansion."),
                      
                      h4("Research Value", class = "mt-4"),
                      p("By comparing historical artistic representations with contemporary photographs, researchers can study changes in vegetation, water levels, wildlife, human development, and climate impacts over 150+ years."),
                      
                      h4("Data Usage", class = "mt-4"),
                      p("All submitted photographs are reviewed before publication. Approved submissions become part of a public research dataset available for ecological, cultural, and art-historical analysis.")
                  ),
                  
                  div(class = "col-md-4",
                      div(class = "painting-card",
                          h4("Technical Details"),
                          tags$ul(
                            tags$li(strong("Platform:"), " R Shiny with Leaflet"),
                            tags$li(strong("Data:"), " 24+ Bierstadt paintings"),
                            tags$li(strong("Review:"), " Manual approval process"),
                            tags$li(strong("Format:"), " Public-facing interface")
                          )
                      ),
                      
                      div(class = "painting-card mt-3",
                          h4("Contact & Feedback"),
                          p("Questions about the project?"),
                          p(icon("envelope"), " ",
                            tags$a(href = "mailto:project@example.edu", "project@example.edu")),
                          p(icon("github"), " ",
                            tags$a(href = "#", "GitHub Repository", target = "_blank"))
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
    leaflet(paintings_data) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$OpenStreetMap, group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 8,
        color = "#3498db",
        fillColor = "#3498db",
        fillOpacity = 0.7,
        weight = 2,
        popup = ~paste0(
          "<div style='width:280px'>",
          "<h5 style='margin-bottom:10px; color:#2c3e50;'>", title, "</h5>",
          "<img src='", image_url, "' style='width:100%; border-radius:4px; margin:10px 0;'>",
          "<p style='margin:5px 0;'><strong>Artist:</strong> ", artist, "</p>",
          "<p style='margin:5px 0;'><strong>Year:</strong> ", year, "</p>",
          "<p style='margin:10px 0; font-size:0.9em;'>", substr(description, 1, 120), "...</p>",
          "<div style='background:#f8f9fa; padding:10px; border-radius:4px; margin-top:10px;'>",
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
  output$gallery <- renderUI({
    div(class = "container",
        lapply(1:nrow(paintings_data), function(i) {
          p <- paintings_data[i, ]
          
          div(class = "painting-card",
              div(class = "row",
                  div(class = "col-md-5",
                      img(src = p$image_url, class = "painting-img", alt = p$title)
                  ),
                  div(class = "col-md-7",
                      h4(p$title, style = "margin-top:0; color:#2c3e50;"),
                      p(class = "text-muted",
                        strong("Artist: "), p$artist, br(),
                        strong("Year: "), p$year, br(),
                        strong("Location: "), sprintf("%.2f°, %.2f°", p$latitude, p$longitude)
                      ),
                      p(p$description),
                      div(class = "info-box",
                          p(icon("map-marker-alt"), strong(" Visit This Location"), br(),
                            sprintf("Coordinates: %.4f, %.4f", p$latitude, p$longitude))
                      )
                  )
              )
          )
        })
    )
  })
  
  # SUBMISSION HANDLER
  observeEvent(input$submit_btn, {
    if (is.null(input$photo_upload) || input$photographer_name == "") {
      output$submit_status <- renderUI({
        div(class = "alert alert-warning",
            icon("exclamation-triangle"), " Please fill in all required fields.")
      })
    } else {
      # TODO: Save to database in production
      output$submit_status <- renderUI({
        div(class = "alert alert-success",
            icon("check-circle"), strong(" Submission Received!"), br(),
            "Thank you for your contribution. Your photo will be reviewed and added to the gallery once approved.")
      })
      
      # Reset form
      shinyjs::reset("photo_upload")
      shinyjs::reset("photographer_name")
      shinyjs::reset("photo_notes")
      shinyjs::reset("photo_lat")
      shinyjs::reset("photo_lon")
    }
  })
  
  # COMPARISON GALLERY
  output$comparisons <- renderUI({
    if (nrow(approved_photos) == 0) {
      return(
        div(class = "container text-center",
            div(class = "info-box",
                icon("images", style = "font-size:3em; color:#ccc;"),
                h4("No Comparisons Available Yet", class = "mt-3"),
                p("Be the first to contribute a modern photograph!")
            )
        )
      )
    }
    
    div(class = "container",
        lapply(1:nrow(approved_photos), function(i) {
          photo <- approved_photos[i, ]
          painting <- paintings_data %>% filter(id == photo$painting_id)
          
          if (nrow(painting) == 0) return(NULL)
          
          div(class = "painting-card",
              h4(painting$title, style = "color:#2c3e50;"),
              p(class = "text-muted",
                sprintf("Historical: %s | Modern: %s | Photographer: %s",
                        painting$year, 
                        format(photo$submission_date, "%Y"),
                        photo$photographer)
              ),
              
              div(class = "comparison-grid",
                  div(
                    h5("Historical (", painting$year, ")", style = "text-align:center;"),
                    img(src = painting$image_url, alt = "Historical painting")
                  ),
                  div(
                    h5("Modern (", format(photo$submission_date, "%Y"), ")", 
                       style = "text-align:center;"),
                    img(src = photo$photo_url, alt = "Modern photograph")
                  )
              ),
              
              if (!is.na(photo$notes) && photo$notes != "") {
                div(class = "info-box mt-3",
                    strong("Photographer's Notes: "), photo$notes)
              }
          )
        })
    )
  })
}

# ==============================================================================
# RUN APP
# ==============================================================================

shinyApp(ui = ui, server = server)