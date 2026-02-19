################################################################################
# HISTORICAL LANDSCAPE COMPARISON APP
# A public-facing geocache-style tool for comparing historical Western U.S. 
# landscape paintings with present-day photographs
#
# Author: Alex Wood
# Date: February 2026
# Deployment: shinyapps.io
################################################################################

# Load required libraries
library(shiny)
library(htmltools)
library(jsonlite)
library(rsconnect)
################################################################################
# CONFIGURATION SECTION
# Replace placeholder values with production credentials before deployment
################################################################################

# ArcGIS Online Web Map Configuration
# TODO: Replace with your published ArcGIS Online Web Map URL
ARCGIS_WEBMAP_URL <- "https://arcg.is/1H4eG01"

# ArcGIS Survey123 Configuration
# TODO: Replace with your Survey123 form URL (must be published and shared publicly)
SURVEY123_URL <- "https://arcg.is/00zzKy0"

# ArcGIS Attachment Viewer Configuration
# TODO: Replace with your published ArcGIS Attachment Viewer URL
# This will display approved photo submissions
ATTACHMENT_VIEWER_URL <- "https://virginiatech.maps.arcgis.com/apps/instant/attachmentviewer/index.html?appid=293ac49e304c45c6807899f92fa0da01"

################################################################################
# PLACEHOLDER DATA
# Replace with actual curated painting data from your research
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
  # High-resolution images from Wikimedia Commons (public domain)
  image_url = c(
    "https://upload.wikimedia.org/wikipedia/commons/thumb/4/45/Albert_Bierstadt_-_The_Rocky_Mountains%2C_Lander%27s_Peak.jpg/800px-Albert_Bierstadt_-_The_Rocky_Mountains%2C_Lander%27s_Peak.jpg",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5f/Bierstadt_-_Among_the_Sierra_Nevada_Mountains_-_1868.jpg/800px-Bierstadt_-_Among_the_Sierra_Nevada_Mountains_-_1868.jpg",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d0/Albert_Bierstadt_-_A_Storm_in_the_Rocky_Mountains%2C_Mt._Rosalie_-_Google_Art_Project.jpg/800px-Albert_Bierstadt_-_A_Storm_in_the_Rocky_Mountains%2C_Mt._Rosalie_-_Google_Art_Project.jpg",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e6/Albert_Bierstadt_-_Valley_of_the_Yosemite_-_Google_Art_Project.jpg/800px-Albert_Bierstadt_-_Valley_of_the_Yosemite_-_Google_Art_Project.jpg",
    "https://upload.wikimedia.org/wikipedia/commons/0/0d/Emigrants_Crossing_the_Plains%2C_or_The_Oregon_Trail_%28Albert_Bierstadt%29%2C_1869.jpg"
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
# UI DEFINITION
################################################################################

ui <- fluidPage(
  
  # Custom CSS for cleaner, more modern styling
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Playfair+Display:wght@400;600;700&family=Source+Sans+Pro:wght@300;400;600&display=swap');
      
      * {
        margin: 0;
        padding: 0;
        box-sizing: border-box;
      }
      
      body {
        font-family: 'Source Sans Pro', sans-serif;
        background-color: #fafafa;
        color: #2d3436;
        line-height: 1.7;
      }
      
      h1, h2, h3, h4 {
        font-family: 'Playfair Display', serif;
        color: #1a1a1a;
        font-weight: 600;
        line-height: 1.3;
      }
      
      /* Navigation */
      .navbar {
        background: linear-gradient(to right, #2d3436, #636e72);
        padding: 1rem 0;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        position: sticky;
        top: 0;
        z-index: 1000;
      }
      
      .navbar-brand {
        font-family: 'Playfair Display', serif;
        font-size: 1.5rem;
        color: #fff !important;
        font-weight: 600;
        letter-spacing: 0.5px;
      }
      
      .nav-link {
        color: rgba(255,255,255,0.9) !important;
        font-weight: 400;
        margin: 0 0.5rem;
        transition: all 0.3s ease;
        font-size: 0.95rem;
        text-decoration: none;
      }
      
      .nav-link:hover {
        color: #fff !important;
        transform: translateY(-2px);
      }
      
      /* Hero Section */
      .hero-section {
        background: linear-gradient(135deg, #4a5568 0%, #2d3748 100%);
        color: white;
        padding: 5rem 2rem;
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
        background: url('data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 1200 120\"><path d=\"M321.39,56.44c58-10.79,114.16-30.13,172-41.86,82.39-16.72,168.19-17.73,250.45-.39C823.78,31,906.67,72,985.66,92.83c70.05,18.48,146.53,26.09,214.34,3V0H0V27.35A600.21,600.21,0,0,0,321.39,56.44Z\" fill=\"rgba(255,255,255,0.05)\"/></svg>') no-repeat bottom;
        background-size: cover;
        opacity: 0.6;
      }
      
      .hero-section .container {
        position: relative;
        z-index: 1;
      }
      
      .hero-section h1 {
        color: white;
        font-size: 3rem;
        margin-bottom: 1.5rem;
        font-weight: 700;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
      }
      
      .hero-section p {
        font-size: 1.15rem;
        max-width: 750px;
        margin: 0 auto;
        opacity: 0.95;
        line-height: 1.8;
      }
      
      /* Section Headers */
      .section-header {
        text-align: center;
        margin: 4rem 0 3rem 0;
      }
      
      .section-header h2 {
        font-size: 2.5rem;
        margin-bottom: 0.5rem;
        position: relative;
        display: inline-block;
      }
      
      .section-header h2::after {
        content: '';
        position: absolute;
        bottom: -10px;
        left: 50%;
        transform: translateX(-50%);
        width: 60px;
        height: 3px;
        background: linear-gradient(to right, #4a5568, #2d3748);
      }
      
      .section-header p {
        color: #636e72;
        font-size: 1.1rem;
        margin-top: 1.5rem;
      }
      
      /* Container */
      .container {
        max-width: 1200px;
        margin: 0 auto;
        padding: 0 2rem;
      }
      
      /* Painting Cards */
      .painting-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(500px, 1fr));
        gap: 2.5rem;
        margin: 3rem 0;
      }
      
      .painting-card {
        background: white;
        border-radius: 12px;
        overflow: hidden;
        box-shadow: 0 4px 20px rgba(0,0,0,0.08);
        transition: all 0.3s ease;
      }
      
      .painting-card:hover {
        transform: translateY(-8px);
        box-shadow: 0 8px 30px rgba(0,0,0,0.15);
      }
      
      .painting-image {
        width: 100%;
        height: 350px;
        object-fit: cover;
        display: block;
      }
      
      .painting-content {
        padding: 1.75rem;
      }
      
      .painting-meta {
        color: #636e72;
        font-size: 0.9rem;
        margin-bottom: 0.75rem;
        font-weight: 400;
      }
      
      .painting-title {
        font-size: 1.5rem;
        color: #1a1a1a;
        margin-bottom: 1rem;
        line-height: 1.3;
      }
      
      .painting-context {
        color: #2d3436;
        line-height: 1.7;
        font-size: 0.95rem;
      }
      
      .painting-location {
        margin-top: 1.25rem;
        padding-top: 1.25rem;
        border-top: 1px solid #e9ecef;
        color: #4a5568;
        font-size: 0.9rem;
        font-weight: 500;
      }
      
      /* Map Container */
      .map-container {
        background: white;
        border-radius: 12px;
        padding: 2rem;
        box-shadow: 0 4px 20px rgba(0,0,0,0.08);
        margin: 3rem 0;
      }
      
      .map-container h3 {
        margin-bottom: 1rem;
        font-size: 1.75rem;
      }
      
      .map-container p {
        color: #636e72;
        margin-bottom: 1.5rem;
      }
      
      .map-container iframe {
        border: none;
        border-radius: 8px;
        width: 100%;
        height: 600px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
      }
      
      /* Survey Container */
      .survey-container {
        background: white;
        border-radius: 12px;
        padding: 2rem;
        box-shadow: 0 4px 20px rgba(0,0,0,0.08);
        margin: 3rem 0;
      }
      
      .survey-container h3 {
        margin-bottom: 1rem;
        font-size: 1.75rem;
      }
      
      .survey-container iframe {
        border: none;
        border-radius: 8px;
        width: 100%;
        height: 800px;
        margin-top: 1rem;
      }
      
      /* Attachment Viewer Container */
      .viewer-container {
        background: white;
        border-radius: 12px;
        padding: 2rem;
        box-shadow: 0 4px 20px rgba(0,0,0,0.08);
        margin: 3rem 0;
      }
      
      .viewer-container h3 {
        margin-bottom: 1rem;
        font-size: 1.75rem;
      }
      
      .viewer-container iframe {
        border: none;
        border-radius: 8px;
        width: 100%;
        height: 700px;
        margin-top: 1rem;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
      }
      
      /* Info Box */
      .info-box {
        background: #f8f9fa;
        border-left: 4px solid #4a5568;
        padding: 1.5rem;
        border-radius: 8px;
        margin: 2rem 0;
      }
      
      .info-box h4 {
        color: #1a1a1a;
        margin-bottom: 1rem;
        font-size: 1.2rem;
      }
      
      .info-box ul {
        margin-left: 1.5rem;
        color: #2d3436;
      }
      
      .info-box ul li {
        margin: 0.5rem 0;
        line-height: 1.6;
      }
      
      /* Footer */
      footer {
        background: linear-gradient(to right, #2d3436, #636e72);
        color: rgba(255,255,255,0.9);
        padding: 3rem 2rem 2rem;
        text-align: center;
        margin-top: 5rem;
      }
      
      footer a {
        color: white;
        text-decoration: none;
        transition: opacity 0.3s ease;
      }
      
      footer a:hover {
        opacity: 0.8;
      }
      
      footer p {
        margin: 0.5rem 0;
      }
      
      .footer-divider {
        width: 60px;
        height: 2px;
        background: rgba(255,255,255,0.5);
        margin: 1.5rem auto;
      }
      
      /* Smooth scrolling */
      html {
        scroll-behavior: smooth;
      }
      
      /* Responsive */
      @media (max-width: 768px) {
        .painting-grid {
          grid-template-columns: 1fr;
        }
        
        .hero-section h1 {
          font-size: 2rem;
        }
        
        .hero-section p {
          font-size: 1rem;
        }
        
        .section-header h2 {
          font-size: 2rem;
        }
        
        .map-container iframe,
        .viewer-container iframe {
          height: 450px;
        }
      }
    "))
  ),
  
  # Navigation Bar
  tags$nav(class = "navbar",
    div(class = "container",
      div(style = "display: flex; justify-content: space-between; align-items: center;",
        tags$a(class = "navbar-brand", href = "#", "Landscape Through Time"),
        tags$div(style = "display: flex; gap: 1rem;",
          tags$a(class = "nav-link", href = "#explore", "Explore"),
          tags$a(class = "nav-link", href = "#contribute", "Contribute"),
          tags$a(class = "nav-link", href = "#gallery", "Gallery")
        )
      )
    )
  ),
  
  # Hero Section
  div(class = "hero-section",
    div(class = "container",
      h1("Landscape Through Time"),
      p("Explore Albert Bierstadt's iconic Western landscapes from the 1860s, visit these locations today, and help document how the American West has changed over more than a century. Your photographs contribute to our understanding of landscape transformation, ecology, and cultural memory.")
    )
  ),
  
  # Main Content
  div(class = "container",
    
    ################################################################################
    # SECTION 1: EXPLORE
    ################################################################################
    
    tags$a(id = "explore"),
    div(class = "section-header",
      h2("Explore Historical Locations"),
      p("Discover where Bierstadt painted these masterpieces and plan your visit")
    ),
    
    # Information Box
    div(class = "info-box",
      h4("How to Use This Map"),
      p("Click on any marker to view the historical painting, learn about its context, and see the exact location. Use this information to plan a visit and capture your own modern photograph of the same view.")
    ),
    
    # ArcGIS Web Map
    div(class = "map-container",
      h3("Interactive Map"),
      p("The map below displays all painting locations. Click markers for details."),
      
      # TODO: Replace ARCGIS_WEBMAP_URL with your published Web Map
      tags$iframe(src = ARCGIS_WEBMAP_URL),
      
      tags$p(style = "color: #95a5a6; font-size: 0.85rem; margin-top: 1rem;",
        "Powered by ArcGIS Online")
    ),
    
    # Painting Gallery
    h3(style = "margin: 4rem 0 2rem 0; font-size: 2rem; text-align: center;", 
       "Painting Collection"),
    
    div(class = "painting-grid",
      lapply(1:nrow(paintings_data), function(i) {
        painting <- paintings_data[i, ]
        
        div(class = "painting-card",
          img(src = painting$image_url, 
              class = "painting-image",
              alt = painting$title,
              onerror = "this.src='https://via.placeholder.com/800x400?text=Image+Not+Available'"),
          div(class = "painting-content",
            div(class = "painting-meta",
              paste(painting$artist, "•", painting$year)
            ),
            h4(class = "painting-title", painting$title),
            p(class = "painting-context", painting$context),
            div(class = "painting-location",
              sprintf("📍 %s°N, %s°W", 
                      round(painting$latitude, 4), 
                      round(abs(painting$longitude), 4))
            )
          )
        )
      })
    ),
    
    ################################################################################
    # SECTION 2: CONTRIBUTE
    ################################################################################
    
    tags$a(id = "contribute"),
    div(class = "section-header",
      h2("Contribute Your Photograph"),
      p("Help us document landscape change by submitting your modern photograph")
    ),
    
    div(class = "info-box",
      h4("Submission Guidelines"),
      tags$ul(
        tags$li("Visit one of the historical painting locations shown above"),
        tags$li("Take a photograph from approximately the same vantage point"),
        tags$li("Upload your photo using the form below with GPS location"),
        tags$li("Add optional notes about what you observed"),
        tags$li("All submissions are reviewed before being made public")
      )
    ),
    
    # Survey123 Form
    div(class = "survey-container",
      h3("Photo Submission Form"),
      
      # TODO: Replace SURVEY123_URL with your published Survey123 form
      tags$iframe(src = SURVEY123_URL),
      
      tags$p(style = "color: #95a5a6; font-size: 0.85rem; margin-top: 1rem;",
        "Powered by ArcGIS Survey123 • All submissions require review before publication")
    ),
    
    ################################################################################
    # SECTION 3: GALLERY (Attachment Viewer)
    ################################################################################
    
    tags$a(id = "gallery"),
    div(class = "section-header",
      h2("Community Photo Gallery"),
      p("Explore approved submissions from visitors who captured modern views of historical painting locations")
    ),
    
    div(class = "info-box",
      h4("About This Gallery"),
      p("Browse through community-contributed photographs showing how these iconic landscapes have changed over time. Each photo has been reviewed and approved by our team. Click on any image to view full details including photographer credits, location data, and observations.")
    ),
    
    # ArcGIS Attachment Viewer
    div(class = "viewer-container",
      h3("Photo Collection"),
      
      # TODO: Replace ATTACHMENT_VIEWER_URL with your published Attachment Viewer
      # The Attachment Viewer should be configured to:
      # - Display only approved submissions (filter: approval_status = 'approved')
      # - Show photo attachments with metadata
      # - Group by painting_id if desired
      # - Enable full-screen viewing
      tags$iframe(src = ATTACHMENT_VIEWER_URL),
      
      tags$p(style = "color: #95a5a6; font-size: 0.85rem; margin-top: 1rem;",
        "Powered by ArcGIS Attachment Viewer")
    ),
    
    ################################################################################
    # FOOTER
    ################################################################################
    
    tags$footer(
      div(class = "container",
        p(style = "font-size: 1.1rem; margin-bottom: 0.5rem;",
          "Landscape Through Time"
        ),
        p(style = "font-size: 0.95rem; opacity: 0.9;",
          "A Public Art History & Ecology Project"
        ),
        div(class = "footer-divider"),
        p(style = "font-size: 0.85rem;",
          "Historical paintings are in the public domain. Modern photographs contributed under ",
          tags$a(href = "https://creativecommons.org/licenses/by/4.0/", 
                 target = "_blank", "CC BY 4.0"), "."
        ),
        p(style = "font-size: 0.75rem; margin-top: 1.5rem; opacity: 0.7;",
          "Built with R Shiny • Powered by ArcGIS Online"
        )
      )
    )
  )
)

################################################################################
# SERVER LOGIC
################################################################################

server <- function(input, output, session) {
  # Minimal server logic - most interactivity handled by embedded ArcGIS services
  # Smooth scrolling is handled by CSS
}

################################################################################
# RUN APPLICATION
################################################################################

shinyApp(ui = ui, server = server)
