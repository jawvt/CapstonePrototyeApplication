################################################################################
# LANDSCAPE THROUGH TIME - MODERN UI REBUILD
# National Geographic editorial style with immersive fullscreen experiences
################################################################################

# INSTALLATION
# install.packages(c("shiny", "bslib", "leaflet", "htmltools", "DT", "shinyjs", "base64enc"))

library(shiny)
library(bslib)
library(leaflet)
library(htmltools)
library(DT)
library(shinyjs)

################################################################################
# DATA STORAGE
################################################################################

DATA_DIR <- "app_data"
SUBMISSIONS_FILE <- file.path(DATA_DIR, "submissions.rds")
APPROVED_FILE <- file.path(DATA_DIR, "approved.rds")

if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)

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

################################################################################
# PAINTING DATA
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
    "Painted during Bierstadt's first major expedition to the American West in 1859, this monumental work depicts the Wind River Range in Wyoming Territory.",
    "Created following Bierstadt's 1863 journey to California, this luminous canvas captures the grandeur of the Sierra Nevada range.",
    "Commissioned by publisher James McHenry, this dramatic composition showcases Bierstadt's mastery of atmospheric perspective and theatrical lighting.",
    "Painted after Bierstadt's first visit to Yosemite Valley in 1863, this work helped introduce Eastern audiences to California's natural wonders.",
    "This work captures the arduous journey of westward migration across the Great Plains."
  ),
  stringsAsFactors = FALSE
)

################################################################################
# UI
################################################################################

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    # Google Fonts
    tags$link(href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@400;700;900&family=Inter:wght@300;400;600;700&display=swap", rel = "stylesheet"),
    
    # Meta viewport for mobile
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, maximum-scale=1"),
    
    tags$style(HTML("
      /* ═══════════════════════════════════════════════════════════════════
         RESET & BASE
         ═══════════════════════════════════════════════════════════════════ */
      * {
        margin: 0;
        padding: 0;
        box-sizing: border-box;
      }
      
      html {
        scroll-behavior: smooth;
        overflow-x: hidden;
      }
      
      body {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif;
        background: #0a0a0a;
        color: #ffffff;
        overflow-x: hidden;
        line-height: 1.6;
      }
      
      /* ═══════════════════════════════════════════════════════════════════
         NAVIGATION
         ═══════════════════════════════════════════════════════════════════ */
      #main-nav {
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        z-index: 1000;
        padding: 20px 40px;
        background: rgba(10, 10, 10, 0);
        backdrop-filter: blur(0px);
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
        border-bottom: 1px solid rgba(255, 255, 255, 0);
      }
      
      #main-nav.scrolled {
        padding: 15px 40px;
        background: rgba(10, 10, 10, 0.95);
        backdrop-filter: blur(20px);
        border-bottom: 1px solid rgba(255, 255, 255, 0.1);
      }
      
      #main-nav .logo {
        font-family: 'Playfair Display', serif;
        font-size: 24px;
        font-weight: 700;
        color: #ffffff;
        letter-spacing: 1px;
      }
      
      /* Section Navigation Dots */
      #section-nav {
        position: fixed;
        right: 40px;
        top: 50%;
        transform: translateY(-50%);
        z-index: 1000;
        display: flex;
        flex-direction: column;
        gap: 16px;
      }
      
      .nav-dot {
        width: 12px;
        height: 12px;
        border-radius: 50%;
        background: rgba(255, 255, 255, 0.3);
        cursor: pointer;
        transition: all 0.3s ease;
        position: relative;
      }
      
      .nav-dot:hover {
        background: rgba(255, 255, 255, 0.6);
        transform: scale(1.3);
      }
      
      .nav-dot.active {
        background: #ffffff;
        transform: scale(1.5);
      }
      
      .nav-dot::after {
        content: attr(data-label);
        position: absolute;
        right: 24px;
        top: 50%;
        transform: translateY(-50%);
        white-space: nowrap;
        font-size: 12px;
        opacity: 0;
        transition: opacity 0.3s;
        pointer-events: none;
      }
      
      .nav-dot:hover::after {
        opacity: 1;
      }
      
      /* ═══════════════════════════════════════════════════════════════════
         SECTIONS
         ═══════════════════════════════════════════════════════════════════ */
      .section {
        min-height: 100vh;
        position: relative;
        padding: 100px 40px;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      
      .section-dark {
        background: #0a0a0a;
        color: #ffffff;
      }
      
      .section-light {
        background: #f8f6f3;
        color: #1a1a1a;
      }
      
      .container {
        max-width: 1400px;
        margin: 0 auto;
        width: 100%;
      }
      
      /* ═══════════════════════════════════════════════════════════════════
         HERO SECTION
         ═══════════════════════════════════════════════════════════════════ */
      #hero {
        height: 100vh;
        position: relative;
        overflow: hidden;
        display: flex;
        align-items: center;
        justify-content: center;
        background: #0a0a0a;
      }
      
      #hero-canvas {
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        z-index: 0;
      }
      
      .hero-content {
        position: relative;
        z-index: 1;
        text-align: center;
        max-width: 1000px;
        padding: 0 40px;
      }
      
      .hero-title {
        font-family: 'Playfair Display', serif;
        font-size: clamp(48px, 8vw, 96px);
        font-weight: 900;
        line-height: 1.1;
        margin-bottom: 30px;
        letter-spacing: -2px;
        opacity: 0;
        transform: translateY(30px);
        animation: fadeUpIn 1s ease forwards 0.2s;
      }
      
      .hero-subtitle {
        font-size: clamp(18px, 2vw, 24px);
        font-weight: 300;
        max-width: 700px;
        margin: 0 auto 50px;
        opacity: 0;
        transform: translateY(30px);
        animation: fadeUpIn 1s ease forwards 0.4s;
        line-height: 1.6;
      }
      
      .hero-cta {
        opacity: 0;
        transform: translateY(30px);
        animation: fadeUpIn 1s ease forwards 0.6s;
      }
      
      @keyframes fadeUpIn {
        to {
          opacity: 1;
          transform: translateY(0);
        }
      }
      
      .scroll-indicator {
        position: absolute;
        bottom: 40px;
        left: 50%;
        transform: translateX(-50%);
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: 10px;
        opacity: 0.6;
        animation: bounce 2s infinite;
      }
      
      @keyframes bounce {
        0%, 100% { transform: translateX(-50%) translateY(0); }
        50% { transform: translateX(-50%) translateY(-10px); }
      }
      
      /* ═══════════════════════════════════════════════════════════════════
         TYPOGRAPHY
         ═══════════════════════════════════════════════════════════════════ */
      .section-title {
        font-family: 'Playfair Display', serif;
        font-size: clamp(36px, 5vw, 72px);
        font-weight: 700;
        margin-bottom: 60px;
        letter-spacing: -1px;
        line-height: 1.2;
      }
      
      .section-light .section-title {
        color: #1a1a1a;
      }
      
      .section-dark .section-title {
        color: #ffffff;
      }
      
      /* ═══════════════════════════════════════════════════════════════════
         BUTTONS
         ═══════════════════════════════════════════════════════════════════ */
      .btn-primary {
        background: #ffffff;
        color: #0a0a0a;
        border: none;
        padding: 18px 40px;
        font-size: 16px;
        font-weight: 600;
        border-radius: 50px;
        cursor: pointer;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        display: inline-block;
        text-decoration: none;
        position: relative;
        overflow: hidden;
      }
      
      .btn-primary:hover {
        transform: translateY(-2px) scale(1.05);
        box-shadow: 0 20px 40px rgba(255, 255, 255, 0.2);
      }
      
      .btn-primary:active {
        transform: translateY(0) scale(0.98);
      }
      
      .btn-secondary {
        background: transparent;
        color: #ffffff;
        border: 2px solid rgba(255, 255, 255, 0.3);
        padding: 16px 38px;
        font-size: 16px;
        font-weight: 600;
        border-radius: 50px;
        cursor: pointer;
        transition: all 0.3s ease;
      }
      
      .btn-secondary:hover {
        background: rgba(255, 255, 255, 0.1);
        border-color: rgba(255, 255, 255, 0.6);
        transform: translateY(-2px);
      }
      
      /* ═══════════════════════════════════════════════════════════════════
         PAINTING CARDS - 3D TILT EFFECT
         ═══════════════════════════════════════════════════════════════════ */
      .paintings-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(450px, 1fr));
        gap: 60px;
        margin-top: 80px;
      }
      
      .painting-card {
        position: relative;
        cursor: pointer;
        transform-style: preserve-3d;
        transition: transform 0.1s ease;
      }
      
      .painting-card-inner {
        background: #1a1a1a;
        border-radius: 24px;
        overflow: hidden;
        transform: translateZ(0);
        transition: all 0.5s cubic-bezier(0.4, 0, 0.2, 1);
      }
      
      .painting-card:hover .painting-card-inner {
        box-shadow: 0 30px 80px rgba(0, 0, 0, 0.5);
      }
      
      .painting-image {
        width: 100%;
        aspect-ratio: 16/10;
        object-fit: cover;
        display: block;
        transition: transform 0.5s ease;
      }
      
      .painting-card:hover .painting-image {
        transform: scale(1.05);
      }
      
      .painting-info {
        padding: 30px;
      }
      
      .painting-title {
        font-family: 'Playfair Display', serif;
        font-size: 28px;
        font-weight: 700;
        margin-bottom: 10px;
        color: #ffffff;
      }
      
      .painting-meta {
        color: rgba(255, 255, 255, 0.6);
        font-size: 14px;
        margin-bottom: 15px;
      }
      
      .painting-context {
        color: rgba(255, 255, 255, 0.8);
        line-height: 1.7;
        font-size: 15px;
      }
      
      /* ═══════════════════════════════════════════════════════════════════
         STATS
         ═══════════════════════════════════════════════════════════════════ */
      .stats-grid {
        display: grid;
        grid-template-columns: repeat(3, 1fr);
        gap: 40px;
        margin: 80px 0;
      }
      
      .stat-box {
        text-align: center;
        padding: 40px;
        background: rgba(255, 255, 255, 0.05);
        border-radius: 20px;
        backdrop-filter: blur(10px);
        border: 1px solid rgba(255, 255, 255, 0.1);
      }
      
      .stat-number {
        font-family: 'Playfair Display', serif;
        font-size: 64px;
        font-weight: 700;
        margin-bottom: 10px;
        background: linear-gradient(135deg, #ffffff 0%, rgba(255, 255, 255, 0.6) 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
      }
      
      .stat-label {
        font-size: 16px;
        text-transform: uppercase;
        letter-spacing: 2px;
        opacity: 0.7;
        font-weight: 600;
      }
      
      /* ═══════════════════════════════════════════════════════════════════
         LIGHTBOX
         ═══════════════════════════════════════════════════════════════════ */
      #lightbox {
        display: none;
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: rgba(0, 0, 0, 0.98);
        z-index: 10000;
        opacity: 0;
        transition: opacity 0.4s ease;
      }
      
      #lightbox.active {
        display: flex;
        opacity: 1;
      }
      
      .lightbox-content {
        width: 100%;
        height: 100%;
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 80px;
        position: relative;
      }
      
      .lightbox-image-container {
        max-width: 90%;
        max-height: 90%;
        position: relative;
        overflow: hidden;
        border-radius: 12px;
      }
      
      .lightbox-image {
        max-width: 100%;
        max-height: 100%;
        display: block;
        animation: kenBurns 20s ease-in-out infinite alternate;
      }
      
      @keyframes kenBurns {
        0% { transform: scale(1) translate(0, 0); }
        100% { transform: scale(1.1) translate(-2%, -2%); }
      }
      
      .lightbox-close {
        position: absolute;
        top: 40px;
        right: 40px;
        width: 50px;
        height: 50px;
        background: rgba(255, 255, 255, 0.1);
        border-radius: 50%;
        display: flex;
        align-items: center;
        justify-content: center;
        cursor: pointer;
        transition: all 0.3s ease;
        border: 1px solid rgba(255, 255, 255, 0.2);
      }
      
      .lightbox-close:hover {
        background: rgba(255, 255, 255, 0.2);
        transform: rotate(90deg);
      }
      
      .lightbox-info {
        position: absolute;
        bottom: 40px;
        left: 40px;
        right: 40px;
        background: rgba(0, 0, 0, 0.7);
        backdrop-filter: blur(20px);
        padding: 30px;
        border-radius: 16px;
        opacity: 0;
        transform: translateY(20px);
        transition: all 0.4s ease;
      }
      
      #lightbox.active .lightbox-info {
        opacity: 1;
        transform: translateY(0);
      }
      
      /* ═══════════════════════════════════════════════════════════════════
         FORM
         ═══════════════════════════════════════════════════════════════════ */
      .form-container {
        max-width: 700px;
        margin: 60px auto;
        background: #ffffff;
        padding: 60px;
        border-radius: 24px;
        box-shadow: 0 20px 60px rgba(0, 0, 0, 0.1);
      }
      
      .form-group {
        margin-bottom: 30px;
      }
      
      .form-group label {
        display: block;
        font-weight: 600;
        margin-bottom: 10px;
        color: #1a1a1a;
        font-size: 15px;
      }
      
      .form-control {
        width: 100%;
        padding: 16px;
        border: 2px solid #e0e0e0;
        border-radius: 12px;
        font-size: 15px;
        transition: all 0.3s ease;
        font-family: 'Inter', sans-serif;
      }
      
      .form-control:focus {
        outline: none;
        border-color: #1a1a1a;
        box-shadow: 0 0 0 4px rgba(26, 26, 26, 0.1);
      }
      
      .upload-zone {
        border: 3px dashed #d0d0d0;
        border-radius: 16px;
        padding: 60px 40px;
        text-align: center;
        cursor: pointer;
        transition: all 0.3s ease;
        background: #fafafa;
      }
      
      .upload-zone:hover {
        border-color: #1a1a1a;
        background: #f5f5f5;
      }
      
      .upload-zone.dragging {
        border-color: #1a1a1a;
        background: #f0f0f0;
        transform: scale(0.98);
      }
      
      /* ═══════════════════════════════════════════════════════════════════
         MAP
         ═══════════════════════════════════════════════════════════════════ */
      .map-container {
        border-radius: 24px;
        overflow: hidden;
        box-shadow: 0 20px 60px rgba(0, 0, 0, 0.3);
        height: 600px;
      }
      
      .leaflet-container {
        height: 100%;
        border-radius: 24px;
      }
      
      /* ═══════════════════════════════════════════════════════════════════
         COMPARISON GALLERY
         ═══════════════════════════════════════════════════════════════════ */
      .comparison-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(350px, 1fr));
        gap: 30px;
        margin-top: 60px;
      }
      
      .comparison-thumb {
        position: relative;
        aspect-ratio: 16/10;
        border-radius: 16px;
        overflow: hidden;
        cursor: pointer;
        transition: transform 0.3s ease;
      }
      
      .comparison-thumb:hover {
        transform: translateY(-8px);
      }
      
      .comparison-thumb img {
        width: 100%;
        height: 100%;
        object-fit: cover;
      }
      
      /* ═══════════════════════════════════════════════════════════════════
         COMPARISON LIGHTBOX (Side-by-side with sync zoom)
         ═══════════════════════════════════════════════════════════════════ */
      #comparison-lightbox {
        display: none;
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: rgba(0, 0, 0, 0.98);
        z-index: 10001;
      }
      
      #comparison-lightbox.active {
        display: flex;
      }
      
      .comparison-container {
        width: 100%;
        height: 100%;
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 2px;
        padding: 80px 40px;
      }
      
      .comparison-side {
        position: relative;
        overflow: hidden;
        background: #000;
        border-radius: 8px;
      }
      
      .comparison-side img {
        width: 100%;
        height: 100%;
        object-fit: contain;
        transition: transform 0.1s ease;
      }
      
      .comparison-label {
        position: absolute;
        top: 20px;
        left: 20px;
        background: rgba(0, 0, 0, 0.7);
        padding: 10px 20px;
        border-radius: 20px;
        font-size: 14px;
        font-weight: 600;
      }
      
      /* ═══════════════════════════════════════════════════════════════════
         RESPONSIVE
         ═══════════════════════════════════════════════════════════════════ */
      @media (max-width: 768px) {
        .section { padding: 60px 20px; }
        .paintings-grid { grid-template-columns: 1fr; gap: 40px; }
        .stats-grid { grid-template-columns: 1fr; }
        #section-nav { display: none; }
        .comparison-container { grid-template-columns: 1fr; }
        .form-container { padding: 40px 30px; }
      }
      
      /* ═══════════════════════════════════════════════════════════════════
         UTILITIES
         ═══════════════════════════════════════════════════════════════════ */
      .fade-in {
        opacity: 0;
        transform: translateY(30px);
        transition: all 0.8s cubic-bezier(0.4, 0, 0.2, 1);
      }
      
      .fade-in.visible {
        opacity: 1;
        transform: translateY(0);
      }
      
      .text-center { text-align: center; }
      .mt-4 { margin-top: 40px; }
      .mb-4 { margin-bottom: 40px; }
    "))
  ),
  
  # Fixed Navigation
  tags$div(id = "main-nav",
    tags$div(class = "logo", "Landscape Through Time")
  ),
  
  # Section Navigation Dots
  tags$div(id = "section-nav",
    tags$div(class = "nav-dot active", `data-section` = "hero", `data-label` = "Home"),
    tags$div(class = "nav-dot", `data-section` = "story", `data-label` = "Story"),
    tags$div(class = "nav-dot", `data-section` = "gallery", `data-label` = "Gallery"),
    tags$div(class = "nav-dot", `data-section` = "map", `data-label` = "Map"),
    tags$div(class = "nav-dot", `data-section` = "submit", `data-label` = "Submit"),
    tags$div(class = "nav-dot", `data-section` = "comparisons", `data-label` = "Compare"),
    tags$div(class = "nav-dot", `data-section` = "admin", `data-label` = "Admin")
  ),
  
  # HERO SECTION
  tags$section(id = "hero", class = "section section-dark",
    tags$canvas(id = "hero-canvas"),
    tags$div(class = "hero-content",
      tags$h1(class = "hero-title", "Landscape Through Time"),
      tags$p(class = "hero-subtitle", 
        "Journey through 150 years of Western landscapes. Experience Albert Bierstadt's iconic paintings alongside modern photographs from the same locations."
      ),
      tags$div(class = "hero-cta",
        actionButton("explore_btn", "Explore the Journey", class = "btn-primary")
      )
    ),
    tags$div(class = "scroll-indicator",
      tags$div("Scroll"),
      HTML("↓")
    )
  ),
  
  # STORY SECTION
  tags$section(id = "story", class = "section section-light fade-in",
    tags$div(class = "container",
      tags$h2(class = "section-title text-center", "A Visual Time Capsule"),
      tags$div(style = "max-width: 800px; margin: 0 auto; font-size: 20px; line-height: 1.8; text-align: center;",
        tags$p("Albert Bierstadt (1830-1902) captured the American West in its most pristine state. His dramatic paintings immortalized landscapes that have transformed dramatically over the past century and a half."),
        tags$p(class = "mt-4", "This project invites you to become part of living history. Visit the exact locations where Bierstadt painted, document what exists today, and contribute to our understanding of landscape transformation.")
      ),
      
      # Stats
      tags$div(class = "stats-grid mt-4",
        tags$div(class = "stat-box",
          tags$div(class = "stat-number", "5"),
          tags$div(class = "stat-label", "Historic Locations")
        ),
        tags$div(class = "stat-box",
          tags$div(class = "stat-number", textOutput("stat_submissions", inline = TRUE)),
          tags$div(class = "stat-label", "Submissions")
        ),
        tags$div(class = "stat-box",
          tags$div(class = "stat-number", textOutput("stat_approved", inline = TRUE)),
          tags$div(class = "stat-label", "Comparisons")
        )
      )
    )
  ),
  
  # GALLERY SECTION
  tags$section(id = "gallery", class = "section section-dark fade-in",
    tags$div(class = "container",
      tags$h2(class = "section-title", "The Collection"),
      tags$div(id = "paintings-container", class = "paintings-grid",
        uiOutput("painting_cards")
      )
    )
  ),
  
  # MAP SECTION
  tags$section(id = "map", class = "section section-light fade-in",
    tags$div(class = "container",
      tags$h2(class = "section-title", "Explore Locations"),
      tags$div(class = "map-container",
        leafletOutput("main_map", height = "100%")
      )
    )
  ),
  
  # SUBMIT SECTION
  tags$section(id = "submit", class = "section section-light fade-in",
    tags$div(class = "container",
      tags$h2(class = "section-title text-center", "Contribute Your Photo"),
      tags$div(class = "form-container",
        uiOutput("submit_message"),
        textInput("submit_name", "Your Name (optional)", placeholder = "John Doe"),
        textInput("submit_email", "Email (optional)", placeholder = "john@example.com"),
        selectInput("submit_painting", "Which location did you visit?",
                   choices = c("Select a location..." = "", setNames(paintings_data$id, paintings_data$title))),
        tags$div(class = "form-group",
          tags$label("Upload Your Photo"),
          tags$div(class = "upload-zone", id = "upload-zone",
            fileInput("submit_photo", NULL, accept = c("image/png", "image/jpeg", "image/jpg"))
          )
        ),
        numericInput("submit_latitude", "Latitude (e.g., 40.7489)", value = NA),
        numericInput("submit_longitude", "Longitude (e.g., -109.5596)", value = NA),
        textAreaInput("submit_observations", "Observations (optional)", rows = 4,
                     placeholder = "What did you notice about the landscape?"),
        actionButton("submit_button", "Submit Photo", class = "btn-primary", style = "width: 100%; background: #1a1a1a; color: white;")
      )
    )
  ),
  
  # COMPARISONS SECTION
  tags$section(id = "comparisons", class = "section section-dark fade-in",
    tags$div(class = "container",
      tags$h2(class = "section-title", "Past vs Present"),
      uiOutput("comparison_gallery")
    )
  ),
  
  # ADMIN SECTION
  tags$section(id = "admin", class = "section section-light fade-in",
    tags$div(class = "container",
      tags$h2(class = "section-title", "Admin Panel"),
      conditionalPanel(
        condition = "output.admin_authenticated == false",
        tags$div(style = "max-width: 400px; margin: 0 auto;",
          passwordInput("admin_password", "Password"),
          actionButton("admin_login", "Login", class = "btn-primary", style = "width: 100%; background: #1a1a1a; color: white;")
        )
      ),
      conditionalPanel(
        condition = "output.admin_authenticated == true",
        actionButton("refresh_admin", "Refresh", class = "btn-secondary"),
        actionButton("approve_submission", "Approve Selected", class = "btn-primary", style = "background: #1a1a1a; color: white; margin-left: 10px;"),
        actionButton("reject_submission", "Reject Selected", class = "btn-secondary", style = "margin-left: 10px;"),
        tags$br(), tags$br(),
        DTOutput("admin_table")
      )
    )
  ),
  
  # LIGHTBOX for paintings
  tags$div(id = "lightbox",
    tags$div(class = "lightbox-content",
      tags$div(class = "lightbox-close", onclick = "closeLightbox()", "✕"),
      tags$div(class = "lightbox-image-container",
        tags$img(id = "lightbox-img", class = "lightbox-image", src = "")
      ),
      tags$div(class = "lightbox-info",
        tags$h3(id = "lightbox-title", style = "font-family: 'Playfair Display', serif; margin-bottom: 10px;"),
        tags$p(id = "lightbox-meta", style = "opacity: 0.7; margin-bottom: 15px;"),
        tags$p(id = "lightbox-context")
      )
    )
  ),
  
  # COMPARISON LIGHTBOX
  tags$div(id = "comparison-lightbox",
    tags$div(class = "lightbox-close", onclick = "closeComparisonLightbox()", style = "position: fixed; top: 40px; right: 40px; z-index: 10002;", "✕"),
    tags$div(class = "comparison-container",
      tags$div(class = "comparison-side",
        tags$div(class = "comparison-label", "Historical"),
        tags$img(id = "comp-historical", src = "", draggable = "false")
      ),
      tags$div(class = "comparison-side",
        tags$div(class = "comparison-label", "Present Day"),
        tags$img(id = "comp-modern", src = "", draggable = "false")
      )
    )
  ),
  
  # JavaScript for interactions
  tags$script(HTML(paste0("
    // ═══════════════════════════════════════════════════════════════════
    // HERO CANVAS ANIMATION
    // ═══════════════════════════════════════════════════════════════════
    (function() {
      const canvas = document.getElementById('hero-canvas');
      if (!canvas) return;
      const ctx = canvas.getContext('2d');
      
      function resize() {
        canvas.width = window.innerWidth;
        canvas.height = window.innerHeight;
      }
      resize();
      window.addEventListener('resize', resize);
      
      const particles = [];
      for (let i = 0; i < 80; i++) {
        particles.push({
          x: Math.random() * canvas.width,
          y: Math.random() * canvas.height,
          vx: (Math.random() - 0.5) * 0.3,
          vy: (Math.random() - 0.5) * 0.3,
          size: Math.random() * 2 + 1
        });
      }
      
      function animate() {
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        
        // Background gradient
        const grad = ctx.createRadialGradient(canvas.width/2, canvas.height/2, 0, canvas.width/2, canvas.height/2, canvas.width);
        grad.addColorStop(0, 'rgba(40, 40, 40, 0.3)');
        grad.addColorStop(1, 'rgba(10, 10, 10, 0)');
        ctx.fillStyle = grad;
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        
        // Particles
        particles.forEach(p => {
          p.x += p.vx;
          p.y += p.vy;
          
          if (p.x < 0 || p.x > canvas.width) p.vx *= -1;
          if (p.y < 0 || p.y > canvas.height) p.vy *= -1;
          
          ctx.fillStyle = 'rgba(255, 255, 255, 0.3)';
          ctx.beginPath();
          ctx.arc(p.x, p.y, p.size, 0, Math.PI * 2);
          ctx.fill();
        });
        
        requestAnimationFrame(animate);
      }
      animate();
    })();
    
    // ═══════════════════════════════════════════════════════════════════
    // SCROLL ANIMATIONS & NAVIGATION
    // ═══════════════════════════════════════════════════════════════════
    const nav = document.getElementById('main-nav');
    const sections = document.querySelectorAll('.section');
    const navDots = document.querySelectorAll('.nav-dot');
    
    // Scroll detection
    window.addEventListener('scroll', () => {
      if (window.scrollY > 100) {
        nav.classList.add('scrolled');
      } else {
        nav.classList.remove('scrolled');
      }
      
      // Update active dot
      let current = '';
      sections.forEach(section => {
        const sectionTop = section.offsetTop;
        if (window.scrollY >= sectionTop - 300) {
          current = section.getAttribute('id');
        }
      });
      
      navDots.forEach(dot => {
        dot.classList.remove('active');
        if (dot.getAttribute('data-section') === current) {
          dot.classList.add('active');
        }
      });
    });
    
    // Dot navigation
    navDots.forEach(dot => {
      dot.addEventListener('click', () => {
        const section = dot.getAttribute('data-section');
        document.getElementById(section).scrollIntoView({ behavior: 'smooth' });
      });
    });
    
    // Intersection Observer for fade-in
    const observer = new IntersectionObserver((entries) => {
      entries.forEach(entry => {
        if (entry.isIntersecting) {
          entry.target.classList.add('visible');
        }
      });
    }, { threshold: 0.1 });
    
    document.querySelectorAll('.fade-in').forEach(el => observer.observe(el));
    
    // Explore button
    document.getElementById('explore_btn').addEventListener('click', () => {
      document.getElementById('story').scrollIntoView({ behavior: 'smooth' });
    });
    
    // ═══════════════════════════════════════════════════════════════════
    // 3D TILT EFFECT ON PAINTING CARDS
    // ═══════════════════════════════════════════════════════════════════
    document.querySelectorAll('.painting-card').forEach(card => {
      card.addEventListener('mousemove', (e) => {
        const rect = card.getBoundingClientRect();
        const x = e.clientX - rect.left;
        const y = e.clientY - rect.top;
        const centerX = rect.width / 2;
        const centerY = rect.height / 2;
        const rotateX = (y - centerY) / 20;
        const rotateY = (centerX - x) / 20;
        
        card.style.transform = `perspective(1000px) rotateX(${rotateX}deg) rotateY(${rotateY}deg)`;
      });
      
      card.addEventListener('mouseleave', () => {
        card.style.transform = 'perspective(1000px) rotateX(0) rotateY(0)';
      });
    });
    
    // ═══════════════════════════════════════════════════════════════════
    // LIGHTBOX FUNCTIONS
    // ═══════════════════════════════════════════════════════════════════
    window.openLightbox = function(id) {
      const paintings = ", jsonlite::toJSON(paintings_data, auto_unbox = TRUE), ";
      const painting = paintings.find(p => p.id === id);
      if (!painting) return;
      
      document.getElementById('lightbox-img').src = painting.image_url;
      document.getElementById('lightbox-title').textContent = painting.title;
      document.getElementById('lightbox-meta').textContent = painting.artist + ' • ' + painting.year;
      document.getElementById('lightbox-context').textContent = painting.context;
      document.getElementById('lightbox').classList.add('active');
      document.body.style.overflow = 'hidden';
    };
    
    window.closeLightbox = function() {
      document.getElementById('lightbox').classList.remove('active');
      document.body.style.overflow = '';
    };
    
    window.openComparisonLightbox = function(historicalUrl, modernUrl) {
      document.getElementById('comp-historical').src = historicalUrl;
      document.getElementById('comp-modern').src = modernUrl;
      document.getElementById('comparison-lightbox').classList.add('active');
      document.body.style.overflow = 'hidden';
      
      // Synchronized zoom
      let isPanning = false;
      let startX, startY, scrollLeft, scrollTop;
      
      const sides = document.querySelectorAll('.comparison-side img');
      sides.forEach(img => {
        img.addEventListener('wheel', (e) => {
          e.preventDefault();
          const delta = e.deltaY * -0.01;
          const scale = Math.max(1, Math.min(3, parseFloat(img.style.transform.replace('scale(', '').replace(')', '') || 1) + delta));
          sides.forEach(s => s.style.transform = `scale(${scale})`);
        });
      });
    };
    
    window.closeComparisonLightbox = function() {
      document.getElementById('comparison-lightbox').classList.remove('active');
      document.body.style.overflow = '';
    };
    
    // Close on ESC
    document.addEventListener('keydown', (e) => {
      if (e.key === 'Escape') {
        closeLightbox();
        closeComparisonLightbox();
      }
    });
  ")))
)

################################################################################
# SERVER
################################################################################

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    admin_auth = FALSE,
    submission_success = FALSE,
    submission_error = NULL,
    submissions = load_data(SUBMISSIONS_FILE),
    approved = load_data(APPROVED_FILE)
  )
  
  # Stats
  output$stat_submissions <- renderText({ as.character(nrow(rv$submissions)) })
  output$stat_approved <- renderText({ as.character(nrow(rv$approved)) })
  
  # Painting cards with click to open lightbox
  output$painting_cards <- renderUI({
    cards <- lapply(1:nrow(paintings_data), function(i) {
      p <- paintings_data[i, ]
      tags$div(class = "painting-card", onclick = sprintf("openLightbox(%d)", p$id),
        tags$div(class = "painting-card-inner",
          tags$img(src = p$image_url, class = "painting-image", alt = p$title),
          tags$div(class = "painting-info",
            tags$h3(class = "painting-title", p$title),
            tags$div(class = "painting-meta", paste0(p$artist, " • ", p$year)),
            tags$p(class = "painting-context", p$context)
          )
        )
      )
    })
    tagList(cards)
  })
  
  # Map
  output$main_map <- renderLeaflet({
    leaflet(paintings_data) %>%
      addTiles() %>%
      addMarkers(
        lng = ~longitude, lat = ~latitude,
        label = ~title,
        popup = ~paste0("<b>", title, "</b><br>", artist, ", ", year)
      ) %>%
      fitBounds(
        lng1 = min(paintings_data$longitude) - 2, lat1 = min(paintings_data$latitude) - 2,
        lng2 = max(paintings_data$longitude) + 2, lat2 = max(paintings_data$latitude) + 2
      )
  })
  
  # Submit message
  output$submit_message <- renderUI({
    if (rv$submission_success) {
      tags$div(class = "alert alert-success", style = "background: #d4edda; color: #155724; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
        "✓ Photo submitted successfully! Pending admin review."
      )
    } else if (!is.null(rv$submission_error)) {
      tags$div(class = "alert alert-danger", style = "background: #f8d7da; color: #721c24; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
        "✕ Error: ", rv$submission_error
      )
    }
  })
  
  # Submit handler
  observeEvent(input$submit_button, {
    rv$submission_success <- FALSE
    rv$submission_error <- NULL
    
    if (input$submit_painting == "") {
      rv$submission_error <- "Please select a location."
      return()
    }
    if (is.null(input$submit_photo)) {
      rv$submission_error <- "Please upload a photo."
      return()
    }
    if (is.na(input$submit_latitude) || is.na(input$submit_longitude)) {
      rv$submission_error <- "Please enter GPS coordinates."
      return()
    }
    if (input$submit_photo$size > 5 * 1024 * 1024) {
      rv$submission_error <- "File must be less than 5MB."
      return()
    }
    
    tryCatch({
      file_data <- readBin(input$submit_photo$datapath, "raw", file.info(input$submit_photo$datapath)$size)
      base64_image <- paste0("data:image/jpeg;base64,", base64enc::base64encode(file_data))
      
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
      
      rv$submissions <- rbind(rv$submissions, new_submission)
      save_data(rv$submissions, SUBMISSIONS_FILE)
      rv$submission_success <- TRUE
      
      updateTextInput(session, "submit_name", value = "")
      updateTextInput(session, "submit_email", value = "")
      updateSelectInput(session, "submit_painting", selected = "")
      updateNumericInput(session, "submit_latitude", value = NA)
      updateNumericInput(session, "submit_longitude", value = NA)
      updateTextAreaInput(session, "submit_observations", value = "")
    }, error = function(e) {
      rv$submission_error <- paste("Failed:", e$message)
    })
  })
  
  # Comparison gallery
  output$comparison_gallery <- renderUI({
    approved <- rv$approved
    
    if (nrow(approved) == 0) {
      return(tags$p(style = "text-align: center; opacity: 0.6; font-size: 18px;", 
                    "No approved comparisons yet. Be the first to contribute!"))
    }
    
    cards <- lapply(1:nrow(approved), function(i) {
      sub <- approved[i, ]
      painting <- paintings_data[paintings_data$id == sub$painting_id, ]
      
      tags$div(class = "comparison-thumb",
        onclick = sprintf("openComparisonLightbox('%s', '%s')", painting$image_url, sub$photo_url),
        tags$img(src = painting$image_url, alt = painting$title)
      )
    })
    
    tags$div(class = "comparison-grid", cards)
  })
  
  # Admin
  observeEvent(input$admin_login, {
    if (input$admin_password == "admin123") rv$admin_auth <- TRUE
  })
  
  output$admin_authenticated <- reactive({ rv$admin_auth })
  outputOptions(output, "admin_authenticated", suspendWhenHidden = FALSE)
  
  output$admin_table <- renderDT({
    input$refresh_admin
    if (nrow(rv$submissions) == 0) return(data.frame(Message = "No submissions"))
    
    datatable(
      rv$submissions[, c("submission_id", "name", "painting_id", "latitude", "longitude", "submission_date", "approval_status")],
      options = list(pageLength = 25, order = list(list(6, 'desc'))),
      rownames = FALSE, selection = 'single'
    )
  })
  
  observeEvent(input$approve_submission, {
    if (length(input$admin_table_rows_selected) > 0) {
      idx <- input$admin_table_rows_selected
      sub <- rv$submissions[idx, ]
      sub$approval_status <- "Approved"
      rv$approved <- rbind(rv$approved, sub)
      save_data(rv$approved, APPROVED_FILE)
      rv$submissions[idx, "approval_status"] <- "Approved"
      save_data(rv$submissions, SUBMISSIONS_FILE)
      showNotification("Approved!", type = "message")
    }
  })
  
  observeEvent(input$reject_submission, {
    if (length(input$admin_table_rows_selected) > 0) {
      idx <- input$admin_table_rows_selected
      rv$submissions[idx, "approval_status"] <- "Rejected"
      save_data(rv$submissions, SUBMISSIONS_FILE)
      showNotification("Rejected.", type = "warning")
    }
  })
  
  observeEvent(input$refresh_admin, {
    rv$submissions <- load_data(SUBMISSIONS_FILE)
    rv$approved <- load_data(APPROVED_FILE)
    showNotification("Refreshed!", type = "message")
  })
}

shinyApp(ui = ui, server = server)
