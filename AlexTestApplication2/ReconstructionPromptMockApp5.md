---
editor_options: 
  markdown: 
    wrap: 72
---

# RECONSTRUCTION PROMPT: LANDSCAPE THROUGH TIME — R SHINY APPLICATION

Paste everything below this line into Claude to recreate the exact
application.

------------------------------------------------------------------------

## ROLE

You are an expert R Shiny developer. Build a complete, single-file
`app.R` application exactly as specified below. Do not summarize, do not
skip sections, do not ask clarifying questions — write the full working
code from top to bottom.

------------------------------------------------------------------------

\`## PROJECT OVERVIEW

A public-facing geocache-style R Shiny web application called
**"Landscape Through Time"**. It allows the public to: - Explore 5
historical Albert Bierstadt landscape paintings on an interactive map -
Browse a painting gallery with details about each location - Submit
modern photographs taken at the same real-world locations - View
approved past-vs-present photo comparisons - Admin panel to
approve/reject submissions

------------------------------------------------------------------------

## PACKAGES

Install with:

``` r
install.packages(c("shiny", "bslib", "leaflet", "htmltools", "DT", "shinyjs", "base64enc"))
```

Load in app:

``` r
library(shiny)
library(bslib)
library(leaflet)
library(htmltools)
library(DT)
library(shinyjs)
```

Do NOT use `shinydashboard`. Do NOT use `googlesheets4`. Do NOT use
`googledrive`.

------------------------------------------------------------------------

## DATA STORAGE — RDS FILES (NO GOOGLE SHEETS)

Use local `.rds` files for persistence. No external services, no API
keys.

``` r
DATA_DIR <- "app_data"
SUBMISSIONS_FILE <- file.path(DATA_DIR, "submissions.rds")
APPROVED_FILE    <- file.path(DATA_DIR, "approved.rds")

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
```

------------------------------------------------------------------------

## PAINTING DATASET

Use exactly this data frame — do not alter coordinates, URLs, or text:

``` r
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
  latitude  = c(40.7489, 37.8651, 39.5501, 37.7459, 41.3114),
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
```

------------------------------------------------------------------------

## UI STRUCTURE

Use `fluidPage()` wrapping a `navbarPage()`. The
`bslib::bs_theme(version = 5)` is set on the `fluidPage`. The navbarPage
sits INSIDE the fluidPage — it is NOT the top-level UI element.

``` r
ui <- fluidPage(
  useShinyjs(),
  theme = bslib::bs_theme(
    version = 5, bg = "#ffffff", fg = "#2c3e50", primary = "#2d5f2d",
    base_font    = bslib::font_google("Source Sans Pro"),
    heading_font = bslib::font_google("Playfair Display")
  ),
  tags$head(tags$style(HTML("  /* ALL CSS HERE */  "))),
  tags$canvas(id = "bgCanvas"),
  tags$script(HTML("  /* ANIMATION JS HERE */  ")),
  navbarPage( ... )
)
```

### navbarPage arguments — use EXACTLY these:

``` r
navbarPage(
  title = div(
    style = "font-family:'Playfair Display',serif; font-size:36px; font-weight:700;
             letter-spacing:1px; text-shadow:2px 2px 4px rgba(0,0,0,0.3); color:white;",
    "Landscape Through Time"
  ),
  id = "main_tabs",
  windowTitle = "Landscape Through Time",
  collapsible = FALSE,
  position = "static-top",
  fluid = TRUE,
  tabPanel("Home",         value = "home",        ...),
  tabPanel("Explore Map",  value = "map",         ...),
  tabPanel("Painting Gallery", value = "gallery", ...),
  tabPanel("Submit Photo", value = "submit",      ...),
  tabPanel("Past vs Present", value = "comparisons", ...),
  tabPanel("Admin",        value = "admin",       ...)
)
```

Do NOT pass a `header=` argument to `navbarPage`. All CSS goes in the
single `tags$style()` block inside `tags$head()`.

------------------------------------------------------------------------

## CSS — COMPLETE SPECIFICATION

All CSS goes in one `tags$head(tags$style(HTML("...")))` block. Include
every rule below:

### Background

``` css
body {
  background-color: #f0f5f0;
  position: relative;
  overflow-x: hidden;
}

#bgCanvas {
  position: fixed; top: 0; left: 0;
  width: 100%; height: 100%;
  z-index: -1;
  pointer-events: none;
}
```

### Navbar — centered title stacked above horizontal pill tabs

This is the most critical CSS. The navbar must stack title then tabs
vertically, and the tabs must be horizontal (flex-direction: row). Use
`!important` on all these rules:

``` css
.navbar {
  background: linear-gradient(135deg, #2d5f2d 0%, #1f4a1f 100%) !important;
  box-shadow: 0 4px 20px rgba(0,0,0,0.15);
  padding: 0 !important;
  margin-bottom: 0 !important;
}

/* Stack title above tabs, center both */
.navbar > .container,
.navbar > .container-fluid {
  display: flex !important;
  flex-direction: column !important;
  align-items: center !important;
  padding: 15px 20px !important;
  width: 100% !important;
}

/* Title */
.navbar-brand {
  font-family: 'Playfair Display', serif !important;
  font-size: 36px !important;
  font-weight: 700 !important;
  color: #ffffff !important;
  text-align: center !important;
  padding: 10px 0 5px 0 !important;
  margin: 0 !important;
  float: none !important;
  display: block !important;
  letter-spacing: 1px;
  text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
  width: 100%;
}

/* Collapse wrapper */
.navbar-collapse {
  display: flex !important;
  flex-grow: 0 !important;
  justify-content: center !important;
  width: 100% !important;
  padding: 8px 0 15px 0 !important;
  border: none !important;
  box-shadow: none !important;
  background: transparent !important;
}

/* CRITICAL: tabs must be flex-direction ROW */
.navbar-nav,
.navbar-nav.navbar-right {
  display: flex !important;
  flex-direction: row !important;
  flex-wrap: wrap !important;
  justify-content: center !important;
  gap: 8px !important;
  float: none !important;
  margin: 0 !important;
  padding: 0 !important;
}

/* Individual tab items */
.navbar-nav > li,
.navbar-nav .nav-item {
  float: none !important;
  display: inline-block !important;
}

/* Tab link — pill style, white text */
.navbar-nav > li > a,
.navbar-nav .nav-link {
  color: #ffffff !important;
  font-weight: 600 !important;
  font-size: 14px !important;
  border: 2px solid rgba(255,255,255,0.35) !important;
  padding: 9px 18px !important;
  border-radius: 25px !important;
  background-color: rgba(255,255,255,0.1) !important;
  white-space: nowrap !important;
  transition: all 0.25s ease !important;
  text-shadow: 1px 1px 2px rgba(0,0,0,0.2) !important;
  line-height: 1.4 !important;
}

.navbar-nav > li > a:hover,
.navbar-nav .nav-link:hover {
  background-color: rgba(255,255,255,0.25) !important;
  border-color: rgba(255,255,255,0.6) !important;
  transform: translateY(-2px);
  box-shadow: 0 4px 12px rgba(0,0,0,0.2);
}

.navbar-nav > li.active > a,
.navbar-nav .nav-link.active {
  background-color: rgba(255,255,255,0.3) !important;
  border-color: rgba(255,255,255,0.7) !important;
  font-weight: 700 !important;
  box-shadow: 0 3px 10px rgba(0,0,0,0.2) !important;
}

/* Hide mobile hamburger toggle */
.navbar-toggle,
.navbar-toggler { display: none !important; }
```

### Buttons — white text always

``` css
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
  box-shadow: 0 4px 15px rgba(45,95,45,0.3);
}
.btn-primary:hover {
  background-color: #1f4a1f;
  transform: translateY(-2px);
}
```

### Hero section

``` css
.hero-section {
  background: linear-gradient(135deg, #2d5f2d 0%, #4a7c59 50%, #9caf88 100%);
  color: white;
  padding: 100px 40px;
  text-align: center;
  position: relative;
  overflow: hidden;
}
.hero-section h1 { color: white; font-size: 56px; font-weight: 700; text-shadow: 2px 2px 4px rgba(0,0,0,0.2); }
.hero-section .subtitle { font-size: 22px; max-width: 900px; margin: 0 auto 40px; line-height: 1.7; }
```

### Content sections, stat boxes, feature cards

``` css
.content-section { padding: 60px 0; max-width: 1200px; margin: 0 auto; }
.section-header { text-align: center; margin-bottom: 50px; }
.section-header h2 { color: #2d5f2d; font-size: 42px; font-weight: 700; }
.stat-box {
  background: white; padding: 40px 20px; border-radius: 12px;
  box-shadow: 0 4px 20px rgba(0,0,0,0.08); text-align: center; margin: 15px;
  transition: all 0.3s ease;
}
.stat-box:hover { transform: translateY(-5px); box-shadow: 0 8px 30px rgba(0,0,0,0.12); }
.stat-number { font-size: 54px; font-weight: bold; color: #2d5f2d; }
.stat-label { font-size: 16px; color: #666; text-transform: uppercase; letter-spacing: 1px; }
.feature-card {
  background: white; border-radius: 12px; box-shadow: 0 4px 20px rgba(0,0,0,0.08);
  padding: 30px; margin-bottom: 30px; transition: all 0.3s ease;
}
.feature-card:hover { transform: translateY(-5px); box-shadow: 0 8px 30px rgba(0,0,0,0.12); }
```

### Painting gallery cards — flexbox grid, centered, not left-aligned

``` css
.painting-card {
  background: white; border-radius: 12px; box-shadow: 0 4px 20px rgba(0,0,0,0.08);
  padding: 0; margin: 0 auto 40px auto; overflow: hidden;
  transition: all 0.3s ease; max-width: 550px;
}
.painting-card:hover { transform: translateY(-8px); box-shadow: 0 12px 35px rgba(0,0,0,0.15); }
.painting-card img { width: 100%; height: 300px; object-fit: cover; }
.painting-card-body { padding: 30px; }
.painting-card h3 { margin-top: 0; font-size: 24px; color: #2d5f2d; margin-bottom: 12px; }
.painting-meta { color: #999; font-size: 14px; margin-bottom: 15px; }
.painting-context { line-height: 1.8; color: #555; margin-bottom: 25px; font-size: 15px; }

/* The grid container — flexbox, not Bootstrap columns */
.gallery-grid {
  display: flex; flex-wrap: wrap; justify-content: center;
  gap: 30px; padding: 0 20px;
}
.gallery-grid .painting-card { flex: 0 1 calc(50% - 15px); max-width: 550px; }
@media (max-width: 992px) {
  .gallery-grid .painting-card { flex: 0 1 100%; max-width: 600px; }
}
```

### Form, comparison cards, info box, alerts

``` css
.form-section {
  background: white; padding: 50px; border-radius: 12px;
  box-shadow: 0 4px 20px rgba(0,0,0,0.08); max-width: 800px; margin: 0 auto;
}
.comparison-card {
  background: white; border-radius: 12px; box-shadow: 0 4px 20px rgba(0,0,0,0.08);
  padding: 30px; margin-bottom: 40px;
}
.comparison-images {
  display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin-bottom: 25px;
}
.comparison-images img { width: 100%; border-radius: 8px; }
.image-label { text-align: center; font-weight: 600; color: #2d5f2d; margin-top: 10px; font-size: 14px; }
.info-box {
  background: linear-gradient(135deg, #e8f4f8 0%, #d1e7ee 100%);
  border-left: 5px solid #87ceeb; padding: 25px; border-radius: 8px; margin-bottom: 30px;
}
.alert { border-radius: 8px; padding: 18px; margin-bottom: 25px; border: none; }
.alert-success { background-color: #d4edda; color: #155724; }
.alert-danger  { background-color: #f8d7da; color: #721c24; }
.tab-content   { padding: 40px 20px; }
.leaflet-container { border-radius: 12px; box-shadow: 0 4px 20px rgba(0,0,0,0.1); }
```

### Responsive

``` css
@media (max-width: 768px) {
  .hero-section h1 { font-size: 36px; }
  .hero-section .subtitle { font-size: 18px; }
  .comparison-images { grid-template-columns: 1fr; }
  .navbar-brand { font-size: 24px !important; }
  .navbar-nav > li > a,
  .navbar-nav .nav-link { font-size: 12px !important; padding: 8px 12px !important; }
}
```

------------------------------------------------------------------------

## ANIMATED BACKGROUND — JAVASCRIPT CANVAS

Do NOT use CSS `body::before` / `body::after` animations. They are
blocked by Shiny's wrapper divs. Instead:

1.  Place `tags$canvas(id = "bgCanvas")` directly inside `fluidPage()`,
    after `tags$head()`.
2.  Place the following `tags$script(HTML(...))` after the canvas
    element.

``` javascript
(function() {
  function initCanvas() {
    var canvas = document.getElementById('bgCanvas');
    if (!canvas) { setTimeout(initCanvas, 100); return; }
    var ctx = canvas.getContext('2d');

    function resize() {
      canvas.width  = window.innerWidth;
      canvas.height = window.innerHeight;
    }
    resize();
    window.addEventListener('resize', resize);

    // 6 soft glowing blobs: base position (0–1 ratio), radius, RGB colour, speed, phase offset
    var blobs = [
      { bx:0.12, by:0.22, r:380, rgb:'255,255,255', a:0.13, sp:0.00018, ph:0.0 },
      { bx:0.70, by:0.55, r:320, rgb:'156,175,136', a:0.11, sp:0.00024, ph:1.2 },
      { bx:0.82, by:0.18, r:290, rgb:'135,206,235', a:0.09, sp:0.00020, ph:2.4 },
      { bx:0.30, by:0.78, r:340, rgb:'255,255,255', a:0.10, sp:0.00022, ph:3.6 },
      { bx:0.50, by:0.08, r:260, rgb:'180,210,190', a:0.09, sp:0.00030, ph:4.8 },
      { bx:0.90, by:0.70, r:300, rgb:'200,225,210', a:0.08, sp:0.00016, ph:1.8 }
    ];

    var t = 0;

    function draw() {
      ctx.clearRect(0, 0, canvas.width, canvas.height);

      // Soft gradient background base
      var base = ctx.createLinearGradient(0, 0, 0, canvas.height);
      base.addColorStop(0,   'rgba(225,240,245,0.25)');
      base.addColorStop(0.5, 'rgba(240,247,242,0.15)');
      base.addColorStop(1,   'rgba(255,255,255,0.08)');
      ctx.fillStyle = base;
      ctx.fillRect(0, 0, canvas.width, canvas.height);

      blobs.forEach(function(b) {
        // Two overlapping sine waves per axis = organic non-repeating drift
        var ox = Math.sin(t * b.sp * 1000 + b.ph)       * 0.09
               + Math.sin(t * b.sp *  600 + b.ph + 1.1) * 0.04;
        var oy = Math.cos(t * b.sp *  800 + b.ph + 0.5) * 0.07
               + Math.cos(t * b.sp *  450 + b.ph + 2.3) * 0.03;

        var x = (b.bx + ox) * canvas.width;
        var y = (b.by + oy) * canvas.height;
        var r = b.r + Math.sin(t * b.sp * 400 + b.ph) * 25;  // gentle radius pulse

        var g = ctx.createRadialGradient(x, y, 0, x, y, r);
        g.addColorStop(0,   'rgba(' + b.rgb + ',' + b.a       + ')');
        g.addColorStop(0.5, 'rgba(' + b.rgb + ',' + (b.a*0.5) + ')');
        g.addColorStop(1,   'rgba(' + b.rgb + ',0)');

        ctx.beginPath();
        ctx.arc(x, y, r, 0, Math.PI * 2);
        ctx.fillStyle = g;
        ctx.fill();
      });

      t += 16;
      requestAnimationFrame(draw);
    }

    draw();
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initCanvas);
  } else {
    initCanvas();
  }
})();
```

------------------------------------------------------------------------

## TAB CONTENT — 6 TABS

### Tab 1: Home

-   Hero section (`div(class="hero-section")`) with title h1, subtitle
    p, and `actionButton("goto_map", ...)`
-   3 stat boxes in `fluidRow`: "5 Historic Locations",
    `textOutput("total_submissions")` Submissions,
    `textOutput("approved_count")` Approved
-   Feature card with About text and How to Participate ordered list

### Tab 2: Explore Map

-   `leafletOutput("main_map", height = 600)`
-   Info box above map: "Click on any marker to view painting details"

### Tab 3: Painting Gallery

-   `uiOutput("painting_cards")` — rendered as
    `div(class="gallery-grid", cards)` — NOT Bootstrap columns
-   Each card: `div(class="painting-card")` containing img then
    `div(class="painting-card-body")` with h3 title, meta div (artist •
    year), meta div (coordinates), p context, actionButton with
    `onclick='Shiny.setInputValue("goto_submit", Math.random())'`

### Tab 4: Submit Photo

Form inside `div(class="form-section")`: - `uiOutput("submit_message")`
for success/error - `textInput("submit_name", ...)` optional -
`textInput("submit_email", ...)` optional -
`selectInput("submit_painting", ...)` — choices from paintings_data -
`fileInput("submit_photo", ...)` — accept images only -
`numericInput("submit_latitude", "Latitude (e.g., 40.7489)", value = NA)`
— NO placeholder arg -
`numericInput("submit_longitude", "Longitude (e.g., -109.5596)", value = NA)`
— NO placeholder arg - `textAreaInput("submit_observations", ...)`
optional - `actionButton("submit_button", "Submit Photo", ...)`

**CRITICAL:** `numericInput()` does NOT accept a `placeholder` argument.
Put example values in the label instead.

### Tab 5: Past vs Present

-   `selectInput("filter_painting", ...)` filter dropdown
-   `uiOutput("comparison_gallery")` — shows side-by-side comparison
    cards

### Tab 6: Admin

-   Password panel (default password: `"admin123"`) using
    `conditionalPanel(condition = "output.admin_authenticated == false", ...)`
-   Authenticated panel with Refresh, Approve Selected, Reject Selected
    buttons
-   `DTOutput("admin_table")` with `selection = 'single'`

------------------------------------------------------------------------

## SERVER LOGIC

### Reactive values

``` r
rv <- reactiveValues(
  admin_auth         = FALSE,
  submission_success = FALSE,
  submission_error   = NULL,
  submissions        = load_data(SUBMISSIONS_FILE),
  approved           = load_data(APPROVED_FILE)
)
```

### Navigation

``` r
observeEvent(input$goto_map,    { updateNavbarPage(session, "main_tabs", selected = "map") })
observeEvent(input$goto_submit, { updateNavbarPage(session, "main_tabs", selected = "submit") })
```

### Leaflet map

``` r
output$main_map <- renderLeaflet({
  # Build popup HTML per painting (300px wide, includes img, title, artist/year, context excerpt, coords)
  leaflet(paintings_data) %>%
    addTiles() %>%
    addMarkers(lng = ~longitude, lat = ~latitude, popup = ~popup_html, label = ~title) %>%
    fitBounds(
      lng1 = min(paintings_data$longitude) - 2, lat1 = min(paintings_data$latitude) - 2,
      lng2 = max(paintings_data$longitude) + 2, lat2 = max(paintings_data$latitude) + 2
    )
})
```

### Photo submission

-   Validate: painting selected, photo uploaded, lat/lng not NA, file \<
    5MB
-   Convert photo to base64 using
    `base64enc::base64encode(readBin(...))`
-   Prepend `"data:image/jpeg;base64,"` to base64 string
-   Build data frame row, `rbind` to `rv$submissions`, call
    `save_data(rv$submissions, SUBMISSIONS_FILE)`
-   Clear all form inputs on success

### Admin approval

``` r
# Approve: copy row to rv$approved, save both files, update status in rv$submissions
# Reject:  update approval_status to "Rejected" in rv$submissions, save
# Refresh: reload both rv$submissions and rv$approved from files
```

### Admin authentication

``` r
output$admin_authenticated <- reactive({ rv$admin_auth })
outputOptions(output, "admin_authenticated", suspendWhenHidden = FALSE)
```

### Stats

``` r
output$total_submissions <- renderText({ as.character(nrow(rv$submissions)) })
output$approved_count    <- renderText({ as.character(nrow(rv$approved)) })
```

------------------------------------------------------------------------

## KNOWN BUGS TO AVOID

1.  **`numericInput()` does not accept `placeholder`** — put example
    text in the label string instead
2.  **Do NOT use `body::before` / `body::after` for animation** — use
    the JS canvas approach above
3.  **Do NOT pass `header=` to `navbarPage()`** — all CSS in one
    `tags$style()` block
4.  **Do NOT use `updateTabItems()`** — use `updateNavbarPage()` for tab
    navigation
5.  **`flex-direction: row !important`** must be explicitly set on
    `.navbar-nav` — Bootstrap defaults can override to column
6.  **Do NOT float navbar items** — use `float: none !important` on
    `.navbar-nav`, `.navbar-brand`, and `.navbar-nav > li`

------------------------------------------------------------------------

## DEPLOYMENT

``` r
# Deploy to shinyapps.io
install.packages("rsconnect")
rsconnect::setAccountInfo(name='YOUR_ACCOUNT', token='YOUR_TOKEN', secret='YOUR_SECRET')
rsconnect::deployApp()
```

Note: RDS files persist during a session on shinyapps.io but reset on
redeploy or instance restart. For permanent cloud storage, replace with
a database or Google Sheets backend.

------------------------------------------------------------------------

## OUTPUT

Write the complete `app.R` file with all code, comments, and
`shinyApp(ui = ui, server = server)` at the bottom. The file should run
with no errors when `shiny::runApp()` is called from the directory
containing `app.R`.
