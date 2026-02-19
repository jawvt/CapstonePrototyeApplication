################################################################################
# LANDSCAPE THROUGH TIME - v2.0
# Tab-based navigation, vibrant earthy tones, college-student-friendly
################################################################################

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
# CUSTOM CSS
################################################################################

app_css <- "
/* ======================================================================
   CSS VARIABLES - EARTHY PALETTE
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

/* ======================================================================
   RESET & BASE
   ====================================================================== */
* { margin: 0; padding: 0; box-sizing: border-box; }

html { scroll-behavior: smooth; }

body {
  font-family: 'DM Sans', -apple-system, BlinkMacSystemFont, sans-serif;
  background: var(--cream);
  color: var(--cream);
  line-height: 1.6;
  overflow-x: hidden;
}

/* ======================================================================
   NAVBAR OVERRIDES
   ====================================================================== */
.navbar {
  background: var(--forest) !important; 
  border-bottom: 3px solid var(--terra) !important;
  padding: 0 !important;
  min-height: 64px;
  box-shadow: 0 4px 20px rgba(30, 51, 40, 0.25);
  position: sticky;
  top: 0;
  z-index: 999;
}

.navbar-brand {
  display: none !important;
}

.navbar-nav .nav-link {
  font-family: 'DM Sans', sans-serif !important;
  font-weight: 600 !important;
  font-size: 14px !important;
  color: rgba(245, 237, 224, 0.75) !important;
  padding: 20px 20px !important;
  letter-spacing: 0.5px;
  text-transform: uppercase;
  transition: all 0.3s var(--ease);
  border-bottom: 3px solid transparent;
  margin-bottom: -3px;
  position: relative;
}

.navbar-nav .nav-link:hover {
  color: var(--sand) !important;
  background: rgba(255,255,255,0.05);
}

.navbar-nav .nav-link.active,
.navbar-nav .nav-item.active .nav-link,
.navbar-nav .nav-link[aria-selected='true'] {
  color: var(--white) !important;
  border-bottom-color: var(--terra) !important;
  background: rgba(194, 113, 79, 0.1);
}

/* Hamburger on mobile */
.navbar-toggler {
  border-color: rgba(245, 237, 224, 0.3) !important;
  color: var(--sand) !important;
  margin-right: 16px;
}

.navbar-toggler-icon {
  filter: invert(1);
}

/* ======================================================================
   TAB CONTENT WRAPPER
   ====================================================================== */
.tab-content {
  background: var(--cream);
}

.tab-pane {
  animation: tabFadeIn 0.4s var(--ease);
}

@keyframes tabFadeIn {
  from { opacity: 0; transform: translateY(12px); }
  to { opacity: 1; transform: translateY(0); }
}

/* ======================================================================
   HERO / HOME TAB
   ====================================================================== */
.hero-banner {
  position: relative;
  background: linear-gradient(135deg, var(--forest) 0%, var(--forest-mid) 40%, var(--terra-dark) 100%);
  min-height: calc(100vh - 64px);
  display: flex;
  align-items: center;
  justify-content: center;
  overflow: hidden;
}

.hero-bg-pattern {
  position: absolute;
  inset: 0;
  opacity: 0.06;
  background-image:
    radial-gradient(circle at 20% 80%, var(--amber) 1px, transparent 1px),
    radial-gradient(circle at 80% 20%, var(--terra-light) 1px, transparent 1px),
    radial-gradient(circle at 50% 50%, var(--sage-light) 0.5px, transparent 0.5px);
  background-size: 60px 60px, 80px 80px, 40px 40px;
}

.hero-glow {
  position: absolute;
  width: 600px;
  height: 600px;
  border-radius: 50%;
  filter: blur(120px);
  opacity: 0.15;
  pointer-events: none;
}

.hero-glow-1 {
  background: var(--terra);
  top: -200px;
  right: -100px;
}

.hero-glow-2 {
  background: var(--sage);
  bottom: -200px;
  left: -100px;
}

.hero-glow-3 {
  background: var(--amber);
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
  width: 400px;
  height: 400px;
  opacity: 0.08;
}

.hero-inner {
  position: relative;
  z-index: 2;
  text-align: center;
  max-width: 800px;
  padding: 60px 32px;
}

.hero-badge {
  display: inline-block;
  background: rgba(194, 113, 79, 0.2);
  border: 1px solid rgba(194, 113, 79, 0.4);
  color: var(--terra-light);
  font-size: 12px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 2.5px;
  padding: 8px 22px;
  border-radius: 50px;
  margin-bottom: 32px;
  animation: fadeUp 0.8s var(--ease) 0.1s both;
}

.hero-title {
  font-family: 'DM Serif Display', Georgia, serif;
  font-size: clamp(40px, 7vw, 80px);
  font-weight: 400;
  color: var(--white);
  line-height: 1.1;
  margin-bottom: 24px;
  animation: fadeUp 0.8s var(--ease) 0.25s both;
}

.hero-title span {
  background: linear-gradient(135deg, var(--white) 0%, var(--terra) 100%);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
}

.hero-subtitle {
  font-size: clamp(16px, 2vw, 20px);
  color: rgba(245, 237, 224, 0.7);
  max-width: 600px;
  margin: 0 auto 44px;
  line-height: 1.7;
  animation: fadeUp 0.8s var(--ease) 0.4s both;
}

.hero-actions {
  display: flex;
  gap: 16px;
  justify-content: center;
  flex-wrap: wrap;
  animation: fadeUp 0.8s var(--ease) 0.55s both;
}

@keyframes fadeUp {
  from { opacity: 0; transform: translateY(24px); }
  to { opacity: 1; transform: translateY(0); }
}

/* Stats strip */
.stats-strip {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 2px;
  margin-top: 70px;
  background: rgba(255,255,255,0.05);
  border-radius: var(--radius-lg);
  overflow: hidden;
  animation: fadeUp 0.8s var(--ease) 0.7s both;
}

.stat-item {
  padding: 28px 20px;
  text-align: center;
  backdrop-filter: blur(10px);
  background: rgba(255,255,255,0.03);
  transition: background 0.3s;
}

.stat-item:hover {
  background: rgba(255,255,255,0.06);
}

.stat-value {
  font-family: 'DM Serif Display', Georgia, serif;
  font-size: 42px;
  color: var(--amber-light);
  line-height: 1;
  margin-bottom: 6px;
}

.stat-label {
  font-size: 11px;
  text-transform: uppercase;
  letter-spacing: 2px;
  color: rgba(245, 237, 224, 0.5);
  font-weight: 600;
}

/* ======================================================================
   BUTTONS
   ====================================================================== */
.btn-terra {
  background: linear-gradient(135deg, var(--terra) 0%, var(--terra-dark) 100%);
  color: var(--white) !important;
  border: none;
  padding: 16px 36px;
  font-size: 15px;
  font-weight: 700;
  border-radius: 50px;
  cursor: pointer;
  transition: all 0.3s var(--ease);
  text-decoration: none;
  display: inline-flex;
  align-items: center;
  gap: 8px;
  box-shadow: 0 4px 15px rgba(194, 113, 79, 0.3);
}

.btn-terra:hover {
  transform: translateY(-2px) scale(1.03);
  box-shadow: 0 8px 25px rgba(194, 113, 79, 0.4);
}

.btn-terra:active {
  transform: translateY(0) scale(0.98);
}

/* middle right button*/
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
}

.btn-sage:hover {
  background: rgba(107, 143, 113, 0.15);
  border-color: var(--sage-light);
  transform: translateY(-2px);
}

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
}

.btn-submit:hover {
  transform: translateY(-2px);
  box-shadow: 0 8px 25px rgba(30, 51, 40, 0.35);
}

/* ======================================================================
   SECTION HEADERS
   ====================================================================== */
.section-header {
  text-align: center;
  padding: 60px 24px 40px;
}

.section-header h2 {
  font-family: 'DM Serif Display', Georgia, serif;
  font-size: clamp(32px, 5vw, 52px);
  color: var(--forest);
  margin-bottom: 12px;
  line-height: 1.15;
}

.section-header p {
  font-size: 17px;
  color: var(--charcoal-light);
  max-width: 560px;
  margin: 0 auto;
  line-height: 1.6;
}

.section-header .accent-line {
  width: 51px;
  height: 4px;
  background: linear-gradient(90deg, var(--terra), var(--amber));
  border-radius: 2px;
  margin: 16px auto 0;
}

/* ======================================================================
   PAINTING CARDS
   ====================================================================== */
.gallery-wrap {
  padding: 0 24px 60px;
  max-width: 1400px;
  margin: 0 auto;
}

.paintings-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(380px, 1fr));
  gap: 32px;
}

.painting-card {
  position: relative;
  cursor: pointer;
  border-radius: var(--radius-lg);
  overflow: hidden;
  background: var(--white);
  box-shadow: var(--shadow-md);
  transition: all 0.4s var(--ease);
  transform-style: preserve-3d;
}

.painting-card:hover {
  transform: translateY(-6px);
  box-shadow: var(--shadow-lg);
}

.painting-card-img-wrap {
  position: relative;
  overflow: hidden;
  aspect-ratio: 16 / 10;
}

.painting-card-img-wrap::after {
  content: '';
  position: absolute;
  inset: 0;
  background: linear-gradient(to top, rgba(30,51,40,0.6) 0%, transparent 50%);
  opacity: 0;
  transition: opacity 0.4s;
}

.painting-card:hover .painting-card-img-wrap::after {
  opacity: 1;
}

.painting-image {
  width: 100%;
  height: 100%;
  object-fit: cover;
  display: block;
  transition: transform 0.6s var(--ease);
}

.painting-card:hover .painting-image {
  transform: scale(1.06);
}

.painting-card-badge {
  position: absolute;
  top: 16px;
  right: 16px;
  background: rgba(30, 51, 40, 0.8);
  backdrop-filter: blur(10px);
  color: var(--amber-light);
  font-size: 12px;
  font-weight: 700;
  padding: 6px 14px;
  border-radius: 20px;
  z-index: 2;
}

.painting-info {
  padding: 24px;
}

.painting-title {
  font-family: 'DM Serif Display', Georgia, serif;
  font-size: 22px;
  color: var(--forest);
  margin-bottom: 6px;
  line-height: 1.3;
}

.painting-meta {
  color: var(--terra);
  font-size: 13px;
  font-weight: 600;
  margin-bottom: 12px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

.painting-context {
  color: var(--charcoal-light);
  font-size: 14px;
  line-height: 1.65;
}

.painting-card-cta {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  color: var(--terra);
  font-weight: 700;
  font-size: 13px;
  text-transform: uppercase;
  letter-spacing: 1px;
  margin-top: 14px;
  transition: gap 0.3s;
}

.painting-card:hover .painting-card-cta {
  gap: 10px;
}

/* ======================================================================
   MAP
   ====================================================================== */

.map-container {
  border-radius: 24px;
  overflow: hidden;
  box-shadow: 0 20px 60px rgba(0, 0, 0, 0.3);
  height: 600px;
  margin-bottom: 150px;
}

.leaflet-container {
  height: 100%;
  border-radius: 24px;
}

/* ======================================================================
   FORM
   ====================================================================== */
.form-wrap {
  padding: 0 24px 60px;
  max-width: 660px;
  margin: 0 auto;
}

.form-card {
  background: var(--white);
  padding: 48px;
  border-radius: var(--radius-lg);
  box-shadow: var(--shadow-md);
  border: 1px solid var(--sand-dark);
}

.form-card .form-group {
  margin-bottom: 24px;
}

.form-card label,
.form-card .control-label {
  font-weight: 600;
  color: var(--forest);
  font-size: 14px;
  margin-bottom: 8px;
  display: block;
}

.form-card .form-control,
.form-card .shiny-input-container input[type='text'],
.form-card .shiny-input-container input[type='number'],
.form-card .shiny-input-container select,
.form-card .shiny-input-container textarea {
  width: 100%;
  padding: 14px 16px;
  border: 2px solid var(--sand-dark);
  border-radius: var(--radius-sm);
  font-size: 15px;
  font-family: 'DM Sans', sans-serif;
  transition: all 0.3s var(--ease);
  background: var(--cream);
  color: var(--charcoal);
}

.form-card .form-control:focus,
.form-card input:focus,
.form-card select:focus,
.form-card textarea:focus {
  outline: none;
  border-color: var(--sage);
  box-shadow: 0 0 0 4px rgba(107, 143, 113, 0.15);
  background: var(--white);
}

.upload-zone {
  border: 3px dashed var(--sand-dark);
  border-radius: var(--radius-md);
  padding: 40px 24px;
  text-align: center;
  transition: all 0.3s var(--ease);
  background: var(--cream);
  cursor: pointer;
}

.upload-zone:hover {
  border-color: var(--sage);
  background: rgba(107, 143, 113, 0.04);
}

.upload-icon {
  font-size: 36px;
  margin-bottom: 8px;
}

/* ======================================================================
   COMPARISONS
   ====================================================================== */
.comparison-wrap {
  padding: 0 24px 60px;
  max-width: 1400px;
  margin: 0 auto;
}

.comparison-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(320px, 1fr));
  gap: 24px;
}

.comparison-thumb {
  position: relative;
  aspect-ratio: 16 / 10;
  border-radius: var(--radius-md);
  overflow: hidden;
  cursor: pointer;
  box-shadow: var(--shadow-sm);
  transition: all 0.4s var(--ease);
}

.comparison-thumb:hover {
  transform: translateY(-6px) scale(1.02);
  box-shadow: var(--shadow-lg);
}

.comparison-thumb img {
  width: 100%;
  height: 100%;
  object-fit: cover;
  transition: transform 0.5s var(--ease);
}

.comparison-thumb:hover img {
  transform: scale(1.08);
}

.comparison-thumb-overlay {
  position: absolute;
  inset: 0;
  background: linear-gradient(to top, rgba(30,51,40,0.7) 0%, transparent 60%);
  display: flex;
  align-items: flex-end;
  padding: 20px;
  opacity: 0;
  transition: opacity 0.3s;
}

.comparison-thumb:hover .comparison-thumb-overlay {
  opacity: 1;
}

.comparison-thumb-label {
  color: var(--white);
  font-weight: 700;
  font-size: 14px;
  display: flex;
  align-items: center;
  gap: 6px;
}

.no-comparisons {
  text-align: center;
  padding: 80px 24px;
  color: var(--charcoal-light);
  font-size: 18px;
}

/* ======================================================================
   LIGHTBOX (Paintings)
   ====================================================================== */
#lightbox {
  display: none;
  position: fixed;
  inset: 0;
  background: rgba(10, 15, 12, 0.97);
  z-index: 10000;
  opacity: 0;
  transition: opacity 0.4s;
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
  padding: 70px;
  position: relative;
}

.lightbox-image-container {
  max-width: 90%;
  max-height: 80%;
  position: relative;
  overflow: hidden;
  border-radius: var(--radius-md);
  box-shadow: 0 30px 80px rgba(0,0,0,0.5);
}

.lightbox-image {
  max-width: 100%;
  max-height: 100%;
  display: block;
  animation: kenBurns 20s ease-in-out infinite alternate;
}

@keyframes kenBurns {
  0% { transform: scale(1) translate(0, 0); }
  100% { transform: scale(1.08) translate(-1.5%, -1.5%); }
}

.lightbox-close {
  position: absolute;
  top: 24px;
  right: 24px;
  width: 48px;
  height: 48px;
  background: rgba(194, 113, 79, 0.2);
  border: 1px solid rgba(194, 113, 79, 0.4);
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  cursor: pointer;
  transition: all 0.3s;
  color: var(--terra-light);
  font-size: 18px;
  z-index: 10;
}

.lightbox-close:hover {
  background: rgba(194, 113, 79, 0.4);
  transform: rotate(90deg) scale(1.1);
}

.lightbox-info {
  position: absolute;
  bottom: 32px;
  left: 32px;
  right: 32px;
  background: rgba(30, 51, 40, 0.85);
  backdrop-filter: blur(20px);
  padding: 28px;
  border-radius: var(--radius-md);
  border: 1px solid rgba(107, 143, 113, 0.2);
  opacity: 0;
  transform: translateY(16px);
  transition: all 0.5s var(--ease) 0.2s;
}

#lightbox.active .lightbox-info {
  opacity: 1;
  transform: translateY(0);
}

.lightbox-info h3 {
  font-family: 'DM Serif Display', Georgia, serif;
  color: var(--sand);
  font-size: 24px;
  margin-bottom: 6px;
}

.lightbox-info .meta {
  color: var(--terra-light);
  font-size: 14px;
  margin-bottom: 10px;
  font-weight: 600;
}

.lightbox-info .context {
  color: rgba(245, 237, 224, 0.7);
  font-size: 15px;
  line-height: 1.6;
}

/* ======================================================================
   COMPARISON LIGHTBOX (Side-by-side)
   ====================================================================== */
#comparison-lightbox {
  display: none;
  position: fixed;
  inset: 0;
  background: rgba(10, 15, 12, 0.97);
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
  gap: 4px;
  padding: 70px 32px;
}

.comparison-side {
  position: relative;
  overflow: hidden;
  background: #000;
  border-radius: var(--radius-sm);
}

.comparison-side img {
  width: 100%;
  height: 100%;
  object-fit: contain;
  transition: transform 0.1s ease;
}

.comparison-label {
  position: absolute;
  top: 16px;
  left: 16px;
  background: rgba(30, 51, 40, 0.8);
  backdrop-filter: blur(8px);
  padding: 8px 18px;
  border-radius: 20px;
  font-size: 13px;
  font-weight: 700;
  color: var(--amber-light);
  border: 1px solid rgba(107, 143, 113, 0.2);
}

/* ======================================================================
   ADMIN
   ====================================================================== */
.admin-wrap {
  padding: 0 24px 60px;
  max-width: 1200px;
  margin: 0 auto;
}

.admin-login-card {
  max-width: 400px;
  margin: 0 auto;
  background: var(--white);
  padding: 48px;
  border-radius: var(--radius-lg);
  box-shadow: var(--shadow-md);
  text-align: center;
}

.admin-login-card h3 {
  font-family: 'DM Serif Display', Georgia, serif;
  color: var(--forest);
  font-size: 28px;
  margin-bottom: 8px;
}

.admin-login-card p {
  color: var(--charcoal-light);
  margin-bottom: 24px;
  font-size: 14px;
}

.admin-login-card .form-control {
  padding: 14px 16px;
  border: 2px solid var(--sand-dark);
  border-radius: var(--radius-sm);
  font-size: 15px;
  transition: all 0.3s;
  text-align: center;
  width: 100%;
  margin-bottom: 16px;
}

.admin-login-card .form-control:focus {
  border-color: var(--sage);
  box-shadow: 0 0 0 4px rgba(107,143,113,0.15);
  outline: none;
}

.admin-toolbar {
  display: flex;
  gap: 12px;
  flex-wrap: wrap;
  margin-bottom: 24px;
}

.admin-toolbar .btn {
  padding: 10px 24px;
  border-radius: var(--radius-sm);
  font-weight: 700;
  font-size: 13px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  cursor: pointer;
  transition: all 0.3s;
  border: 2px solid;
}

.admin-toolbar .btn-approve {
  background: var(--sage);
  border-color: var(--sage);
  color: white;
}
.admin-toolbar .btn-approve:hover {
  background: var(--sage-dark);
}

.admin-toolbar .btn-reject {
  background: transparent;
  border-color: var(--terra);
  color: var(--terra);
}
.admin-toolbar .btn-reject:hover {
  background: rgba(194,113,79,0.1);
}

.admin-toolbar .btn-refresh {
  background: transparent;
  border-color: var(--charcoal-light);
  color: var(--charcoal-light);
}
.admin-toolbar .btn-refresh:hover {
  background: rgba(0,0,0,0.04);
}

/* DataTable overrides */
.dataTables_wrapper {
  background: var(--white);
  border-radius: var(--radius-md);
  padding: 24px;
  box-shadow: var(--shadow-sm);
}

table.dataTable thead th {
  font-weight: 700;
  font-size: 12px;
  text-transform: uppercase;
  letter-spacing: 1px;
  color: var(--forest);
  border-bottom: 2px solid var(--sand-dark) !important;
}

/* ======================================================================
   ALERTS
   ====================================================================== */
.alert-success-custom {
  background: rgba(107, 143, 113, 0.1);
  border: 1px solid var(--sage);
  color: var(--sage-dark);
  padding: 16px 20px;
  border-radius: var(--radius-sm);
  margin-bottom: 24px;
  font-weight: 600;
}

.alert-error-custom {
  background: rgba(194, 113, 79, 0.1);
  border: 1px solid var(--terra);
  color: var(--terra-dark);
  padding: 16px 20px;
  border-radius: var(--radius-sm);
  margin-bottom: 24px;
  font-weight: 600;
}

/* ======================================================================
   RESPONSIVE
   ====================================================================== */
@media (max-width: 768px) {
  .paintings-grid { grid-template-columns: 1fr; }
  .comparison-grid { grid-template-columns: 1fr; }
  .stats-strip { grid-template-columns: 1fr; }
  .comparison-container { grid-template-columns: 1fr; }
  .form-card { padding: 32px 24px; }
  .hero-inner { padding: 40px 20px; }
  .lightbox-content { padding: 20px; }
  .lightbox-info { left: 16px; right: 16px; bottom: 16px; padding: 20px; }
  .section-header { padding: 40px 20px 30px; }
}

/* ======================================================================
   SHINY SPECIFIC OVERRIDES
   ====================================================================== */
.shiny-input-container { width: 100% !important; }
.selectize-input { border: 2px solid var(--sand-dark) !important; border-radius: var(--radius-sm) !important; padding: 10px 14px !important; }
.selectize-input.focus { border-color: var(--sage) !important; box-shadow: 0 0 0 4px rgba(107,143,113,0.15) !important; }
"

################################################################################
# UI
################################################################################

ui <- page_navbar(
  useShinyjs(),
  title = NULL,
  id = "main_tabs",
  theme = bs_theme(
    version = 5,
    bg = "#FBF8F3",
    fg = "#2D2D2D",
    primary = "#C2714F",
    secondary = "#6B8F71",
    success = "#6B8F71",
    info = "#D4A843",
    base_font = font_google("DM Sans"),
    heading_font = font_google("DM Serif Display")
  ),
  header = tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=DM+Serif+Display&family=DM+Sans:wght@300;400;600;700&display=swap", rel = "stylesheet"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML(app_css))
  ),

  # ── TAB 1: HOME ──────────────────────────────────────────────────────────
  nav_panel(
    title = "Home",
    icon = icon("mountain-sun"),
    tags$div(class = "hero-banner",
      tags$div(class = "hero-bg-pattern"),
      tags$div(class = "hero-glow hero-glow-1"),
      tags$div(class = "hero-glow hero-glow-2"),
      tags$div(class = "hero-glow hero-glow-3"),
      tags$div(class = "hero-inner",
        tags$h1(class = "hero-title", HTML("Landscape<br>Through <span>Time</span>")),
        tags$p(class = "hero-subtitle",
          "Journey through 150 years of Western landscapes. Compare Albert Bierstadt's iconic paintings with modern photographs from the same locations."
        ),
        tags$div(class = "hero-actions",
          actionButton("go_gallery", HTML("View the Collection &rarr;"), class = "btn-terra"),
          actionButton("go_submit", "Contribute a Photo", class = "btn-sage")
        ),
        tags$div(class = "stats-strip",
          tags$div(class = "stat-item",
            tags$div(class = "stat-value", "5"),
            tags$div(class = "stat-label", "Locations")
          ),
          tags$div(class = "stat-item",
            tags$div(class = "stat-value", textOutput("stat_submissions", inline = TRUE)),
            tags$div(class = "stat-label", "Submissions")
          ),
          tags$div(class = "stat-item",
            tags$div(class = "stat-value", textOutput("stat_approved", inline = TRUE)),
            tags$div(class = "stat-label", "Comparisons")
          )
        )
      )
    )
  ),

  # ── TAB 2: GALLERY ───────────────────────────────────────────────────────
  nav_panel(
    title = "Gallery",
    icon = icon("images"),
    tags$div(class = "section-header",
      tags$h2("The Collection"),
      tags$p("Click any painting to explore it in full detail with Ken Burns animation."),
      tags$div(class = "accent-line")
    ),
    tags$div(class = "gallery-wrap",
      tags$div(id = "paintings-container", class = "paintings-grid",
        uiOutput("painting_cards")
      )
    )
  ),

  # ── TAB 3: MAP ──────────────────────────────────────────────────────────
  nav_panel(
    title = "Map",
    icon = icon("map-location-dot"),
    tags$div(class = "section-header",
      tags$h2("Explore Locations"),
      tags$p("See where Bierstadt set up his easel across the American West."),
      tags$div(class = "accent-line")
    ),
    tags$div(class = "map-wrap",
      tags$div(class = "map-container",
        leafletOutput("main_map", height = "100%")
      )
    )
  ),

  # ── TAB 4: SUBMIT ──────────────────────────────────────────────────────
  nav_panel(
    title = "Submit",
    icon = icon("camera"),
    tags$div(class = "section-header",
      tags$h2("Contribute Your Photo"),
      tags$p("Visit a Bierstadt painting location and share what it looks like today."),
      tags$div(class = "accent-line")
    ),
    tags$div(class = "form-wrap",
      tags$div(class = "form-card",
        uiOutput("submit_message"),
        tags$div(class = "form-group",
          textInput("submit_name", "Your Name (optional)", placeholder = "Jane Doe")
        ),
        tags$div(class = "form-group",
          textInput("submit_email", "Email (optional)", placeholder = "jane@university.edu")
        ),
        tags$div(class = "form-group",
          selectInput("submit_painting", "Which location did you visit?",
                     choices = c("Select a location..." = "", setNames(paintings_data$id, paintings_data$title)))
        ),
        tags$div(class = "form-group",
          tags$label("Upload Your Photo"),
          tags$div(class = "upload-zone",
            tags$div(class = "upload-icon", HTML("&#128247;")),
            tags$p(style = "color: var(--charcoal-light); font-size: 14px;", "Drag & drop or click to browse"),
            fileInput("submit_photo", NULL, accept = c("image/png", "image/jpeg", "image/jpg"))
          )
        ),
        tags$div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 16px;",
          tags$div(class = "form-group",
            numericInput("submit_latitude", "Latitude", value = NA, step = 0.0001)
          ),
          tags$div(class = "form-group",
            numericInput("submit_longitude", "Longitude", value = NA, step = 0.0001)
          )
        ),
        tags$div(class = "form-group",
          textAreaInput("submit_observations", "Observations (optional)", rows = 3,
                       placeholder = "What did you notice about how the landscape has changed?")
        ),
        actionButton("submit_button", HTML("Submit Photo &rarr;"), class = "btn-submit")
      )
    )
  ),

  # ── TAB 5: COMPARE ─────────────────────────────────────────────────────
  nav_panel(
    title = "Compare",
    icon = icon("arrows-left-right"),
    tags$div(class = "section-header",
      tags$h2("Past vs Present"),
      tags$p("See how these landscapes have transformed over 150 years. Click to open side-by-side."),
      tags$div(class = "accent-line")
    ),
    tags$div(class = "comparison-wrap",
      uiOutput("comparison_gallery")
    )
  ),

  # ── SPACER (pushes Login to the far right) ──────────────────────────
  nav_spacer(),

  # ── TAB 6: LOGIN ───────────────────────────────────────────────────────
  nav_panel(
    title = "Login",
    icon = icon("right-to-bracket"),
    tags$div(class = "section-header",
      tags$h2("Admin Login"),
      tags$p("Sign in to review and manage community submissions."),
      tags$div(class = "accent-line")
    ),
    tags$div(class = "admin-wrap",
      conditionalPanel(
        condition = "output.admin_authenticated == false",
        tags$div(class = "admin-login-card",
          tags$h3("Sign In"),
          tags$p("Enter admin credentials to manage submissions."),
          passwordInput("admin_password", NULL, placeholder = "Password"),
          actionButton("admin_login", "Sign In", class = "btn-submit")
        )
      ),
      conditionalPanel(
        condition = "output.admin_authenticated == true",
        tags$div(class = "admin-toolbar",
          actionButton("refresh_admin", "Refresh", class = "btn btn-refresh"),
          actionButton("approve_submission", "Approve Selected", class = "btn btn-approve"),
          actionButton("reject_submission", "Reject Selected", class = "btn btn-reject")
        ),
        DTOutput("admin_table")
      )
    )
  ),

  # ── LIGHTBOXES (always in DOM) ─────────────────────────────────────────
  footer = tagList(
    # Painting lightbox
    tags$div(id = "lightbox",
      tags$div(class = "lightbox-content",
        tags$div(class = "lightbox-close", onclick = "closeLightbox()", HTML("&times;")),
        tags$div(class = "lightbox-image-container",
          tags$img(id = "lightbox-img", class = "lightbox-image", src = "")
        ),
        tags$div(class = "lightbox-info",
          tags$h3(id = "lightbox-title"),
          tags$p(id = "lightbox-meta", class = "meta"),
          tags$p(id = "lightbox-context", class = "context")
        )
      )
    ),

    # Comparison lightbox
    tags$div(id = "comparison-lightbox",
      tags$div(class = "lightbox-close", onclick = "closeComparisonLightbox()",
               style = "position: fixed; top: 24px; right: 24px; z-index: 10002;", HTML("&times;")),
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

    # JavaScript
    tags$script(HTML(paste0("
      // ── LIGHTBOX ──
      var paintingsData = ", jsonlite::toJSON(paintings_data, auto_unbox = TRUE), ";

      window.openLightbox = function(id) {
        var p = paintingsData.find(function(x) { return x.id === id; });
        if (!p) return;
        document.getElementById('lightbox-img').src = p.image_url;
        document.getElementById('lightbox-title').textContent = p.title;
        document.getElementById('lightbox-meta').textContent = p.artist + ' \\u2022 ' + p.year;
        document.getElementById('lightbox-context').textContent = p.context;
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

        var sides = document.querySelectorAll('.comparison-side img');
        sides.forEach(function(img) {
          img.addEventListener('wheel', function(e) {
            e.preventDefault();
            var current = parseFloat(img.style.transform.replace('scale(', '').replace(')', '') || 1);
            var delta = e.deltaY * -0.01;
            var scale = Math.max(1, Math.min(3, current + delta));
            sides.forEach(function(s) { s.style.transform = 'scale(' + scale + ')'; });
          });
        });
      };

      window.closeComparisonLightbox = function() {
        document.getElementById('comparison-lightbox').classList.remove('active');
        document.body.style.overflow = '';
      };

      document.addEventListener('keydown', function(e) {
        if (e.key === 'Escape') { closeLightbox(); closeComparisonLightbox(); }
      });

      // ── 3D TILT ON CARDS ──
      function initTilt() {
        document.querySelectorAll('.painting-card').forEach(function(card) {
          card.addEventListener('mousemove', function(e) {
            var rect = card.getBoundingClientRect();
            var x = e.clientX - rect.left;
            var y = e.clientY - rect.top;
            var rotX = (y - rect.height / 2) / 25;
            var rotY = (rect.width / 2 - x) / 25;
            card.style.transform = 'perspective(800px) rotateX(' + rotX + 'deg) rotateY(' + rotY + 'deg) translateY(-6px)';
          });
          card.addEventListener('mouseleave', function() {
            card.style.transform = '';
          });
        });
      }

      // Re-init tilt when Shiny re-renders cards
      $(document).on('shiny:value', function(e) {
        if (e.name === 'painting_cards') {
          setTimeout(initTilt, 100);
        }
      });
      setTimeout(initTilt, 500);

      // ── TAB NAVIGATION from hero buttons ──
      Shiny.addCustomMessageHandler('switchTab', function(tab) {
        var tabLink = document.querySelector('a.nav-link[data-value=\"' + tab + '\"]');
        if (tabLink) tabLink.click();
      });

      // ── FIX LEAFLET IN TABS ──
      $(document).on('shown.bs.tab', function(e) {
        if (e.target && e.target.getAttribute('data-value') === 'Map') {
          setTimeout(function() {
            window.dispatchEvent(new Event('resize'));
          }, 250);
        }
      });
    ")))
  )
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

  # Hero button navigation
  observeEvent(input$go_gallery, {
    session$sendCustomMessage("switchTab", "Gallery")
  })
  observeEvent(input$go_submit, {
    session$sendCustomMessage("switchTab", "Submit")
  })

  # Stats
  output$stat_submissions <- renderText({ as.character(nrow(rv$submissions)) })
  output$stat_approved <- renderText({ as.character(nrow(rv$approved)) })

  # Painting cards
  output$painting_cards <- renderUI({
    cards <- lapply(1:nrow(paintings_data), function(i) {
      p <- paintings_data[i, ]
      tags$div(class = "painting-card", onclick = sprintf("openLightbox(%d)", p$id),
        tags$div(class = "painting-card-img-wrap",
          tags$img(src = p$image_url, class = "painting-image", alt = p$title),
          tags$div(class = "painting-card-badge", p$year)
        ),
        tags$div(class = "painting-info",
          tags$h3(class = "painting-title", p$title),
          tags$div(class = "painting-meta", paste0(p$artist, " \u2022 ", p$year)),
          tags$p(class = "painting-context", p$context),
          tags$div(class = "painting-card-cta", HTML("View Full &rarr;"))
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

  # Prevent Shiny from suspending the map output when tab is hidden
  outputOptions(output, "main_map", suspendWhenHidden = FALSE)

  # Trigger resize when Map tab becomes visible
  observeEvent(input$main_tabs, {
    if (input$main_tabs == "Map") {
      shinyjs::delay(200, {
        shinyjs::runjs("window.dispatchEvent(new Event('resize'));")
      })
    }
  })

  # Submit message
  output$submit_message <- renderUI({
    if (rv$submission_success) {
      tags$div(class = "alert-success-custom",
        HTML("&#10003; Photo submitted successfully! It's pending admin review.")
      )
    } else if (!is.null(rv$submission_error)) {
      tags$div(class = "alert-error-custom",
        HTML(paste0("&#10007; ", rv$submission_error))
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
      return(tags$div(class = "no-comparisons",
        HTML("&#127796; No approved comparisons yet. Be the first to contribute!")
      ))
    }

    cards <- lapply(1:nrow(approved), function(i) {
      sub <- approved[i, ]
      painting <- paintings_data[paintings_data$id == sub$painting_id, ]

      tags$div(class = "comparison-thumb",
        onclick = sprintf("openComparisonLightbox('%s', '%s')", painting$image_url, sub$photo_url),
        tags$img(src = painting$image_url, alt = painting$title),
        tags$div(class = "comparison-thumb-overlay",
          tags$div(class = "comparison-thumb-label", HTML("&#8644; Compare"))
        )
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
    if (nrow(rv$submissions) == 0) return(data.frame(Message = "No submissions yet"))

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
    showNotification("Data refreshed!", type = "message")
  })
}

shinyApp(ui = ui, server = server)
