library(shiny)
library(bslib)
library(tidyverse)
library(sf)
library(terra)
library(leaflet)
library(DT)
library(rstatix)
library(testflow)
library(here)

# ---------------------------------------------------------
# Data Loading Helper Functions
# ---------------------------------------------------------
datasets_metadata <- list(
  "1" = list(name = "Coastal Forest Structure", file = "theme1_coastal_forest.csv", theme = "Coastal Forest"),
  "2" = list(name = "GHG Industrial Cement (IPPU)", file = "theme2_ghg_ippu.csv", theme = "GHG IPPU"),
  "3" = list(name = "GHG Household Energy", file = "theme3_ghg_energy.csv", theme = "GHG Energy"),
  "4" = list(name = "Marine Ecology & Dolphins", file = "theme4_marine_ecology.csv", theme = "Marine Ecology"),
  "5" = list(name = "GHG Agriculture & AFOLU", file = "theme5_ghg_afolu.csv", theme = "GHG AFOLU"),
  "6" = list(name = "Plastic Waste Spatial Survey", file = "theme6_plastic_waste.csv", theme = "Plastic Waste"),
  "7" = list(name = "GHG Anaerobic Digestion (Waste)", file = "theme7_ghg_waste.csv", theme = "GHG Waste")
)

load_dataset <- function(id) {
  meta <- datasets_metadata[[as.character(id)]]
  path <- here("data", meta$file)
  if (file.exists(path)) {
    return(read_csv(path, show_col_types = FALSE))
  }
  # Fallback to current working directory
  fallback_path <- file.path("data", meta$file)
  if (file.exists(fallback_path)) {
    return(read_csv(fallback_path, show_col_types = FALSE))
  }
  return(NULL)
}

# ---------------------------------------------------------
# UI Design System (Modern Yacht / Forest theme)
# ---------------------------------------------------------
theme <- bs_theme(
  bootswatch = "yeti",
  primary = "#0A3B5C", # Deep Marine Blue
  secondary = "#4A7C59", # Coastal Forest Green
  success = "#2ECC71",
  info = "#17A2B8",
  warning = "#F39C12",
  danger = "#E74C3C",
  base_font = font_google("Inter"),
  heading_font = font_google("Outfit"),
  code_font = font_google("Fira Code")
)

# Stats Guide Subtab UI Builder
stats_tab_content_ui <- function(id, theme_name, dataset_name, x_var, y_var, group_var, explanation) {
  fluidPage(
    # Thematic Header
    tags$div(
      style = "padding: 1.25rem; background: linear-gradient(135deg, #0A3B5C 0%, #0f766e 100%); color: white; border-radius: 10px; margin-bottom: 1.25rem; box-shadow: 0 4px 15px rgba(0,71,99,0.1);",
      h4(style = "font-family: 'Outfit'; font-weight: 800; margin: 0 0 0.4rem 0;", sprintf("Thematic Track: %s", theme_name)),
      p(style = "font-size: 0.88rem; opacity: 0.95; margin: 0; line-height: 1.4;", sprintf("Analyzing the preloaded dataset '%s'. %s", dataset_name, explanation))
    ),
    layout_columns(
      col_widths = c(4, 8),

      # Left Column: Configuration & Hypothesis
      div(
        card(
          class = "mb-3",
          card_header(span(icon("lightbulb"), " Hypothesis Formulation", style = "font-weight:700; color:#0A3B5C;")),
          tags$div(
            style = "font-size: 0.85rem; line-height: 1.6; color: #334155; padding: 0.5rem;",
            h6("Null Hypothesis (H0):", style = "font-weight:700; color:#0f766e; margin-top:0.2rem; margin-bottom: 0.2rem;"),
            p(style = "margin-bottom:0.8rem;", sprintf("There is no significant difference in the mean of %s across different categories of %s.", y_var, group_var)),
            h6("Alternative Hypothesis (H1):", style = "font-weight:700; color:#b91c1c; margin-bottom: 0.2rem;"),
            p(style = "margin-bottom:0.8rem;", sprintf("There is a significant difference in the mean of %s across different categories of %s.", y_var, group_var)),
            hr(style = "margin: 0.8rem 0;"),
            h6("Assumption Verification Checklist:", style = "font-weight:700; color:#0A3B5C; margin-bottom: 0.4rem;"),
            tags$ul(
              style = "padding-left:1.2rem; margin-bottom:0;",
              tags$li("Independent random samples"),
              tags$li("Normality assessment (Shapiro-Wilk test)"),
              tags$li("Homogeneity of variance (Levene's test)")
            )
          )
        ),
        card(
          class = "mb-3",
          card_header(span(icon("code-branch"), " Practical R Syntax", style = "font-weight:700; color:#0A3B5C;")),
          tags$div(
            style = "font-family: 'Fira Code', monospace; font-size: 0.76rem; background: #f8fafc; border-radius: 6px; padding: 10px; border: 1px solid #e2e8f0; color: #334155;",
            pre(
              style = "margin:0;",
              sprintf("# Install & load rstatix
install.packages('rstatix')
library(rstatix)

# 1. Summarize
dataset |> get_summary_stats(%s)

# 2. Compare means
dataset |> t_test(%s ~ %s)

# 3. ANOVA
dataset |> anova_test(%s ~ %s)

# 4. Correlation Matrix
dataset |> cor_mat()", y_var, y_var, group_var, y_var, group_var)
            )
          )
        )
      ),

      # Right Column: Live Analysis Tabs
      div(
        navset_card_pill(
          # Pill 1: Descriptive Stats
          nav_panel(
            title = "Descriptive Stats",
            icon = icon("table"),
            p(style = "font-size:0.9rem; color:#475569;", "Using ", code("get_summary_stats()"), " to compute descriptive measures:"),
            tags$div(
              style = "font-family: 'Fira Code', monospace; font-size: 0.78rem; background: #f8fafc; border-radius: 6px; padding: 10px; border: 1px solid #e2e8f0; color: #475569; margin-bottom:1rem;",
              pre(style = "margin:0;", sprintf("dataset |>\n  get_summary_stats(%s, type = 'common')", y_var))
            ),
            h6("Live R Output (DT summary table):", style = "font-weight:700; color:#0A3B5C; margin-bottom: 0.5rem;"),
            DTOutput(NS(id, "desc_table"))
          ),

          # Pill 2: Comparing Means
          nav_panel(
            title = "Comparing Means",
            icon = icon("scale-balanced"),
            p(style = "font-size:0.9rem; color:#475569;", "Comparing group means using the parametric ", code("t_test()"), " function:"),
            tags$div(
              style = "font-family: 'Fira Code', monospace; font-size: 0.78rem; background: #f8fafc; border-radius: 6px; padding: 10px; border: 1px solid #e2e8f0; color: #475569; margin-bottom:1rem;",
              pre(style = "margin:0;", sprintf("dataset |>\n  t_test(%s ~ %s, paired = FALSE)", y_var, group_var))
            ),
            h6("Live R Output (T-Test table):", style = "font-weight:700; color:#0A3B5C; margin-bottom: 0.5rem;"),
            DTOutput(NS(id, "ttest_table")),
            br(),
            h6("Live Boxplot with p-value annotation:", style = "font-weight:700; color:#0A3B5C; margin-bottom: 0.5rem;"),
            plotOutput(NS(id, "ttest_plot"), height = "280px")
          ),

          # Pill 3: ANOVA
          nav_panel(
            title = "ANOVA",
            icon = icon("chart-line"),
            p(style = "font-size:0.9rem; color:#475569;", "Performing analysis of variance (ANOVA) to compare multiple groups using ", code("anova_test()"), ":"),
            tags$div(
              style = "font-family: 'Fira Code', monospace; font-size: 0.78rem; background: #f8fafc; border-radius: 6px; padding: 10px; border: 1px solid #e2e8f0; color: #475569; margin-bottom:1rem;",
              pre(style = "margin:0;", sprintf("dataset |>\n  anova_test(%s ~ %s)", y_var, group_var))
            ),
            h6("Live R Output (ANOVA Table):", style = "font-weight:700; color:#0A3B5C; margin-bottom: 0.5rem;"),
            DTOutput(NS(id, "anova_table"))
          ),

          # Pill 4: Correlation Matrix
          nav_panel(
            title = "Correlation Matrix",
            icon = icon("circle-nodes"),
            p(style = "font-size:0.9rem; color:#475569;", "Generating correlation matrix with significance markers via ", code("cor_mat()"), ":"),
            tags$div(
              style = "font-family: 'Fira Code', monospace; font-size: 0.78rem; background: #f8fafc; border-radius: 6px; padding: 10px; border: 1px solid #e2e8f0; color: #475569; margin-bottom:1rem;",
              pre(style = "margin:0;", "dataset |>\n  select_if(is.numeric) |>\n  cor_mat() |>\n  cor_mark_significant()")
            ),
            h6("Live R Output (Correlation Matrix table):", style = "font-weight:700; color:#0A3B5C; margin-bottom: 0.5rem;"),
            DTOutput(NS(id, "cormat_table"))
          )
        )
      )
    )
  )
}

# Custom Shiny Dashboard UI
ui <- page_navbar(
  theme = theme,
  title = tags$div(
    style = "display: flex; align-items: center; gap: 10px;",
    tags$img(src = "icon-192.png", height = "32px", alt = "IUCN Logo", style = "border-radius: 4px;"),
    # tags$span("Bahari Yetu", style = "font-family: 'Outfit'; font-weight: 800; color: #ffffff;")
  ),
  window_title = "IUCN Bahari Yetu Portal",
  bg = "#0A3B5C",

  # Custom Head Styles
  header = tags$head(
    uiOutput("dynamic_palette_css"),
    # PWA Support Tags
    tags$link(rel = "manifest", href = "manifest.json"),
    tags$meta(name = "theme-color", content = "#0A3B5C"),
    tags$meta(name = "mobile-web-app-capable", content = "yes"),
    tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
    tags$meta(name = "apple-mobile-web-app-status-bar-style", content = "black-translucent"),
    tags$link(rel = "apple-touch-icon", href = "icon-192.png"),

    # PWA Service Worker & Auto-Reconnect/Reload on Sleep
    tags$script(HTML("
      if ('serviceWorker' in navigator) {
        const registerSW = () => {
          // Resolve base path dynamically to support subfolders or different base URIs
          const basePath = window.location.pathname.substring(0, window.location.pathname.lastIndexOf('/') + 1);
          const swUrl = basePath + 'service-worker.js';
          
          navigator.serviceWorker.register(swUrl).then(function(reg) {
            console.log('ServiceWorker registered with scope: ', reg.scope);
          }).catch(function(err) {
            console.log('ServiceWorker registration failed: ', err);
          });
        };

        if (document.readyState === 'complete') {
          registerSW();
        } else {
          window.addEventListener('load', registerSW);
        }
      }

      // Offline network monitoring
      const toggleOfflineBanner = () => {
        const offlineBanner = document.getElementById('pwa-offline-banner');
        if (offlineBanner) {
          if (!navigator.onLine) {
            offlineBanner.style.setProperty('display', 'block', 'important');
          } else {
            offlineBanner.style.setProperty('display', 'none', 'important');
          }
        }
      };

      window.addEventListener('online', toggleOfflineBanner);
      window.addEventListener('offline', toggleOfflineBanner);

      // Check if context is secure (PWA installation requires HTTPS or localhost)
      $(document).ready(function() {
        // Inject offline banner
        const bannerHtml = `
          <div id=\"pwa-offline-banner\" style=\"display: none; position: fixed; top: 0; left: 0; width: 100vw; background: #e11d48; color: white; text-align: center; padding: 0.5rem 1rem; font-size: 0.8rem; font-weight: 700; z-index: 1000000; box-shadow: 0 2px 10px rgba(0,0,0,0.15); animation: slideDownSW 0.3s ease;\">
            <i class=\"fa fa-exclamation-triangle\"></i> You are currently offline. Dynamic R calculations and map routing require connection to the R Server.
          </div>
          <style>
            @keyframes slideDownSW {
              from { transform: translateY(-50px); opacity: 0; }
              to { transform: translateY(0); opacity: 1; }
            }
          </style>
        `;
        $('body').append(bannerHtml);
        toggleOfflineBanner();

        if (!window.isSecureContext) {
          console.warn('PWA installation requires a secure context (HTTPS or localhost).');
          const warningEl = document.getElementById('pwa-secure-warning');
          if (warningEl) {
            warningEl.style.setProperty('display', 'block', 'important');
          }
        }

        // Dynamically inject the installation popup HTML/CSS
        const popupHtml = `
          <div id=\"pwa-install-popup\" style=\"display: none; position: fixed; bottom: 20px; right: 20px; max-width: 320px; background: rgba(255, 255, 255, 0.95); backdrop-filter: blur(12px); -webkit-backdrop-filter: blur(12px); border: 1px solid rgba(255, 255, 255, 0.4); border-radius: 12px; box-shadow: 0 10px 25px rgba(0, 0, 0, 0.15); padding: 1.25rem; z-index: 100000; font-family: 'Inter', sans-serif; animation: slideUpSW 0.4s cubic-bezier(0.16, 1, 0.3, 1);\">
            <div style=\"display: flex; align-items: start; gap: 12px; margin-bottom: 12px;\">
              <img src=\"icon-192.png\" style=\"width: 48px; height: 48px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);\" alt=\"App Icon\">
              <div>
                <h5 style=\"margin: 0; font-family: 'Outfit'; font-weight: 800; font-size: 1rem; color: #0A3B5C;\">IUCN Bahari Yetu</h5>
                <p style=\"margin: 4px 0 0 0; font-size: 0.8rem; color: #475569; line-height: 1.4;\">Install this app on your device for offline access and faster loading.</p>
              </div>
            </div>
            <div style=\"display: flex; gap: 8px; justify-content: flex-end;\">
              <button id=\"pwa-popup-close-btn\" style=\"background: transparent; border: 1px solid #CBD5E1; color: #475569; padding: 0.4rem 0.8rem; border-radius: 6px; font-size: 0.78rem; font-weight: 600; cursor: pointer; transition: all 0.2s;\">Later</button>
              <button id=\"pwa-popup-install-btn\" style=\"background: #2ECC71; border: none; color: white; padding: 0.4rem 0.8rem; border-radius: 6px; font-size: 0.78rem; font-weight: 700; cursor: pointer; transition: all 0.2s; box-shadow: 0 4px 6px rgba(46, 204, 113, 0.15);\">Install</button>
            </div>
            <style>
              @keyframes slideUpSW {
                from { transform: translateY(50px); opacity: 0; }
                to { transform: translateY(0); opacity: 1; }
              }
            </style>
          </div>
        `;
        $('body').append(popupHtml);
      });

      // PWA Installation handling
      let deferredPrompt;
      window.addEventListener('beforeinstallprompt', (e) => {
        // Prevent Chrome 67 and earlier from automatically showing the prompt
        e.preventDefault();
        // Stash the event so it can be triggered later.
        deferredPrompt = e;
        
        // Show PWA sidebar container
        const installContainer = document.getElementById('pwa-install-container');
        if (installContainer) {
          installContainer.style.setProperty('display', 'block', 'important');
        }

        // Show PWA popup if not dismissed in this session
        if (!sessionStorage.getItem('pwa-dismissed')) {
          const installPopup = document.getElementById('pwa-install-popup');
          if (installPopup) {
            installPopup.style.setProperty('display', 'block', 'important');
          }
        }
      });

      window.addEventListener('appinstalled', (evt) => {
        console.log('App was installed successfully!');
        const installContainer = document.getElementById('pwa-install-container');
        if (installContainer) {
          installContainer.style.setProperty('display', 'none', 'important');
        }
        const installPopup = document.getElementById('pwa-install-popup');
        if (installPopup) {
          installPopup.style.setProperty('display', 'none', 'important');
        }
      });

      // Click handler for the custom install button
      $(document).on('click', '#pwa-install-btn', async function() {
        if (deferredPrompt) {
          deferredPrompt.prompt();
          const { outcome } = await deferredPrompt.userChoice;
          console.log('User response to the install prompt: ' + outcome);
          deferredPrompt = null;
          const installContainer = document.getElementById('pwa-install-container');
          if (installContainer) {
            installContainer.style.setProperty('display', 'none', 'important');
          }
          const installPopup = document.getElementById('pwa-install-popup');
          if (installPopup) {
            installPopup.style.setProperty('display', 'none', 'important');
          }
        }
      });

      // Click handler for the custom install popup button
      $(document).on('click', '#pwa-popup-install-btn', async function() {
        if (deferredPrompt) {
          deferredPrompt.prompt();
          const { outcome } = await deferredPrompt.userChoice;
          console.log('User response to the install prompt: ' + outcome);
          deferredPrompt = null;
          const installContainer = document.getElementById('pwa-install-container');
          if (installContainer) {
            installContainer.style.setProperty('display', 'none', 'important');
          }
          const installPopup = document.getElementById('pwa-install-popup');
          if (installPopup) {
            installPopup.style.setProperty('display', 'none', 'important');
          }
        }
      });

      // Click handler for the custom install popup 'Later' button
      $(document).on('click', '#pwa-popup-close-btn', function() {
        sessionStorage.setItem('pwa-dismissed', 'true');
        const installPopup = document.getElementById('pwa-install-popup');
        if (installPopup) {
          installPopup.style.setProperty('display', 'none', 'important');
        }
      });

      // Keep alive ping every 25 seconds to prevent R Shiny idle timeouts
      setInterval(function() {
        if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
          Shiny.setInputValue('keep_alive_ping', new Date().getTime());
        }
      }, 25000);

      // Auto-reload on disconnection / server sleep
      $(document).on('shiny:disconnected', function(event) {
        console.log('Shiny disconnected / sleeping. Waking up and reloading...');

        // Build premium colored sleeping/reconnect overlay
        var overlay = document.createElement('div');
        overlay.id = 'shiny-sleep-overlay';
        overlay.style.position = 'fixed';
        overlay.style.top = '0';
        overlay.style.left = '0';
        overlay.style.width = '100vw';
        overlay.style.height = '100vh';
        overlay.style.backgroundColor = 'rgba(10, 59, 92, 0.95)';
        overlay.style.color = '#FFFFFF';
        overlay.style.display = 'flex';
        overlay.style.flexDirection = 'column';
        overlay.style.alignItems = 'center';
        overlay.style.justifyContent = 'center';
        overlay.style.zIndex = '99999';
        overlay.style.fontFamily = \"'Outfit', sans-serif\";

        overlay.innerHTML = '<div style=\"text-align:center;\">' +
          '<div style=\"border: 4px solid rgba(255,255,255,0.15); border-left-color: #34D399; width: 50px; height: 50px; border-radius: 50%; animation: spin 1s linear infinite; margin: 0 auto 1.5rem auto;\"></div>' +
          '<h2 style=\"font-weight:800; margin-bottom:0.5rem;\">Applied Statistics Application is Sleeping</h2>' +
          '<p style=\"color:#CBD5E1; font-size:1rem; margin-bottom:0;\">Reconnecting and waking up the R server, please wait...</p>' +
          '</div>' +
          '<style>' +
          '@keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }' +
          '</style>';

        document.body.appendChild(overlay);

        // Auto reload after 2.5 seconds
        setTimeout(function() {
          window.location.reload();
        }, 2500);
      });
    ")),
    tags$style(HTML("
      /* Full-screen Loading Overlay when Shiny is busy calculating */
      body.shiny-busy::before {
        content: '';
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: rgba(15, 23, 42, 0.45);
        backdrop-filter: blur(4px);
        -webkit-backdrop-filter: blur(4px);
        z-index: 999999;
        display: block;
      }
      body.shiny-busy::after {
        content: '';
        position: fixed;
        top: 50%;
        left: 50%;
        width: 60px;
        height: 60px;
        margin-top: -30px;
        margin-left: -30px;
        border: 5px solid rgba(255, 255, 255, 0.2);
        border-top-color: #34d399; /* matches --accent-color */
        border-radius: 50%;
        z-index: 1000000;
        animation: shiny-busy-spinner 0.8s linear infinite;
        display: block;
      }
      @keyframes shiny-busy-spinner {
        to { transform: rotate(360deg); }
      }

      :root {
        --primary-color: #0A3B5C;
        --accent-color: #34D399;
        --gradient-start: #0A3B5C;
        --gradient-end: #1b4e3e;
        --card-bg: rgba(255, 255, 255, 0.85);
        --body-bg: #f8fafc;
        --text-color: #334155;
      }
      body {
        background-color: var(--body-bg);
        color: var(--text-color);
        transition: background-color 0.3s ease, color 0.3s ease;
      }
      .navbar {
        background-color: var(--primary-color) !important;
        box-shadow: 0 4px 20px rgba(10, 59, 92, 0.15) !important;
        padding: 0.8rem 1.5rem !important;
        border-bottom: 2.5px solid var(--accent-color) !important;
        transition: background-color 0.3s ease, border-color 0.3s ease;
      }
      .navbar-brand {
        font-family: 'Outfit', sans-serif !important;
        font-weight: 800 !important;
        letter-spacing: 0.5px;
      }
      .hero-banner {
        background: linear-gradient(135deg, var(--gradient-start) 0%, var(--primary-color) 50%, var(--gradient-end) 100%);
        color: white;
        padding: 3.5rem 2rem;
        border-radius: 16px;
        margin-bottom: 2rem;
        box-shadow: 0 10px 30px rgba(10,59,92,0.12);
        position: relative;
        overflow: hidden;
      }
      .hero-banner::after {
        content: '';
        position: absolute;
        top: -50%;
        right: -30%;
        width: 300px;
        height: 300px;
        background: radial-gradient(circle, rgba(255, 255, 255, 0.15) 0%, transparent 70%);
        border-radius: 50%;
        pointer-events: none;
      }
      .hero-banner::before {
        content: '';
        position: absolute;
        bottom: -40%;
        left: -10%;
        width: 400px;
        height: 400px;
        background: radial-gradient(circle, rgba(54, 211, 153, 0.12) 0%, transparent 70%);
        border-radius: 50%;
        pointer-events: none;
      }
      .hero-banner h1 {
        font-family: 'Outfit', sans-serif;
        font-weight: 800;
        font-size: 2.8rem;
        margin-bottom: 0.5rem;
      }
      .hero-banner p {
        font-size: 1.15rem;
        opacity: 0.9;
        max-width: 800px;
        margin-bottom: 0;
      }
      .tna-card {
        background: var(--card-bg);
        backdrop-filter: blur(8px);
        -webkit-backdrop-filter: blur(8px);
        border-radius: 16px;
        padding: 1.5rem;
        box-shadow: 0 8px 32px 0 rgba(15, 23, 42, 0.04);
        border: 1px solid rgba(255, 255, 255, 0.45);
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        height: 100%;
      }
      .tna-card:hover {
        transform: translateY(-4px);
        box-shadow: 0 16px 36px 0 rgba(10, 59, 92, 0.09);
        border-color: var(--primary-color) !important;
      }
      .tna-stat {
        font-size: 2.25rem;
        font-weight: 800;
        color: var(--primary-color);
        font-family: 'Outfit', sans-serif;
        line-height: 1;
        margin-bottom: 0.4rem;
      }
      .tna-label {
        font-size: 0.85rem;
        font-weight: 700;
        color: #475569;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      .tna-desc {
        font-size: 0.85rem;
        color: #64748b;
        margin-top: 0.5rem;
      }
      .custom-card {
        background: var(--card-bg);
        backdrop-filter: blur(8px);
        -webkit-backdrop-filter: blur(8px);
        border-radius: 16px;
        padding: 1.8rem;
        box-shadow: 0 8px 32px 0 rgba(15, 23, 42, 0.04);
        border: 1px solid rgba(255, 255, 255, 0.45);
        margin-bottom: 1.5rem;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
      }
      .custom-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 12px 36px 0 rgba(15, 23, 42, 0.07);
        border-color: rgba(10, 59, 92, 0.2);
      }
      .section-title {
        font-family: 'Outfit', sans-serif;
        font-weight: 700;
        color: var(--primary-color);
        margin-bottom: 1.2rem;
        border-bottom: 2.5px solid var(--accent-color);
        padding-bottom: 0.4rem;
        display: inline-block;
      }
      .code-container {
        background: #0b0f19;
        color: #f8fafc;
        padding: 1.2rem;
        border-radius: 12px;
        font-family: 'Fira Code', monospace;
        font-size: 0.88rem;
        position: relative;
        overflow-x: auto;
        margin-top: 0.8rem;
        box-shadow: 0 10px 30px rgba(0, 0, 0, 0.25);
        border: 1px solid #1e293b;
      }
      .code-container pre {
        margin: 0;
        background: transparent;
        color: inherit;
        border: none;
        padding: 0;
      }
      .schedule-item {
        border-left: 3px solid #0A3B5C;
        padding-left: 1.2rem;
        margin-bottom: 1.2rem;
        position: relative;
      }
      .schedule-time {
        font-weight: 700;
        color: #0A3B5C;
        font-size: 0.88rem;
      }
      .schedule-title {
        font-weight: 600;
        font-size: 1.05rem;
        margin: 0.2rem 0;
        color: #1e293b;
      }
      .schedule-desc {
        color: #64748b;
        font-size: 0.88rem;
      }
      .wide-table, .tidy-table {
        font-size: 0.85rem;
      }
      .nav-tabs .nav-link {
        font-family: 'Outfit', sans-serif;
        font-weight: 600;
      }
      .badge-custom {
        background-color: #e0f2fe;
        color: #0369a1;
        font-weight: 600;
        padding: 0.3em 0.6em;
        border-radius: 4px;
      }
      /* Flowchart & Educational Diagram Styles */
      .flowchart-container {
        display: flex;
        align-items: stretch;
        justify-content: space-between;
        margin: 2rem 0;
        gap: 1.5rem;
      }
      .flowchart-column {
        flex: 1;
        background: white;
        border-radius: 16px;
        padding: 2rem;
        border: 1px solid #e2e8f0;
        box-shadow: 0 4px 20px rgba(0,0,0,0.02);
        display: flex;
        flex-direction: column;
      }
      .flowchart-column.excel {
        border-top: 5px solid #E74C3C; /* Red warning accent */
      }
      .flowchart-column.coding {
        border-top: 5px solid #2ECC71; /* Green success accent */
      }
      .flowchart-header {
        font-family: 'Outfit', sans-serif;
        font-weight: 700;
        font-size: 1.3rem;
        margin-bottom: 1rem;
        display: flex;
        align-items: center;
        gap: 0.6rem;
      }
      .flowchart-step {
        display: flex;
        align-items: center;
        background: #f8fafc;
        padding: 1rem;
        border-radius: 10px;
        margin-bottom: 0.8rem;
        border: 1px solid #f1f5f9;
        position: relative;
      }
      .flowchart-step-icon {
        width: 32px;
        height: 32px;
        border-radius: 50%;
        display: flex;
        align-items: center;
        justify-content: center;
        font-weight: 700;
        font-size: 0.95rem;
        margin-right: 1rem;
        flex-shrink: 0;
      }
      .excel .flowchart-step-icon {
        background: #fde8e8;
        color: #E74C3C;
      }
      .coding .flowchart-step-icon {
        background: #e6fcf5;
        color: #2ECC71;
      }
      .flowchart-step-text {
        font-size: 0.9rem;
        font-weight: 600;
        color: #1e293b;
        display: block;
      }
      .flowchart-step-desc {
        font-size: 0.8rem;
        color: #64748b;
        display: block;
      }
      .flowchart-arrow {
        text-align: center;
        color: #cbd5e1;
        font-size: 1.1rem;
        margin-bottom: 0.8rem;
      }
      .flowchart-footer {
        margin-top: auto;
        padding-top: 1rem;
        font-size: 0.9rem;
        font-weight: 700;
        text-align: center;
        border-top: 1px solid #f1f5f9;
      }
      @media (max-width: 768px) {
        .flowchart-container {
          flex-direction: column;
        }
      }
      /* Day Tabs README styling */
      .day-header-banner {
        background: linear-gradient(135deg, #4A7C59 0%, #1e4e3e 100%);
        color: white;
        padding: 1.8rem;
        border-radius: 12px;
        margin-bottom: 1.5rem;
        box-shadow: 0 4px 15px rgba(0,0,0,0.03);
      }
      .day-header-banner h3 {
        font-family: 'Outfit', sans-serif;
        font-weight: 700;
        margin: 0;
      }
      .day-header-banner p {
        margin: 0.4rem 0 0 0;
        font-size: 0.95rem;
        opacity: 0.9;
      }
      .objective-item {
        display: flex;
        align-items: flex-start;
        margin-bottom: 0.8rem;
      }
      .objective-icon {
        color: #2ECC71;
        margin-right: 0.8rem;
        font-size: 1.05rem;
        margin-top: 0.15rem;
      }
      .objective-text {
        font-size: 0.88rem;
        color: #475569;
        line-height: 1.4;
      }
      .resource-link-btn {
        display: inline-flex;
        align-items: center;
        gap: 0.5rem;
        background: #f1f5f9;
        color: #0A3B5C;
        padding: 0.5rem 0.8rem;
        border-radius: 8px;
        font-size: 0.8rem;
        font-weight: 600;
        text-decoration: none;
        border: 1px solid #e2e8f0;
        margin-bottom: 0.5rem;
        margin-right: 0.5rem;
        transition: all 0.2s ease;
      }
      .resource-link-btn:hover {
        background: #e2e8f0;
        color: #0A3B5C;
        transform: translateY(-1px);
        box-shadow: 0 2px 6px rgba(0,0,0,0.05);
      }
      .resource-link-btn i {
        font-size: 0.9rem;
      }

      /* Profile Card & Registry Directory Styles */
      .profile-grid-container {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
        gap: 1.5rem;
        margin-top: 1rem;
      }
      .directory-card {
        background: #ffffff;
        border-radius: 12px;
        border: 1px solid #e2e8f0;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.05), 0 2px 4px -1px rgba(0, 0, 0, 0.03);
        display: flex;
        flex-direction: column;
        overflow: hidden;
        transition: transform 0.25s ease, box-shadow 0.25s ease;
        height: 100%;
      }
      .directory-card:hover {
        transform: translateY(-4px);
        box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, 0.05);
      }
      .profile-card-header-bar {
        background: linear-gradient(135deg, #0A3B5C 0%, #0d9488 100%);
        height: 8px;
        width: 100%;
      }
      .directory-card-body {
        padding: 1.25rem;
        display: flex;
        flex-direction: column;
        align-items: center;
        text-align: center;
        flex-grow: 1;
      }
      .profile-photo-wrapper {
        position: relative;
        width: 90px;
        height: 90px;
        border-radius: 50%;
        margin-bottom: 0.8rem;
        border: 3.5px solid #ffffff;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        overflow: hidden;
        display: flex;
        align-items: center;
        justify-content: center;
        background: linear-gradient(135deg, #e2e8f0 0%, #cbd5e1 100%);
      }
      .profile-photo-img {
        width: 100%;
        height: 100%;
        object-fit: cover;
      }
      .profile-photo-placeholder {
        width: 100%;
        height: 100%;
        display: flex;
        align-items: center;
        justify-content: center;
        font-family: 'Outfit', sans-serif;
        font-weight: 700;
        font-size: 1.6rem;
        color: #ffffff;
        background: linear-gradient(135deg, #0A3B5C 0%, #0d9488 100%);
      }
      .profile-name {
        font-family: 'Outfit', sans-serif;
        font-weight: 700;
        font-size: 1.15rem;
        color: #0A3B5C;
        margin-bottom: 0.2rem;
      }
      .profile-badge {
        font-size: 0.72rem;
        font-weight: 700;
        padding: 0.25rem 0.6rem;
        border-radius: 50px;
        margin-bottom: 0.6rem;
        display: inline-block;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      .badge-role-trainer {
        background-color: #d1fae5;
        color: #065f46;
      }
      .badge-role-trainee {
        background-color: #dbeafe;
        color: #1e40af;
      }
      .badge-role-support {
        background-color: #fef3c7;
        color: #92400e;
      }
      .badge-role-partner {
        background-color: #f3e8ff;
        color: #6b21a8;
      }
      .profile-meta-item {
        font-size: 0.8rem;
        color: #475569;
        margin-bottom: 0.35rem;
        display: flex;
        align-items: center;
        gap: 6px;
        text-align: left;
        width: 100%;
      }
      .profile-meta-item i {
        color: #0d9488;
        width: 16px;
        text-align: center;
      }
      .profile-bio-text {
        font-size: 0.78rem;
        color: #64748b;
        line-height: 1.45;
        margin-top: 0.6rem;
        border-top: 1px solid #f1f5f9;
        padding-top: 0.6rem;
        text-align: left;
        width: 100%;
        flex-grow: 1;
      }
      .profile-contact-btn {
        margin-top: 0.8rem;
        width: 100%;
        text-align: center;
        background: #0A3B5C;
        color: #ffffff !important;
        font-weight: 600;
        font-size: 0.78rem;
        padding: 0.4rem;
        border-radius: 6px;
        display: block;
        text-decoration: none;
        transition: background 0.2s ease;
      }
      .profile-contact-btn:hover {
        background: #0d9488;
      }
    "))
  ),

  # ---------------------------------------------------------
  # TAB: Home
  # ---------------------------------------------------------
  nav_panel(
    title = "Home",
    icon = icon("home"),
    fluidPage(
      div(
        class = "hero-banner",
        h1("IUCN Bahari Yetu Scholarly Web App"),
        p("Accelerating research outcomes by transitioning from manual spreadsheets to automated, reproducible environmental and marine analytics pipelines in R.")
      ),
      layout_sidebar(
        sidebar = sidebar(
          title = "Applied Statistics",
          # 1. Quick Info Block
          div(
            style = "font-size:0.8rem; line-height: 1.5; color:#334155;",
            div(style = "font-weight: 700; font-size: 0.85rem; color: #0A3B5C; margin-bottom: 0.4rem;", "Training At-A-Glance"),
            div(
              style = "margin-bottom: 0.5rem;",
              span(icon("map-marker-alt", class = "me-2", style = "color: #0d9488;")),
              strong("Venue:"), " EDEMA Hall, Morogoro"
            ),
            div(
              style = "margin-bottom: 0.5rem;",
              span(icon("calendar-alt", class = "me-2", style = "color: #0d9488;")),
              strong("Dates:"), " August 3 - 8, 2026"
            ),
            div(
              style = "margin-bottom: 0.5rem;",
              span(icon("chalkboard-user", class = "me-2", style = "color: #0d9488;")),
              strong("Instructor:"), " Masumbuko Semba"
            ),
            div(
              style = "margin-bottom: 0.5rem;",
              span(icon("users", class = "me-2", style = "color: #0d9488;")),
              strong("Audience:"), " 28 Postgraduate Scholars"
            )
          ),
          hr(style = "margin: 0.6rem 0;"),

          # 2. Key Objectives
          div(
            style = "font-size:0.78rem; line-height: 1.5; color:#475569;",
            div(style = "font-weight: 700; font-size: 0.8rem; color: #0A3B5C; margin-bottom: 0.4rem;", "Key Objectives"),
            tags$ul(
              style = "list-style-type: none; padding-left: 0; margin-bottom: 0;",
              tags$li(
                style = "margin-bottom: 0.4rem; display: flex; align-items: start; gap: 8px;",
                span(icon("arrows-rotate"), style = "color: #0d9488; font-size: 0.85rem; margin-top: 2px;"),
                div("Transition spreadsheets to reproducible R pipelines.")
              ),
              tags$li(
                style = "margin-bottom: 0.4rem; display: flex; align-items: start; gap: 8px;",
                span(icon("gears"), style = "color: #0d9488; font-size: 0.85rem; margin-top: 2px;"),
                div("Automate biostatistics & spatial data wrangling workflows.")
              ),
              tags$li(
                style = "margin-bottom: 0.4rem; display: flex; align-items: start; gap: 8px;",
                span(icon("file-export"), style = "color: #0d9488; font-size: 0.85rem; margin-top: 2px;"),
                div("Generate dynamic reports using Quarto compilation engines.")
              )
            )
          ),
          hr(style = "margin: 0.6rem 0;"),

          # 3. Weekly Syllabus Quick View
          div(
            style = "font-size:0.78rem; line-height: 1.5; color:#475569;",
            div(style = "font-weight: 700; font-size: 0.8rem; color: #0A3B5C; margin-bottom: 0.4rem;", "Syllabus Highlights"),
            tags$ul(
              style = "list-style-type: none; padding-left: 0; margin-bottom: 0;",
              tags$li(
                style = "margin-bottom: 0.4rem; display: flex; align-items: start; gap: 8px;",
                span(icon("laptop-code"), style = "color: #0d9488; font-size: 0.85rem; margin-top: 2px;"),
                div(strong("Day 1:"), " R Environment Setup & Basics")
              ),
              tags$li(
                style = "margin-bottom: 0.4rem; display: flex; align-items: start; gap: 8px;",
                span(icon("database"), style = "color: #0d9488; font-size: 0.85rem; margin-top: 2px;"),
                div(strong("Day 2:"), " Data Import & Wrangling (tidyverse)")
              ),
              tags$li(
                style = "margin-bottom: 0.4rem; display: flex; align-items: start; gap: 8px;",
                span(icon("chart-line"), style = "color: #0d9488; font-size: 0.85rem; margin-top: 2px;"),
                div(strong("Day 3:"), " High-Quality Visuals (ggplot2)")
              ),
              tags$li(
                style = "margin-bottom: 0.4rem; display: flex; align-items: start; gap: 8px;",
                span(icon("map"), style = "color: #0d9488; font-size: 0.85rem; margin-top: 2px;"),
                div(strong("Day 4:"), " Electives: Spatial GIS vs. Stats")
              ),
              tags$li(
                style = "margin-bottom: 0.4rem; display: flex; align-items: start; gap: 8px;",
                span(icon("file-lines"), style = "color: #0d9488; font-size: 0.85rem; margin-top: 2px;"),
                div(strong("Day 5:"), " Quarto & Reproducible Reports")
              )
            )
          ),
          # PWA Offline Install Button (Only visible if the browser triggers beforeinstallprompt)
          tags$div(
            id = "pwa-install-container",
            style = "display: none; margin-bottom: 0.8rem;",
            tags$button(
              id = "pwa-install-btn",
              class = "btn btn-success w-100",
              style = "font-size:0.78rem; font-weight:700; background-color:#2ECC71; border-color:#2ECC71; padding:0.5rem; border-radius:6px; box-shadow: 0 4px 6px rgba(46, 204, 113, 0.15); display:flex; align-items:center; justify-content:center; gap:8px;",
              icon("download"), "Install App for Offline Use"
            )
          ),
          tags$div(
            id = "pwa-secure-warning",
            style = "display: none; margin-bottom: 0.8rem; font-size: 0.72rem; color: #b91c1c; background-color: #fee2e2; border: 1px solid #fca5a5; padding: 0.5rem; border-radius: 6px;",
            icon("circle-exclamation"), " Offline installation is only available when accessing via localhost or HTTPS."
          ),

          # 4. Resources & Support
          div(
            style = "font-size:0.78rem; line-height: 1.4; color:#475569;",
            div(style = "font-weight: 700; font-size: 0.8rem; color: #0A3B5C; margin-bottom: 0.4rem;", "Logistics Resources"),
            downloadButton(
              "download_tna_report",
              "Download TNA Report",
              icon = icon("file-pdf"),
              class = "resource-link-btn",
              style = "text-align:center; font-size:0.75rem; padding:0.4rem; display:block; margin-bottom: 0.5rem; width:100%;"
            ),
            div(
              style = "font-size: 0.72rem; color: #64748b; margin-top: 0.4rem; line-height: 1.3;",
              icon("info-circle"), " Technical issues? Contact Masumbuko Semba: ",
              tags$a(href = "mailto:lugosemba@gmail.com", "lugosemba@gmail.com")
            ),
            div(
              style = "font-size: 0.72rem; color: #64748b; margin-top: 0.4rem; line-height: 1.3;",
              icon("envelope"), " Logistics questions? Contact Herry Lugala: ",
              tags$a(href = "mailto:herry.lugala@iucn.org", "herry.lugala@iucn.org")
            )
          ),
          hr(style = "margin: 0.6rem 0;"),

          # 5. Interface Customizer
          h5("Interface Customizer", style = "font-weight:700; color:#0A3B5C; font-size:0.8rem; margin-top:0; margin-bottom: 0.2rem;"),
          p("Select a layout theme:", style = "font-size:0.72rem; color:#64748b; margin-top:0.1rem; margin-bottom: 0.4rem;"),
          selectInput("app_theme_select", "Interface Theme Style:",
            choices = c(
              "Ocean Blue (Default)" = "default",
              "Emerald Canopy" = "emerald",
              "Midnight Slate (Dark Mode)" = "midnight",
              "Sunset Gold" = "sunset"
            ), selected = "default"
          )
        ),

        # Main Panel Content
        # 1. Partner and Funding Acknowledgement (Logo Banner)
        tags$div(
          style = "display: flex; align-items: center; justify-content: space-between; gap: 20px; padding: 1.2rem; background: #ffffff; border-radius: 10px; border: 1px solid #e2e8f0; margin-bottom: 1.5rem; box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.05); flex-wrap: wrap;",
          # EU Logo block
          tags$div(
            style = "display: flex; align-items: center; gap: 12px;",
            HTML('
              <svg viewBox="0 0 810 540" width="75" height="50" style="border-radius: 4px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);">
                <rect width="810" height="540" fill="#003399"/>
                <g fill="#FFCC00" transform="translate(405,270)">
                  <g id="star"><polygon points="0,-30 -9,-9 -30,-9 -13,4 -20,25 0,12 20,25 13,4 30,-9 9,-9"/></g>
                  <use href="#star" x="0" y="-180"/>
                  <use href="#star" x="90" y="-156"/>
                  <use href="#star" x="156" y="-90"/>
                  <use href="#star" x="180" y="0"/>
                  <use href="#star" x="156" y="90"/>
                  <use href="#star" x="90" y="156"/>
                  <use href="#star" x="0" y="180"/>
                  <use href="#star" x="-90" y="156"/>
                  <use href="#star" x="-156" y="90"/>
                  <use href="#star" x="-180" y="0"/>
                  <use href="#star" x="-156" y="-90"/>
                  <use href="#star" x="-90" y="-156"/>
                </g>
              </svg>
            '),
            tags$div(
              style = "display: flex; flex-direction: column; font-family: \'Outfit\', sans-serif; line-height: 1.2;",
              tags$span("Funded by", style = "font-size: 0.65rem; font-weight: 600; color: #64748b; text-transform: uppercase; letter-spacing: 0.7px;"),
              tags$span("the European Union", style = "font-size: 0.95rem; font-weight: 800; color: #003399;")
            )
          ),
          # Implemented by IUCN block
          tags$div(
            style = "display: flex; align-items: center; gap: 12px;",
            tags$img(src = "IUCN_logo.svg", height = "45px", style = "border-radius: 4px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);"),
            tags$div(
              style = "display: flex; flex-direction: column; font-family: \'Outfit\', sans-serif; line-height: 1.2;",
              tags$span("Implemented by", style = "font-size: 0.65rem; font-weight: 600; color: #64748b; text-transform: uppercase; letter-spacing: 0.7px;"),
              tags$span("IUCN", style = "font-size: 1.1rem; font-weight: 800; color: #0A3B5C;")
            )
          )
        ),

        # 2. Applied Statistics Program Details (from the PDF engagement letter)
        navset_card_tab(
          # Subtab 1: Pamoja Tuhifadhi Bahari Yetu Training Initiative
          nav_panel(
            title = "Training Initiative",
            icon = icon("lightbulb"),
            div(
              style = "padding: 0.5rem;",
              # 2. Applied Statistics Program Details
              div(
                class = "custom-card", style = "margin-top:0; margin-bottom: 1.5rem;",
                h4(class = "section-title", "Pamoja Tuhifadhi Bahari Yetu Training Initiative"),
                p("The practical training on ", strong("Applied Statistics for Research and Scientific Data Analysis"), " is organized under the ", strong("Pamoja Tuhifadhi Bahari Yetu Project"), ", a European Union-funded initiative implemented by the International Union for Conservation of Nature (IUCN)."),
                p("Under ", strong("Work Package Four (Institutional Capacity Strengthening)"), ", the project is supporting twenty-eight (28) MSc and PhD scholarship beneficiaries from Sokoine University of Agriculture (SUA), the University of Dar es Salaam (UDSM), the Nelson Mandela African Institution of Science and Technology (NM-AIST), and the State University of Zanzibar (SUZA)."),
                h5("Scholarly Thematic Research Areas:", style = "color:#0A3B5C; font-weight:700; margin-top:1.2rem; margin-bottom:0.6rem;"),
                layout_column_wrap(
                  width = 1 / 2,
                  tags$ul(
                    style = "list-style-type: none; padding-left: 0; margin-bottom: 0; font-size: 0.88rem; line-height: 1.6; color: #475569;",
                    tags$li(
                      style = "margin-bottom: 0.5rem; display: flex; align-items: center; gap: 10px;",
                      span(icon("smog"), style = "color: #0d9488; font-size: 0.95rem; width: 20px; text-align: center;"),
                      strong("Greenhouse Gas (GHG) Inventories")
                    ),
                    tags$li(
                      style = "margin-bottom: 0.5rem; display: flex; align-items: center; gap: 10px;",
                      span(icon("fish"), style = "color: #0d9488; font-size: 0.95rem; width: 20px; text-align: center;"),
                      strong("Marine Ecology")
                    ),
                    tags$li(
                      style = "margin-bottom: 0.5rem; display: flex; align-items: center; gap: 10px;",
                      span(icon("tree"), style = "color: #0d9488; font-size: 0.95rem; width: 20px; text-align: center;"),
                      strong("Coastal Forests")
                    ),
                    tags$li(
                      style = "margin-bottom: 0.5rem; display: flex; align-items: center; gap: 10px;",
                      span(icon("recycle"), style = "color: #0d9488; font-size: 0.95rem; width: 20px; text-align: center;"),
                      strong("Marine Plastics & Waste Mitigation")
                    )
                  ),
                  tags$ul(
                    style = "list-style-type: none; padding-left: 0; margin-bottom: 0; font-size: 0.88rem; line-height: 1.6; color: #475569;",
                    tags$li(
                      style = "margin-bottom: 0.5rem; display: flex; align-items: center; gap: 10px;",
                      span(icon("leaf"), style = "color: #0d9488; font-size: 0.95rem; width: 20px; text-align: center;"),
                      strong("Biodiversity Conservation")
                    ),
                    tags$li(
                      style = "margin-bottom: 0.5rem; display: flex; align-items: center; gap: 10px;",
                      span(icon("chart-pie"), style = "color: #0d9488; font-size: 0.95rem; width: 20px; text-align: center;"),
                      strong("Fisheries Stock Assessment")
                    ),
                    tags$li(
                      style = "margin-bottom: 0.5rem; display: flex; align-items: center; gap: 10px;",
                      span(icon("earth-africa"), style = "color: #0d9488; font-size: 0.95rem; width: 20px; text-align: center;"),
                      strong("Climate Change Adaptation")
                    ),
                    tags$li(
                      style = "margin-bottom: 0.5rem; display: flex; align-items: center; gap: 10px;",
                      span(icon("chart-line"), style = "color: #0d9488; font-size: 0.95rem; width: 20px; text-align: center;"),
                      strong("Blue Economy Development")
                    )
                  )
                ),
                tags$div(
                  style = "margin-top: 1.2rem; padding: 1rem; background: #f8fafc; border-radius: 8px; border: 1px solid #e2e8f0; font-size: 0.88rem; color: #475569; line-height: 1.6;",
                  p(style = "margin-bottom: 0.5rem;", icon("info-circle"), " The proposed training is intended to strengthen scholars' statistical analysis competencies, enabling transition from manual spreadsheet pipelines to reproducible R workflows, and improving the quality of postgraduate research and scientific publications."),
                  p(style = "margin:0;", tags$strong("Expected Outcomes & Outputs:"), " This training initiative focuses on equipping MSc and PhD scholars with key competencies to effectively manage, store, analyse, plot, and report environmental and marine research data, fostering complete reproducibility in their postgraduate theses and scientific publications.")
                )
              ),
              layout_column_wrap(
                width = 1 / 4,
                div(
                  class = "tna-card", style = "padding: 1rem; border-radius: 8px;",
                  div(class = "tna-stat", style = "font-size:1.6rem;", "44.5%"),
                  div(class = "tna-label", style = "font-size:0.8rem; font-weight:700; margin-top:0.2rem;", "Excel Cleaning Time"),
                  div(class = "tna-desc", style = "font-size:0.75rem; line-height:1.4; color:#64748b;", "Research time spent on manual cleaning, copy-pasting, and sorting. Goal: reduce by 80% with code.")
                ),
                div(
                  class = "tna-card", style = "padding: 1rem; border-radius: 8px;",
                  div(class = "tna-stat", style = "font-size:1.6rem;", "57.1%"),
                  div(class = "tna-label", style = "font-size:0.8rem; font-weight:700; margin-top:0.2rem;", "R Beginners"),
                  div(class = "tna-desc", style = "font-size:0.75rem; line-height:1.4; color:#64748b;", "Scholars entering training with no prior coding experience. Course starts from absolute foundations.")
                ),
                div(
                  class = "tna-card", style = "padding: 1rem; border-radius: 8px;",
                  div(class = "tna-stat", style = "font-size:1.6rem;", "28"),
                  div(class = "tna-label", style = "font-size:0.8rem; font-weight:700; margin-top:0.2rem;", "Cohort Scholars"),
                  div(class = "tna-desc", style = "font-size:0.75rem; line-height:1.4; color:#64748b;", "Selected MSc & PhD candidates from SUA, UDSM, NM-AIST, and SUZA under IUCN Bahari Yetu.")
                ),
                div(
                  class = "tna-card", style = "padding: 1rem; border-radius: 8px;",
                  div(class = "tna-stat", style = "font-size:1.6rem;", "5 Days"),
                  div(class = "tna-label", style = "font-size:0.8rem; font-weight:700; margin-top:0.2rem;", "Intensive Course"),
                  div(class = "tna-desc", style = "font-size:0.75rem; line-height:1.4; color:#64748b;", "Curriculum structure moving from setup (Day 1) to aggregation, graphics, GIS, and reporting.")
                )
              ),
              div(
                class = "custom-card",
                h4(class = "section-title", "Training Needs Assessment Survey Results"),
                layout_columns(
                  col_widths = c(4, 8),
                  div(
                    style = "padding-right: 15px; border-right: 1px solid #e2e8f0; display: flex; flex-direction: column; justify-content: center; height: 100%;",
                    h5("TNA Analytics Selector", style = "font-weight:700; color:#0A3B5C; font-size:0.95rem; margin-top:0;"),
                    p("Choose a survey metric to visualize cohort bottlenecks:", style = "font-size:0.78rem; color:#64748b; margin-top:0.2rem; margin-bottom:1rem;"),
                    selectInput("tna_chart_select", "Select Survey Data:",
                      choices = c(
                        "Time Bottlenecks (Tasks)" = "time",
                        "Skill Gap Distribution" = "skills",
                        "Prior R Experience" = "exp"
                      ), selected = "time"
                    )
                  ),
                  plotOutput("tna_plot", height = "320px")
                )
              ),
              br(),
              div(
                class = "custom-card",
                h4(class = "section-title", "Why Code? The Excel Spreadsheet Trap vs. R Loop"),
                p("A visual comparison of manual spreadsheet manipulation (copy-paste, point-and-click) versus reproducible R programming workflows."),
                div(
                  class = "flowchart-container", style = "margin: 1rem 0; gap: 1rem;",
                  div(
                    class = "flowchart-column excel", style = "padding: 1.2rem;",
                    div(
                      class = "flowchart-header", style = "font-size: 1rem; margin-bottom: 0.8rem;",
                      icon("times-circle", style = "color:#E74C3C;"), "The Manual Excel Trap"
                    ),
                    div(
                      class = "flowchart-step", style = "margin-bottom:0.4rem;",
                      div(class = "flowchart-step-icon", style = "width:20px; height:20px; font-size:0.8rem;", "1"),
                      div(span(class = "flowchart-step-text", style = "font-size:0.85rem;", "Copy-Paste Importing"))
                    ),
                    div(class = "flowchart-arrow", style = "margin: 0.1rem 0;", icon("arrow-down", style = "font-size:0.8rem;")),
                    div(
                      class = "flowchart-step", style = "margin-bottom:0.4rem;",
                      div(class = "flowchart-step-icon", style = "width:20px; height:20px; font-size:0.8rem;", "2"),
                      div(span(class = "flowchart-step-text", style = "font-size:0.85rem;", "Point-and-Click Wrangling"))
                    ),
                    div(class = "flowchart-arrow", style = "margin: 0.1rem 0;", icon("arrow-down", style = "font-size:0.8rem;")),
                    div(
                      class = "flowchart-step", style = "margin-bottom:0.4rem;",
                      div(class = "flowchart-step-icon", style = "width:20px; height:20px; font-size:0.8rem;", "3"),
                      div(span(class = "flowchart-step-text", style = "font-size:0.85rem;", "Manual Plot Design"))
                    ),
                    div(class = "flowchart-arrow", style = "margin: 0.1rem 0;", icon("arrow-down", style = "font-size:0.8rem;")),
                    div(
                      class = "flowchart-step", style = "margin-bottom:0.4rem;",
                      div(class = "flowchart-step-icon", style = "width:20px; height:20px; font-size:0.8rem;", "4"),
                      div(span(class = "flowchart-step-text", style = "font-size:0.85rem;", "Ad-hoc Report Assembly"))
                    ),
                    div(class = "flowchart-footer", style = "color:#E74C3C; font-size:0.8rem; margin-top:0.8rem;", "Sinks 44.5% of research time!")
                  ),
                  div(
                    class = "flowchart-column coding", style = "padding: 1.2rem;",
                    div(
                      class = "flowchart-header", style = "font-size: 1rem; margin-bottom: 0.8rem;",
                      icon("check-circle", style = "color:#2ECC71;"), "The Reproducible R Loop"
                    ),
                    div(
                      class = "flowchart-step", style = "margin-bottom:0.4rem;",
                      div(class = "flowchart-step-icon", style = "width:20px; height:20px; font-size:0.8rem;", "1"),
                      div(span(class = "flowchart-step-text", style = "font-size:0.85rem;", "read_csv() / here()"))
                    ),
                    div(class = "flowchart-arrow", style = "margin: 0.1rem 0;", icon("arrow-down", style = "font-size:0.8rem;")),
                    div(
                      class = "flowchart-step", style = "margin-bottom:0.4rem;",
                      div(class = "flowchart-step-icon", style = "width:20px; height:20px; font-size:0.8rem;", "2"),
                      div(span(class = "flowchart-step-text", style = "font-size:0.85rem;", "dplyr Wrangle Pipes"))
                    ),
                    div(class = "flowchart-arrow", style = "margin: 0.1rem 0;", icon("arrow-down", style = "font-size:0.8rem;")),
                    div(
                      class = "flowchart-step", style = "margin-bottom:0.4rem;",
                      div(class = "flowchart-step-icon", style = "width:20px; height:20px; font-size:0.8rem;", "3"),
                      div(span(class = "flowchart-step-text", style = "font-size:0.85rem;", "ggplot2 Layered Plots"))
                    ),
                    div(class = "flowchart-arrow", style = "margin: 0.1rem 0;", icon("arrow-down", style = "font-size:0.8rem;")),
                    div(
                      class = "flowchart-step", style = "margin-bottom:0.4rem;",
                      div(class = "flowchart-step-icon", style = "width:20px; height:20px; font-size:0.8rem;", "4"),
                      div(span(class = "flowchart-step-text", style = "font-size:0.85rem;", "Quarto (.qmd) Rendering"))
                    ),
                    div(class = "flowchart-footer", style = "color:#2ECC71; font-size:0.8rem; margin-top:0.8rem;", "Updates instantly on data changes!")
                  )
                )
              )
            )
          ),
          # Subtab 2: Venue & Location Map
          nav_panel(
            title = "Venue & Location Map",
            icon = icon("map-location-dot"),
            div(
              style = "padding: 0.5rem;",
              div(
                class = "custom-card", style = "margin-top:0;",
                p("The training is held at the ", strong("EDEMA Conference Hall"), " in Morogoro, Tanzania. Toggle map basemaps via the control panel. Enter your accommodation coordinates below to calculate paths and estimate travel times dynamically."),
                leafletOutput("venue_map", height = "400px"),
                br(),
                # Interactive Route Planner
                fluidRow(
                  column(
                    width = 6,
                    div(
                      style = "background: #f8fafc; border: 1px solid #e2e8f0; padding: 1.2rem; border-radius: 10px; height: 100%;",
                      h5("Accommodation Route Calculator", style = "font-weight:700; color:#0A3B5C; margin-top:0; margin-bottom:1rem;"),
                      fluidRow(
                        column(
                          width = 6,
                          numericInput("accom_lat", "Latitude:", value = -6.8164, min = -7.2, max = -6.5, step = 0.0001)
                        ),
                        column(
                          width = 6,
                          numericInput("accom_lon", "Longitude:", value = 37.6545, min = 37.4, max = 37.9, step = 0.0001)
                        )
                      ),
                      selectInput("travel_mode", "Mode of Transport:",
                        choices = c("Driving (Car)" = "driving", "Walking (Foot)" = "foot")
                      ),
                      actionButton("calculate_route", "Find Path to Edema", class = "btn-primary", style = "width:100%;", icon = icon("route")),
                      p("Real-time network estimation powered by OSRM.", style = "font-size:0.75rem; color:#94a3b8; margin-top:0.5rem; margin-bottom:0;")
                    )
                  ),
                  column(
                    width = 6,
                    uiOutput("route_summary")
                  )
                ),
                br(),
                h5("General Logistics Checklist:", style = "color:#0A3B5C; font-weight:600;"),
                layout_column_wrap(
                  width = 1 / 2,
                  tags$ul(
                    style = "padding-left:1.2rem; font-size: 0.9rem; line-height: 1.6;",
                    tags$li("Accommodation check-in opens at 14:00 Sunday."),
                    tags$li("Laptops must have R 4.6.1 + IDE installed."),
                    tags$li("Bring active raw field measurements for Friday's session.")
                  ),
                  tags$ul(
                    style = "padding-left:1.2rem; font-size: 0.9rem; line-height: 1.6;",
                    tags$li("Course books are preloaded on the USB drive."),
                    tags$li("Certificate requirements: 100% lab submission."),
                    tags$li("Facilitator consultation hours: 17:00 - 18:00 daily.")
                  )
                )
              )
            )
          )
        )
      )
    )
  ),

  # ---------------------------------------------------------
  # TAB: Day 1
  # ---------------------------------------------------------
  nav_panel(
    title = "Day 1",
    icon = icon("folder-open"),
    fluidPage(
      div(
        class = "day-header-banner",
        h3("Day 1: Foundations of R, Modern IDE Setup, & Tidy Data Principles"),
        p("Transitioning from manual spreadsheet manipulation to reproducible, programmatic workflows in R.")
      ),
      fluidRow(
        column(
          width = 7,
          div(
            class = "custom-card",
            h4(class = "section-title", "Daily Schedule"),
            div(
              class = "schedule-item",
              div(class = "schedule-time", "09:00 - 10:30"),
              div(class = "schedule-title", "Session 1: IDE Setup & Projects"),
              div(class = "schedule-desc", "Positron/VS Code interface, RStudio Projects, Directory structure (/data, /scripts, /outputs), the here package.")
            ),
            div(
              class = "schedule-item",
              div(class = "schedule-time", "11:00 - 12:30"),
              div(class = "schedule-title", "Session 2: R Syntax Foundations"),
              div(class = "schedule-desc", "Objects, vectors, assignment operator (<-), functions, data frames.")
            ),
            div(
              class = "schedule-item",
              div(class = "schedule-time", "14:00 - 15:30"),
              div(class = "schedule-title", "Session 3: Tidy Data & Import"),
              div(class = "schedule-desc", "Tidy principles, importing using readr::read_csv() and readxl::read_excel().")
            ),
            div(
              class = "schedule-item",
              div(class = "schedule-time", "15:30 - 17:00"),
              div(class = "schedule-title", "Hands-On Lab 1 (BYOD)"),
              div(class = "schedule-desc", "Set up project directories and import active thesis data.")
            )
          )
        ),
        column(
          width = 5,
          div(
            class = "custom-card",
            h4(class = "section-title", "Learning Objectives"),
            div(
              class = "objective-item",
              icon("check-circle", class = "objective-icon"),
              div(class = "objective-text", "Navigate the RStudio/Positron IDE interface.")
            ),
            div(
              class = "objective-item",
              icon("check-circle", class = "objective-icon"),
              div(class = "objective-text", "Establish a self-contained project directory with clean folder structures.")
            ),
            div(
              class = "objective-item",
              icon("check-circle", class = "objective-icon"),
              div(class = "objective-text", "Understand R syntax basics (objects, functions, vectors, and data frames).")
            ),
            div(
              class = "objective-item",
              icon("check-circle", class = "objective-icon"),
              div(class = "objective-text", "Apply the three core rules of Tidy Data.")
            ),
            div(
              class = "objective-item",
              icon("check-circle", class = "objective-icon"),
              div(class = "objective-text", "Import raw tabular datasets (.csv and .xlsx) into R using relative paths.")
            )
          ),
          div(
            class = "custom-card",
            h4(class = "section-title", "Packages & Resources"),
            h6("Required Setup Commands:", style = "font-weight:600; color:#475569; margin-top:0;"),
            div(
              class = "code-container", style = "margin-top:0.3rem; margin-bottom:1rem; padding:0.8rem;",
              pre("install.packages(c(\"tidyverse\", \"here\", \"readxl\"))")
            ),
            h6("Recommended Readings:", style = "font-weight:600; color:#475569; margin-top:0.5rem;"),
            tags$a(
              href = "https://r4ds.hadley.nz/workflow-scripts", target = "_blank", class = "resource-link-btn",
              icon("book-open"), "Workflow: Projects (R4DS)"
            ),
            tags$a(
              href = "https://www.jstatsoft.org/article/view/v059i10", target = "_blank", class = "resource-link-btn",
              icon("file-alt"), "Tidy Data Paper (Wickham)"
            ),
            hr(),
            h6("Course Slides & Worksheets:", style = "font-weight:600; color:#475569; margin-top:0.5rem;"),
            div(
              style = "display:flex; flex-direction:column; gap:0.4rem; margin-top:0.3rem;",
              div(style = "font-size:0.8rem; font-weight:700; color:#0A3B5C;", "RevealJS Slide Decks:"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Session 1: Setup & Projects (session1.qmd)"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Session 2: Syntax Foundations (session2.qmd)"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Session 3: Tidy Data & Import (session3.qmd)"),
              div(style = "font-size:0.8rem; font-weight:700; color:#0A3B5C; margin-top:0.3rem;", "Hands-On Exercises:"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Exercise 1: Directory Setup (exercise1.qmd)"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Exercise 2: R Syntax (exercise2.qmd)"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Exercise 3: Importing CSVs (exercise3.qmd)")
            )
          )
        )
      ),
      div(
        class = "custom-card",
        h4(class = "section-title", "Interactive Lab: Tidy Data Reshaping (pivot_longer)"),
        p("Tidy data requires: (1) columns as variables, (2) rows as observations, and (3) cells as individual values. Excel sheets are often created in 'wide' layout (messy for plotting). Customize the values below to pivot the wide dataset into a clean, analysis-ready format."),
        layout_sidebar(
          sidebar = sidebar(
            title = "Pivoting Parameters",
            selectizeInput("d1_pivot_cols", "Columns to Pivot:",
              choices = c("CoralCover_2024", "CoralCover_2025", "CoralCover_2026"),
              selected = c("CoralCover_2024", "CoralCover_2025", "CoralCover_2026"),
              multiple = TRUE
            ),
            textInput("d1_names_to", "names_to (Year column):", "Year"),
            textInput("d1_values_to", "values_to (Measurement column):", "PercentCover"),
            textInput("d1_names_prefix", "names_prefix (strip text):", "CoralCover_"),
            checkboxInput("d1_drop_na", "Drop Missing Values (drop_na)", FALSE)
          ),
          fluidRow(
            column(
              width = 6,
              h5("Messy / Wide Format (Excel style)", style = "color:#E74C3C; font-weight:600;"),
              tableOutput("wide_table_view")
            ),
            column(
              width = 6,
              h5("Clean / Tidy Format (Long style)", style = "color:#2ECC71; font-weight:600;"),
              tableOutput("tidy_table_view")
            )
          ),
          hr(),
          h5("Generated pivot_longer R Code:", style = "font-weight:600; color:#0A3B5C;"),
          uiOutput("d1_code_output")
        )
      )
    )
  ),

  # ---------------------------------------------------------
  # TAB: Day 2
  # ---------------------------------------------------------
  nav_panel(
    title = "Day 2",
    icon = icon("filter"),
    fluidPage(
      div(
        class = "day-header-banner",
        h3("Day 2: Data Wrangling, Joining, and Reshaping"),
        p("Transforming raw field data into clean, analysis-ready formats using tidyverse pipelines.")
      ),
      fluidRow(
        column(
          width = 7,
          div(
            class = "custom-card",
            h4(class = "section-title", "Daily Schedule"),
            div(
              class = "schedule-item",
              div(class = "schedule-time", "09:00 - 10:30"),
              div(class = "schedule-title", "Session 1: Single-Table Wrangling"),
              div(class = "schedule-desc", "Pipes, filter() for rows, select() for columns, and mutate() for calculating new variables.")
            ),
            div(
              class = "schedule-item",
              div(class = "schedule-time", "11:00 - 12:30"),
              div(class = "schedule-title", "Session 2: Group Summaries & Aggregation"),
              div(class = "schedule-desc", "Grouping datasets using group_by(), summarizing with functions like mean(), sd(), n().")
            ),
            div(
              class = "schedule-item",
              div(class = "schedule-time", "14:00 - 15:30"),
              div(class = "schedule-title", "Session 3: Relational Joins & Reshaping"),
              div(class = "schedule-desc", "Joining tables, appending rows, pivoting data long and wide.")
            ),
            div(
              class = "schedule-item",
              div(class = "schedule-time", "15:30 - 17:00"),
              div(class = "schedule-title", "Hands-On Lab 2 (BYOD)"),
              div(class = "schedule-desc", "Clean, join, and summarize active research datasets.")
            )
          )
        ),
        column(
          width = 5,
          div(
            class = "custom-card",
            h4(class = "section-title", "Learning Objectives"),
            div(
              class = "objective-item",
              icon("check-circle", class = "objective-icon"),
              div(class = "objective-text", "Chain R commands efficiently using the pipe operator ( |> or %>% ).")
            ),
            div(
              class = "objective-item",
              icon("check-circle", class = "objective-icon"),
              div(class = "objective-text", "Subset and transform datasets using dplyr core verbs (filter, select, mutate).")
            ),
            div(
              class = "objective-item",
              icon("check-circle", class = "objective-icon"),
              div(class = "objective-text", "Summarize complex datasets grouped by key factors using group_by and summarize.")
            ),
            div(
              class = "objective-item",
              icon("check-circle", class = "objective-icon"),
              div(class = "objective-text", "Merge multiple data sheets using joins (left_join, bind_rows).")
            ),
            div(
              class = "objective-item",
              icon("check-circle", class = "objective-icon"),
              div(class = "objective-text", "Reshape datasets between wide and long layouts using pivot_longer and pivot_wider.")
            )
          ),
          div(
            class = "custom-card",
            h4(class = "section-title", "Packages & Resources"),
            h6("Required Setup Commands:", style = "font-weight:600; color:#475569; margin-top:0;"),
            div(
              class = "code-container", style = "margin-top:0.3rem; margin-bottom:1rem; padding:0.8rem;",
              pre("library(tidyverse)")
            ),
            h6("Recommended Readings:", style = "font-weight:600; color:#475569; margin-top:0.5rem;"),
            tags$a(
              href = "https://r4ds.hadley.nz/data-transform", target = "_blank", class = "resource-link-btn",
              icon("book-open"), "Data Transformation (R4DS)"
            ),
            tags$a(
              href = "https://r4ds.hadley.nz/data-import", target = "_blank", class = "resource-link-btn",
              icon("book-open"), "Data Import (R4DS)"
            ),
            hr(),
            h6("Course Slides & Worksheets:", style = "font-weight:600; color:#475569; margin-top:0.5rem;"),
            div(
              style = "display:flex; flex-direction:column; gap:0.4rem; margin-top:0.3rem;",
              div(style = "font-size:0.8rem; font-weight:700; color:#0A3B5C;", "RevealJS Slide Decks:"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Session 1: Single-Table Wrangling (session1.qmd)"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Session 2: Summaries & Aggregation (session2.qmd)"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Session 3: Relational Joins & Reshaping (session3.qmd)"),
              div(style = "font-size:0.8rem; font-weight:700; color:#0A3B5C; margin-top:0.3rem;", "Hands-On Exercises:"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Exercise 1: Single-Table Wrangling (exercise1.qmd)"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Exercise 2: Group Summaries (exercise2.qmd)"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Exercise 3: Joins & Pivoting (exercise3.qmd)")
            )
          )
        )
      ),
      layout_sidebar(
        sidebar = sidebar(
          title = "Wrangling Controller",
          selectInput("d2_dataset", "Select Research Theme Dataset:",
            choices = c(
              "Coastal Forest" = "1",
              "GHG Cement (IPPU)" = "2",
              "GHG Energy" = "3",
              "Marine Ecology" = "4",
              "GHG AFOLU" = "5",
              "Plastic Waste" = "6",
              "GHG Waste" = "7"
            ), selected = "1"
          ),
          selectizeInput("d2_select_cols", "Select Columns to Retain:",
            choices = NULL, multiple = TRUE
          ),
          uiOutput("d2_filter_ui"),
          selectInput("d2_arrange_var", "Arrange By (Sort Column):", choices = c("None" = "")),
          checkboxInput("d2_do_summary", "Enable Group Summary", FALSE),
          conditionalPanel(
            condition = "input.d2_do_summary == true",
            selectInput("d2_group_var", "Group By:", choices = NULL),
            selectInput("d2_summary_var", "Summary Target Variable:", choices = NULL),
            selectInput("d2_summary_fun", "Summary Statistic:",
              choices = c("Mean" = "mean", "Maximum" = "max", "Minimum" = "min", "Count" = "n")
            )
          ),
          hr(),
          uiOutput("d2_row_metric")
        ),
        div(
          class = "custom-card",
          h4(class = "section-title", "Interactive Data Wrangling Playground"),
          p("Adjust the wrangling parameters in the sidebar. The code block and data table below update in real-time."),
          h5("Generated R Code:"),
          uiOutput("d2_code_output"),
          br(),
          h5("Wrangled Output Table:"),
          DTOutput("d2_table_output")
        )
      )
    )
  ),

  # ---------------------------------------------------------
  # TAB: Day 3
  # ---------------------------------------------------------
  nav_panel(
    title = "Day 3",
    icon = icon("chart-line"),
    fluidPage(
      div(
        class = "day-header-banner",
        h3("Day 3: Publication-Ready Graphics with ggplot2"),
        p("Mastering the Grammar of Graphics to generate high-resolution figures for journal submission.")
      ),
      fluidRow(
        column(
          width = 7,
          div(
            class = "custom-card",
            h4(class = "section-title", "Daily Schedule"),
            div(
              class = "schedule-item",
              div(class = "schedule-time", "09:00 - 10:30"),
              div(class = "schedule-title", "Session 1: Grammar of Graphics & Geoms"),
              div(class = "schedule-desc", "Data binding, aesthetic mappings (aes), geoms (geom_point, geom_boxplot, geom_line).")
            ),
            div(
              class = "schedule-item",
              div(class = "schedule-time", "11:00 - 12:30"),
              div(class = "schedule-title", "Session 2: Styling & Themes"),
              div(class = "schedule-desc", "Theme modifications (theme_classic, theme_minimal), scale adjustments, legends, and faceting.")
            ),
            div(
              class = "schedule-item",
              div(class = "schedule-time", "14:00 - 15:30"),
              div(class = "schedule-title", "Session 3: Exporting to Journals"),
              div(class = "schedule-desc", "Resolution control, vector PDF/TIFF export, pixel sizes, and ggsave().")
            ),
            div(
              class = "schedule-item",
              div(class = "schedule-time", "15:30 - 17:00"),
              div(class = "schedule-title", "Hands-On Lab 3 (BYOD)"),
              div(class = "schedule-desc", "Create and refine a publication-quality figure using active thesis datasets.")
            )
          )
        ),
        column(
          width = 5,
          div(
            class = "custom-card",
            h4(class = "section-title", "Learning Objectives"),
            div(
              class = "objective-item",
              icon("check-circle", class = "objective-icon"),
              div(class = "objective-text", "Understand the components of the Grammar of Graphics (Data, Aesthetics, Geoms).")
            ),
            div(
              class = "objective-item",
              icon("check-circle", class = "objective-icon"),
              div(class = "objective-text", "Choose and construct appropriate plots (scatters, boxes, histograms, bar charts).")
            ),
            div(
              class = "objective-item",
              icon("check-circle", class = "objective-icon"),
              div(class = "objective-text", "Apply customized color scales (e.g., Viridis, ColorBrewer) and themes.")
            ),
            div(
              class = "objective-item",
              icon("check-circle", class = "objective-icon"),
              div(class = "objective-text", "Implement panel layouts using facet_wrap and facet_grid.")
            ),
            div(
              class = "objective-item",
              icon("check-circle", class = "objective-icon"),
              div(class = "objective-text", "Save plots with precise resolutions and dimensions using ggsave().")
            )
          ),
          div(
            class = "custom-card",
            h4(class = "section-title", "Packages & Resources"),
            h6("Required Setup Commands:", style = "font-weight:600; color:#475569; margin-top:0;"),
            div(
              class = "code-container", style = "margin-top:0.3rem; margin-bottom:1rem; padding:0.8rem;",
              pre("library(tidyverse)")
            ),
            h6("Recommended Readings:", style = "font-weight:600; color:#475569; margin-top:0.5rem;"),
            tags$a(
              href = "https://r4ds.hadley.nz/data-visualize", target = "_blank", class = "resource-link-btn",
              icon("book-open"), "Data Visualization (R4DS)"
            ),
            tags$a(
              href = "https://ggplot2.tidyverse.org/", target = "_blank", class = "resource-link-btn",
              icon("external-link-alt"), "ggplot2 Documentation"
            ),
            hr(),
            h6("Course Slides & Worksheets:", style = "font-weight:600; color:#475569; margin-top:0.5rem;"),
            div(
              style = "display:flex; flex-direction:column; gap:0.4rem; margin-top:0.3rem;",
              div(style = "font-size:0.8rem; font-weight:700; color:#0A3B5C;", "RevealJS Slide Decks:"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Session 1: Grammar of Graphics & Geoms (session1.qmd)"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Session 2: Styling & Themes (session2.qmd)"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Session 3: Exporting to Journals (session3.qmd)"),
              div(style = "font-size:0.8rem; font-weight:700; color:#0A3B5C; margin-top:0.3rem;", "Hands-On Exercises:"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Exercise 1: Scatter Plots (exercise1.qmd)"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Exercise 2: Boxplots & Themes (exercise2.qmd)"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Exercise 3: Facets & Exports (exercise3.qmd)")
            )
          )
        )
      ),
      layout_sidebar(
        sidebar = sidebar(
          title = "Graphics Design",
          selectInput("d3_dataset", "Select Dataset:",
            choices = c(
              "Coastal Forest" = "1",
              "GHG Energy" = "3",
              "Marine Ecology" = "4",
              "GHG AFOLU" = "5",
              "Plastic Waste" = "6",
              "GHG Waste" = "7"
            ), selected = "1"
          ),
          selectInput("d3_x", "X Variable (Aesthetic):", choices = NULL),
          selectInput("d3_y", "Y Variable (Aesthetic):", choices = NULL),
          selectInput("d3_color", "Color Variable (Optional):", choices = c("None" = "")),
          selectInput("d3_facet", "Facet Panel Variable (Optional):", choices = c("None" = "")),
          selectInput("d3_geom", "Plot Type (Geom):",
            choices = c("Scatter Plot" = "point", "Boxplot" = "boxplot", "Bar Chart" = "bar", "Line Plot" = "line")
          ),
          checkboxInput("d3_add_jitter", "Overlay Raw Data Points (Jitter)", FALSE),
          hr(),
          h5("Labelling & Typography:"),
          textInput("d3_title", "Plot Title:", "Publication Figure"),
          textInput("d3_subtitle", "Plot Subtitle:", "Generated via ggplot2"),
          textInput("d3_xlab", "X-Axis Label (Leave empty for default):", ""),
          textInput("d3_ylab", "Y-Axis Label (Leave empty for default):", ""),
          sliderInput("d3_base_size", "Theme Base Font Size:", min = 8, max = 22, value = 11, step = 1),
          hr(),
          selectInput("d3_theme", "Visual Theme:",
            choices = c("Classic" = "theme_classic", "Minimal" = "theme_minimal", "Black & White" = "theme_bw", "Light" = "theme_light")
          ),
          selectInput("d3_palette", "Color Palette:",
            choices = c("Default" = "default", "Viridis" = "viridis", "ColorBrewer (Set2)" = "set2", "ColorBrewer (Dark2)" = "dark2")
          ),
          hr(),
          h5("Export Parameters:"),
          sliderInput("d3_width", "Width (inches):", min = 4, max = 12, value = 7, step = 0.5),
          sliderInput("d3_height", "Height (inches):", min = 3, max = 10, value = 5, step = 0.5),
          sliderInput("d3_dpi", "DPI Resolution:", min = 72, max = 400, value = 150, step = 10),
          downloadButton("d3_download", "Download Publication Figure", class = "btn-success", style = "width:100%;")
        ),
        div(
          class = "custom-card",
          h4(class = "section-title", "Publication-Ready ggplot2 Builder"),
          p("Define aesthetics and geoms. The resulting R code block can be copied directly into your research scripts."),
          plotOutput("d3_plot", height = "450px"),
          br(),
          h5("Generated ggplot2 Code:"),
          uiOutput("d3_code_output")
        )
      )
    )
  ),

  # ---------------------------------------------------------
  # TAB: Day 4
  # ---------------------------------------------------------
  nav_panel(
    title = "Day 4",
    icon = icon("laptop-code"),
    fluidPage(
      navset_pill(
        # Sub-tab: Syllabus Overview
        nav_panel(
          title = "Track Overview & Schedule",
          fluidPage(
            br(),
            div(
              class = "day-header-banner",
              h3("Day 4: Domain-Specific Modules (Elective Tracks)"),
              p("Cohort splits into parallel tracks covering Spatial GIS mapping and Advanced Statistical modeling.")
            ),
            fluidRow(
              column(
                width = 7,
                div(
                  class = "custom-card",
                  h4(class = "section-title", "Daily Schedule"),
                  div(
                    class = "schedule-item",
                    div(class = "schedule-time", "09:00 - 10:30"),
                    div(class = "schedule-title", "Session 1: Foundations"),
                    div(class = "schedule-desc", "Track A: Vector GIS shapefiles using sf, projections, and cartography. / Track B: Hypothesis testing (t-tests, ANOVA) using testflow and rstatix.")
                  ),
                  div(
                    class = "schedule-item",
                    div(class = "schedule-time", "11:00 - 12:30"),
                    div(class = "schedule-title", "Session 2: Advanced Operations"),
                    div(class = "schedule-desc", "Track A: Raster calculations with terra, DEM extractions. / Track B: Multiple regression lm(), coefficients, diagnostics.")
                  ),
                  div(
                    class = "schedule-item",
                    div(class = "schedule-time", "14:00 - 15:30"),
                    div(class = "schedule-title", "Session 3: Elective Practice"),
                    div(class = "schedule-desc", "Hands-on lab 4-A (GIS maps and raster values) and lab 4-B (statistical modeling on environmental data).")
                  ),
                  div(
                    class = "schedule-item",
                    div(class = "schedule-time", "15:30 - 17:00"),
                    div(class = "schedule-title", "Joint Synthesis & Presentations"),
                    div(class = "schedule-desc", "Cohorts merge back to present maps, regression matrices, and model diagnostic parameters.")
                  )
                )
              ),
              column(
                width = 5,
                div(
                  class = "custom-card",
                  h4(class = "section-title", "Learning Objectives"),
                  div(
                    class = "objective-item",
                    icon("check-circle", class = "objective-icon"),
                    div(class = "objective-text", "Read vector shapefiles using sf and plot map layers.")
                  ),
                  div(
                    class = "objective-item",
                    icon("check-circle", class = "objective-icon"),
                    div(class = "objective-text", "Perform spatial coordinate transformations (CRS 4326 to CRS 32737).")
                  ),
                  div(
                    class = "objective-item",
                    icon("check-circle", class = "objective-icon"),
                    div(class = "objective-text", "Manage raster DEM models using terra and extract values.")
                  ),
                  div(
                    class = "objective-item",
                    icon("check-circle", class = "objective-icon"),
                    div(class = "objective-text", "Execute t-tests, ANOVA comparisons, and regression models.")
                  ),
                  div(
                    class = "objective-item",
                    icon("check-circle", class = "objective-icon"),
                    div(class = "objective-text", "Evaluate regression assumptions and model diagnostic plots.")
                  )
                ),
                div(
                  class = "custom-card",
                  h4(class = "section-title", "Required Setup Packages"),
                  h6("Track A (Spatial):", style = "font-weight:600; color:#475569; margin-top:0;"),
                  div(
                    class = "code-container", style = "margin-top:0.3rem; margin-bottom:0.8rem; padding:0.6rem; font-size:0.75rem;",
                    pre("install.packages(c(\"sf\", \"terra\", \"tidyterra\"))")
                  ),
                  h6("Track B (Stats):", style = "font-weight:600; color:#475569; margin-top:0.2rem;"),
                  div(
                    class = "code-container", style = "margin-top:0.3rem; margin-bottom:0.8rem; padding:0.6rem; font-size:0.75rem;",
                    pre("install.packages(c(\"testflow\", \"car\", \"flextable\", \"rstatix\"))")
                  ),
                  h6("Recommended Readings:", style = "font-weight:600; color:#475569; margin-top:0.4rem;"),
                  tags$a(
                    href = "https://r.geocompx.org/", target = "_blank", class = "resource-link-btn",
                    icon("globe"), "Geocomputation with R"
                  ),
                  tags$a(
                    href = "https://www.modernstatisticswithr.com/", target = "_blank", class = "resource-link-btn",
                    icon("calculator"), "Modern Stats with R"
                  ),
                  tags$a(
                    href = "https://rpkgs.datanovia.com/rstatix/", target = "_blank", class = "resource-link-btn",
                    icon("book"), "rstatix Package Reference Guide"
                  ),
                  hr(),
                  h6("Course Slides & Worksheets:", style = "font-weight:600; color:#475569; margin-top:0.5rem;"),
                  div(
                    style = "display:flex; flex-direction:column; gap:0.4rem; margin-top:0.3rem;",
                    div(style = "font-size:0.8rem; font-weight:700; color:#0A3B5C;", "RevealJS Slide Decks:"),
                    tags$span(style = "font-size:0.75rem; color:#475569;", "• Track A: Vector GIS & Projections (session1_track_a.qmd)"),
                    tags$span(style = "font-size:0.75rem; color:#475569;", "• Track A: Raster GIS & Calcs (session2_track_a.qmd)"),
                    tags$span(style = "font-size:0.75rem; color:#475569;", "• Track B: Hypothesis Testing (session1_track_b.qmd)"),
                    tags$span(style = "font-size:0.75rem; color:#475569;", "• Track B: Multiple Linear Regression (session2_track_b.qmd)"),
                    div(style = "font-size:0.8rem; font-weight:700; color:#0A3B5C; margin-top:0.3rem;", "Hands-On Exercises:"),
                    tags$span(style = "font-size:0.75rem; color:#475569;", "• Track A: Vector Geometries (exercise1_track_a.qmd)"),
                    tags$span(style = "font-size:0.75rem; color:#475569;", "• Track A: Raster Operations (exercise2_track_a.qmd)"),
                    tags$span(style = "font-size:0.75rem; color:#475569;", "• Track B: Comparison Tests (exercise1_track_b.qmd)"),
                    tags$span(style = "font-size:0.75rem; color:#475569;", "• Track B: Regression Models (exercise2_track_b.qmd)")
                  )
                )
              )
            )
          )
        ),

        # Sub-tab: Track A
        nav_panel(
          title = "Track A: Spatial Data & GIS",
          fluidPage(
            br(),
            div(
              class = "custom-card",
              h4(class = "section-title", "Track A: GIS Interactive Playground"),
              p("This playground implements vector GIS mapping. Adjust the slider to filter plastic cleanup stations and view coordinates mapped via Leaflet.")
            ),
            fluidRow(
              column(
                width = 8,
                div(
                  class = "custom-card",
                  h5("Interactive Plastic Waste Coastal Map", style = "color:#0A3B5C; font-weight:600;"),
                  leafletOutput("d4_spatial_map", height = "500px")
                )
              ),
              column(
                width = 4,
                div(
                  class = "custom-card",
                  h5("Filter Sites", style = "color:#0A3B5C; font-weight:600;"),
                  sliderInput("d4_plastic_min", "Min Macroplastics Count:", min = 0, max = 500, value = 50),
                  hr(),
                  h5("Spatial Grid Selector (CRS)", style = "color:#0A3B5C; font-weight:600;"),
                  selectInput("d4_crs_proj", "Choose Projected System:",
                    choices = c(
                      "Tanzania UTM Zone 37S (EPSG:32737)" = "32737",
                      "Arc 1960 / UTM Zone 37S (EPSG:21037)" = "21037",
                      "Web Mercator (EPSG:3857)" = "3857"
                    ), selected = "32737"
                  ),
                  hr(),
                  h5("Distance Calculator to EDEMA", style = "color:#0A3B5C; font-weight:600;"),
                  selectInput("d4_target_site", "Select Target Station:", choices = NULL),
                  uiOutput("d4_distance_report"),
                  hr(),
                  h5("Accommodation Route Calculator", style = "color:#0A3B5C; font-weight:600;"),
                  fluidRow(
                    column(
                      width = 6,
                      numericInput("d4_accom_lat", "Latitude:", value = -6.8164, min = -7.2, max = -6.5, step = 0.0001)
                    ),
                    column(
                      width = 6,
                      numericInput("d4_accom_lon", "Longitude:", value = 37.6545, min = 37.4, max = 37.9, step = 0.0001)
                    )
                  ),
                  selectInput("d4_travel_mode", "Mode of Transport:",
                    choices = c("Driving (Car)" = "driving", "Walking (Foot)" = "foot")
                  ),
                  actionButton("d4_calculate_route", "Find Path to Edema", class = "btn-primary", style = "width:100%;", icon = icon("route")),
                  uiOutput("d4_route_summary"),
                  hr(),
                  h5("Spatial Operations Code (sf)", style = "color:#0A3B5C; font-weight:600;"),
                  uiOutput("d4_spatial_code_ui")
                )
              )
            )
          )
        ),

        # Sub-tab: Track B
        nav_panel(
          title = "Track B: Statistical Modeling",
          fluidPage(
            br(),
            div(
              class = "custom-card",
              h4(class = "section-title", "Track B: Model Interactive Playground"),
              p("This playground fits statistical comparisons and regression models. Select model options in the sidebar and view parameters and residual checks.")
            ),
            layout_sidebar(
              sidebar = sidebar(
                title = "Model Settings",
                selectInput("d4_model_type", "Choose Analysis Model:",
                  choices = c(
                    "Two-Sample T-Test (testflow)" = "ttest",
                    "Multiple Linear Regression (lm)" = "regression"
                  )
                ),
                conditionalPanel(
                  condition = "input.d4_model_type == 'ttest'",
                  selectInput("d4_ttest_response", "Numeric Response Variable:",
                    choices = c("Soil Organic Carbon (%)" = "soil_organic_carbon_pct", "Crop Yield (t/ha)" = "yield_tonnes_ha")
                  ),
                  selectInput("d4_ttest_group", "Grouping Factor:",
                    choices = c("Crop Type" = "crop_type", "District" = "district")
                  )
                ),
                conditionalPanel(
                  condition = "input.d4_model_type == 'regression'",
                  selectInput("d4_reg_response", "Response Variable (Y):",
                    choices = c("Tree Height (m)" = "height_m", "Crop Yield (t/ha)" = "yield_tonnes_ha")
                  ),
                  selectizeInput("d4_reg_predictors", "Predictor Variables (X):",
                    choices = NULL, multiple = TRUE
                  ),
                  hr(),
                  uiOutput("d4_prediction_inputs")
                ),
                actionButton("d4_run_model", "Run Model Analysis", class = "btn-primary", style = "width:100%;")
              ),
              div(
                class = "custom-card",
                h5("Statistical & Diagnostic Outputs", style = "color:#0A3B5C; font-weight:600;"),
                p("Click 'Run Model Analysis' to update calculations and assumptions."),
                tabsetPanel(
                  tabPanel(
                    "Report Summary",
                    verbatimTextOutput("d4_model_summary")
                  ),
                  tabPanel(
                    "Model Assumptions",
                    plotOutput("d4_model_diagnostics", height = "400px")
                  ),
                  tabPanel(
                    "Assumptions Scorecard",
                    br(),
                    uiOutput("d4_assumptions_scorecard")
                  ),
                  tabPanel(
                    "Predictive Tool",
                    br(),
                    uiOutput("d4_prediction_card")
                  ),
                  tabPanel(
                    "R Code Block",
                    uiOutput("d4_model_code")
                  )
                )
              )
            )
          )
        )
      )
    )
  ),

  # ---------------------------------------------------------
  # TAB: Day 5
  # ---------------------------------------------------------
  nav_panel(
    title = "Day 5",
    icon = icon("file-alt"),
    fluidPage(
      div(
        class = "day-header-banner",
        h3("Day 5: Reproducible Quarto Reports & Project Delivery"),
        p("Integrating code, tables, figures, and narratives into automated PDF, HTML, and Word formats.")
      ),
      fluidRow(
        column(
          width = 7,
          div(
            class = "custom-card",
            h4(class = "section-title", "Daily Schedule"),
            div(
              class = "schedule-item",
              div(class = "schedule-time", "09:00 - 10:30"),
              div(class = "schedule-title", "Session 1: Quarto Foundations"),
              div(class = "schedule-desc", "YAML frontmatter, Markdown syntax, embedding code chunks, and rendering reports.")
            ),
            div(
              class = "schedule-item",
              div(class = "schedule-time", "11:00 - 12:30"),
              div(class = "schedule-title", "Session 2: Formatting Options"),
              div(class = "schedule-desc", "Cross-referencing figures and tables, chunk control options (echo, warning), Word/PDF templates.")
            ),
            div(
              class = "schedule-item",
              div(class = "schedule-time", "14:00 - 16:30"),
              div(class = "schedule-title", "Session 3: Cohort Presentations"),
              div(class = "schedule-desc", "Final Theme Table presentations of reproducible R data workflows.")
            ),
            div(
              class = "schedule-item",
              div(class = "schedule-time", "16:30 - 17:00"),
              div(class = "schedule-title", "Applied Statistics Wrap-up"),
              div(class = "schedule-desc", "Distribution of certificates, post-training feedback survey, and closing remarks.")
            )
          )
        ),
        column(
          width = 5,
          div(
            class = "custom-card",
            h4(class = "section-title", "Learning Objectives"),
            div(
              class = "objective-item",
              icon("check-circle", class = "objective-icon"),
              div(class = "objective-text", "Construct and compile a Quarto document (.qmd).")
            ),
            div(
              class = "objective-item",
              icon("check-circle", class = "objective-icon"),
              div(class = "objective-text", "Style reports using Markdown headers, lists, links, and math equations.")
            ),
            div(
              class = "objective-item",
              icon("check-circle", class = "objective-icon"),
              div(class = "objective-text", "Configure code chunk parameters (echo, warning, message, tbl-cap, fig-cap).")
            ),
            div(
              class = "objective-item",
              icon("check-circle", class = "objective-icon"),
              div(class = "objective-text", "Render documents directly to Microsoft Word, PDF, or HTML formats.")
            ),
            div(
              class = "objective-item",
              icon("check-circle", class = "objective-icon"),
              div(class = "objective-text", "Apply professional styling using document layout presets.")
            )
          ),
          div(
            class = "custom-card",
            h4(class = "section-title", "Packages & Resources"),
            h6("Required Setup Commands:", style = "font-weight:600; color:#475569; margin-top:0;"),
            div(
              class = "code-container", style = "margin-top:0.3rem; margin-bottom:1rem; padding:0.8rem;",
              pre("library(tidyverse)\nlibrary(flextable)\nlibrary(knitr)")
            ),
            h6("Recommended Readings:", style = "font-weight:600; color:#475569; margin-top:0.5rem;"),
            tags$a(
              href = "https://quarto.org/", target = "_blank", class = "resource-link-btn",
              icon("external-link-alt"), "Quarto Official Website"
            ),
            tags$a(
              href = "https://www.markdownguide.org/", target = "_blank", class = "resource-link-btn",
              icon("book"), "Markdown Style Guide"
            ),
            hr(),
            h6("Course Slides & Worksheets:", style = "font-weight:600; color:#475569; margin-top:0.5rem;"),
            div(
              style = "display:flex; flex-direction:column; gap:0.4rem; margin-top:0.3rem;",
              div(style = "font-size:0.8rem; font-weight:700; color:#0A3B5C;", "RevealJS Slide Decks:"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Session 1: Quarto Foundations (session1.qmd)"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Session 2: Formatting Options (session2.qmd)"),
              div(style = "font-size:0.8rem; font-weight:700; color:#0A3B5C; margin-top:0.3rem;", "Hands-On Exercises & Templates:"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Exercise 1: Quarto Document Structure (exercise1.qmd)"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Exercise 2: Cross-Referencing (exercise2.qmd)"),
              tags$span(style = "font-size:0.75rem; color:#475569;", "• Thesis Chapter Template (template_report.qmd)")
            )
          )
        )
      ),
      layout_sidebar(
        sidebar = sidebar(
          title = "Quarto Customizer",
          h5("YAML Frontmatter Settings:"),
          textInput("d5_title_input", "Document Title:", "Marine Ecology Report"),
          textInput("d5_subtitle_input", "Document Subtitle:", "Applied Spatial Wrangling"),
          textInput("d5_author_input", "Author Name:", "Scholar Name"),
          selectInput("d5_theme_input", "HTML Theme Style:",
            choices = c("cosmo", "flatly", "sandstone", "united", "lux", "slate")
          ),
          selectInput("d5_code_fold", "Code Folding:",
            choices = c("Show Code (none)" = "none", "Fold Code (true)" = "true", "Hide Code (false)" = "false")
          ),
          checkboxInput("d5_toc_input", "Include Table of Contents", TRUE),
          hr(),
          h5("Export Deliverables:"),
          downloadButton("d5_download_qmd", "Download QMD Template", class = "btn-success", style = "width:100%;"),
          br(),
          downloadButton("d5_download_pdf", "Download Mock Rendered PDF", class = "btn-primary", style = "width:100%;"),
          br(),
          downloadButton("d5_download_templates", "Download Script Templates (.ZIP)", class = "btn-info", style = "width:100%;")
        ),
        div(
          class = "custom-card",
          h4(class = "section-title", "Quarto Source Code Preview"),
          p("Below is an interactive view of a Quarto report template configured with chunk headers."),
          uiOutput("d5_preview_ui")
        )
      )
    )
  ),

  # ---------------------------------------------------------
  # TAB: Day 6
  # ---------------------------------------------------------
  nav_panel(
    title = "Day 6",
    icon = icon("file-export"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Report Configuration",
        selectInput("rep_thematic_select", "Scholarly Thematic Area:",
          choices = c(
            "Greenhouse Gas (GHG) Inventories",
            "Marine Ecology",
            "Coastal Forests",
            "Marine Plastics & Waste Mitigation",
            "Biodiversity Conservation",
            "Fisheries Stock Assessment",
            "Climate Change Adaptation",
            "Blue Economy Development"
          )
        ),
        radioButtons("rep_source", "Data Source:",
          choices = c(
            "Use Project Theme Dataset" = "theme",
            "Upload Custom Dataset (.csv)" = "upload"
          )
        ),
        conditionalPanel(
          condition = "input.rep_source == 'upload'",
          fileInput("rep_custom_file", "Choose CSV File:", accept = ".csv")
        ),
        hr(),
        h6("Report Metadata Parameters:", style = "font-weight:700; color:#0A3B5C; margin-top:0.5rem;"),
        textInput("rep_title_input", "Report Title:", "Scientific Analysis Report"),
        textInput("rep_subtitle_input", "Subtitle / Topic:", "Reproducible Research Pipeline"),
        textInput("rep_author_input", "Scholar / Author Name:", "Dr. Masumbuko Semba"),
        selectInput("rep_format_select", "Select Report Format:",
          choices = c(
            "Microsoft Word (.docx)" = "docx",
            "EPUB Ebook (.epub)" = "epub",
            "PDF Document (.pdf)" = "pdf"
          )
        ),
        downloadButton("rep_download_report", "Generate & Download Report", class = "btn-success", style = "width:100%;")
      ),
      fluidPage(
        layout_columns(
          col_widths = c(6, 6),

          # Left Column: Raw Data Explorer
          div(
            class = "custom-card", style = "margin-top:0; height: 100%; display: flex; flex-direction: column;",
            h4(class = "section-title", "Step 1: Raw Data Explorer"),
            p("Preview the active theme or uploaded dataset below before generating calculations."),
            div(style = "flex-grow: 1; overflow-x: auto;", DTOutput("rep_data_table"))
          ),

          # Right Column: Interactive Plot & Aesthetics
          div(
            class = "custom-card", style = "margin-top:0; height: 100%; display: flex; flex-direction: column;",
            h4(class = "section-title", "Step 2: Interactive Plot & Aesthetics"),
            p("Select axes variables and plot parameters to visualize data layers dynamically."),
            fluidRow(
              column(
                width = 4,
                selectInput("rep_x_var", "X-Axis Variable:", choices = NULL),
                selectInput("rep_y_var", "Y-Axis Variable:", choices = NULL),
                selectInput("rep_plot_type", "Plot Style Type:",
                  choices = c(
                    "Scatter Plot" = "point",
                    "Boxplot" = "boxplot",
                    "Bar Column Chart" = "bar",
                    "Line Plot" = "line"
                  )
                ),
                selectInput("rep_color_theme", "Color Palette:",
                  choices = c(
                    "Classic Ocean Blue" = "default",
                    "Viridis Colorblind-Safe" = "viridis",
                    "Dark2 Publication Theme" = "dark2"
                  )
                )
              ),
              column(
                width = 8,
                plotOutput("rep_ggplot", height = "320px")
              )
            )
          )
        ),

        # Third row: Interpretation & Narrative
        div(
          class = "custom-card",
          h4(class = "section-title", "Step 3: Scholar Narrative & Interpretation"),
          p("Write your scientific interpretation below. This narrative block is dynamically rendered in the final report output."),
          textAreaInput("rep_interpretation_input", "Scientific Interpretation & Discussion:",
            value = "The statistical analysis reveals key patterns in the environmental indicators. Further analysis utilizing generalized additive models (GAMs) will be performed to refine the research findings.",
            rows = 6, width = "100%"
          )
        ),

        # Fourth row: Source Code Preview
        div(
          class = "custom-card",
          h4(class = "section-title", "Step 4: Quarto Source Code Preview"),
          p("Preview the underlying Quarto (.qmd) report template code that will render the report."),
          uiOutput("rep_source_preview")
        )
      )
    )
  ),

  # ---------------------------------------------------------
  # TAB: Knowledge Quiz
  # ---------------------------------------------------------
  nav_panel(
    title = "Quiz",
    icon = icon("question-circle"),
    fluidPage(
      div(
        class = "day-header-banner",
        h3("Bahari Yetu Knowledge Quiz"),
        p("Test your understanding of the R coding, plotting, mapping, and modeling concepts covered throughout the training.")
      ),
      fluidRow(
        column(
          width = 3,
          div(
            class = "custom-card",
            h4(class = "section-title", "Quiz Navigator"),
            selectInput("quiz_day", "Choose a Day:",
              choices = c(
                "Day 1: Foundations & Tidy Data" = "day1",
                "Day 2: Data Wrangling (dplyr)" = "day2",
                "Day 3: ggplot2 Graphics" = "day3",
                "Day 4: Spatial GIS & Modeling" = "day4",
                "Day 5: Reproducible Quarto" = "day5"
              )
            ),
            selectInput("quiz_question_num", "Choose a Question:",
              choices = c(
                "Question 1" = "q1",
                "Question 2" = "q2",
                "Question 3" = "q3",
                "Question 4" = "q4",
                "Question 5" = "q5"
              )
            ),
            hr(),
            uiOutput("quiz_score_ratio")
          )
        ),
        column(
          width = 5,
          div(
            class = "custom-card", style = "min-height: 400px; display: flex; flex-direction: column; justify-content: space-between;",
            div(
              h4(class = "section-title", "Active Question"),
              uiOutput("quiz_question_ui")
            ),
            div(
              style = "margin-top: 1.5rem;",
              actionButton("quiz_submit", "Submit Answer", class = "btn-primary", style = "width:100%;")
            )
          )
        ),
        column(
          width = 4,
          uiOutput("quiz_feedback_ui")
        )
      )
    )
  ),
  # ---------------------------------------------------------
  # TAB: Stats Guide
  # ---------------------------------------------------------
  nav_panel(
    title = "Stats Guide",
    icon = icon("calculator"),
    fluidPage(
      div(
        class = "day-header-banner",
        h1("Applied Biostatistics & Stats Guide"),
        p("Learn how to use the rstatix package to build hypotheses, validate assumptions, compare means, run ANOVA models, and construct correlation matrices across the 8 scholarly research tracks.")
      ),
      fluidRow(
        column(
          width = 8,
          navset_card_tab(
            # title = "Thematic",
            # Subtab 1: Greenhouse Gas (GHG) Inventories
            nav_panel(
              title = "GHG",
              icon = icon("cloud-sun"),
              uiOutput("stats_ghg_ui")
            ),
            # Subtab 2: Marine Ecology
            nav_panel(
              title = "Marine Ecology",
              icon = icon("fish"),
              uiOutput("stats_marine_ui")
            ),
            # Subtab 3: Coastal Forests
            nav_panel(
              title = "Coastal Forests",
              icon = icon("tree"),
              uiOutput("stats_forest_ui")
            ),
            # Subtab 4: Marine Plastics
            nav_panel(
              title = "Marine Plastics",
              icon = icon("recycle"),
              uiOutput("stats_plastics_ui")
            ),
            # Subtab 5: Biodiversity
            nav_panel(
              title = "Biodiversity",
              icon = icon("dna"),
              uiOutput("stats_biodiv_ui")
            ),
            # Subtab 6: Fisheries
            nav_panel(
              title = "Fisheries",
              icon = icon("ship"),
              uiOutput("stats_fisheries_ui")
            ),
            # Subtab 7: Climate Change
            nav_panel(
              title = "Climate",
              icon = icon("temperature-high"),
              uiOutput("stats_climate_ui")
            ),
            # Subtab 8: Blue Economy
            nav_panel(
              title = "Blue Economy",
              icon = icon("coins"),
              uiOutput("stats_blue_ui")
            )
          )
        ),
        column(
          width = 4,
          div(
            class = "custom-card", style = "margin-top:0; height: 100%; display: flex; flex-direction: column; background: #f8fafc; border: 1px solid #e2e8f0; padding: 1.25rem; border-radius: 10px;",
            h5(
              style = "font-weight:700; color:#0A3B5C; margin-top:0; margin-bottom: 0.8rem; display:flex; align-items:center; gap:0.5rem;",
              icon("robot", class = "text-info"), "AI Stats Assistant"
            ),
            p(style = "font-size: 0.82rem; color: #64748b; margin-bottom: 1rem;", "Type any question about biostatistics, hypothesis testing, or rstatix syntax:"),
            textAreaInput("stats_ai_prompt_input", "Ask your question:",
              placeholder = "e.g., How do I run a t-test? or Explain p-values.",
              rows = 3, width = "100%"
            ),
            actionButton("stats_trigger_ai", "Ask Copilot", class = "btn-info", style = "width:100%; color: white; font-weight: 700;", icon = icon("paper-plane")),
            br(),
            h6(style = "font-weight: 700; margin-top: 1rem; color: #0A3B5C;", "AI Response:"),
            div(
              style = "flex-grow: 1; overflow-y:auto; min-height: 250px; max-height: 450px; padding: 12px; background: white; border: 1px solid #e2e8f0; border-radius: 8px; font-size: 0.85rem; line-height: 1.5; color: #334155; box-shadow: inset 0 2px 4px rgba(0,0,0,0.02);",
              uiOutput("stats_ai_response")
            )
          )
        )
      )
    )
  ),
  # ---------------------------------------------------------
  # TAB: Directory
  # ---------------------------------------------------------
  nav_panel(
    title = "Directory",
    icon = icon("address-book"),
    fluidPage(
      div(
        class = "day-header-banner",
        h1("Network Directory & Scholar Registry"),
        p("Explore profiles of trainers, trainees, scholars, and other project participants under the Pamoja Tuhifadhi Bahari Yetu Initiative. Register your own profile to join the network directory.")
      ),
      layout_sidebar(
        sidebar = sidebar(
          title = "Directory & Registry Portal",

          # 1. Login / Management Portal
          uiOutput("login_portal_ui"),
          hr(style = "margin: 0.8rem 0;"),

          # 2. Registration / Profile Editor Form
          h5("Register / Update Profile", style = "font-weight: 700; color: #0A3B5C; font-size: 0.85rem; margin-top: 0.4rem; margin-bottom: 0.6rem;"),
          textInput("reg_name", "Full Name:", placeholder = "e.g., Jane Doe"),
          selectInput("reg_role", "Role / Capacity:",
            choices = c(
              "Scholar / Trainee" = "Trainee",
              "Trainer / Instructor" = "Trainer",
              "Support Staff" = "Support",
              "Project Partner" = "Partner"
            ), selected = "Trainee"
          ),
          textInput("reg_affiliation", "Affiliation / Org:", placeholder = "e.g., UDSM, SUA, IUCN"),
          selectInput("reg_focus", "Thematic Research Area:",
            choices = c(
              "None / General" = "General",
              "Greenhouse Gas (GHG) Inventories" = "GHG",
              "Marine Ecology" = "Marine Ecology",
              "Coastal Forests" = "Coastal Forests",
              "Marine Plastics & Waste Mitigation" = "Marine Plastics",
              "Biodiversity Conservation" = "Biodiversity",
              "Fisheries Stock Assessment" = "Fisheries",
              "Climate Change Adaptation" = "Climate",
              "Blue Economy Development" = "Blue Economy"
            ), selected = "General"
          ),
          textInput("reg_email", "Email Address:", placeholder = "e.g., jane.doe@example.com"),
          passwordInput("reg_password", "Profile Password:", placeholder = "To edit your profile later"),
          textAreaInput("reg_bio", "Short Bio / Profile Summary:", rows = 3, placeholder = "Briefly describe your scholarly focus or project involvement..."),
          fileInput("reg_photo", "Profile Photo:", accept = c("image/png", "image/jpeg", "image/jpg")),
          uiOutput("reg_submit_button_ui"),
          uiOutput("reg_status")
        ),

        # Main Panel Content
        fluidPage(
          # Filter Controls Row
          div(
            class = "custom-card mb-3 p-3",
            style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px;",
            layout_columns(
              col_widths = c(6, 6),
              textInput("search_dir_name", "Search by Name:", placeholder = "Type to search name..."),
              selectInput("filter_dir_role", "Filter by Role:",
                choices = c(
                  "All Roles" = "All",
                  "Scholar / Trainee" = "Trainee",
                  "Trainer / Instructor" = "Trainer",
                  "Support Staff" = "Support",
                  "Project Partner" = "Partner"
                ), selected = "All"
              )
            )
          ),
          # Profile grid
          uiOutput("profiles_grid_ui")
        )
      )
    )
  ),
  nav_spacer(),

  # ---------------------------------------------------------
  # TAB: Developer
  # ---------------------------------------------------------
  nav_panel(
    title = "Developer",
    icon = icon("user-gear"),
    fluidPage(
      layout_columns(
        col_widths = c(4, 8),

        # Left Column: Developer Profile Card & Credentials
        div(
          card(
            class = "p-3 mb-3",
            tags$div(
              style = "margin: 15px auto; width: 130px; height: 130px; border-radius: 50%; background: linear-gradient(135deg, #004763 0%, #0f766e 100%); display:flex; align-items:center; justify-content:center; box-shadow: 0 4px 10px rgba(0,71,99,0.2);",
              icon("user-tie", class = "fa-4x text-white")
            ),
            tags$h3("Mr. Masumbuko Semba", style = "color:#004763; font-weight:800; margin-bottom:4px; text-align:center; font-family:'Outfit';"),
            tags$p("Data Scientist | Web App Developer | Spatial Analyst", style = "color:#64748b; font-size:0.85em; font-weight:500; text-align:center;"),
            tags$hr(),
            tags$div(
              style = "display:flex; justify-content:center; gap:16px; font-size:1.3em; margin-bottom: 15px;",
              tags$a(href = "https://github.com/lugoga", target = "_blank", icon("github"), style = "color:#334155;", title = "GitHub"),
              tags$a(href = "https://www.linkedin.com/in/masumbuko-semba-20936a4b/", target = "_blank", icon("linkedin"), style = "color:#0a66c2;", title = "LinkedIn"),
              tags$a(href = "https://orcid.org/0000-0002-5002-9747", target = "_blank", icon("orcid"), style = "color:#a6ce39;", title = "ORCID"),
              tags$a(href = "https://www.youtube.com/channel/UC9uZ1KPyo7zIzI4BnUtHPfA", target = "_blank", icon("youtube"), style = "color:#ff0000;", title = "YouTube"),
              tags$a(href = "https://lugoga.github.io/kitaa/", target = "_blank", icon("globe"), style = "color:#0f766e;", title = "Personal Website")
            ),
            tags$div(
              style = "font-size: 0.85em; text-align: left; color:#475569; background:#f8fafc; border-radius:8px; padding:12px; border:1px solid #e2e8f0; line-height: 1.6;",
              tags$div(tags$strong("Citizenship:"), " Tanzania"),
              tags$div(tags$strong("Address:"), " P.O. Box 447, Tengeru, Arusha"),
              tags$div(tags$strong("Mobile:"), " +255 717 603 703"),
              tags$div(tags$strong("Email:"), tags$a(href = "mailto:lugosemba@gmail.com", "lugosemba@gmail.com"))
            )
          ),
          card(
            class = "mb-3",
            card_header(span(icon("graduation-cap"), " Education & Credentials", style = "font-weight: 700; color: #004763;")),
            tags$ul(
              style = "padding-left: 20px; font-size: 0.9em; line-height: 1.6; color: #334155; margin-bottom: 0;",
              tags$li(
                tags$strong("PhD in Physical Oceanography"), br(),
                "University of Dar es Salaam", br(),
                tags$span(style = "font-size:0.82em; color:#475569; display:block; margin-top:2px; line-height:1.4;", "Specialized in numerical hydrodynamic modeling of coastal currents, monsoonal wind patterns, and ocean-atmosphere interactions in the Western Indian Ocean.")
              ),
              tags$li(
                tags$strong("MSc in Marine Sciences and Management"), br(),
                "University of Dar es Salaam", br(),
                tags$span(style = "font-size:0.82em; color:#475569; display:block; margin-top:2px; line-height:1.4;", "Focused on integrated coastal zone management (ICZM), marine protected area (MPA) planning, marine pollution assessments, and marine ecological conservation.")
              ),
              tags$li(
                tags$strong("BSc in Fisheries and Aquaculture"), br(),
                "University of Dar es Salaam", br(),
                tags$span(style = "font-size:0.82em; color:#475569; display:block; margin-top:2px; line-height:1.4;", "Covered fisheries biology, population dynamics modeling, stock assessment techniques, and sustainable aquaculture design systems.")
              )
            ),
            tags$hr(style = "margin: 10px 0;"),
            tags$strong("Core Language Proficiency:", style = "font-size:0.9em; color:#004763;"),
            tags$p("English (Fluent) | Swahili (Native)", style = "font-size:0.85em; color:#475569; margin-bottom: 5px;")
          ),
          card(
            class = "mb-3",
            card_header(span(icon("building-columns"), " Affiliations & Forums", style = "font-weight: 700; color: #004763;")),
            tags$ul(
              style = "padding-left: 20px; font-size: 0.85em; line-height: 1.5; color: #475569; margin-bottom: 5px;",
              tags$li("Western Indian Ocean Marine Science Association (WIOMSA)"),
              tags$li("East Africa Geospatial Forum"),
              tags$li("Tanga-Pemba Multistakeholder Forum"),
              tags$li("Mtwara Seascape Multistakeholder Forum")
            )
          )
        ),

        # Right Column: Professional History & Decision-Support Portfolio
        div(
          card(
            class = "mb-3",
            card_header(span(icon("briefcase"), " Professional History & Key Projects", style = "font-weight: 700; color: #004763;")),
            card_body(
              tags$div(
                style = "max-height: 220px; overflow-y: auto; padding-right:10px; font-size: 0.85em;",
                tags$div(
                  class = "border-start border-primary border-3 ps-3 mb-3",
                  tags$h6("Mar 2025 - Present | Reef Restoration in Unguja North & Mtwara", style = "color:#004763; margin-bottom: 2px; font-weight: 700;"),
                  tags$p("The Nature Conservancy (TNC) - Supporting technical implementation of coral reef restoration to strengthen coastal protection, biodiversity recovery, and community resilience.", style = "margin:0; color:#475569;")
                ),
                tags$div(
                  class = "border-start border-primary border-3 ps-3 mb-3",
                  tags$h6("Aug 2025 - Present | Inventory of Benthic Ecosystems in Tanzania", style = "color:#004763; margin-bottom: 2px; font-weight: 700;"),
                  tags$p("The Nature Conservancy (TNC) - Leading inventory mapping across coastal waters to build evidence bases for marine spatial planning.", style = "margin:0; color:#475569;")
                ),
                tags$div(
                  class = "border-start border-primary border-3 ps-3 mb-3",
                  tags$h6("Aug 2025 - Feb 2026 | Mtwara Seascape Multistakeholder Forum", style = "color:#004763; margin-bottom: 2px; font-weight: 700;"),
                  tags$p("IUCN - Facilitated forum establishment for improved co-management and participatory governance of Mtwara's marine resources.", style = "margin:0; color:#475569;")
                ),
                tags$div(
                  class = "border-start border-primary border-3 ps-3 mb-3",
                  tags$h6("Jul 2025 - Present | Marine Spatial Planning Technical Leadership", style = "color:#004763; margin-bottom: 2px; font-weight: 700;"),
                  tags$p("The Nature Conservancy (TNC) - Integrating blue economy priorities with conservation targets using data-driven spatial models.", style = "margin:0; color:#475569;")
                ),
                tags$div(
                  class = "border-start border-primary border-3 ps-3 mb-3",
                  tags$h6("2014 - Present | Lecturer / Data Science & Oceanography", style = "color:#004763; margin-bottom: 2px; font-weight: 700;"),
                  tags$p("Nelson Mandela African Institution of Science and Technology (NM-AIST) - Mentoring in geospatial analysis and climate resilience systems.", style = "margin:0; color:#475569;")
                )
              )
            )
          ),
          card(
            class = "mb-3",
            card_header(span(icon("book"), " Digital Books & Publications", style = "font-weight: 700; color: #004763;")),
            card_body(
              style = "max-height: 270px; overflow-y: auto; padding-right:10px; font-size: 0.82em; line-height:1.5;",
              tags$div(
                class = "mb-3",
                tags$h6(tags$a(href = "https://lugoga.github.io/geomarine/", target = "_blank", tags$strong("Practical Spatial Data"), icon("external-link-alt", class = "ms-1", style = "font-size: 0.85em;")), style = "margin-bottom:4px; font-weight:700; font-size:1.05em;"),
                tags$p("Practical Spatial Data is an invaluable resource for those interested in working with spatial data, particularly in coastal and marine environments. Authored by Masumbuko Semba, this book provides a comprehensive introduction to R programming with a specific focus on handling spatial data. From importing geographic information to performing spatial analysis, readers gain practical insights and hands-on experience in utilizing R for geospatial applications.", style = "color:#475569; margin:0;")
              ),
              tags$div(
                tags$h6(tags$a(href = "https://lugoga.github.io/spatialgoR/", target = "_blank", tags$strong("Geospatial Technology and Spatial Analysis in R"), icon("external-link-alt", class = "ms-1", style = "font-size: 0.85em;")), style = "margin-bottom:4px; font-weight:700; font-size:1.05em;"),
                tags$p("Geospatial Technology and Spatial Analysis in R delves deeper into the realm of geospatial data analysis using R. Written by Masumbuko Semba, this book explores the latest tools and packages available for modern spatial data handling and manipulation. Through step-by-step tutorials and real-world examples, readers learn how to harness the power of R for tasks such as geographic visualization, spatial statistics, and remote sensing analysis.", style = "color:#475569; margin:0;")
              )
            )
          ),
          card(
            class = "mb-3",
            card_header(span(icon("cubes"), " Decision-Support Portfolio Apps", style = "font-weight: 700; color: #004763;")),
            card_body(
              tags$p("Explore other interactive web tools developed by Dr. Masumbuko Semba:", style = "font-size:0.9em; color:#475569;"),
              tags$div(
                style = "max-height: 250px; overflow-y: auto; padding-right:10px;",
                class = "list-group",
                tags$a(
                  href = "https://rugo.shinyapps.io/semba/", target = "_blank", class = "list-group-item list-group-item-action",
                  tags$div(
                    class = "d-flex w-100 justify-content-between",
                    tags$h6(class = "mb-1 text-primary", icon("globe"), "Inventory"),
                    tags$small("BEIV")
                  ),
                  tags$p(class = "mb-1", style = "font-size:0.8em; color:#64748b;", "Integrated decision-support portal for oceanographic and marine ecosystem analysis.")
                ),
                tags$a(
                  href = "https://lugoga.github.io/mnazi/", target = "_blank", class = "list-group-item list-group-item-action",
                  tags$div(
                    class = "d-flex w-100 justify-content-between",
                    tags$h6(class = "mb-1 text-primary", icon("map"), " Mtwara Seascape Web Map"),
                    tags$small("M-SSF")
                  ),
                  tags$p(class = "mb-1", style = "font-size:0.8em; color:#64748b;", "Interactive web map visualizing benthic resources, boundary markers, and seascape features.")
                ),
                tags$a(
                  href = "https://rugo.shinyapps.io/mssf/", target = "_blank", class = "list-group-item list-group-item-action",
                  tags$div(
                    class = "d-flex w-100 justify-content-between",
                    tags$h6(class = "mb-1 text-primary", icon("gauge-high"), " Mtwara Seascape Forum"),
                    tags$small("M-SSF")
                  ),
                  tags$p(class = "mb-1", style = "font-size:0.8em; color:#64748b;", "Mtwara Seascape Forum, Interactive dashboard for participatory spatial planning.")
                ),
                tags$a(
                  href = "https://lugoga.github.io/inventory-digital/", target = "_blank", class = "list-group-item list-group-item-action",
                  tags$div(
                    class = "d-flex w-100 justify-content-between",
                    tags$h6(class = "mb-1 text-primary", icon("location-dot"), " Benthic Inventory Mapper"),
                    tags$small("BIM")
                  ),
                  tags$p(class = "mb-1", style = "font-size:0.8em; color:#64748b;", "Mapping coastal benthic inventory across Tanzania & Zanzibar.")
                ),
                tags$a(
                  href = "https://semba.shinyapps.io/faims/", target = "_blank", class = "list-group-item list-group-item-action",
                  tags$div(
                    class = "d-flex w-100 justify-content-between",
                    tags$h6(class = "mb-1 text-primary", icon("anchor"), " Fisheries & Aquaculture Information System"),
                    tags$small("FAIMS")
                  ),
                  tags$p(class = "mb-1", style = "font-size:0.8em; color:#64748b;", "Fisheries monitoring, catch statistics, and stock assessment used data poor methods.")
                ),
                tags$a(
                  href = "https://semba.shinyapps.io/besa/", target = "_blank", class = "list-group-item list-group-item-action",
                  tags$div(
                    class = "d-flex w-100 justify-content-between",
                    tags$h6(class = "mb-1 text-primary", icon("chart-area"), " Benthic Ecological Survey Tool"),
                    tags$small("BESA")
                  ),
                  tags$p(class = "mb-1", style = "font-size:0.8em; color:#64748b;", "Visualizing benthic habitats, corals, and substrate data.")
                ),
                tags$a(
                  href = "https://semba.shinyapps.io/vizingaApp/", target = "_blank", class = "list-group-item list-group-item-action",
                  tags$div(
                    class = "d-flex w-100 justify-content-between",
                    tags$h6(class = "mb-1 text-primary", icon("fish-fins"), " Cage Aquaculture Suitability Tool"),
                    tags$small("CAGE")
                  ),
                  tags$p(class = "mb-1", style = "font-size:0.8em; color:#64748b;", "Spatial suitability modeling for marine/freshwater cage farming.")
                ),
                tags$a(
                  href = "https://semba.shinyapps.io/digital/", target = "_blank", class = "list-group-item list-group-item-action",
                  tags$div(
                    class = "d-flex w-100 justify-content-between",
                    tags$h6(class = "mb-1 text-primary", icon("city"), "KWALA Digital Master Plan"),
                    tags$small("KWALA")
                  ),
                  tags$p(class = "mb-1", style = "font-size:0.8em; color:#64748b;", "Urban resilience & investment master planning dashboard.")
                ),
                tags$a(
                  href = "https://bionutra.shinyapps.io/coding/", target = "_blank", class = "list-group-item list-group-item-action",
                  tags$div(
                    class = "d-flex w-100 justify-content-between",
                    tags$h6(class = "mb-1 text-primary", icon("code"), " IUCN's Scholar Capacity Need"),
                    tags$small("ISCN")
                  ),
                  tags$p(class = "mb-1", style = "font-size:0.8em; color:#64748b;", "Interactive coding, training resources, and biostatistics data workflows.")
                )
              )
            )
          ),
          card(
            class = "mb-3",
            card_header(span(icon("laptop-code"), " Web & R Package Developments", style = "font-weight: 700; color: #004763;")),
            card_body(
              tags$p("Explore additional websites, blogs, and packages developed by Dr. Masumbuko Semba:", style = "font-size:0.85em; color:#475569; margin-bottom:10px;"),
              tags$div(
                style = "max-height: 250px; overflow-y: auto; padding-right:10px;",
                class = "list-group",

                # R Packages
                tags$a(
                  href = "https://github.com/lugoga/wior", target = "_blank", class = "list-group-item list-group-item-action",
                  tags$div(
                    class = "d-flex w-100 justify-content-between",
                    tags$h6(class = "mb-1 text-primary", icon("box-archive"), " wior: R Package for coastal and marine Data"),
                    tags$small("R PACKAGE")
                  ),
                  tags$p(class = "mb-1", style = "font-size:0.8em; color:#64748b;", "An R package developed to facilitate access and tidy processing of Western Indian Ocean oceanographic and marine data.")
                ),

                # Websites and blogs
                tags$a(
                  href = "https://lugoga.github.io/kitaa/blog.html", target = "_blank", class = "list-group-item list-group-item-action",
                  tags$div(
                    class = "d-flex w-100 justify-content-between",
                    tags$h6(class = "mb-1 text-primary", icon("blog"), " Kitaa Blog"),
                    tags$small("BLOG")
                  ),
                  tags$p(class = "mb-1", style = "font-size:0.8em; color:#64748b;", "Articles and updates on spatial data analysis, programming in R, and coastal research insights.")
                ),
                tags$a(
                  href = "https://semba-blog.netlify.app/", target = "_blank", class = "list-group-item list-group-item-action",
                  tags$div(
                    class = "d-flex w-100 justify-content-between",
                    tags$h6(class = "mb-1 text-primary", icon("square-rss"), " Semba's Data Science Blog"),
                    tags$small("BLOG")
                  ),
                  tags$p(class = "mb-1", style = "font-size:0.8em; color:#64748b;", "A Netlify blog covering modern statistics, spatial models, and interactive dashboard tutorials in R.")
                ),
                tags$a(
                  href = "https://lugoga.github.io/semba-quarto/", target = "_blank", class = "list-group-item list-group-item-action",
                  tags$div(
                    class = "d-flex w-100 justify-content-between",
                    tags$h6(class = "mb-1 text-primary", icon("bookmark"), " Semba Quarto Hub"),
                    tags$small("QUARTO")
                  ),
                  tags$p(class = "mb-1", style = "font-size:0.8em; color:#64748b;", "Reproducible Quarto publications, research documents, and interactive geospatial web widgets.")
                )
              )
            )
          )
        )
      )
    )
  )
)

# ---------------------------------------------------------
# SERVER SIDE LOGIC
# ---------------------------------------------------------
server <- function(input, output, session) {
  # Register Pandoc directory path from Positron system tools
  try(rmarkdown::find_pandoc(dir = "C:/Program Files/Positron/resources/app/quarto/bin/tools"), silent = TRUE)

  # Reactive dynamic CSS palette injection
  output$dynamic_palette_css <- renderUI({
    theme <- input$app_theme_select
    req(theme)

    if (theme == "midnight") {
      tags$style(HTML("
        :root {
          --primary-color: #0f172a;
          --accent-color: #38bdf8;
          --gradient-start: #1e293b;
          --gradient-end: #0f172a;
          --card-bg: rgba(30, 41, 59, 0.85);
          --body-bg: #0b0f19;
          --text-color: #f1f5f9;
        }
        body { background-color: #0b0f19 !important; color: #f1f5f9 !important; }
        .custom-card { background: rgba(30, 41, 59, 0.85) !important; border: 1px solid rgba(255, 255, 255, 0.1) !important; color: #f1f5f9 !important; }
        .tna-card { background: rgba(30, 41, 59, 0.85) !important; border: 1px solid rgba(255, 255, 255, 0.1) !important; color: #f1f5f9 !important; }
        .tna-stat { color: #38bdf8 !important; }
        .tna-label { color: #94a3b8 !important; }
        .tna-desc { color: #cbd5e1 !important; }
        .objective-text { color: #cbd5e1 !important; }
        .schedule-title { color: #f1f5f9 !important; }
        .schedule-desc { color: #94a3b8 !important; }
        .navbar { background-color: #0f172a !important; border-bottom: 2px solid #38bdf8 !important; }
        .btn-primary { background: linear-gradient(135deg, #1e293b 0%, #0f172a 100%) !important; }
        .tna-card:hover { border-color: #38bdf8 !important; }
        .custom-card:hover { border-color: rgba(56, 189, 248, 0.4) !important; }
        .schedule-item { border-left: 3px solid #38bdf8 !important; }
        .schedule-time { color: #38bdf8 !important; }
        .day-header-banner { background: linear-gradient(135deg, #1e293b 0%, #0f172a 100%) !important; box-shadow: 0 4px 15px rgba(0,0,0,0.3) !important; }
      "))
    } else if (theme == "emerald") {
      tags$style(HTML("
        :root {
          --primary-color: #0d5c46;
          --accent-color: #10B981;
          --gradient-start: #0d5c46;
          --gradient-end: #047857;
          --card-bg: rgba(255, 255, 255, 0.85);
          --body-bg: #f8fafc;
          --text-color: #334155;
        }
        body { background-color: #f8fafc !important; color: #334155 !important; }
        .custom-card { background: rgba(255, 255, 255, 0.85) !important; border: 1px solid #e2e8f0 !important; color: #334155 !important; }
        .tna-card { background: rgba(255, 255, 255, 0.85) !important; border: 1px solid #e2e8f0 !important; color: #334155 !important; }
        .tna-stat { color: #0d5c46 !important; }
        .tna-label { color: #475569 !important; }
        .tna-desc { color: #64748b !important; }
        .objective-text { color: #475569 !important; }
        .schedule-title { color: #1e293b !important; }
        .schedule-desc { color: #64748b !important; }
        .navbar { background-color: #0d5c46 !important; border-bottom: 2px solid #10B981 !important; }
        .btn-primary { background: linear-gradient(135deg, #0d5c46 0%, #047857 100%) !important; }
        .tna-card:hover { border-color: #0d5c46 !important; }
        .custom-card:hover { border-color: rgba(13, 92, 70, 0.4) !important; }
        .schedule-item { border-left: 3px solid #10B981 !important; }
        .schedule-time { color: #10B981 !important; }
        .day-header-banner { background: linear-gradient(135deg, #0d5c46 0%, #047857 100%) !important; }
      "))
    } else if (theme == "sunset") {
      tags$style(HTML("
        :root {
          --primary-color: #854D0E;
          --accent-color: #F59E0B;
          --gradient-start: #854D0E;
          --gradient-end: #B45309;
          --card-bg: rgba(255, 255, 255, 0.85);
          --body-bg: #f8fafc;
          --text-color: #334155;
        }
        body { background-color: #f8fafc !important; color: #334155 !important; }
        .custom-card { background: rgba(255, 255, 255, 0.85) !important; border: 1px solid #e2e8f0 !important; color: #334155 !important; }
        .tna-card { background: rgba(255, 255, 255, 0.85) !important; border: 1px solid #e2e8f0 !important; color: #334155 !important; }
        .tna-stat { color: #854D0E !important; }
        .tna-label { color: #475569 !important; }
        .tna-desc { color: #64748b !important; }
        .objective-text { color: #475569 !important; }
        .schedule-title { color: #1e293b !important; }
        .schedule-desc { color: #64748b !important; }
        .navbar { background-color: #854D0E !important; border-bottom: 2px solid #F59E0B !important; }
        .btn-primary { background: linear-gradient(135deg, #854D0E 0%, #B45309 100%) !important; }
        .tna-card:hover { border-color: #854D0E !important; }
        .custom-card:hover { border-color: rgba(133, 77, 14, 0.4) !important; }
        .schedule-item { border-left: 3px solid #F59E0B !important; }
        .schedule-time { color: #F59E0B !important; }
        .day-header-banner { background: linear-gradient(135deg, #854D0E 0%, #B45309 100%) !important; }
      "))
    } else {
      # Default Ocean Blue
      tags$style(HTML("
        :root {
          --primary-color: #0A3B5C;
          --accent-color: #34D399;
          --gradient-start: #0A3B5C;
          --gradient-end: #1b4e3e;
          --card-bg: rgba(255, 255, 255, 0.85);
          --body-bg: #f8fafc;
          --text-color: #334155;
        }
        body { background-color: #f8fafc !important; color: #334155 !important; }
        .custom-card { background: rgba(255, 255, 255, 0.85) !important; border: 1px solid #e2e8f0 !important; color: #334155 !important; }
        .tna-card { background: rgba(255, 255, 255, 0.85) !important; border: 1px solid #e2e8f0 !important; color: #334155 !important; }
        .tna-stat { color: #0A3B5C !important; }
        .tna-label { color: #475569 !important; }
        .tna-desc { color: #64748b !important; }
        .objective-text { color: #475569 !important; }
        .schedule-title { color: #1e293b !important; }
        .schedule-desc { color: #64748b !important; }
        .navbar { background-color: #0A3B5C !important; border-bottom: 2px solid #34D399 !important; }
        .btn-primary { background: linear-gradient(135deg, #0A3B5C 0%, #1e3a5f 100%) !important; }
        .tna-card:hover { border-color: #0A3B5C !important; }
        .custom-card:hover { border-color: rgba(10, 59, 92, 0.4) !important; }
        .schedule-item { border-left: 3px solid #0A3B5C !important; }
        .schedule-time { color: #0A3B5C !important; }
        .day-header-banner { background: linear-gradient(135deg, #4A7C59 0%, #1e4e3e 100%) !important; }
      "))
    }
  })

  # ---------------------------------------------------------
  # Day 1: Tidy Data Pivot Demo
  # ---------------------------------------------------------
  # Simulated wide dataset
  wide_data <- tibble(
    Site = c("Changuu Reef", "Bawe Reef", "Chumbe Sanctuary"),
    CoralCover_2024 = c(42.5, 31.2, 58.9),
    CoralCover_2025 = c(40.1, 29.8, 61.2),
    CoralCover_2026 = c(37.4, 28.5, 63.4)
  )

  output$wide_table_view <- renderTable(
    {
      wide_data
    },
    striped = TRUE,
    bordered = TRUE,
    align = "c"
  )

  output$tidy_table_view <- renderTable(
    {
      cols <- input$d1_pivot_cols
      if (length(cols) == 0) {
        return(data.frame(Message = "Select columns to pivot"))
      }

      res <- wide_data |>
        pivot_longer(
          cols = all_of(cols),
          names_to = input$d1_names_to,
          names_prefix = input$d1_names_prefix,
          values_to = input$d1_values_to
        )

      if (input$d1_drop_na) {
        res <- res |> drop_na(all_of(input$d1_values_to))
      }

      res
    },
    striped = TRUE,
    bordered = TRUE,
    align = "c"
  )

  output$d1_code_output <- renderUI({
    cols_str <- paste(sprintf('"%s"', input$d1_pivot_cols), collapse = ", ")
    drop_str <- if (input$d1_drop_na) sprintf(" |>\n  drop_na(%s)", input$d1_values_to) else ""

    code_text <- sprintf(
      "library(tidyverse)

# Pivot the wide dataset to a tidy long dataset
tidy_data <- wide_data |>
  pivot_longer(
    cols = c(%s),
    names_to = \"%s\",
    names_prefix = \"%s\",
    values_to = \"%s\"
  )%s",
      cols_str, input$d1_names_to, input$d1_names_prefix, input$d1_values_to, drop_str
    )

    div(class = "code-container", pre(code_text))
  })

  # ---------------------------------------------------------
  # Day 2: Wrangling Playground
  # ---------------------------------------------------------
  # Reactive loader for selected dataset
  d2_raw_data <- reactive({
    load_dataset(input$d2_dataset)
  })

  # Update column selection options based on chosen dataset
  observe({
    df <- d2_raw_data()
    if (!is.null(df)) {
      updateSelectizeInput(session, "d2_select_cols",
        choices = colnames(df),
        selected = colnames(df)
      )
    }
  })

  # Render a dynamic slider filter based on numeric column
  output$d2_filter_ui <- renderUI({
    df <- d2_raw_data()
    if (is.null(df)) {
      return(NULL)
    }

    # Find first numeric column to slide on
    num_cols <- sapply(df, is.numeric)
    num_col_names <- names(num_cols)[num_cols]

    if (length(num_col_names) == 0) {
      return(NULL)
    }

    target_col <- num_col_names[1]
    min_val <- min(df[[target_col]], na.rm = TRUE)
    max_val <- max(df[[target_col]], na.rm = TRUE)

    # Show slider
    sliderInput("d2_filter_range",
      label = sprintf("Filter Range of %s:", target_col),
      min = floor(min_val),
      max = ceiling(max_val),
      value = c(floor(min_val), ceiling(max_val))
    )
  })

  # Update group by / summary choices
  observe({
    df <- d2_raw_data()
    if (!is.null(df)) {
      char_cols <- colnames(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
      num_cols <- colnames(df)[sapply(df, is.numeric)]

      updateSelectInput(session, "d2_group_var", choices = char_cols)
      updateSelectInput(session, "d2_summary_var", choices = num_cols)
      updateSelectInput(session, "d2_arrange_var", choices = c("None" = "", colnames(df)))
    }
  })

  # Wrangled calculations
  d2_wrangled_result <- reactive({
    df <- d2_raw_data()
    if (is.null(df)) {
      return(NULL)
    }

    # 1. Select
    selected_cols <- input$d2_select_cols
    if (length(selected_cols) > 0) {
      df <- df |> select(all_of(selected_cols))
    }

    # 2. Filter
    num_cols <- sapply(df, is.numeric)
    num_col_names <- names(num_cols)[num_cols]
    if (length(num_col_names) > 0 && !is.null(input$d2_filter_range)) {
      target_col <- num_col_names[1]
      df <- df[df[[target_col]] >= input$d2_filter_range[1] & df[[target_col]] <= input$d2_filter_range[2], ]
    }

    # 3. Summarize
    if (input$d2_do_summary) {
      g_var <- input$d2_group_var
      s_var <- input$d2_summary_var
      s_fun <- input$d2_summary_fun

      if (!is.null(g_var) && g_var != "" && !is.null(s_var) && s_var != "") {
        # Dynamically build summary
        if (s_fun == "mean") {
          df <- df |>
            group_by(.data[[g_var]]) |>
            summarize(Mean = round(mean(.data[[s_var]], na.rm = TRUE), 2))
        } else if (s_fun == "max") {
          df <- df |>
            group_by(.data[[g_var]]) |>
            summarize(Max = max(.data[[s_var]], na.rm = TRUE))
        } else if (s_fun == "min") {
          df <- df |>
            group_by(.data[[g_var]]) |>
            summarize(Min = min(.data[[s_var]], na.rm = TRUE))
        } else {
          df <- df |>
            group_by(.data[[g_var]]) |>
            summarize(Count = n())
        }
      }
    }

    # 4. Arrange (Sort)
    if (!is.null(input$d2_arrange_var) && input$d2_arrange_var != "None" && input$d2_arrange_var != "") {
      df <- df |> arrange(.data[[input$d2_arrange_var]])
    }

    return(df)
  })

  # Code generator
  output$d2_code_output <- renderUI({
    meta <- datasets_metadata[[as.character(input$d2_dataset)]]

    select_str <- ""
    if (length(input$d2_select_cols) > 0) {
      select_str <- sprintf("  select(%s) |>\n", paste(input$d2_select_cols, collapse = ", "))
    }

    filter_str <- ""
    df_raw <- d2_raw_data()
    if (!is.null(df_raw)) {
      num_cols <- sapply(df_raw, is.numeric)
      num_col_names <- names(num_cols)[num_cols]
      if (length(num_col_names) > 0 && !is.null(input$d2_filter_range)) {
        filter_str <- sprintf(
          "  filter(%s >= %s & %s <= %s) |>\n",
          num_col_names[1], input$d2_filter_range[1],
          num_col_names[1], input$d2_filter_range[2]
        )
      }
    }

    sum_str <- ""
    if (input$d2_do_summary && input$d2_group_var != "" && input$d2_summary_var != "") {
      if (input$d2_summary_fun == "mean") {
        fun_str <- sprintf("mean(%s, na.rm = TRUE)", input$d2_summary_var)
      } else if (input$d2_summary_fun == "max") {
        fun_str <- sprintf("max(%s, na.rm = TRUE)", input$d2_summary_var)
      } else if (input$d2_summary_fun == "min") {
        fun_str <- sprintf("min(%s, na.rm = TRUE)", input$d2_summary_var)
      } else {
        fun_str <- "n()"
      }
      sum_str <- sprintf("  group_by(%s) |>\n  summarize(Value = %s) |>\n", input$d2_group_var, fun_str)
    }

    arrange_str <- ""
    if (!is.null(input$d2_arrange_var) && input$d2_arrange_var != "None" && input$d2_arrange_var != "") {
      arrange_str <- sprintf("  arrange(%s) |>\n", input$d2_arrange_var)
    }

    # Combine pipelines
    pipeline <- paste0(select_str, filter_str, sum_str, arrange_str)
    # Strip trailing " |>\n"
    if (grepl(" \\|>\n$", pipeline)) {
      pipeline <- gsub(" \\|>\n$", "", pipeline)
    }

    code_text <- sprintf(
      "library(tidyverse)\nlibrary(here)\n\n# Load raw theme dataset\ndf <- read_csv(here(\"data\", \"%s\"))\n\ndf_wrangled <- df |>\n%s",
      meta$file, pipeline
    )

    div(class = "code-container", pre(code_text))
  })

  # Row retention metric output
  output$d2_row_metric <- renderUI({
    df_raw <- d2_raw_data()
    df_wrangled <- d2_wrangled_result()
    if (is.null(df_raw) || is.null(df_wrangled)) {
      return(NULL)
    }

    n_raw <- nrow(df_raw)
    n_wrangled <- nrow(df_wrangled)
    pct <- round((n_wrangled / n_raw) * 100)

    div(
      style = "font-size:0.82rem; color:#475569; margin-top:0.5rem;",
      div(
        style = "display:flex; justify-content:space-between; font-weight:700; margin-bottom:0.2rem;",
        span("Row Retention:"),
        span(sprintf("%d / %d (%d%%)", n_wrangled, n_raw, pct))
      ),
      div(
        class = "progress", style = "height:6px; background-color:#e2e8f0; border-radius:4px; overflow:hidden;",
        div(class = "progress-bar", style = sprintf("width: %d%%; background-color: #2ECC71; height:100%%; transition: width 0.3s ease;", pct))
      )
    )
  })

  output$d2_table_output <- renderDT({
    res <- d2_wrangled_result()
    if (is.null(res)) {
      return(NULL)
    }
    datatable(res, options = list(pageLength = 8, scrollX = TRUE), class = "cell-border stripe")
  })

  # ---------------------------------------------------------
  # Day 3: ggplot2 Builder
  # ---------------------------------------------------------
  d3_raw_data <- reactive({
    load_dataset(input$d3_dataset)
  })

  # Update X, Y, Color, and Facet choices
  observe({
    df <- d3_raw_data()
    if (!is.null(df)) {
      updateSelectInput(session, "d3_x", choices = colnames(df), selected = colnames(df)[1])
      updateSelectInput(session, "d3_y", choices = colnames(df), selected = colnames(df)[if (ncol(df) > 1) 2 else 1])

      char_cols <- colnames(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
      updateSelectInput(session, "d3_color", choices = c("None" = "", char_cols))
      updateSelectInput(session, "d3_facet", choices = c("None" = "", char_cols))
    }
  })

  # Construct ggplot reactively
  d3_plot_obj <- reactive({
    df <- d3_raw_data()
    if (is.null(df) || is.null(input$d3_x) || is.null(input$d3_y)) {
      return(NULL)
    }

    # Build plot mapping
    if (input$d3_color != "") {
      p <- ggplot(df, aes_string(x = input$d3_x, y = input$d3_y, color = input$d3_color, fill = input$d3_color))
    } else {
      p <- ggplot(df, aes_string(x = input$d3_x, y = input$d3_y))
    }

    # Add Geoms
    if (input$d3_geom == "point") {
      p <- p + geom_point(size = 3.5, alpha = 0.8)
    } else if (input$d3_geom == "boxplot") {
      p <- p + geom_boxplot(alpha = 0.7, width = 0.5, outlier.shape = NA)
      if (input$d3_add_jitter) {
        p <- p + geom_jitter(width = 0.15, alpha = 0.5, size = 1.8)
      }
    } else if (input$d3_geom == "bar") {
      p <- p + geom_col(alpha = 0.8, width = 0.6)
      if (input$d3_add_jitter) {
        p <- p + geom_jitter(width = 0.15, alpha = 0.5, size = 1.8)
      }
    } else if (input$d3_geom == "line") {
      p <- p + geom_line(size = 1.2) + geom_point(size = 2.5)
    }

    # Apply theme with custom base font size
    b_size <- input$d3_base_size
    if (input$d3_theme == "theme_classic") {
      p <- p + theme_classic(base_size = b_size)
    } else if (input$d3_theme == "theme_minimal") {
      p <- p + theme_minimal(base_size = b_size)
    } else if (input$d3_theme == "theme_bw") {
      p <- p + theme_bw(base_size = b_size)
    } else if (input$d3_theme == "theme_light") {
      p <- p + theme_light(base_size = b_size)
    }

    # Apply color palette
    if (input$d3_color != "") {
      if (input$d3_palette == "viridis") {
        p <- p + scale_color_viridis_d() + scale_fill_viridis_d()
      } else if (input$d3_palette == "set2") {
        p <- p + scale_color_brewer(palette = "Set2") + scale_fill_brewer(palette = "Set2")
      } else if (input$d3_palette == "dark2") {
        p <- p + scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2")
      } else {
        p <- p + scale_color_manual(values = c("#0A3B5C", "#4A7C59", "#E74C3C", "#F39C12", "#9B59B6")) +
          scale_fill_manual(values = c("#0A3B5C", "#4A7C59", "#E74C3C", "#F39C12", "#9B59B6"))
      }
    } else {
      if (input$d3_geom == "point") {
        p <- p + scale_color_manual(values = "#0A3B5C")
      } else {
        p <- p + scale_fill_manual(values = "#0A3B5C")
      }
    }

    # Add faceting
    if (!is.null(input$d3_facet) && input$d3_facet != "None" && input$d3_facet != "") {
      p <- p + facet_wrap(as.formula(paste("~", input$d3_facet)))
    }

    # Premium text adjustments
    p <- p + theme(
      text = element_text(family = "Inter"),
      plot.title = element_text(family = "Outfit", face = "bold", color = "#0A3B5C"),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold")
    )

    title_text <- if (input$d3_title != "") input$d3_title else sprintf("Publication Quality Analysis: %s", datasets_metadata[[as.character(input$d3_dataset)]]$name)
    sub_text <- if (input$d3_subtitle != "") input$d3_subtitle else sprintf("Plotted using ggplot2 | %s geom", input$d3_geom)
    x_label <- if (input$d3_xlab != "") input$d3_xlab else input$d3_x
    y_label <- if (input$d3_ylab != "") input$d3_ylab else input$d3_y

    p <- p + labs(
      title = title_text,
      subtitle = sub_text,
      x = x_label,
      y = y_label
    )

    return(p)
  })

  output$d3_plot <- renderPlot({
    p <- d3_plot_obj()
    if (!is.null(p)) p
  })

  # Code output
  output$d3_code_output <- renderUI({
    meta <- datasets_metadata[[as.character(input$d3_dataset)]]

    color_aes <- ""
    color_scale <- ""
    if (input$d3_color != "") {
      color_aes <- sprintf(", color = %s, fill = %s", input$d3_color, input$d3_color)

      if (input$d3_palette == "viridis") {
        color_scale <- "  scale_color_viridis_d() +\n  scale_fill_viridis_d() +\n"
      } else if (input$d3_palette == "set2") {
        color_scale <- "  scale_color_brewer(palette = \"Set2\") +\n  scale_fill_brewer(palette = \"Set2\") +\n"
      } else if (input$d3_palette == "dark2") {
        color_scale <- "  scale_color_brewer(palette = \"Dark2\") +\n  scale_fill_brewer(palette = \"Dark2\") +\n"
      }
    }

    jitter_str <- ""
    if (input$d3_add_jitter && input$d3_geom %in% c("boxplot", "bar")) {
      jitter_str <- " +\n  geom_jitter(width = 0.15, alpha = 0.5, size = 1.8)"
    }

    geom_str <- switch(input$d3_geom,
      "point" = "geom_point(size = 3.5, alpha = 0.8)",
      "boxplot" = sprintf("geom_boxplot(alpha = 0.7, width = 0.5, outlier.shape = NA)%s", jitter_str),
      "bar" = sprintf("geom_col(alpha = 0.8, width = 0.6)%s", jitter_str),
      "line" = "geom_line(size = 1.2) + geom_point(size = 2.5)"
    )

    facet_str <- ""
    if (!is.null(input$d3_facet) && input$d3_facet != "None" && input$d3_facet != "") {
      facet_str <- sprintf("  facet_wrap(~%s) +\n", input$d3_facet)
    }

    title_val <- if (input$d3_title != "") input$d3_title else "Publication Quality Analysis"
    x_val <- if (input$d3_xlab != "") input$d3_xlab else input$d3_x
    y_val <- if (input$d3_ylab != "") input$d3_ylab else input$d3_y

    code_text <- sprintf(
      "library(tidyverse)
library(here)

# Load data
df <- read_csv(here(\"data\", \"%s\"))

# Plot figure
p <- ggplot(df, aes(x = %s, y = %s%s)) +
  %s +
  %s%s  %s(base_size = %d) +
  labs(
    title = \"%s\",
    x = \"%s\",
    y = \"%s\"
  )

# Save high-res plot for journal submission
ggsave(\"outputs/figure1.tiff\", plot = p, width = %s, height = %s, dpi = %s)",
      meta$file, input$d3_x, input$d3_y, color_aes, geom_str, color_scale, facet_str, input$d3_theme, input$d3_base_size,
      title_val, x_val, y_val, input$d3_width, input$d3_height, input$d3_dpi
    )

    div(class = "code-container", pre(code_text))
  })

  # Download handler
  output$d3_download <- downloadHandler(
    filename = function() {
      paste0("figure_", Sys.Date(), ".tiff")
    },
    content = function(file) {
      ggsave(file,
        plot = d3_plot_obj(),
        width = input$d3_width,
        height = input$d3_height,
        dpi = input$d3_dpi,
        device = "tiff"
      )
    }
  )

  # ---------------------------------------------------------
  # Day 4: Track A (Spatial Map)
  # ---------------------------------------------------------
  d4_spatial_data <- reactive({
    load_dataset("6") # Theme 6: Plastic Waste
  })

  # Populate target stations select dynamically
  observe({
    df <- d4_spatial_data()
    if (!is.null(df)) {
      updateSelectInput(session, "d4_target_site", choices = df$station_id)
    }
  })

  output$d4_spatial_map <- renderLeaflet({
    df <- d4_spatial_data()
    if (is.null(df)) {
      return(NULL)
    }

    # Filter
    df_filt <- df |> filter(macroplastics_count >= input$d4_plastic_min)

    # Reproject coordinates on-the-fly to chosen CRS
    crs_target <- as.numeric(input$d4_crs_proj)
    crs_label <- switch(input$d4_crs_proj,
      "32737" = "UTM 37S (EPSG:32737)",
      "21037" = "Arc 1960 Zone 37S (EPSG:21037)",
      "3857" = "Web Mercator (EPSG:3857)"
    )

    if (nrow(df_filt) > 0) {
      df_sf <- st_as_sf(df_filt, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
      df_proj <- st_transform(df_sf, crs = crs_target)
      proj_coords <- st_coordinates(df_proj)
      df_filt$proj_x <- round(proj_coords[, 1], 1)
      df_filt$proj_y <- round(proj_coords[, 2], 1)
    } else {
      df_filt$proj_x <- numeric(0)
      df_filt$proj_y <- numeric(0)
    }

    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = df$macroplastics_count
    )

    leaflet(df_filt) |>
      addTiles(group = "OpenStreetMap") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
      addProviderTiles(providers$CartoDB.Positron, group = "Light (CartoDB)") |>
      setView(lng = 39.25, lat = -6.75, zoom = 11) |>
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = ~ sqrt(macroplastics_count) * 1.5,
        color = ~ pal(macroplastics_count),
        stroke = TRUE, fillOpacity = 0.8,
        weight = 1.5,
        popup = ~ sprintf(
          "<strong>Station:</strong> %s<br/><strong>Location:</strong> %s<br/><strong>Macroplastics Count:</strong> %d<br/><strong>GPS Coord:</strong> %.4f, %.4f<br/><strong>Grid:</strong> %s<br/><strong>X:</strong> %.1f, <strong>Y:</strong> %.1f",
          station_id, location, macroplastics_count, lat, lon, crs_label, proj_x, proj_y
        )
      ) |>
      addLegend(
        pal = pal, values = ~macroplastics_count,
        title = "Macroplastics Count",
        position = "bottomright"
      ) |>
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Satellite", "Light (CartoDB)"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })

  observeEvent(input$d4_calculate_route, {
    start_lat <- input$d4_accom_lat
    start_lon <- input$d4_accom_lon
    mode <- input$d4_travel_mode

    if (is.null(start_lat) || is.null(start_lon)) {
      return()
    }

    # Query OSRM API (http://router.project-osrm.org/route/v1/mode/lon1,lat1;lon2,lat2)
    # Target: Edema Hall coordinates (37.660060809193034, -6.801425980395493)
    url <- sprintf(
      "http://router.project-osrm.org/route/v1/%s/%f,%f;37.660060809193034,-6.801425980395493?overview=full&geometries=geojson",
      mode, start_lon, start_lat
    )

    res <- tryCatch(
      {
        jsonlite::fromJSON(url)
      },
      error = function(e) {
        NULL
      }
    )

    if (is.null(res) || res$code != "Ok") {
      output$d4_route_summary <- renderUI({
        div(
          class = "alert alert-danger", style = "margin-top:0.5rem; font-size:0.8rem;",
          "Error: Could not retrieve route. Check connection or start location."
        )
      })
      return()
    }

    # Parse OSRM response details
    dist_km <- round(res$routes$distance / 1000, 2)
    duration_min <- round(res$routes$duration / 60, 1)

    # Get geometry path coordinates (N x 2 matrix: Lon, Lat)
    coords <- res$routes$geometry$coordinates[[1]]
    lons <- coords[, 1]
    lats <- coords[, 2]

    output$d4_route_summary <- renderUI({
      mode_icon <- if (mode == "driving") "car" else "walking"
      mode_label <- if (mode == "driving") "Driving" else "Walking"

      div(
        style = "margin-top:0.5rem; border-left: 3px solid #4A7C59; background: #f8fafc; padding: 0.6rem; border-radius: 4px;",
        div(style = "font-size:0.75rem; font-weight:700; color:#475569;", sprintf("OSRM Path (%s):", mode_label)),
        div(
          style = "display:flex; justify-content:space-between; margin-top:0.2rem;",
          span(style = "font-weight:800; color:#0A3B5C; font-size:0.95rem;", sprintf("%.2f km", dist_km)),
          span(style = "font-weight:800; color:#0A3B5C; font-size:0.95rem;", sprintf("%.1f min", duration_min))
        )
      )
    })

    # Update map elements
    leafletProxy("d4_spatial_map") |>
      clearGroup("d4_route_path") |>
      addMarkers(
        lng = 37.660060809193034, lat = -6.801425980395493,
        popup = "<strong>EDEMA Conference Hall</strong><br/>Morogoro, Tanzania",
        group = "d4_route_path"
      ) |>
      addMarkers(
        lng = start_lon, lat = start_lat,
        popup = "Start Location (Accommodation)",
        group = "d4_route_path"
      ) |>
      addPolylines(
        lng = lons, lat = lats,
        color = if (mode == "driving") "#0A3B5C" else "#4A7C59",
        weight = 5, opacity = 0.8,
        group = "d4_route_path"
      ) |>
      fitBounds(
        lng1 = min(lons, 37.66006), lat1 = min(lats, -6.80143),
        lng2 = max(lons, 37.66006), lat2 = max(lats, -6.80143)
      )
  })

  # Distance Calculator to EDEMA
  output$d4_distance_report <- renderUI({
    df <- d4_spatial_data()
    req(input$d4_target_site)
    if (is.null(df)) {
      return(NULL)
    }

    target_row <- df |> filter(station_id == input$d4_target_site)
    if (nrow(target_row) == 0) {
      return(NULL)
    }

    # EDEMA location
    edema_sf <- st_sfc(st_point(c(37.660060809193034, -6.801425980395493)), crs = 4326)
    target_sf <- st_sfc(st_point(c(target_row$lon[1], target_row$lat[1])), crs = 4326)

    # Calculate distance using projected coordinates
    dist_val <- as.numeric(st_distance(st_transform(edema_sf, crs = 32737), st_transform(target_sf, crs = 32737)))
    dist_km <- round(dist_val / 1000, 2)

    div(
      class = "custom-card", style = "border-left: 5px solid #0A3B5C; background: #f8fafc; padding: 0.8rem; box-shadow:none; margin-top:0.5rem;",
      div(style = "font-size:0.75rem; font-weight:700; color:#64748b;", "Projected Distance to EDEMA:"),
      div(style = "font-size:1.4rem; font-weight:800; color:#0A3B5C; margin:0.2rem 0;", sprintf("%.2f km", dist_km)),
      div(style = "font-size:0.72rem; color:#475569;", sprintf("Station %s (%s)", target_row$station_id[1], target_row$location[1]))
    )
  })

  # Dynamic Spatial Code Output
  output$d4_spatial_code_ui <- renderUI({
    crs_target <- input$d4_crs_proj

    code_text <- sprintf(
      "library(sf)
library(tidyverse)

# 1. Convert tabular data to spatial simple feature (WGS 84)
clean_sites_sf <- st_as_sf(
  cleanup_sites,
  coords = c(\"lon\", \"lat\"),
  crs = 4326
)

# 2. Transform coordinate reference system (CRS)
clean_sites_projected <- st_transform(
  clean_sites_sf,
  crs = %s
)", crs_target
    )

    div(class = "code-container", pre(code_text))
  })

  # Update predictors list and dynamically render prediction sliders
  observe({
    df1 <- load_dataset("1")
    df5 <- load_dataset("5")

    if (input$d4_model_type == "regression") {
      if (input$d4_reg_response == "height_m" && !is.null(df1)) {
        updateSelectizeInput(session, "d4_reg_predictors",
          choices = c("dbh_cm"),
          selected = c("dbh_cm")
        )
      } else if (input$d4_reg_response == "yield_tonnes_ha" && !is.null(df5)) {
        updateSelectizeInput(session, "d4_reg_predictors",
          choices = c("fertilizer_nitrogen_kg_ha", "soil_organic_carbon_pct"),
          selected = c("fertilizer_nitrogen_kg_ha", "soil_organic_carbon_pct")
        )
      }
    }
  })

  # Dynamic prediction inputs in sidebar
  output$d4_prediction_inputs <- renderUI({
    req(input$d4_model_type == "regression")
    preds <- input$d4_reg_predictors
    resp <- input$d4_reg_response
    if (length(preds) == 0) {
      return(NULL)
    }

    df <- if (resp == "height_m") load_dataset("1") else load_dataset("5")
    if (is.null(df)) {
      return(NULL)
    }

    slider_list <- lapply(preds, function(col) {
      min_val <- min(df[[col]], na.rm = TRUE)
      max_val <- max(df[[col]], na.rm = TRUE)
      mean_val <- mean(df[[col]], na.rm = TRUE)

      label_map <- c(
        "dbh_cm" = "Diameter at Breast Height (DBH, cm)",
        "fertilizer_nitrogen_kg_ha" = "Nitrogen fertilizer (kg/ha)",
        "soil_organic_carbon_pct" = "Soil Organic Carbon (%)"
      )

      label <- if (col %in% names(label_map)) label_map[[col]] else col

      sliderInput(
        inputId = paste0("pred_val_", col),
        label = label,
        min = round(min_val, 1),
        max = round(max_val, 1),
        value = round(mean_val, 1),
        step = if (max_val - min_val > 10) 1 else 0.1
      )
    })

    tagList(
      h5("Predictive Estimator Values:", style = "font-weight:600; color:#0A3B5C; font-size:0.9rem;"),
      slider_list
    )
  })

  # Reactive runner for models
  model_results <- eventReactive(input$d4_run_model,
    {
      if (input$d4_model_type == "ttest") {
        df <- load_dataset("5")
        if (is.null(df)) {
          return(list(error = "Dataset not found"))
        }

        resp <- input$d4_ttest_response
        grp <- input$d4_ttest_group

        res <- tryCatch(
          {
            comp <- test_t_two_sample(data = df, response = resp, group = grp)
            list(type = "ttest", obj = comp, method = "testflow")
          },
          error = function(e) {
            stat_t <- df |> t_test(as.formula(paste(resp, "~", grp)), detailed = TRUE)
            list(type = "ttest_fallback", obj = stat_t, method = "rstatix", error = e$message)
          }
        )

        return(res)
      } else {
        resp <- input$d4_reg_response
        preds <- input$d4_reg_predictors

        if (length(preds) == 0) {
          return(list(error = "No predictors chosen"))
        }

        df <- if (resp == "height_m") load_dataset("1") else load_dataset("5")
        if (is.null(df)) {
          return(list(error = "Dataset not found"))
        }

        formula_str <- paste(resp, "~", paste(preds, collapse = " + "))
        model_fit <- lm(as.formula(formula_str), data = df)

        return(list(type = "regression", obj = model_fit))
      }
    },
    ignoreNULL = FALSE
  )

  output$d4_model_summary <- renderPrint({
    res <- model_results()
    if (!is.null(res$error) && is.null(res$obj)) {
      cat("Error running model: ", res$error, "\n")
      return()
    }

    if (res$type == "ttest") {
      tryCatch(
        {
          report(res$obj)
        },
        error = function(e) {
          print(res$obj)
        }
      )
    } else if (res$type == "ttest_fallback") {
      cat("Warning: testflow run failed. Outputting rstatix fallback table:\n\n")
      print(res$obj)
    } else if (res$type == "regression") {
      print(summary(res$obj))
    }
  })

  output$d4_model_diagnostics <- renderPlot({
    res <- model_results()
    if (is.null(res) || !is.null(res$error)) {
      return(NULL)
    }

    if (res$type == "ttest") {
      tryCatch(
        {
          plot(res$obj)
        },
        error = function(e) {
          df <- load_dataset("5")
          boxplot(as.formula(paste(input$d4_ttest_response, "~", input$d4_ttest_group)),
            data = df,
            col = c("#0A3B5C", "#4A7C59"), main = "Comparison Boxplot"
          )
        }
      )
    } else if (res$type == "ttest_fallback") {
      df <- load_dataset("5")
      boxplot(as.formula(paste(input$d4_ttest_response, "~", input$d4_ttest_group)),
        data = df,
        col = c("#0A3B5C", "#4A7C59"), main = "Comparison Boxplot"
      )
    } else if (res$type == "regression") {
      par(mfrow = c(1, 2))
      plot(res$obj, which = 1:2, col = "#0A3B5C")
      par(mfrow = c(1, 1))
    }
  })

  # Assumptions Scorecard (Normality Test / Diagnostic metrics)
  output$d4_assumptions_scorecard <- renderUI({
    res <- model_results()
    if (is.null(res) || !is.null(res$error)) {
      return(p("Run model analysis to view assumptions scorecard.", style = "color:#64748b; font-style:italic;"))
    }

    if (res$type == "regression") {
      model_fit <- res$obj
      resids <- residuals(model_fit)
      shapiro_p <- tryCatch(shapiro.test(resids)$p.value, error = function(e) NA)

      # Check BP test (homoscedasticity)
      bp_p <- tryCatch(car::ncvTest(model_fit)$p, error = function(e) NA)

      norm_alert <- if (!is.na(shapiro_p) && shapiro_p >= 0.05) {
        div(
          class = "alert alert-success", style = "border-left:5px solid #2ECC71;",
          h5(style = "color:#27ae60; font-weight:700; margin-top:0;", icon("check-circle"), "Residual Normality Met"),
          p(sprintf("Shapiro-Wilk test p-value = %.4f (p >= 0.05). Residuals appear normally distributed.", shapiro_p))
        )
      } else {
        div(
          class = "alert alert-danger", style = "border-left:5px solid #E74C3C;",
          h5(style = "color:#c0392b; font-weight:700; margin-top:0;", icon("exclamation-triangle"), "Residual Normality Violated"),
          p(sprintf("Shapiro-Wilk test p-value = %.4f (p < 0.05). Consider log-transforming response variables.", shapiro_p))
        )
      }

      var_alert <- if (!is.na(bp_p) && bp_p >= 0.05) {
        div(
          class = "alert alert-success", style = "border-left:5px solid #2ECC71;",
          h5(style = "color:#27ae60; font-weight:700; margin-top:0;", icon("check-circle"), "Homoscedasticity Met"),
          p(sprintf("Score test for non-constant error variance p-value = %.4f (p >= 0.05). Constant variance holds.", bp_p))
        )
      } else {
        div(
          class = "alert alert-danger", style = "border-left:5px solid #E74C3C;",
          h5(style = "color:#c0392b; font-weight:700; margin-top:0;", icon("exclamation-triangle"), "Heteroscedasticity Detected"),
          p(sprintf("Score test p-value = %.4f (p < 0.05). Standard errors may be biased; check Scale-Location plot.", bp_p))
        )
      }

      tagList(norm_alert, br(), var_alert)
    } else {
      # T-Test normality
      df <- load_dataset("5")
      resp <- input$d4_ttest_response
      grp <- input$d4_ttest_group

      shapiro_res <- tryCatch(
        {
          p_vals <- df |>
            group_by(.data[[grp]]) |>
            summarize(p = shapiro.test(.data[[resp]])$p.value)
          p_vals
        },
        error = function(e) NULL
      )

      if (is.null(shapiro_res)) {
        return(p("Shapiro-Wilk test could not be calculated (requires between 3 and 5000 samples per group)."))
      }

      alerts <- lapply(1:nrow(shapiro_res), function(i) {
        group_name <- shapiro_res[[1]][i]
        p_val <- shapiro_res$p[i]

        if (p_val >= 0.05) {
          div(
            class = "alert alert-success", style = "border-left:5px solid #2ECC71; margin-bottom:0.8rem;",
            h6(
              style = "color:#27ae60; font-weight:700; margin:0;",
              icon("check-circle"), sprintf("Group [%s]: Normality Met (p = %.4f)", group_name, p_val)
            )
          )
        } else {
          div(
            class = "alert alert-danger", style = "border-left:5px solid #E74C3C; margin-bottom:0.8rem;",
            h6(
              style = "color:#c0392b; font-weight:700; margin:0;",
              icon("exclamation-triangle"), sprintf("Group [%s]: Normality Violated (p = %.4f)", group_name, p_val)
            )
          )
        }
      })

      tagList(
        h5("Group-wise Residual Normality Check:", style = "font-weight:700; color:#0A3B5C; margin-bottom:0.8rem;"),
        alerts
      )
    }
  })

  # Live Predictions Card
  output$d4_prediction_card <- renderUI({
    res <- model_results()
    if (is.null(res) || res$type != "regression") {
      return(p("Prediction tools are only available for Multiple Linear Regression models.", style = "color:#64748b; font-style:italic;"))
    }

    model_fit <- res$obj
    preds <- input$d4_reg_predictors

    # Collect slider inputs
    newdata <- list()
    for (col in preds) {
      val <- input[[paste0("pred_val_", col)]]
      if (is.null(val)) {
        return(p("Set values on the left sidebar to generate prediction outputs."))
      }
      newdata[[col]] <- val
    }

    newdata_df <- as.data.frame(newdata)
    pred_res <- tryCatch(
      {
        predict(model_fit, newdata = newdata_df, interval = "confidence")
      },
      error = function(e) NULL
    )

    if (is.null(pred_res)) {
      return(p("Error calculating predictions."))
    }

    fit_val <- round(pred_res[1, "fit"], 2)
    lwr_val <- round(pred_res[1, "lwr"], 2)
    upr_val <- round(pred_res[1, "upr"], 2)

    y_name <- switch(input$d4_reg_response,
      "height_m" = "Tree Height (meters)",
      "yield_tonnes_ha" = "Crop Yield (tonnes/ha)"
    )

    div(
      class = "custom-card", style = "border-left: 5px solid #0A3B5C; padding: 1.5rem;",
      h4(style = "color:#0A3B5C; font-weight:700; margin-top:0;", icon("calculator"), "Live Predictive Estimate"),
      p(sprintf("Based on regression coefficients fitted from historical theme data, the predicted value for <strong>%s</strong> is:", y_name)),
      div(style = "font-size: 2.8rem; font-weight: 800; color: #0A3B5C; margin: 1rem 0;", sprintf("%.2f", fit_val)),
      div(
        style = "font-size: 0.95rem; font-weight: 700; color: #475569;",
        icon("info-circle"), sprintf("95%% Confidence Interval: [%.2f to %.2f]", lwr_val, upr_val)
      )
    )
  })

  # Code output for Modeling
  output$d4_model_code <- renderUI({
    code_text <- ""
    if (input$d4_model_type == "ttest") {
      code_text <- sprintf(
        "library(tidyverse)\nlibrary(testflow)\nlibrary(here)\n\n# Load Agriculture AFOLU dataset\nstudy_data <- read_csv(here(\"data\", \"theme5_ghg_afolu.csv\"))\n\n# Run two-sample t-test comparison\ncomp <- test_t_two_sample(\n  data = study_data,\n  response = \"%s\",\n  group = \"%s\"\n)\n\n# Render scholarly report details\nreport(comp)\n\n# Show assumptions plots\nplot(comp)",
        input$d4_ttest_response, input$d4_ttest_group
      )
    } else {
      resp <- input$d4_reg_response
      preds <- input$d4_reg_predictors
      ds <- if (resp == "height_m") "theme1_coastal_forest.csv" else "theme5_ghg_afolu.csv"

      code_text <- sprintf(
        "library(tidyverse)\nlibrary(here)\n\n# Load dataset\nstudy_data <- read_csv(here(\"data\", \"%s\"))\n\n# Fit multiple linear regression model\nmodel_fit <- lm(%s ~ %s, data = study_data)\n\n# Display model parameters\nsummary(model_fit)\n\n# Predict new value with 95%% confidence interval\n# Example values set on UI sliders\nnew_obs <- data.frame(%s)\npredict(model_fit, newdata = new_obs, interval = \"confidence\")\n\n# Plot residuals diagnostics\npar(mfrow = c(1, 2))\nplot(model_fit, which = 1:2)\npar(mfrow = c(1, 1))",
        ds, resp, paste(preds, collapse = " + "),
        paste(sprintf("%s = val", preds), collapse = ", ")
      )
    }

    div(class = "code-container", pre(code_text))
  })

  # ---------------------------------------------------------
  # Day 5: Quarto report compilation download
  # ---------------------------------------------------------
  # Helper to generate custom QMD content reactively
  d5_generate_qmd_content <- reactive({
    title_val <- input$d5_title_input
    sub_val <- input$d5_subtitle_input
    author_val <- input$d5_author_input
    theme_val <- input$d5_theme_input
    toc_val <- if (input$d5_toc_input) "true" else "false"

    fold_line <- ""
    if (input$d5_code_fold == "true") {
      fold_line <- "\n    code-fold: true"
    } else if (input$d5_code_fold == "false") {
      fold_line <- "\n    code-fold: false"
    }

    qmd_txt <- sprintf(
      "---
title: \"%s\"
subtitle: \"%s\"
author: \"%s\"
date: today
format:
  html:
    toc: %s
    theme: %s%s
---

# Introduction
This report outlines a reproducible spatial analysis pipeline of plastic waste cleanups.

```{r}
#| label: load-packages
#| echo: true
#| warning: false
#| message: false
library(tidyverse)
library(sf)

# Load theme plastic cleanup data
cleanup_sites <- tibble(
  station_id = c(\"ST-01\", \"ST-02\", \"ST-03\"),
  location = c(\"Changuu Island\", \"Bawe Island\", \"Chumbe Reef\"),
  macroplastics_count = c(120, 85, 340),
  lat = c(-6.1189, -6.1524, -6.2829),
  lon = c(39.1633, 39.1417, 39.1994)
)
head(cleanup_sites)
```

# Spatial Vector Operations
We convert the tabular dataset to a coordinate-aware simple feature spatial object.

```{r}
#| label: convert-sf
#| echo: true
# Convert to sf class under WGS-84 (EPSG 4326)
cleanup_sf <- st_as_sf(cleanup_sites, coords = c(\"lon\", \"lat\"), crs = 4326)

# Transform projection to local UTM Zone 37S
cleanup_utm <- st_transform(cleanup_sf, crs = 32737)
print(cleanup_utm)
```

# Visualization of Density
Below is a descriptive comparison column chart of macroplastic counts per station.

```{r}
#| label: fig-counts
#| echo: false
#| fig-cap: \"Macroplastic waste counts at surveyed marine stations.\"
ggplot(cleanup_sites, aes(x = station_id, y = macroplastics_count, fill = station_id)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  theme_minimal(base_size = 12) +
  scale_fill_brewer(palette = \"Set2\") +
  labs(x = \"Station ID\", y = \"Macroplastics Count (n)\")
```",
      title_val, sub_val, author_val, toc_val, theme_val, fold_line
    )
    qmd_txt
  })

  output$d5_preview_ui <- renderUI({
    qmd_text <- d5_generate_qmd_content()
    div(
      class = "code-container", style = "max-height: 450px; overflow-y: auto;",
      pre(qmd_text)
    )
  })

  output$d5_download_qmd <- downloadHandler(
    filename = function() {
      "custom_scholarly_report.qmd"
    },
    content = function(file) {
      writeLines(d5_generate_qmd_content(), file)
    }
  )

  output$d5_download_pdf <- downloadHandler(
    filename = function() {
      "reproducible_scholarly_report.pdf"
    },
    content = function(file) {
      pdf_path <- here("TNA_report_v2.pdf") # Serve the available PDF as a simulated render
      if (file.exists(pdf_path)) {
        file.copy(pdf_path, file)
      } else {
        # Create empty file
        writeLines("PDF render mock", file)
      }
    }
  )

  output$download_tna_report <- downloadHandler(
    filename = function() {
      "TNA_report_v2.pdf"
    },
    content = function(file) {
      pdf_path <- here("TNA_report_v2.pdf")
      if (file.exists(pdf_path)) {
        file.copy(pdf_path, file)
      } else {
        writeLines("PDF report not found", file)
      }
    }
  )

  # Dynamic templates download handler (.ZIP)
  # Dynamic templates download handler (.ZIP)
  output$d5_download_templates <- downloadHandler(
    filename = function() {
      "bahari_yetu_course_materials.zip"
    },
    content = function(file) {
      # Create temporary folder for template scripts
      temp_root <- file.path(tempdir(), "course_materials_zip")
      dir.create(temp_root, showWarnings = FALSE, recursive = TRUE)

      # 1. Create a "scripts" subfolder for R templates
      temp_dir <- file.path(temp_root, "scripts")
      dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

      # Write Day 1 template
      writeLines(c(
        "# Day 1 Foundations Worksheet",
        "library(tidyverse)",
        "library(here)",
        "library(readxl)",
        "",
        "# 1. Setup project paths",
        "here::i_am(\"scripts/day1_foundations.R\")",
        "",
        "# 2. Load dataset",
        "coastal_forest <- read_csv(here(\"data\", \"theme1_coastal_forest.csv\"))",
        "head(coastal_forest)"
      ), file.path(temp_dir, "day1_foundations.R"))

      # Write Day 2 template
      writeLines(c(
        "# Day 2 Data Wrangling Worksheet",
        "library(tidyverse)",
        "library(here)",
        "",
        "study_data <- read_csv(here(\"data\", \"theme5_ghg_afolu.csv\"))",
        "",
        "# Perform wrangling pipeline",
        "wrangled_summary <- study_data |>",
        "  select(crop_type, yield_tonnes_ha, fertilizer_nitrogen_kg_ha) |>",
        "  filter(yield_tonnes_ha > 1.5) |>",
        "  group_by(crop_type) |>",
        "  summarize(Mean_Yield = mean(yield_tonnes_ha, na.rm=TRUE))",
        "",
        "print(wrangled_summary)"
      ), file.path(temp_dir, "day2_wrangling.R"))

      # Write Day 3 template
      writeLines(c(
        "# Day 3 ggplot2 Plotting Template",
        "library(tidyverse)",
        "library(here)",
        "",
        "study_data <- read_csv(here(\"data\", \"theme1_coastal_forest.csv\"))",
        "",
        "# Build publication-quality plot",
        "p <- ggplot(study_data, aes(x = dbh_cm, y = height_m, color = location)) +",
        "  geom_point(alpha = 0.7, size = 2) +",
        "  scale_color_viridis_d() +",
        "  theme_classic() +",
        "  labs(",
        "    title = \"Coastal Forest Canopy Structure\",",
        "    x = \"Diameter at Breast Height (dbh, cm)\",",
        "    y = \"Tree Height (m)\",",
        "    color = \"Sampling Location\"",
        "  )",
        "",
        "# Save plot to output folder",
        "ggsave(here(\"outputs\", \"forest_canopy_plot.tiff\"), plot = p, device = \"tiff\", width = 7, height = 5, dpi = 300)"
      ), file.path(temp_dir, "day3_plotting.R"))

      # Write Day 4 GIS template
      writeLines(c(
        "# Day 4 Spatial Analysis Worksheet",
        "library(tidyverse)",
        "library(sf)",
        "library(here)",
        "",
        "# Load coordinates",
        "clean_sites <- read_csv(here(\"data\", \"theme6_plastic_waste.csv\"))",
        "",
        "# Convert to spatial points (WGS 84)",
        "sites_sf <- st_as_sf(clean_sites, coords = c(\"lon\", \"lat\"), crs = 4326)",
        "",
        "# Reproject to Tanzania local UTM Zone 37S",
        "sites_utm <- st_transform(sites_sf, crs = 32737)",
        "print(sites_utm)"
      ), file.path(temp_dir, "day4_gis_reprojection.R"))

      # Write Day 4 Modeling template
      writeLines(c(
        "# Day 4 Modeling & Statistics Worksheet",
        "library(tidyverse)",
        "library(testflow)",
        "library(here)",
        "",
        "study_data <- read_csv(here(\"data\", \"theme5_ghg_afolu.csv\"))",
        "",
        "# 1. Run parametric t-test comparison",
        "comp_test <- test_t_two_sample(study_data, response = \"yield_tonnes_ha\", group = \"crop_type\")",
        "report(comp_test)",
        "plot(comp_test)"
      ), file.path(temp_dir, "day4_modeling.R"))

      # 2. Copy the actual daily folders (day1, day2, day3, day4, day5) including README, exercises, and presentations
      days <- c("day1", "day2", "day3", "day4", "day5")
      for (d in days) {
        src_day <- here::here(d)
        if (dir.exists(src_day)) {
          dest_day <- file.path(temp_root, d)
          dir.create(dest_day, showWarnings = FALSE, recursive = TRUE)

          # Copy README.md
          if (file.exists(file.path(src_day, "README.md"))) {
            file.copy(file.path(src_day, "README.md"), file.path(dest_day, "README.md"), overwrite = TRUE)
          }

          # Copy exercises folder
          src_ex <- file.path(src_day, "exercises")
          if (dir.exists(src_ex)) {
            dest_ex <- file.path(dest_day, "exercises")
            dir.create(dest_ex, showWarnings = FALSE, recursive = TRUE)
            ex_files <- list.files(src_ex, full.names = TRUE)
            file.copy(ex_files, dest_ex, overwrite = TRUE, recursive = TRUE)
          }

          # Copy presentations folder
          src_pres <- file.path(src_day, "presentations")
          if (dir.exists(src_pres)) {
            dest_pres <- file.path(dest_day, "presentations")
            dir.create(dest_pres, showWarnings = FALSE, recursive = TRUE)
            pres_files <- list.files(src_pres, full.names = TRUE)
            file.copy(pres_files, dest_pres, overwrite = TRUE, recursive = TRUE)
          }

          # Copy template_report.qmd in day5
          if (d == "day5" && file.exists(file.path(src_day, "template_report.qmd"))) {
            file.copy(file.path(src_day, "template_report.qmd"), file.path(dest_day, "template_report.qmd"), overwrite = TRUE)
          }
        }
      }

      # 3. Zip everything inside temp_root
      old_wd <- getwd()
      setwd(temp_root)
      on.exit(setwd(old_wd))

      # list files recursively relative to temp_root
      files_to_zip <- list.files(recursive = TRUE)
      zip(file, files = files_to_zip, flags = "-r")
    }
  )

  # ---------------------------------------------------------
  # TAB: Reporting Server-Side logic
  # ---------------------------------------------------------
  # Load dataset dynamically based on choice
  rep_loaded_data <- reactive({
    if (input$rep_source == "theme") {
      theme_idx <- switch(input$rep_thematic_select,
        "Greenhouse Gas (GHG) Inventories" = "2",
        "Marine Ecology" = "4",
        "Coastal Forests" = "1",
        "Marine Plastics & Waste Mitigation" = "6",
        "Biodiversity Conservation" = "4",
        "Fisheries Stock Assessment" = "4",
        "Climate Change Adaptation" = "5",
        "Blue Economy Development" = "4"
      )
      load_dataset(theme_idx)
    } else {
      req(input$rep_custom_file)
      tryCatch(
        {
          readr::read_csv(input$rep_custom_file$datapath)
        },
        error = function(e) {
          NULL
        }
      )
    }
  })

  # Update X and Y axis select choices dynamically when dataset changes
  observe({
    df <- rep_loaded_data()
    if (!is.null(df) && ncol(df) > 0) {
      updateSelectInput(session, "rep_x_var", choices = colnames(df), selected = colnames(df)[1])
      if (ncol(df) > 1) {
        updateSelectInput(session, "rep_y_var", choices = colnames(df), selected = colnames(df)[2])
      } else {
        updateSelectInput(session, "rep_y_var", choices = colnames(df), selected = colnames(df)[1])
      }
    }
  })

  # Render DataTable
  output$rep_data_table <- renderDT({
    df <- rep_loaded_data()
    req(df)
    datatable(df, options = list(pageLength = 5, scrollX = TRUE), class = "cell-border stripe")
  })

  # Generate plot
  rep_ggplot_obj <- reactive({
    df <- rep_loaded_data()
    req(df)
    req(input$rep_x_var)
    req(input$rep_y_var)

    p <- ggplot(df, aes_string(x = input$rep_x_var, y = input$rep_y_var))

    if (input$rep_plot_type == "point") {
      p <- p + geom_point(size = 3.5, alpha = 0.8, color = "#0A3B5C")
    } else if (input$rep_plot_type == "boxplot") {
      p <- p + geom_boxplot(fill = "#0A3B5C", alpha = 0.7, color = "#334155")
    } else if (input$rep_plot_type == "bar") {
      p <- p + geom_col(fill = "#0A3B5C", alpha = 0.8)
    } else if (input$rep_plot_type == "line") {
      p <- p + geom_line(size = 1.2, color = "#0A3B5C") + geom_point(size = 2.5, color = "#4A7C59")
    }

    p <- p + theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", color = "#0A3B5C"),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      ) +
      labs(
        title = sprintf("Analysis of %s vs %s", input$rep_x_var, input$rep_y_var),
        subtitle = sprintf("Thematic Area: %s", input$rep_thematic_select),
        x = input$rep_x_var,
        y = input$rep_y_var
      )

    if (input$rep_color_theme == "viridis") {
      if (input$rep_plot_type %in% c("boxplot", "bar")) {
        p <- p + scale_fill_viridis_d()
      } else {
        p <- p + scale_color_viridis_d()
      }
    } else if (input$rep_color_theme == "dark2") {
      if (input$rep_plot_type %in% c("boxplot", "bar")) {
        p <- p + scale_fill_brewer(palette = "Dark2")
      } else {
        p <- p + scale_color_brewer(palette = "Dark2")
      }
    }

    return(p)
  })

  output$rep_ggplot <- renderPlot({
    p <- rep_ggplot_obj()
    req(p)
    p
  })

  # Dynamic preview of the Quarto source code
  output$rep_source_preview <- renderUI({
    ext <- input$rep_format_select
    fmt <- switch(ext,
      "docx" = "docx",
      "epub" = "epub",
      "pdf" = "pdf"
    )

    qmd_text <- sprintf(
      "---
title: \"%s\"
subtitle: \"%s (%s)\"
author: \"%s\"
date: today
format: %s
---

# Introduction
This report was programmatically generated by the IUCN Bahari Yetu scholarly web App for the thematic research area: **%s**.

# Exploratory Data Analysis
A statistical summary of the loaded dataset variables is computed below:

```{r}
#| label: data-summary
#| echo: false
# summary(dataset)
```

# Visualization
The relationship between `%s` and `%s` is plotted using `ggplot2`:

```{r}
#| label: plot-visualization
#| echo: false
# ggplot(dataset, aes(x = %s, y = %s)) + geom_%s()
```

# Scientific Narrative & Interpretation
%s
",
      input$rep_title_input,
      input$rep_subtitle_input,
      input$rep_thematic_select,
      input$rep_author_input,
      fmt,
      input$rep_thematic_select,
      input$rep_x_var,
      input$rep_y_var,
      input$rep_x_var,
      input$rep_y_var,
      input$rep_plot_type,
      input$rep_interpretation_input
    )

    div(class = "code-container", style = "max-height: 400px; overflow-y: auto;", pre(qmd_text))
  })

  # Generate and Download Report (epub, docx, pdf)
  output$rep_download_report <- downloadHandler(
    filename = function() {
      ext <- input$rep_format_select
      sprintf("scholar_report_%s.%s", tolower(gsub("[^a-zA-Z0-9]+", "_", input$rep_thematic_select)), ext)
    },
    content = function(file) {
      ext <- input$rep_format_select

      title_val <- input$rep_title_input
      subtitle_val <- input$rep_subtitle_input
      author_val <- input$rep_author_input
      theme_val <- input$rep_thematic_select
      interpretation_val <- input$rep_interpretation_input

      # Generate a specialized scientific paragraph based on the thematic area
      thematic_intro_paragraph <- switch(theme_val,
        "Greenhouse Gas (GHG) Inventories" = paste(
          "Greenhouse gas accounting is critical for tracking national contributions under the UNFCCC Paris Agreement.",
          "This thematic area explores IPPU, waste, energy, and AFOLU sector emissions. Research focuses on compiling activity data,",
          "applying IPCC emission factors, and developing automated R pipelines to replace manual calculation templates."
        ),
        "Marine Ecology" = paste(
          "Marine ecology studies the complex interactions between marine organisms and their chemical and physical environments.",
          "Research under this theme focuses on Mnazi Bay seascape mapping, coral reef cover assessments, benthic invertebrates",
          "inventories, and modeling seagrass restoration metrics to inform coastal conservation policies in Tanzania."
        ),
        "Coastal Forests" = paste(
          "Coastal forests and mangroves play an indispensable role in carbon sequestration (blue carbon) and coastal defense.",
          "The scientific focus here is to analyze forest inventory data, compute biomass expansion factors, estimate above-ground carbon,",
          "and trace spatial canopy changes over time using tidy data and simple features (sf) structures."
        ),
        "Marine Plastics & Waste Mitigation" = paste(
          "Marine plastic debris is a major threat to marine biodiversity and coastal livelihoods. This research area focuses on",
          "characterizing macro- and microplastic distribution along shoreline stations, assessing waste accumulation rates,",
          "and developing automated analytics to evaluate the efficiency of community-based cleanup campaigns."
        ),
        "Biodiversity Conservation" = paste(
          "Biodiversity conservation aims to prevent species extinction and preserve ecosystem functions. Research under this area",
          "utilizes species occurrence coordinates, habitat suitability index modeling, and spatial conservation prioritization tools in R",
          "to design marine protected areas (MPAs) and wildlife corridors."
        ),
        "Fisheries Stock Assessment" = paste(
          "Sustainable fisheries management relies on accurate stock assessments. This thematic area addresses length-frequency analysis,",
          "catch-per-unit-effort (CPUE) dynamics, growth parameter estimates, and spawning potential ratios to establish catch limits",
          "and avoid overexploitation of pelagic and demersal marine stocks."
        ),
        "Climate Change Adaptation" = paste(
          "Coastal communities in Tanzania face vulnerability to sea-level rise, ocean acidification, and extreme weather events.",
          "Scientific investigation focuses on analyzing historical temperature and precipitation anomalies, modeling vulnerability indicators,",
          "and assessing nature-based solutions (NbS) for adaptive capacity enhancement."
        ),
        "Blue Economy Development" = paste(
          "The blue economy seeks to promote economic growth, improved livelihoods, and ocean ecosystem health. Research tracks",
          "sustainable aquaculture feasibility, marine tourism carrying capacities, and spatial conflicts in maritime zones using marine",
          "spatial planning (MSP) toolkits."
        ),
        "Scientific inquiry across environmental data layers."
      )

      temp_qmd <- file.path(tempdir(), "scholar_report.Rmd")

      fmt <- switch(ext,
        "docx" = "word_document",
        "epub" = "epub_document",
        "pdf" = "pdf_document"
      )

      # Build lines for Rmd
      rmd_lines <- c(
        "---",
        sprintf("title: | \n  | %s\n  | \\vspace{0.8cm}", title_val),
        sprintf("subtitle: | \n  | %s\n  | Scholarly Thematic Track: %s\n  | \\vspace{0.5cm}", subtitle_val, theme_val),
        sprintf("author: | \n  | Compiled by: %s\n  | Sponsoring Organization: IUCN Tanzania\n  | Project: Pamoja Tuhifadhi Bahari Yetu (EU-Funded)", author_val),
        "date: \"`r Sys.Date()`\"",
        "output:",
        sprintf("  %s:", fmt),
        "    toc: true",
        "    toc_depth: 3",
        "    number_sections: true",
        "---",
        "",
        "\\newpage",
        "",
        "# Introduction",
        "This scientific report outlines research activities and data analytics completed under the **Pamoja Tuhifadhi Bahari Yetu Project**, a European Union-funded initiative implemented by the International Union for Conservation of Nature (IUCN). The primary goal of this investigation is to strengthen statistical analysis competencies and transition environmental pipelines into reproducible workflows.",
        "",
        "## Context and Objectives",
        sprintf("The research aligns with the priority thematic area: **%s**.", theme_val),
        "",
        thematic_intro_paragraph,
        "",
        "# Methods",
        "Data wrangling and analysis were executed using the R programming language within a self-contained project structure.",
        "",
        "## Data Wrangling Pipeline",
        "We used the `tidyverse` framework (specifically `dplyr` and `readr`) to load, subset, filter, and summarize the data layers. Relative directory pathing was controlled via the `here` package.",
        "",
        "## Visualizations & Statistics",
        "Descriptive statistics (minimum, maximum, mean, and quantiles) were calculated. Relationships between variables were modeled and visualized using the Grammar of Graphics via `ggplot2` to generate publication-ready plots.",
        "",
        "# Results",
        "The exploratory data analysis reveals key patterns within the selected dataset.",
        "",
        "## Summary Statistics",
        "The computed summary statistics for the target dataset are detailed below:",
        "",
        "```{r echo=FALSE}",
        "summary(rep_data)",
        "```",
        "",
        "## Graphical Visualization",
        sprintf("Figure 1 illustrates the relationship between `%s` and `%s` plotted dynamically using a `%s` geom style.", input$rep_x_var, input$rep_y_var, input$rep_plot_type),
        "",
        "```{r echo=FALSE, fig.width=6, fig.height=4, warning=FALSE, message=FALSE, fig.align='center'}",
        "print(rep_plot)",
        "```",
        "",
        "# Discussion",
        "Scientific narrative and physical oceanography/ecological discussion of the results:",
        "",
        interpretation_val,
        "",
        "# Recommendation",
        "1. **Workflow Automation**: Transition all raw data cleaning spreadsheets to reproducible R scripts to minimize transcription errors.",
        "2. **Data Archiving**: Maintain clean version-controlled datasets at institutional nodes (SUA, UDSM, NM-AIST, SUZA).",
        "3. **Metadata Documentation**: Accompany all custom environmental datasets with robust text metadata files to improve searchability.",
        "",
        "# References",
        "- Wickham, H., 2014. Tidy data. *Journal of Statistical Software*, 59(10), pp.1-23.",
        "- Semba, M., 2026. *Practical Spatial Data in R*. Arusha: NM-AIST Press.",
        "- IUCN, 2026. *Pamoja Tuhifadhi Bahari Yetu Technical Capacity Report*. Dar es Salaam.",
        "",
        "\\newpage",
        "",
        "# Appendix & Index",
        "## Detailed Variables Index",
        "The table below indexes all column names and data types available in the active dataset:",
        "",
        "```{r echo=FALSE}",
        "data.frame(Variable = colnames(rep_data), Type = sapply(rep_data, class))",
        "```"
      )

      writeLines(rmd_lines, temp_qmd)

      rep_data <- rep_loaded_data()
      rep_plot <- rep_ggplot_obj()

      temp_out <- tempfile(fileext = paste0(".", ext))

      compiled_successfully <- tryCatch(
        {
          env <- new.env()
          assign("rep_data", rep_data, envir = env)
          assign("rep_plot", rep_plot, envir = env)
          rmarkdown::render(temp_qmd, output_format = fmt, output_file = temp_out, envir = env, quiet = FALSE)
          if (file.exists(temp_out)) {
            file.copy(temp_out, file, overwrite = TRUE)
            TRUE
          } else {
            FALSE
          }
        },
        error = function(e) {
          message("--- PDF COMPILE ERROR ---")
          message(e$message)
          FALSE
        }
      )

      if (!compiled_successfully) {
        if (ext == "pdf") {
          # Use R's native pdf device to generate a valid, uncorrupt PDF!
          pdf(file, width = 8.5, height = 11)

          # Page 1: Cover Page
          plot(NULL, xlim = c(0, 10), ylim = c(0, 10), type = "n", axes = FALSE, xlab = "", ylab = "")
          rect(-1, -1, 11, 11, col = "#f8fafc", border = NA)
          rect(-1, 8.5, 11, 11, col = "#0A3B5C", border = NA)

          text(5, 7.5, title_val, cex = 1.8, font = 2, col = "#0A3B5C")
          text(5, 7.0, subtitle_val, cex = 1.2, font = 3, col = "#475569")
          text(5, 6.5, sprintf("Scholarly Track: %s", theme_val), cex = 1.0, font = 2, col = "#0F766E")

          text(5, 5.0, sprintf("Author: %s", author_val), cex = 1.1, font = 2)
          text(5, 4.6, "Sponsoring Organization: IUCN Tanzania", cex = 0.9, col = "#64748b")
          text(5, 4.3, "Project: Pamoja Tuhifadhi Bahari Yetu (EU-Funded)", cex = 0.9, col = "#64748b")

          rect(1.5, 2.0, 8.5, 3.0, col = "#ffffff", border = "#e2e8f0")
          text(5, 2.5, "Funded by the European Union & Implemented by IUCN", cex = 0.8, font = 2, col = "#0A3B5C")

          # Page 2: Report Content
          plot(NULL, xlim = c(0, 10), ylim = c(0, 10), type = "n", axes = FALSE, xlab = "", ylab = "")
          text(0.5, 9.5, toupper(title_val), adj = 0, cex = 1.2, font = 2, col = "#0A3B5C")
          abline(h = 9.3, col = "#e2e8f0", lwd = 2)

          # Section 1: Intro
          text(0.5, 8.8, "1. INTRODUCTION", adj = 0, cex = 1.0, font = 2, col = "#0F766E")
          intro_p1 <- "This scientific report outlines research activities and data analytics completed under the Pamoja Tuhifadhi Bahari Yetu Project, a European Union-funded initiative implemented by the International Union for Conservation of Nature (IUCN). The primary goal of this investigation is to strengthen statistical analysis competencies and transition environmental pipelines into reproducible workflows."
          wrapped_intro <- paste(strwrap(intro_p1, width = 70), collapse = "\n")
          text(0.5, 8.0, wrapped_intro, adj = c(0, 1), cex = 0.9)

          wrapped_thematic <- paste(strwrap(thematic_intro_paragraph, width = 70), collapse = "\n")
          text(0.5, 6.3, wrapped_thematic, adj = c(0, 1), cex = 0.9)

          # Section 2: Methods
          text(0.5, 4.6, "2. METHODS", adj = 0, cex = 1.0, font = 2, col = "#0F766E")
          methods_text <- "Data wrangling and analysis were executed using the R programming language. We used the tidyverse framework (specifically dplyr and readr) to load, subset, filter, and summarize the data layers. Descriptive statistics and graphical visualizations (via ggplot2) were dynamically compiled."
          wrapped_methods <- paste(strwrap(methods_text, width = 70), collapse = "\n")
          text(0.5, 3.8, wrapped_methods, adj = c(0, 1), cex = 0.9)

          # Section 3: Results
          text(0.5, 2.3, "3. RESULTS & DYNAMIC VISUALIZATION", adj = 0, cex = 1.0, font = 2, col = "#0F766E")
          results_text <- "Exploratory statistics are summarized below. The dynamic chart displays variable correlation:"
          wrapped_results <- paste(strwrap(results_text, width = 70), collapse = "\n")
          text(0.5, 1.6, wrapped_results, adj = c(0, 1), cex = 0.9)

          # Page 3: Plot Page
          # Embed active plot directly inside fallback PDF page!
          print(rep_plot)

          # Page 4: Discussion & Recommendations & References
          plot(NULL, xlim = c(0, 10), ylim = c(0, 10), type = "n", axes = FALSE, xlab = "", ylab = "")
          text(0.5, 9.5, toupper(title_val), adj = 0, cex = 1.2, font = 2, col = "#0A3B5C")
          abline(h = 9.3, col = "#e2e8f0", lwd = 2)

          # 4. Discussion
          text(0.5, 8.8, "4. DISCUSSION", adj = 0, cex = 1.0, font = 2, col = "#0F766E")
          wrapped_disc <- paste(strwrap(interpretation_val, width = 70), collapse = "\n")
          text(0.5, 8.0, wrapped_disc, adj = c(0, 1), cex = 0.9)

          # 5. Recommendations
          text(0.5, 5.0, "5. RECOMMENDATIONS", adj = 0, cex = 1.0, font = 2, col = "#0F766E")
          rec_text <- "- Transition all raw data cleaning spreadsheets to reproducible R scripts.\n- Maintain version-controlled datasets at institutional nodes (SUA, UDSM, NM-AIST, SUZA).\n- Accompany all custom environmental datasets with robust text metadata documentation."
          text(0.5, 4.2, rec_text, adj = c(0, 1), cex = 0.9)

          # 6. References
          text(0.5, 2.5, "6. REFERENCES", adj = 0, cex = 1.0, font = 2, col = "#0F766E")
          ref_text <- "- Wickham, H., 2014. Tidy data. Journal of Statistical Software, 59(10), pp.1-23.\n- Semba, M., 2026. Practical Spatial Data in R. Arusha: NM-AIST Press.\n- IUCN, 2026. Pamoja Tuhifadhi Bahari Yetu Technical Capacity Report."
          text(0.5, 1.8, ref_text, adj = c(0, 1), cex = 0.85)

          dev.off()
        } else {
          # Comprehensive fallback document writing for Docx, or EPUB
          fallback_text <- c(
            "==================================================================",
            sprintf("TITLE: %s", toupper(title_val)),
            sprintf("SUBTITLE: %s", subtitle_val),
            sprintf("SCHOLARLY THEMATIC TRACK: %s", theme_val),
            sprintf("AUTHOR: %s", author_val),
            sprintf("DATE: %s", Sys.Date()),
            "SPONSORING ORGANIZATION: IUCN Tanzania",
            "PROJECT: Pamoja Tuhifadhi Bahari Yetu (EU-Funded)",
            "==================================================================",
            "",
            "TABLE OF CONTENTS",
            "1. Introduction",
            "2. Methods",
            "3. Results",
            "4. Discussion",
            "5. Recommendation",
            "6. References",
            "7. Appendix & Index",
            "",
            "1. INTRODUCTION",
            "This scientific report outlines research activities and data analytics completed under the Pamoja Tuhifadhi Bahari Yetu Project, a European Union-funded initiative implemented by the International Union for Conservation of Nature (IUCN). The primary goal of this investigation is to strengthen statistical analysis competencies and transition environmental pipelines into reproducible workflows.",
            "",
            thematic_intro_paragraph,
            "",
            "2. METHODS",
            "Data wrangling and analysis were executed using the R programming language within a self-contained project structure. We used the tidyverse framework (specifically dplyr and readr) to load, subset, filter, and summarize the data layers. Descriptive statistics and graphical visualizations (via ggplot2) were dynamically compiled.",
            "",
            "3. RESULTS",
            "Exploratory data analysis has been performed. The summary stats are calculated from raw column layers.",
            "Variable Index list:",
            paste(sprintf("- %s (%s)", colnames(rep_data), sapply(rep_data, class)), collapse = "\n"),
            "",
            "4. DISCUSSION",
            interpretation_val,
            "",
            "5. RECOMMENDATION",
            "1. Transition all raw data cleaning spreadsheets to reproducible R scripts.",
            "2. Maintain version-controlled datasets at institutional nodes.",
            "3. Accompany all custom environmental datasets with robust text metadata documentation.",
            "",
            "6. REFERENCES",
            "- Wickham, H., 2014. Tidy data. Journal of Statistical Software, 59(10), pp.1-23.",
            "- Semba, M., 2026. Practical Spatial Data in R. Arusha: NM-AIST Press.",
            "- IUCN, 2026. Pamoja Tuhifadhi Bahari Yetu Technical Capacity Report. Dar es Salaam.",
            "",
            "7. APPENDIX & INDEX",
            "Calculated variables names list: ",
            paste(colnames(rep_data), collapse = ", ")
          )

          writeLines(fallback_text, file)
        }
      }
    }
  )

  # ---------------------------------------------------------
  # Home Tab: TNA Data Explorer Charts
  # ---------------------------------------------------------
  tna_time_data <- tibble(
    Task = c("Data Import / Formatting", "Data Cleaning", "Data Analysis / Stats", "Writing / Reporting", "Fieldwork"),
    Hours_Pct = c(18.5, 44.5, 15.0, 12.0, 10.0)
  )

  tna_skill_data <- tibble(
    Category = rep(c("Tidy Data", "dplyr Wrangling", "ggplot2 Graphics", "GIS / sf Mapping", "Quarto Reports"), each = 3),
    Level = rep(c("Beginner", "Intermediate", "Advanced"), 5),
    Percentage = c(
      60, 30, 10, # Tidy
      57, 33, 10, # dplyr
      70, 25, 5, # ggplot
      85, 12, 3, # GIS
      90, 8, 2 # Quarto
    )
  )

  tna_exp_data <- tibble(
    Experience = c("Never used R", "Used R once/twice", "Intermediate user", "Regularly write R"),
    Percentage = c(57.1, 28.6, 11.4, 2.9)
  )

  output$tna_plot <- renderPlot({
    chart <- input$tna_chart_select

    if (chart == "time") {
      ggplot(tna_time_data, aes(x = reorder(Task, -Hours_Pct), y = Hours_Pct, fill = Task)) +
        geom_col(show.legend = FALSE, width = 0.6) +
        geom_text(aes(label = sprintf("%.1f%%", Hours_Pct)), vjust = -0.5, fontface = "bold", color = "#0A3B5C") +
        scale_fill_manual(values = c("#0A3B5C", "#E74C3C", "#4A7C59", "#64748b", "#334155")) +
        theme_minimal(base_family = "sans") +
        labs(
          title = "Average Research Project Time Bottlenecks",
          x = "Research Activity",
          y = "Percentage of Project Time (%)"
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 14, color = "#0A3B5C"),
          axis.text.x = element_text(angle = 15, hjust = 1, face = "bold"),
          panel.grid.major.x = element_blank()
        )
    } else if (chart == "skills") {
      tna_skill_data$Level <- factor(tna_skill_data$Level, levels = c("Beginner", "Intermediate", "Advanced"))
      ggplot(tna_skill_data, aes(x = Category, y = Percentage, fill = Level)) +
        geom_col(position = "dodge", width = 0.7) +
        scale_fill_manual(values = c("Beginner" = "#E74C3C", "Intermediate" = "#F1C40F", "Advanced" = "#4A7C59")) +
        theme_minimal(base_family = "sans") +
        labs(
          title = "Cohort R Competency Skill Gaps",
          x = "Training Topic Area",
          y = "Percentage of Cohort (%)",
          fill = "Skill Level"
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 14, color = "#0A3B5C"),
          axis.text.x = element_text(face = "bold"),
          panel.grid.major.x = element_blank()
        )
    } else {
      ggplot(tna_exp_data, aes(x = reorder(Experience, -Percentage), y = Percentage, fill = Experience)) +
        geom_col(show.legend = FALSE, width = 0.5) +
        geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5, fontface = "bold", color = "#0A3B5C") +
        scale_fill_manual(values = c("#0A3B5C", "#4A7C59", "#64748b", "#E74C3C")) +
        theme_minimal(base_family = "sans") +
        labs(
          title = "Self-Reported R Experience Profile",
          x = "Prior R Experience Level",
          y = "Percentage of Scholars (%)"
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 14, color = "#0A3B5C"),
          axis.text.x = element_text(face = "bold"),
          panel.grid.major.x = element_blank()
        )
    }
  })

  # ---------------------------------------------------------
  # Quiz Tab: Interactive Question Parser
  # ---------------------------------------------------------
  # Server Quiz Logic (25 Questions)
  quiz_answers <- list(
    day1 = list(
      q1 = list(
        q = "1. What is the primary operator used for variable assignment in R?",
        choices = c(
          "A. =" = "A",
          "B. <-" = "B",
          "C. ==" = "C",
          "D. <<-" = "D"
        ),
        correct = "B",
        explanation = "Correct! The arrow operator <- is R's standard assignment operator. While = works in some contexts, <- is preferred by R style guidelines."
      ),
      q2 = list(
        q = "2. Which R package is specifically designed for creating relative paths that start from a project directory?",
        choices = c(
          "A. here" = "A",
          "B. tidyverse" = "B",
          "C. readr" = "C"
        ),
        correct = "A",
        explanation = "Correct! The here package resolves file paths relative to your project's root directory, preventing broken paths when sharing code."
      ),
      q3 = list(
        q = "3. In a 'Tidy Data' layout, what does each row represent?",
        choices = c(
          "A. A variable" = "A",
          "B. A cell value" = "B",
          "C. An observation" = "C",
          "D. A dataset" = "D"
        ),
        correct = "C",
        explanation = "Correct! The three rules of tidy data are: (1) each variable forms a column, (2) each observation forms a row, and (3) each value must have its own cell."
      ),
      q4 = list(
        q = "4. Which function from readr is used to load a comma-separated values (.csv) file into R?",
        choices = c(
          "A. read.csv()" = "A",
          "B. read_excel()" = "B",
          "C. read_csv()" = "C",
          "D. load_csv()" = "D"
        ),
        correct = "C",
        explanation = "Correct! read_csv() from the readr package imports CSV files as tidy tibbles, which is faster and cleaner than base R's read.csv()."
      ),
      q5 = list(
        q = "5. What data structure in R is 2-dimensional, can hold heterogeneous data types, and is the standard for tabular data?",
        choices = c(
          "A. Vector" = "A",
          "B. List" = "B",
          "C. Matrix" = "C",
          "D. Data Frame (or Tibble)" = "D"
        ),
        correct = "D",
        explanation = "Correct! Data frames (and tibbles) are 2D tabular structures where different columns can store different data types (numeric, character, logical)."
      )
    ),
    day2 = list(
      q1 = list(
        q = "1. Which dplyr function is used to retain only the rows that meet specific logical criteria?",
        choices = c(
          "A. select()" = "A",
          "B. filter()" = "B",
          "C. mutate()" = "C",
          "D. arrange()" = "D"
        ),
        correct = "B",
        explanation = "Correct! filter() subsets rows based on logical conditions, whereas select() subsets columns."
      ),
      q2 = list(
        q = "2. Which symbol is the native R pipe operator introduced in version 4.1?",
        choices = c(
          "A. %>%" = "A",
          "B. |>" = "B",
          "C. %<%" = "C"
        ),
        correct = "B",
        explanation = "Correct! The native R pipe |> passes the left-hand side object as the first argument to the right-hand side function."
      ),
      q3 = list(
        q = "3. Which dplyr function allows you to calculate new columns based on existing ones?",
        choices = c(
          "A. mutate()" = "A",
          "B. add_column()" = "B",
          "C. summarize()" = "C",
          "D. transmute()" = "D"
        ),
        correct = "A",
        explanation = "Correct! mutate() creates new columns or overwrites existing ones while maintaining the overall row structure."
      ),
      q4 = list(
        q = "4. How does a left_join() combine two tables, x and y?",
        choices = c(
          "A. Keeps all rows from y and matching rows from x" = "A",
          "B. Keeps all rows from x and matching rows from y" = "B",
          "C. Keeps only rows matching in both" = "C",
          "D. Keeps all rows from both tables" = "D"
        ),
        correct = "B",
        explanation = "Correct! left_join(x, y) retains all observations in table x, and merges matching columns from table y based on common keys."
      ),
      q5 = list(
        q = "5. Which combination of dplyr verbs is typically used to calculate aggregated metrics (like averages) across sub-groups?",
        choices = c(
          "A. filter() and mutate()" = "A",
          "B. select() and summarize()" = "B",
          "C. group_by() and summarize()" = "C",
          "D. arrange() and group_by()" = "D"
        ),
        correct = "C",
        explanation = "Correct! You group the data by one or more categorical variables using group_by(), then aggregate using summarize()."
      )
    ),
    day3 = list(
      q1 = list(
        q = "1. What are the three core layers required to display any graphic in ggplot2?",
        choices = c(
          "A. Title, Legend, Axes" = "A",
          "B. Data, Aesthetic Mappings (aes), and Geoms" = "B",
          "C. Colors, Theme, Background" = "C",
          "D. Line, Point, Bar" = "D"
        ),
        correct = "B",
        explanation = "Correct! A basic ggplot needs: (1) your data frame, (2) visual mappings aes(), and (3) a geometric representation layer (geom) like geom_point()."
      ),
      q2 = list(
        q = "2. Which aesthetic mapping is used to separate data points visually by group using outline colors in geom_point()?",
        choices = c(
          "A. fill" = "A",
          "B. color" = "B",
          "C. stroke" = "C"
        ),
        correct = "B",
        explanation = "Correct! In geom_point(), the color aesthetic changes the outline/point color. For 2D geoms like bars or boxplots, fill changes the interior color."
      ),
      q3 = list(
        q = "3. What ggplot2 function is used to split a single plot into multiple sub-plots (panels) based on a categorical variable?",
        choices = c(
          "A. split_plot()" = "A",
          "B. facet_wrap()" = "B",
          "C. panel_grid()" = "C",
          "D. layout_wrap()" = "D"
        ),
        correct = "B",
        explanation = "Correct! facet_wrap() (and facet_grid()) splits the data into sub-panels, rendering a separate plot for each level of the factor."
      ),
      q4 = list(
        q = "4. What does the dpi parameter control inside ggsave()?",
        choices = c(
          "A. The font size" = "A",
          "B. The plot margins" = "B",
          "C. The resolution (dots per inch)" = "C",
          "D. The aspect ratio" = "D"
        ),
        correct = "C",
        explanation = "Correct! dpi stands for Dots Per Inch, which controls the resolution. Journals typically require 300 to 600 DPI for publication-ready figures."
      ),
      q5 = list(
        q = "5. Which theme removes the default gray background grid and leaves a clean minimalist white background in ggplot2?",
        choices = c(
          "A. theme_gray()" = "A",
          "B. theme_classic()" = "B",
          "C. theme_dark()" = "C",
          "D. theme_void()" = "D"
        ),
        correct = "B",
        explanation = "Correct! theme_classic() provides a clean look with no gridlines and simple black axis lines, making it popular for scientific papers."
      )
    ),
    day4 = list(
      q1 = list(
        q = "1. Which package is the modern standard for handling vector spatial data (points, lines, polygons) as standard data frames in R?",
        choices = c(
          "A. sp" = "A",
          "B. sf (Simple Features)" = "B",
          "C. rgdal" = "C"
        ),
        correct = "B",
        explanation = "Correct! The sf package represents spatial features as standard data frames with a list-column containing geometry structures."
      ),
      q2 = list(
        q = "2. Which coordinate reference system (CRS) code represents the unprojected global GPS standard (WGS 84)?",
        choices = c(
          "A. EPSG:32737" = "A",
          "B. EPSG:4326" = "B",
          "C. EPSG:3857" = "C",
          "D. EPSG:21037" = "D"
        ),
        correct = "B",
        explanation = "Correct! EPSG:4326 is the coordinate reference system (CRS) for unprojected WGS 84 lat/long coordinates."
      ),
      q3 = list(
        q = "3. What package is used in this Applied Statistics course to perform parametric t-tests and generate automated scholarly report outputs?",
        choices = c(
          "A. car" = "A",
          "B. rstatix" = "B",
          "C. testflow" = "C",
          "D. stats" = "D"
        ),
        correct = "C",
        explanation = "Correct! testflow is the custom package installed for standard parametric tests, t-test reporting, and linear regression diagnostic pipelines."
      ),
      q4 = list(
        q = "4. Which diagnostic plot is primarily used to evaluate if the residuals of a linear model follow a normal distribution?",
        choices = c(
          "A. Residuals vs Fitted" = "A",
          "B. Normal Q-Q Plot" = "B",
          "C. Scale-Location" = "C",
          "D. Residuals vs Leverage" = "D"
        ),
        correct = "B",
        explanation = "Correct! The Normal Q-Q plot compares the distribution of standardized residuals against theoretical normal quantiles. Linear points indicate normality."
      ),
      q5 = list(
        q = "5. What sf function is used to transform spatial coordinate reference systems (e.g. from GPS lat/lon to projected UTM)?",
        choices = c(
          "A. st_crs()" = "A",
          "B. st_transform()" = "B",
          "C. st_reproject()" = "C"
        ),
        correct = "B",
        explanation = "Correct! st_transform() changes the coordinate reference system (CRS) of simple features objects using proj library bindings."
      )
    ),
    day5 = list(
      q1 = list(
        q = "1. What markup block at the very top of a Quarto .qmd file is used to configure title, authors, date, and output formats?",
        choices = c(
          "A. HTML head tags" = "A",
          "B. YAML frontmatter" = "B",
          "C. XML tags" = "C",
          "D. CSS styles" = "D"
        ),
        correct = "B",
        explanation = "Correct! The YAML block (enclosed between three dashes ---) configures compilation settings and document parameters."
      ),
      q2 = list(
        q = "2. How do you write a comment or chunk option inside an R code chunk in Quarto?",
        choices = c(
          "A. #| option: value" = "A",
          "B. // option: value" = "B",
          "C. <!-- option -->" = "C"
        ),
        correct = "A",
        explanation = "Correct! Quarto uses the special comment prefix #| (hash-pipe) inside code chunks to specify execution parameters."
      ),
      q3 = list(
        q = "3. Which chunk option is configured to hide code warning messages from appearing in the rendered output?",
        choices = c(
          "A. echo: false" = "A",
          "B. eval: false" = "B",
          "C. warning: false" = "C",
          "D. message: false" = "D"
        ),
        correct = "C",
        explanation = "Correct! Setting #| warning: false suppresses warnings from R packages and function calls inside the final rendered document."
      ),
      q4 = list(
        q = "4. What command-line tool (or RStudio button) is triggered to compile a .qmd file into HTML, Word, or PDF?",
        choices = c(
          "A. knit()" = "A",
          "B. render()" = "B",
          "C. compile()" = "C",
          "D. quarto render" = "D"
        ),
        correct = "D",
        explanation = "Correct! Running quarto render filename.qmd in the command line calls the Quarto compiler to build final outputs."
      ),
      q5 = list(
        q = "5. Which execution option allows you to run the code chunk but hide the R script input block itself, showing only the plots/tables?",
        choices = c(
          "A. echo: false" = "A",
          "B. include: false" = "B",
          "C. eval: false" = "C"
        ),
        correct = "A",
        explanation = "Correct! #| echo: false hides the R source code block while letting the code execute and print charts or tables."
      )
    )
  )

  # Reactive score tracking
  quiz_state <- reactiveValues(
    submitted = character(),
    correct_count = 0,
    score_status = list(),
    current_feedback = NULL
  )

  output$quiz_question_ui <- renderUI({
    day_key <- input$quiz_day
    q_key <- input$quiz_question_num
    q_data <- quiz_answers[[day_key]][[q_key]]

    radioButtons("quiz_choice",
      label = q_data$q,
      choices = q_data$choices, selected = character(0)
    )
  })

  observeEvent(input$quiz_submit, {
    day_key <- input$quiz_day
    q_key <- input$quiz_question_num
    q_data <- quiz_answers[[day_key]][[q_key]]
    user_choice <- input$quiz_choice

    if (is.null(user_choice) || user_choice == "") {
      quiz_state$current_feedback <- div(class = "alert alert-warning", "Please select an answer option first.")
      return()
    }

    is_correct <- (user_choice == q_data$correct)
    combined_key <- paste(day_key, q_key, sep = "_")

    # Check if already counted in score
    if (!(combined_key %in% quiz_state$submitted)) {
      quiz_state$submitted <- c(quiz_state$submitted, combined_key)
      quiz_state$score_status[[combined_key]] <- is_correct
      if (is_correct) {
        quiz_state$correct_count <- quiz_state$correct_count + 1
      }
    }

    if (is_correct) {
      quiz_state$current_feedback <- div(
        class = "custom-card", style = "border-left: 5px solid #2ECC71; background: #f0fdf4; padding: 1.5rem; min-height: 400px;",
        h4(style = "color:#2ECC71; font-weight:700; margin-top:0;", icon("check-circle"), "Correct!"),
        p(q_data$explanation, style = "color:#1e293b; font-size:0.95rem; margin-bottom:0;")
      )
    } else {
      quiz_state$current_feedback <- div(
        class = "custom-card", style = "border-left: 5px solid #E74C3C; background: #fef2f2; padding: 1.5rem; min-height: 400px;",
        h4(style = "color:#E74C3C; font-weight:700; margin-top:0;", icon("times-circle"), "Incorrect Answer"),
        p("That is not correct. Try studying the curriculum outline or reviewing the day tab, then try again!", style = "color:#1e293b; font-size:0.95rem; margin-bottom:0;")
      )
    }
  })

  # Clear feedback when switching questions
  observe({
    input$quiz_day
    input$quiz_question_num
    quiz_state$current_feedback <- NULL
  })

  output$quiz_feedback_ui <- renderUI({
    if (is.null(quiz_state$current_feedback)) {
      div(
        class = "custom-card", style = "min-height:400px; display:flex; align-items:center; justify-content:center; border-style:dashed; border-color:#cbd5e1; background:transparent;",
        div(
          style = "text-align:center; color:#94a3b8;",
          icon("lightbulb", style = "font-size:3rem; margin-bottom:1rem;"),
          p("Select an answer and submit to view explanations.")
        )
      )
    } else {
      quiz_state$current_feedback
    }
  })

  render_svg_gauge <- function(score, total) {
    if (total == 0) {
      pct <- 0
      label <- "0%"
      gradient_id <- "gradient-gray"
      glow_color <- "#94a3b8"
      score_text <- "Quiz Not Started"
    } else {
      pct <- round((score / total) * 100)
      label <- sprintf("%d%%", pct)
      gradient_id <- if (pct >= 80) "gradient-green" else if (pct >= 50) "gradient-yellow" else "gradient-red"
      glow_color <- if (pct >= 80) "#10B981" else if (pct >= 50) "#F59E0B" else "#EF4444"
      score_text <- sprintf("Score: %d / %d Correct", score, total)
    }

    circumference <- 314.16
    dashoffset <- circumference * (1 - pct / 100)

    HTML(sprintf('
      <div style="display: flex; flex-direction: column; align-items: center; justify-content: center; padding: 0.5rem 0;">
        <svg width="140" height="140" viewBox="0 0 120 120" style="filter: drop-shadow(0px 6px 12px rgba(10, 59, 92, 0.08));">
          <defs>
            <filter id="glow-%s" x="-20%%" y="-20%%" width="140%%" height="140%%">
              <feDropShadow dx="0" dy="3" stdDeviation="3" flood-color="%s" flood-opacity="0.3" />
            </filter>

            <linearGradient id="gradient-green" x1="0%%" y1="0%%" x2="100%%" y2="100%%">
              <stop offset="0%%" stop-color="#34D399" />
              <stop offset="100%%" stop-color="#059669" />
            </linearGradient>
            <linearGradient id="gradient-yellow" x1="0%%" y1="0%%" x2="100%%" y2="100%%">
              <stop offset="0%%" stop-color="#FBBF24" />
              <stop offset="100%%" stop-color="#D97706" />
            </linearGradient>
            <linearGradient id="gradient-red" x1="0%%" y1="0%%" x2="100%%" y2="100%%">
              <stop offset="0%%" stop-color="#F87171" />
              <stop offset="100%%" stop-color="#DC2626" />
            </linearGradient>
            <linearGradient id="gradient-gray" x1="0%%" y1="0%%" x2="100%%" y2="100%%">
              <stop offset="0%%" stop-color="#CBD5E1" />
              <stop offset="100%%" stop-color="#94A3B8" />
            </linearGradient>
          </defs>

          <circle cx="60" cy="60" r="50" fill="none" stroke="#F8FAFC" stroke-width="11"></circle>
          <circle cx="60" cy="60" r="50" fill="none" stroke="#E2E8F0" stroke-width="11" stroke-opacity="0.6"></circle>
          <circle cx="60" cy="60" r="44" fill="#FFFFFF"></circle>

          <circle cx="60" cy="60" r="50" fill="none"
                  stroke="url(#%s)" stroke-width="11"
                  stroke-dasharray="314.16" stroke-dashoffset="%f"
                  stroke-linecap="round"
                  filter="url(#glow-%s)"
                  transform="rotate(-90 60 60)"
                  style="transition: stroke-dashoffset 0.8s cubic-bezier(0.4, 0, 0.2, 1);"></circle>

          <text x="60" y="58" text-anchor="middle" font-family="\'Outfit\', sans-serif" font-size="1.7rem" font-weight="900" fill="#0A3B5C">%s</text>
          <text x="60" y="74" text-anchor="middle" font-family="\'Inter\', sans-serif" font-size="0.65rem" font-weight="800" fill="#64748B" letter-spacing="1.5px">SCORE</text>
        </svg>
        <div style="margin-top: 0.8rem; font-size: 0.92rem; font-weight: 700; color: #334155; font-family: \'Outfit\', sans-serif; text-align: center;">
          %s
        </div>
      </div>
    ', gradient_id, glow_color, gradient_id, dashoffset, gradient_id, label, score_text))
  }

  output$quiz_score_ratio <- renderUI({
    render_svg_gauge(quiz_state$correct_count, length(quiz_state$submitted))
  })

  # ---------------------------------------------------------
  # Facilitator Tab: Maps & Info
  # ---------------------------------------------------------
  output$venue_map <- renderLeaflet({
    leaflet() |>
      addTiles(group = "OpenStreetMap") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
      addProviderTiles(providers$CartoDB.Positron, group = "Light (CartoDB)") |>
      setView(lng = 37.660060809193034, lat = -6.801425980395493, zoom = 14) |>
      addMarkers(
        lng = 37.660060809193034, lat = -6.801425980395493,
        popup = "<strong>EDEMA Conference Hall</strong><br/>Morogoro, Tanzania<br/>Venue of the IUCN Bahari Yetu Applied Statistics Program",
        options = markerOptions(zIndexOffset = 1000)
      ) |>
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Satellite", "Light (CartoDB)"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  observeEvent(input$calculate_route, {
    start_lat <- input$accom_lat
    start_lon <- input$accom_lon
    mode <- input$travel_mode

    if (is.null(start_lat) || is.null(start_lon)) {
      return()
    }

    # Query OSRM API (http://router.project-osrm.org/route/v1/mode/lon1,lat1;lon2,lat2)
    # Target: Edema Hall coordinates (37.660060809193034, -6.801425980395493)
    url <- sprintf(
      "http://router.project-osrm.org/route/v1/%s/%f,%f;37.660060809193034,-6.801425980395493?overview=full&geometries=geojson",
      mode, start_lon, start_lat
    )

    res <- tryCatch(
      {
        jsonlite::fromJSON(url)
      },
      error = function(e) {
        NULL
      }
    )

    if (is.null(res) || res$code != "Ok") {
      output$route_summary <- renderUI({
        div(
          class = "alert alert-danger", style = "margin-top:0;",
          "Error: Could not retrieve route coordinates from OSRM. Please check your internet connection or start location."
        )
      })
      return()
    }

    # Parse OSRM response details
    dist_km <- round(res$routes$distance / 1000, 2)
    duration_min <- round(res$routes$duration / 60, 1)

    # Get geometry path coordinates (N x 2 matrix: Lon, Lat)
    coords <- res$routes$geometry$coordinates[[1]]
    lons <- coords[, 1]
    lats <- coords[, 2]

    output$route_summary <- renderUI({
      mode_icon <- if (mode == "driving") "car" else "walking"
      mode_label <- if (mode == "driving") "Driving (Car)" else "Walking (Foot)"

      div(
        class = "custom-card", style = "margin-top:0; border-left: 5px solid #0A3B5C; background: #f8fafc; padding: 1.2rem; height:100%; box-shadow:none;",
        h5(style = "margin-top:0; font-weight:700; color:#0A3B5C;", "Calculated Route:"),
        fluidRow(
          column(
            width = 12,
            div(
              style = "display:flex; align-items:center; gap:0.8rem; margin-bottom:1rem;",
              icon(mode_icon, style = "font-size:1.5rem; color:#4A7C59;"),
              span(style = "font-weight:600; color:#334155; font-size:0.95rem;", mode_label)
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            div(
              div(style = "font-size:1.5rem; font-weight:800; color:#0A3B5C; line-height:1.2;", sprintf("%.2f km", dist_km)),
              div(style = "font-size:0.75rem; font-weight:600; color:#64748b;", "Distance")
            )
          ),
          column(
            width = 6,
            div(
              div(style = "font-size:1.5rem; font-weight:800; color:#0A3B5C; line-height:1.2;", sprintf("%.1f min", duration_min)),
              div(style = "font-size:0.75rem; font-weight:600; color:#64748b;", "Est. Travel Time")
            )
          )
        )
      )
    })

    leafletProxy("venue_map") |>
      clearGroup("route_path") |>
      addMarkers(
        lng = 37.660060809193034, lat = -6.801425980395493,
        popup = "<strong>EDEMA Conference Hall</strong><br/>Morogoro, Tanzania",
        group = "route_path"
      ) |>
      addMarkers(
        lng = start_lon, lat = start_lat,
        popup = "Start Location (Accommodation)",
        group = "route_path"
      ) |>
      addPolylines(
        lng = lons, lat = lats,
        color = if (mode == "driving") "#0A3B5C" else "#4A7C59",
        weight = 5, opacity = 0.8,
        group = "route_path"
      ) |>
      fitBounds(
        lng1 = min(lons, 37.66006), lat1 = min(lats, -6.80143),
        lng2 = max(lons, 37.66006), lat2 = max(lats, -6.80143)
      )
  })

  # =========================================================
  # STATS GUIDE SERVER LOGIC
  # =========================================================

  # Reusable function to set up server reactions for Stats Guide thematic subtabs
  setup_thematic_stats <- function(id, file_path, x_var, y_var, group_var) {
    # Load dataset
    data_reactive <- reactive({
      read.csv(file_path)
    })

    # 1. Descriptive Stats Table
    output[[paste0(id, "-desc_table")]] <- renderDT({
      df <- data_reactive()
      stats <- df %>% get_summary_stats(!!sym(y_var), type = "common")
      datatable(stats, options = list(dom = "t", scrollX = TRUE), rownames = FALSE, class = "cell-border stripe")
    })

    # 2. T-Test Table
    output[[paste0(id, "-ttest_table")]] <- renderDT({
      df <- data_reactive()
      df[[group_var]] <- as.factor(df[[group_var]])
      test_res <- df %>% t_test(as.formula(sprintf("`%s` ~ `%s`", y_var, group_var)))
      datatable(test_res, options = list(dom = "t", scrollX = TRUE), rownames = FALSE, class = "cell-border stripe")
    })

    # 3. T-Test Visualization (Boxplot)
    output[[paste0(id, "-ttest_plot")]] <- renderPlot({
      df <- data_reactive()
      df[[group_var]] <- as.factor(df[[group_var]])

      test_res <- df %>% t_test(as.formula(sprintf("`%s` ~ `%s`", y_var, group_var)))
      p_val <- if (nrow(test_res) > 0) test_res$p[1] else NA
      p_text <- if (!is.na(p_val)) {
        if (p_val < 0.001) "p < 0.001" else sprintf("p = %.4f", p_val)
      } else {
        ""
      }

      p <- ggplot(df, aes_string(x = group_var, y = y_var, fill = group_var)) +
        geom_boxplot(alpha = 0.7, width = 0.5) +
        geom_jitter(width = 0.15, alpha = 0.4, color = "#475569") +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", color = "#0A3B5C"),
          axis.title = element_text(face = "bold"),
          legend.position = "none"
        ) +
        labs(
          title = sprintf("Distribution of %s by %s", y_var, group_var),
          x = group_var, y = y_var
        ) +
        scale_fill_brewer(palette = "Set2")

      if (p_text != "") {
        y_max <- max(df[[y_var]], na.rm = TRUE)
        y_min <- min(df[[y_var]], na.rm = TRUE)
        y_pos <- y_max + (y_max - y_min) * 0.05
        p <- p + annotate("text", x = 1.5, y = y_pos, label = sprintf("T-Test: %s", p_text), fontface = "bold", color = "#b91c1c", size = 4.5)
      }
      p
    })

    # 4. ANOVA Table
    output[[paste0(id, "-anova_table")]] <- renderDT({
      df <- data_reactive()
      df[[group_var]] <- as.factor(df[[group_var]])
      anova_res <- df %>% anova_test(as.formula(sprintf("`%s` ~ `%s`", y_var, group_var)))
      anova_df <- as.data.frame(anova_res)
      datatable(anova_df, options = list(dom = "t", scrollX = TRUE), rownames = FALSE, class = "cell-border stripe")
    })

    # 5. Correlation Matrix Table
    output[[paste0(id, "-cormat_table")]] <- renderDT({
      df <- data_reactive()
      numeric_df <- df %>% select_if(is.numeric)
      if (ncol(numeric_df) >= 2) {
        cor_mat_obj <- numeric_df %>%
          cor_mat() %>%
          cor_mark_significant()
        datatable(cor_mat_obj, options = list(dom = "t", scrollX = TRUE), rownames = FALSE, class = "cell-border stripe")
      } else {
        datatable(data.frame(Status = "Insufficient numeric variables for correlation matrix"), options = list(dom = "t"))
      }
    })
  }

  # Render UIs for the 8 subtabs
  output$stats_ghg_ui <- renderUI({
    stats_tab_content_ui("stats_ghg", "Greenhouse Gas (GHG) Inventories", "theme2_ghg_ippu.csv", "clinker_factor", "cement_produced_tonnes", "facility_id", "Comparing industrial cement production levels across multiple regional manufacturing facilities.")
  })
  output$stats_marine_ui <- renderUI({
    stats_tab_content_ui("stats_marine", "Marine Ecology", "theme4_marine_ecology.csv", "boat_density_index", "dolphin_detections", "site", "Investigating dolphin presence counts relative to human conservation and research sites.")
  })
  output$stats_forest_ui <- renderUI({
    stats_tab_content_ui("stats_forest", "Coastal Forests", "theme1_coastal_forest.csv", "height_m", "dbh_cm", "tree_species", "Comparing DBH (Diameter at Breast Height) dimensions across major coastal tree species.")
  })
  output$stats_plastics_ui <- renderUI({
    stats_tab_content_ui("stats_plastics", "Marine Plastics & Waste Mitigation", "theme6_plastic_waste.csv", "station_id", "macroplastics_count", "location", "Analyzing macroplastic count differences collected across distinct coastal research locations.")
  })
  output$stats_biodiv_ui <- renderUI({
    stats_tab_content_ui("stats_biodiv", "Biodiversity Conservation", "theme4_marine_ecology.csv", "boat_density_index", "dolphin_detections", "site", "Studying marine biodiversity proxies and species counts across targeted conservation areas.")
  })
  output$stats_fisheries_ui <- renderUI({
    stats_tab_content_ui("stats_fisheries", "Fisheries Stock Assessment", "theme4_marine_ecology.csv", "site", "boat_density_index", "site", "Monitoring fishing activity proxy indexes across local ports and coastal sites.")
  })
  output$stats_climate_ui <- renderUI({
    stats_tab_content_ui("stats_climate", "Climate Change Adaptation", "theme5_ghg_afolu.csv", "soil_organic_carbon_pct", "yield_tonnes_ha", "crop_type", "Assessing agricultural yields across crop varieties under climate resilience models.")
  })
  output$stats_blue_ui <- renderUI({
    stats_tab_content_ui("stats_blue", "Blue Economy Development", "theme7_ghg_waste.csv", "day", "daily_methane_yield_ml_g", "feedstock", "Evaluating renewable biogas yields from marine feedstock sources.")
  })

  # Setup the server reactive bindings for the 8 subtabs
  setup_thematic_stats("stats_ghg", "data/theme2_ghg_ippu.csv", "clinker_factor", "cement_produced_tonnes", "facility_id")
  setup_thematic_stats("stats_marine", "data/theme4_marine_ecology.csv", "boat_density_index", "dolphin_detections", "site")
  setup_thematic_stats("stats_forest", "data/theme1_coastal_forest.csv", "height_m", "dbh_cm", "tree_species")
  setup_thematic_stats("stats_plastics", "data/theme6_plastic_waste.csv", "station_id", "macroplastics_count", "location")
  setup_thematic_stats("stats_biodiv", "data/theme4_marine_ecology.csv", "boat_density_index", "dolphin_detections", "site")
  setup_thematic_stats("stats_fisheries", "data/theme4_marine_ecology.csv", "site", "boat_density_index", "site")
  setup_thematic_stats("stats_climate", "data/theme5_ghg_afolu.csv", "soil_organic_carbon_pct", "yield_tonnes_ha", "crop_type")
  setup_thematic_stats("stats_blue", "data/theme7_ghg_waste.csv", "day", "daily_methane_yield_ml_g", "feedstock")

  # =========================================================
  # STATS GUIDE ON-SITE AI CO-PILOT
  # =========================================================
  stats_ai_result <- reactiveVal("")

  observeEvent(input$stats_trigger_ai, {
    query <- tolower(trimws(input$stats_ai_prompt_input))
    if (query == "") {
      stats_ai_result("Please type a question to get started (e.g. 'how to do t-test' or 'explain p-value').")
      return()
    }

    response <- ""

    if (grepl("p-value|p value|significance|significant|alpha", query)) {
      response <- paste(
        "🤖 AI Stats Assistant:\n\n",
        "• **P-value**: The probability of obtaining a test statistic at least as extreme as the observed value, assuming the null hypothesis (H0) is true.\n",
        "• **Interpretation Rule**:\n",
        "  - **p < 0.05**: Reject H0. The difference is statistically significant.\n",
        "  - **p >= 0.05**: Fail to reject H0. No statistically significant difference.\n",
        "• **Alpha Level (α)**: The significance threshold (usually 0.05). If p < α, it means the observed outcome is highly unlikely to have occurred by chance alone."
      )
    } else if (grepl("t-test|t test|compare|means|group", query) && !grepl("anova", query)) {
      response <- paste(
        "🤖 AI Stats Assistant:\n\n",
        "• **Comparing Two Means (T-Test)**: Used to evaluate if the means of two groups are statistically different.\n",
        "• **Using rstatix in R**:\n",
        "  - **Independent T-Test** (different groups): `df |> t_test(response ~ group, paired = FALSE)`\n",
        "  - **Paired T-Test** (same subjects measured twice): `df |> t_test(response ~ group, paired = TRUE)`\n",
        "  - **One-Sample T-Test** (compare to a target mean): `df |> t_test(response ~ 1, mu = target_value)`\n",
        "• **Non-parametric Alternative**: If data normality is violated, run a Wilcoxon test using `wilcox_test(response ~ group)`."
      )
    } else if (grepl("anova|f-test|f test|variance", query)) {
      response <- paste(
        "🤖 AI Stats Assistant:\n\n",
        "• **ANOVA (Analysis of Variance)**: Compares the means of three or more groups to determine if at least one group mean is different.\n",
        "• **Using rstatix in R**:\n",
        "  - **One-Way ANOVA**: `df |> anova_test(response ~ group)`\n",
        "  - **Two-Way ANOVA** (with interaction): `df |> anova_test(response ~ factor1 * factor2)`\n",
        "  - **Repeated Measures ANOVA** (longitudinal/within-subject): `df |> anova_test(dv = response, wid = id, within = factor)`\n",
        "• **Post-hoc Test**: After a significant ANOVA, perform pairwise comparisons with `tukey_hsd()` or `pairwise_t_test()` to pinpoint which groups differ."
      )
    } else if (grepl("correlation|cor|matrix|pearson|spearman", query)) {
      response <- paste(
        "🤖 AI Stats Assistant:\n\n",
        "• **Correlation Analysis**: Measures the strength and direction of the linear relationship between two continuous variables.\n",
        "• **Using rstatix in R**:\n",
        "  - **Two variables**: `df |> cor_test(var1, var2, method = 'pearson')`\n",
        "  - **Correlation Matrix**: `df |> cor_mat()`\n",
        "  - **Flag significance**: `df |> cor_mat() |> cor_mark_significant()`\n",
        "• **Correlation Coefficient (r)**:\n",
        "  - **+1**: Perfect positive relationship\n",
        "  - **-1**: Perfect negative relationship\n",
        "  - **0**: No linear association"
      )
    } else if (grepl("normality|assumption|shapiro|levene|variance|assumptions", query)) {
      response <- paste(
        "🤖 AI Stats Assistant:\n\n",
        "• **Assumption Testing**: Before running parametric tests (T-Test, ANOVA), check these criteria:\n",
        "  1. **Normality**: Data should be normally distributed. Use `df |> shapiro_test(variable)`.\n",
        "  2. **Homogeneity of Variance**: Spread of variance across groups should be equal. Use `df |> levene_test(response ~ group)`.\n",
        "• **Violation Guide**: If assumptions are violated, use non-parametric tests like `wilcox_test()` or `kruskal_test()`."
      )
    } else if (grepl("hypothesis|hypotheses|h0|h1", query)) {
      response <- paste(
        "🤖 AI Stats Assistant:\n\n",
        "• **Null Hypothesis (H0)**: States there is no effect, no difference, or no association (e.g., 'Group A mean = Group B mean').\n",
        "• **Alternative Hypothesis (H1/HA)**: States there is a difference or effect (e.g., 'Group A mean != Group B mean').\n",
        "• **Testing Workflow**:\n",
        "  1. Formulate H0 and H1.\n",
        "  2. Choose and execute the test.\n",
        "  3. Check the p-value. If p < 0.05, reject H0 in favor of H1."
      )
    } else if (grepl("rstatix|install|load|library|kassambara|devtools|package", query) && !grepl("missing|outlier", query)) {
      response <- paste(
        "🤖 AI Stats Assistant:\n\n",
        "• **rstatix Package**: A pipe-friendly framework for running basic statistical tests in R, aligning perfectly with the tidyverse.\n",
        "• **Installation**:\n",
        "  - CRAN: `install.packages('rstatix')`\n",
        "  - GitHub (Dev version): `devtools::install_github('kassambara/rstatix')`\n",
        "• **Troubleshooting Installation**:\n",
        "  - If devtools fails, ensure compilation tools are installed (Rtools on Windows, Xcode CLI on macOS).\n",
        "  - Verify internet connection or use a localized mirror: `install.packages('rstatix', repos='http://cran.us.r-project.org')`"
      )
    } else if (grepl("missing|na|nan|outlier|clean|outliers|tidy", query)) {
      response <- paste(
        "🤖 AI Stats Assistant:\n\n",
        "• **Handling Missing Data (NAs)**:\n",
        "  - Remove rows with NAs: `df |> na.omit()` or `df |> filter(!is.na(variable))`\n",
        "  - Replace NAs with mean/median: `df |> mutate(variable = ifelse(is.na(variable), mean(variable, na.rm=TRUE), variable))`\n",
        "• **Identifying Outliers (rstatix)**:\n",
        "  - Identify outliers: `df |> identify_outliers(variable)`\n",
        "  - Filter outliers: Extreme outliers can skew results. Consider winsorizing or running non-parametric tests if outliers cannot be removed."
      )
    } else if (grepl("pdf|docx|epub|compile|render|pandoc|quarto|markdown|download|report", query)) {
      response <- paste(
        "🤖 AI Stats Assistant:\n\n",
        "• **Document Compilation Issues**:\n",
        "  - **Pandoc Path**: Positron users must locate Pandoc using: `rmarkdown::find_pandoc(dir = 'C:/Program Files/Positron/resources/app/quarto/bin/tools')`.\n",
        "  - **Corrupted PDF**: If pandoc/LaTeX is missing, a custom vector-device fallback draws sections (Intro, Methods, Results, Discussion) and active ggplot2 charts to prevent corrupt downloads.\n",
        "  - **File Extension Enforcing**: When compiling reports, name files with exact extensions (`temp_out <- tempfile(fileext = paste0('.', ext))`) before downloading to ensure format alignment."
      )
    } else if (grepl("ggplot|plot|chart|export|color|theme|ggsave|palette", query)) {
      response <- paste(
        "🤖 AI Stats Assistant:\n\n",
        "• **Plotting & Visual Customization**:\n",
        "  - **Saving plots**: Use `ggsave('my_plot.png', plot = p, width = 8, height = 6, dpi = 300)`.\n",
        "  - **Fonts**: In PDF graphics devices, avoid custom web fonts (like Inter/Outfit) to prevent font-not-found errors. Stick to standard families (e.g. `\"sans\"`, `\"serif\"`, `\"Helvetica\"`).\n",
        "  - **Color Palettes**: Use colorblind-friendly scales, like `scale_color_viridis_d()` or `scale_fill_brewer(palette = 'Dark2')`."
      )
    } else if (grepl("dplyr|tidyverse|filter|mutate|select|pipe|\\|>|\\%>\\%", query)) {
      response <- paste(
        "🤖 AI Stats Assistant:\n\n",
        "• **Data Wrangling (dplyr & Tidyverse)**:\n",
        "  - **Filter rows**: `df |> filter(variable > threshold)`\n",
        "  - **Mutate/Create columns**: `df |> mutate(new_col = colA * colB)`\n",
        "  - **Select columns**: `df |> select(colA, colB)`\n",
        "  - **Pipes**: The native R pipe (`|>`) passes left-hand results to the first argument of the right-hand function. It works identically to magrittr's `%>%` in modern workflows."
      )
    } else if (grepl("ghg|greenhouse|ipcc|emission|clinker|afolu|ippu", query)) {
      response <- paste(
        "🤖 AI Stats Assistant:\n\n",
        "• **Greenhouse Gas (GHG) Inventories & IPCC guidelines**:\n",
        "  - **IPPU Sector**: Industrial Processes and Product Use covers emissions from chemical processes. Example: cement clinker calcination.\n",
        "  - **Clinker Factor**: Represents the ratio of clinker to total cement. Reducing this factor (e.g., using calcined clay) decreases process CO2 emissions.\n",
        "  - **AFOLU Sector**: Agriculture, Forestry, and Other Land Use. Focuses on soil organic carbon percent (SOC%) and livestock carbon pools."
      )
    } else if (grepl("marine|dolphin|plastic|waste|ecology|plastics", query)) {
      response <- paste(
        "🤖 AI Stats Assistant:\n\n",
        "• **Marine Ecology & Plastic Mitigation**:\n",
        "  - **Marine Ecology**: Dolphin detections count models are assessed against human disturbances (boat density index).\n",
        "  - **Plastics & Waste**: Plastic debris density tracks micro/macroplastics collected across research stations to identify waste mitigation targets and evaluate circular community cleanups."
      )
    } else if (grepl("help|guide|topics|info|list", query)) {
      response <- paste(
        "🤖 AI Stats Assistant:\n\n",
        "I am ready to help you with biostatistics and R issues! Type keywords related to:\n",
        "• **t-test** / **anova** / **correlation**: Statistical comparisons.\n",
        "• **p-value** / **significance** / **hypothesis**: Statistical reasoning.\n",
        "• **normality** / **assumptions**: Assumption testing criteria.\n",
        "• **installation** / **library**: Package loading errors.\n",
        "• **missing** / **na** / **clean**: Data cleaning and handling NAs.\n",
        "• **pdf** / **compile** / **quarto**: Export and render issues.\n",
        "• **ggplot** / **ggsave** / **color**: Plot styles and styling.\n",
        "• **dplyr** / **pipe**: Data wrangling operations.\n",
        "• **ghg** / **ipcc** / **marine**: Scholar thematic area contexts."
      )
    } else {
      response <- paste(
        "🤖 AI Stats Assistant:\n\n",
        "I recognized your request, but I'm specialized in biostatistics and R development issues. Try asking:\n",
        "• 'How do I perform a t-test?'\n",
        "• 'Explain ANOVA models'\n",
        "• 'What does p-value mean?'\n",
        "• 'How do I fix a PDF compile error?'\n",
        "• 'How do I deal with missing NA data?'"
      )
    }

    stats_ai_result(response)
  })
  output$stats_ai_response <- renderUI({
    res <- stats_ai_result()
    if (res == "") {
      "AI Assistant is ready. Enter your question in the box above and click 'Ask Copilot'."
    } else {
      tags$div(
        style = "white-space: pre-wrap; font-family: 'Inter', sans-serif;",
        res
      )
    }
  })
  # SERVER COMPONENT: Network Directory & Scholar Registry
  # ---------------------------------------------------------
  initial_profiles <- list(
    list(
      id = 1,
      name = "Mr. Masumbuko Semba",
      role = "Trainer",
      affiliation = "University of Dar es Salaam",
      focus = "Marine Ecology & Physical Oceanography",
      email = "lugosemba@gmail.com",
      password = "semba123",
      bio = "Lead Instructor of the Applied Statistics and scientific report rendering course. Specialized in numerical hydrodynamic modeling, marine stock assessment, and tidyverse data science workflows.",
      photo = NULL
    ),
    list(
      id = 2,
      name = "Mr. Herry Lugala",
      role = "Support",
      affiliation = "IUCN Tanzania Office",
      focus = "Logistics Coordination & Support",
      email = "herry.lugala@iucn.org",
      password = "lugala123",
      bio = "Logistics Coordinator under the Pamoja Tuhifadhi Bahari Yetu Project. Handles scholar accommodations, transport, allowances, and on-site conference hall coordination.",
      photo = NULL
    ),
    list(
      id = 3,
      name = "Imakulata Raphael Mwenda",
      role = "Trainee",
      affiliation = "The Nelson Mandela African Institution of Science and Technology (NM-AIST)",
      focus = "Marine Plastics",
      email = "imakulata.mwenda@example.com",
      password = "mwenda123",
      bio = "MSc candidate in Waste Management at Nelson Mandela African Institution of Science and Technology (NMAIST). Specialized in waste characterization and marine plastics mitigation.",
      photo = NULL
    ),
    list(
      id = 4,
      name = "Musa Yusuph Mungah",
      role = "Trainee",
      affiliation = "The Nelson Mandela African Institution of Science and Technology (NM-AIST)",
      focus = "Coastal Forests",
      email = "musa.mungah@example.com",
      password = "musa123",
      bio = "MSc candidate in Coastal Forest Management at Nelson Mandela African Institution of Science and Technology (NMAIST). Studying biodiversity in coastal woodlands.",
      photo = NULL
    ),
    list(
      id = 5,
      name = "Sitta Malulu Buluma",
      role = "Trainee",
      affiliation = "The Nelson Mandela African Institution of Science and Technology (NM-AIST)",
      focus = "Coastal Forests",
      email = "sitta.buluma@example.com",
      password = "sitta123",
      bio = "MSc candidate in Coastal Forest Management at Nelson Mandela African Institution of Science and Technology (NMAIST). Investigating vegetation dynamics in coastal ecosystems.",
      photo = NULL
    ),
    list(
      id = 6,
      name = "Bhoke Masisi",
      role = "Trainee",
      affiliation = "The Nelson Mandela African Institution of Science and Technology (NM-AIST)",
      focus = "GHG",
      email = "bhoke.masisi@example.com",
      password = "bhoke123",
      bio = "PhD scholar in Industrial Processes and Product Use (IPPU) at NMAIST. Researching green technologies and greenhouse gas inventory mitigation frameworks.",
      photo = NULL
    ),
    list(
      id = 7,
      name = "Dativa Byarufu",
      role = "Trainee",
      affiliation = "The Nelson Mandela African Institution of Science and Technology (NM-AIST)",
      focus = "Marine Plastics",
      email = "dativa.byarufu@example.com",
      password = "dativa123",
      bio = "PhD researcher in Waste Management at NMAIST. Focused on plastic waste recovery models and circular economy approaches in coastal regions.",
      photo = NULL
    ),
    list(
      id = 8,
      name = "Alselm Bonaventure Sengerere",
      role = "Trainee",
      affiliation = "Sokoine University of Agriculture (SUA)",
      focus = "Marine Plastics",
      email = "alselm.sengerere@example.com",
      password = "alselm123",
      bio = "MSc candidate in Plastic Waste Management at Sokoine University of Agriculture (SUA). Researching agricultural plastic recycling and marine environment protection.",
      photo = NULL
    ),
    list(
      id = 9,
      name = "Aikande Frank Lema",
      role = "Trainee",
      affiliation = "Sokoine University of Agriculture (SUA)",
      focus = "GHG",
      email = "aikande.lema@example.com",
      password = "aikande123",
      bio = "MSc candidate in IPPU at Sokoine University of Agriculture (SUA). Assessing industrial process emission inventories and energy efficiency schemes.",
      photo = NULL
    ),
    list(
      id = 10,
      name = "Bahati Beatrice Pius",
      role = "Trainee",
      affiliation = "Sokoine University of Agriculture (SUA)",
      focus = "GHG",
      email = "bahati.pius@example.com",
      password = "bahati123",
      bio = "MSc researcher in Energy Studies at Sokoine University of Agriculture (SUA). Investigating renewable energy systems and greenhouse gas reduction pathways.",
      photo = NULL
    ),
    list(
      id = 11,
      name = "Emanuel H. Lengeju",
      role = "Trainee",
      affiliation = "Sokoine University of Agriculture (SUA)",
      focus = "GHG",
      email = "emanuel.lengeju@example.com",
      password = "emanuel123",
      bio = "MSc scholar in Energy Studies at Sokoine University of Agriculture (SUA). Focused on cleaner energy technologies and carbon pricing models.",
      photo = NULL
    ),
    list(
      id = 12,
      name = "Ester Eleuteri Temba",
      role = "Trainee",
      affiliation = "Sokoine University of Agriculture (SUA)",
      focus = "Coastal Forests",
      email = "ester.temba@example.com",
      password = "ester123",
      bio = "MSc candidate in Coastal Forest Management at Sokoine University of Agriculture (SUA). Researching carbon storage and mangrove ecosystem protection.",
      photo = NULL
    ),
    list(
      id = 13,
      name = "Faraja Sauly Kakulwa",
      role = "Trainee",
      affiliation = "Sokoine University of Agriculture (SUA)",
      focus = "Marine Ecology",
      email = "faraja.kakulwa@example.com",
      password = "faraja123",
      bio = "MSc scholar in Marine Ecology at Sokoine University of Agriculture (SUA). Investigating benthic biodiversity and coral reef resilience indicators.",
      photo = NULL
    ),
    list(
      id = 14,
      name = "Given Edward Mwakyembe",
      role = "Trainee",
      affiliation = "Sokoine University of Agriculture (SUA)",
      focus = "GHG",
      email = "given.mwakyembe@example.com",
      password = "given123",
      bio = "MSc candidate in AFOLU (Agriculture, Forestry, and Other Land Use) at Sokoine University of Agriculture (SUA). Studying soil carbon dynamic modeling.",
      photo = NULL
    ),
    list(
      id = 15,
      name = "Gladness Edwin Mguluka",
      role = "Trainee",
      affiliation = "Sokoine University of Agriculture (SUA)",
      focus = "GHG",
      email = "gladness.mguluka@example.com",
      password = "gladness123",
      bio = "MSc scholar in Energy Studies at Sokoine University of Agriculture (SUA). Focussed on sustainable biomass combustion and local fuel optimization.",
      photo = NULL
    ),
    list(
      id = 16,
      name = "Joel Samwel Mandia",
      role = "Trainee",
      affiliation = "Sokoine University of Agriculture (SUA)",
      focus = "GHG",
      email = "joel.mandia@example.com",
      password = "joel123",
      bio = "MSc candidate in IPPU at Sokoine University of Agriculture (SUA). Analyzing heavy industry emission inventories and green chemistry pathways.",
      photo = NULL
    ),
    list(
      id = 17,
      name = "Musa Luhula Bujunju",
      role = "Trainee",
      affiliation = "Sokoine University of Agriculture (SUA)",
      focus = "GHG",
      email = "musa.bujunju@example.com",
      password = "musa123",
      bio = "MSc researcher in AFOLU at Sokoine University of Agriculture (SUA). Studying agroforestry techniques and community-based land use inventories.",
      photo = NULL
    ),
    list(
      id = 18,
      name = "Perpetua Nyagwaswa",
      role = "Trainee",
      affiliation = "Sokoine University of Agriculture (SUA)",
      focus = "GHG",
      email = "perpetua.nyagwaswa@example.com",
      password = "perpetua123",
      bio = "MSc scholar in IPPU at Sokoine University of Agriculture (SUA). Specializing in industrial energy audits and greenhouse gas reduction strategies.",
      photo = NULL
    ),
    list(
      id = 19,
      name = "Diana Lawerence Tesha",
      role = "Trainee",
      affiliation = "Sokoine University of Agriculture (SUA)",
      focus = "Coastal Forests",
      email = "diana.tesha@example.com",
      password = "diana123",
      bio = "PhD researcher in Coastal Forest Ecosystems at Sokoine University of Agriculture (SUA). Modeling forest biomass mapping and conservation policy impacts.",
      photo = NULL
    ),
    list(
      id = 20,
      name = "Sarafina Masanja",
      role = "Trainee",
      affiliation = "Sokoine University of Agriculture (SUA)",
      focus = "GHG",
      email = "sarafina.masanja@example.com",
      password = "sarafina123",
      bio = "PhD candidate in Energy Engineering at SUA. Focuses on regional hybrid power grids and off-grid solar-thermal integration frameworks.",
      photo = NULL
    ),
    list(
      id = 21,
      name = "Maryam Ramadhan Mwinyi",
      role = "Trainee",
      affiliation = "State University of Zanzibar (SUZA)",
      focus = "Marine Plastics",
      email = "maryam.mwinyi@example.com",
      password = "maryam123",
      bio = "MSc candidate in Plastic Waste Mitigation at SUZA. Assessing marine microplastic densities and local coastal waste recycling loops.",
      photo = NULL
    ),
    list(
      id = 22,
      name = "Annette Ambrose Kessy",
      role = "Trainee",
      affiliation = "University of Dar es Salaam (UDSM)",
      focus = "Marine Plastics",
      email = "annette.kessy@example.com",
      password = "annette123",
      bio = "MSc candidate in Marine Plastics and Waste Mitigation at UDSM. Focused on marine litter mapping and plastic pollution policy assessment.",
      photo = NULL
    ),
    list(
      id = 23,
      name = "Mwajabu Fadhili Mkindi",
      role = "Trainee",
      affiliation = "University of Dar es Salaam (UDSM)",
      focus = "Marine Plastics",
      email = "mwajabu.mkindi@example.com",
      password = "mwajabu123",
      bio = "MSc candidate in Waste Management at UDSM. Researching ocean plastics biodegradation rates and microbial degradation in marine environments.",
      photo = NULL
    ),
    list(
      id = 24,
      name = "Mustapha Issa",
      role = "Trainee",
      affiliation = "University of Dar es Salaam (UDSM)",
      focus = "Marine Ecology",
      email = "mustapha.issa@example.com",
      password = "mustapha123",
      bio = "MSc researcher in Marine Ecology at UDSM. Studying benthic ecosystems, ocean acidification variables, and sea cucumber density mapping.",
      photo = NULL
    ),
    list(
      id = 25,
      name = "Stanley Wilson Charles",
      role = "Trainee",
      affiliation = "University of Dar es Salaam (UDSM)",
      focus = "Marine Plastics",
      email = "stanley.charles@example.com",
      password = "stanley123",
      bio = "MSc candidate in Waste Management at UDSM. Focussed on urban runoff sediment controls and plastic trash trapping techniques in rivers.",
      photo = NULL
    ),
    list(
      id = 26,
      name = "David Kavana",
      role = "Trainee",
      affiliation = "University of Dar es Salaam (UDSM)",
      focus = "Marine Ecology",
      email = "david.kavana@example.com",
      password = "david123",
      bio = "PhD candidate in Marine Ecology at UDSM. Modeling dolphin population distributions, acoustic signatures, and biodiversity indicators.",
      photo = NULL
    ),
    list(
      id = 27,
      name = "Lynder Gesase",
      role = "Trainee",
      affiliation = "University of Dar es Salaam (UDSM)",
      focus = "Marine Plastics",
      email = "lynder.gesase@example.com",
      password = "lynder123",
      bio = "PhD researcher in Plastic Waste Mitigation at UDSM. Studying life-cycle emissions of single-use food packaging vs sustainable alternatives.",
      photo = NULL
    )
  )

  profiles_list <- reactiveVal(initial_profiles)
  max_id <- reactiveVal(27)
  current_editing_index <- reactiveVal(NULL)
  logged_in_user_data <- reactiveVal(NULL)
  login_error_msg <- reactiveVal("")
  reg_msg <- reactiveVal("")

  # Reactive helpers for authorization
  is_admin <- reactive({
    logged_user_val <- logged_in_user_data()
    !is.null(logged_user_val) && logged_user_val$role == "Admin"
  })

  logged_user <- reactive({
    logged_in_user_data()
  })

  # Dynamic Access Portal rendering in the sidebar
  output$login_portal_ui <- renderUI({
    logged_user_val <- logged_user()
    if (is.null(logged_user_val)) {
      actionButton("btn_show_login_modal", "Log In / Authenticate", class = "btn-outline-primary w-100 mt-1", icon = icon("right-to-bracket"))
    } else {
      div(
        style = "padding: 0.6rem; background: #e0f2fe; border: 1px solid #bae6fd; border-radius: 6px; font-size: 0.82rem; color: #0369a1;",
        div(style = "font-weight: 700;", paste("Logged in as:", logged_user_val$name)),
        div(style = "font-size: 0.75rem; opacity: 0.95; margin-top: 2px;", paste("Role:", logged_user_val$role)),
        actionButton("btn_user_logout", "Log Out", class = "btn-secondary btn-sm w-100 mt-2", icon = icon("right-from-bracket"))
      )
    }
  })

  # Trigger login modal popup
  observeEvent(input$btn_show_login_modal, {
    login_error_msg("")
    showModal(modalDialog(
      title = tags$div(
        style = "color: #0A3B5C; font-weight: 700; display: flex; align-items: center; gap: 8px;",
        icon("lock"), "Directory Access Portal"
      ),
      selectInput("modal_login_mode", "Access Type:",
        choices = c(
          "User Account" = "user",
          "Admin Key" = "admin"
        )
      ),
      uiOutput("modal_login_fields_ui"),
      footer = tagList(
        modalButton("Cancel"),
        uiOutput("modal_login_button_ui")
      ),
      easyClose = TRUE,
      size = "s"
    ))
  })

  # Render login fields inside the modal dynamically based on Access Type
  output$modal_login_fields_ui <- renderUI({
    mode <- input$modal_login_mode
    if (is.null(mode)) mode <- "user"

    fields <- if (mode == "user") {
      tagList(
        selectInput("login_email", "Select Your Email:", choices = sapply(profiles_list(), function(x) x$email)),
        passwordInput("login_password", "Password:")
      )
    } else {
      tagList(
        passwordInput("login_admin_key", "Admin Key:", placeholder = "Enter admin password")
      )
    }

    tagList(
      fields,
      uiOutput("login_error_ui")
    )
  })

  # Render login submit buttons inside the modal footer
  output$modal_login_button_ui <- renderUI({
    mode <- input$modal_login_mode
    if (is.null(mode)) mode <- "user"

    if (mode == "user") {
      actionButton("btn_user_login", "Log In", class = "btn-success", style = "background: #0d9488; border: none;")
    } else {
      actionButton("btn_admin_login", "Log In", class = "btn-success", style = "background: #0d9488; border: none;")
    }
  })

  observeEvent(input$btn_user_login, {
    email <- input$login_email
    pw <- input$login_password

    lst <- profiles_list()
    match_idx <- which(sapply(lst, function(x) x$email) == email & sapply(lst, function(x) x$password) == pw)

    if (length(match_idx) > 0) {
      logged_in_user_data(lst[[match_idx]])
      login_error_msg("")
      removeModal()
    } else {
      login_error_msg(span("⚠️ Invalid email/password.", style = "color: #dc2626; font-size: 0.75rem; font-weight: 600; display: block; margin-top: 5px;"))
    }
  })

  observeEvent(input$btn_admin_login, {
    pw <- input$login_admin_key
    if (!is.null(pw) && pw == "iucn2026") {
      logged_in_user_data(list(id = 0, name = "Administrator", role = "Admin"))
      login_error_msg("")
      removeModal()
    } else {
      login_error_msg(span("⚠️ Invalid Admin Key.", style = "color: #dc2626; font-size: 0.75rem; font-weight: 600; display: block; margin-top: 5px;"))
    }
  })

  observeEvent(input$btn_user_logout, {
    logged_in_user_data(NULL)
    login_error_msg("")
    current_editing_index(NULL)
    updateTextInput(session, "reg_name", value = "")
    updateTextInput(session, "reg_email", value = "")
    updateTextInput(session, "reg_affiliation", value = "")
    updateTextInput(session, "reg_password", value = "")
    updateTextAreaInput(session, "reg_bio", value = "")
  })

  output$login_error_ui <- renderUI({
    login_error_msg()
  })

  output$reg_submit_button_ui <- renderUI({
    if (is.null(current_editing_index())) {
      actionButton("reg_submit", "Register Profile", class = "btn-success w-100 mt-2", icon = icon("user-plus"))
    } else {
      div(
        actionButton("reg_submit", "Update Profile", class = "btn-primary w-100 mt-2", icon = icon("floppy-disk")),
        actionButton("reg_cancel_edit", "Cancel Edit", class = "btn-secondary w-100 mt-1", icon = icon("xmark"))
      )
    }
  })

  observeEvent(input$reg_cancel_edit, {
    current_editing_index(NULL)
    reg_msg("")
    updateTextInput(session, "reg_name", value = "")
    updateTextInput(session, "reg_email", value = "")
    updateTextInput(session, "reg_affiliation", value = "")
    updateTextInput(session, "reg_password", value = "")
    updateTextAreaInput(session, "reg_bio", value = "")
  })

  observeEvent(input$reg_submit, {
    name <- trimws(input$reg_name)
    email <- trimws(input$reg_email)
    affiliation <- trimws(input$reg_affiliation)
    bio <- trimws(input$reg_bio)
    pw <- input$reg_password

    if (name == "") {
      reg_msg(span("⚠️ Error: Full Name is required.", style = "color: #dc2626; font-weight: 600; display: block; margin-top: 10px;"))
      return()
    }

    photo_data <- NULL
    if (!is.null(input$reg_photo)) {
      tryCatch(
        {
          ext <- tools::file_ext(input$reg_photo$name)
          if (tolower(ext) %in% c("png", "jpg", "jpeg")) {
            photo_data <- base64enc::dataURI(file = input$reg_photo$datapath, mime = input$reg_photo$type)
          } else {
            reg_msg(span("⚠️ Warning: Photo ignored. Only PNG or JPG files are supported.", style = "color: #d97706; font-weight: 600; display: block; margin-top: 10px;"))
          }
        },
        error = function(e) {
          # ignore photo if error occurs
        }
      )
    }

    target_id <- current_editing_index()
    if (!is.null(target_id)) {
      # Server-side authentication verification
      is_admin_active <- is_admin()
      logged_user_val <- logged_user()
      is_owner <- !is.null(logged_user_val) && target_id == logged_user_val$id

      if (!is_admin_active && !is_owner) {
        reg_msg(span("⚠️ Error: Unauthorized profile edit attempt.", style = "color: #dc2626; font-weight: 600; display: block; margin-top: 10px;"))
        return()
      }

      lst <- profiles_list()
      match_idx <- which(sapply(lst, function(x) x$id) == target_id)
      if (length(match_idx) > 0) {
        if (is.null(photo_data)) {
          photo_data <- lst[[match_idx]]$photo
        }
        saved_pw <- ifelse(pw == "", lst[[match_idx]]$password, pw)

        updated_entry <- list(
          id = target_id,
          name = name,
          role = input$reg_role,
          affiliation = ifelse(affiliation == "", "Independent / Scholar", affiliation),
          focus = input$reg_focus,
          email = ifelse(email == "", "No Email Provided", email),
          password = saved_pw,
          bio = ifelse(bio == "", "Participant in the IUCN Bahari Yetu training course.", bio),
          photo = photo_data
        )
        lst[[match_idx]] <- updated_entry
        profiles_list(lst)
        current_editing_index(NULL)

        # sync state if user updated themselves
        if (!is_admin_active && is_owner) {
          logged_in_user_data(updated_entry)
        }

        reg_msg(span("🎉 Profile updated successfully!", style = "color: #16a34a; font-weight: 600; display: block; margin-top: 10px;"))
      }
    } else {
      new_id <- max_id() + 1
      max_id(new_id)

      new_entry <- list(
        id = new_id,
        name = name,
        role = input$reg_role,
        affiliation = ifelse(affiliation == "", "Independent / Scholar", affiliation),
        focus = input$reg_focus,
        email = ifelse(email == "", "No Email Provided", email),
        password = ifelse(pw == "", "password123", pw),
        bio = ifelse(bio == "", "Participant in the IUCN Bahari Yetu training course.", bio),
        photo = photo_data
      )

      current_list <- profiles_list()
      current_list <- c(current_list, list(new_entry))
      profiles_list(current_list)
      reg_msg(span("🎉 Profile registered successfully!", style = "color: #16a34a; font-weight: 600; display: block; margin-top: 10px;"))
    }

    updateTextInput(session, "reg_name", value = "")
    updateTextInput(session, "reg_email", value = "")
    updateTextInput(session, "reg_affiliation", value = "")
    updateTextInput(session, "reg_password", value = "")
    updateTextAreaInput(session, "reg_bio", value = "")
  })

  observeEvent(input$dir_delete_index, {
    # Server-Side verification (Only Administrators can delete profiles)
    if (!is_admin()) {
      reg_msg(span("⚠️ Error: Only administrators can delete profiles.", style = "color: #dc2626; font-weight: 600; display: block; margin-top: 10px;"))
      return()
    }

    target_id <- as.integer(input$dir_delete_index)
    lst <- profiles_list()
    match_idx <- which(sapply(lst, function(x) x$id) == target_id)
    if (length(match_idx) > 0) {
      if (!is.null(current_editing_index()) && current_editing_index() == target_id) {
        current_editing_index(NULL)
        updateTextInput(session, "reg_name", value = "")
        updateTextInput(session, "reg_email", value = "")
        updateTextInput(session, "reg_affiliation", value = "")
        updateTextInput(session, "reg_password", value = "")
        updateTextAreaInput(session, "reg_bio", value = "")
      }
      lst[[match_idx]] <- NULL
      profiles_list(lst)
      reg_msg(span("🗑️ Profile deleted successfully!", style = "color: #dc2626; font-weight: 600; display: block; margin-top: 10px;"))
    }
  })

  observeEvent(input$dir_edit_index, {
    target_id <- as.integer(input$dir_edit_index)

    # Server-side authentication check for editing
    is_admin_active <- is_admin()
    logged_user_val <- logged_user()
    is_owner <- !is.null(logged_user_val) && target_id == logged_user_val$id

    if (!is_admin_active && !is_owner) {
      reg_msg(span("⚠️ Error: Unauthorized edit attempt.", style = "color: #dc2626; font-weight: 600; display: block; margin-top: 10px;"))
      return()
    }

    lst <- profiles_list()
    match_idx <- which(sapply(lst, function(x) x$id) == target_id)
    if (length(match_idx) > 0) {
      p <- lst[[match_idx]]
      current_editing_index(p$id)

      updateTextInput(session, "reg_name", value = p$name)
      updateSelectInput(session, "reg_role", selected = p$role)
      updateTextInput(session, "reg_affiliation", value = p$affiliation)
      updateSelectInput(session, "reg_focus", selected = p$focus)
      updateTextInput(session, "reg_email", value = p$email)
      updateTextInput(session, "reg_password", value = "")
      updateTextAreaInput(session, "reg_bio", value = p$bio)

      reg_msg(span("✍️ Editing profile. Make changes and click 'Update Profile'.", style = "color: #0284c7; font-weight: 600; display: block; margin-top: 10px;"))
    }
  })

  output$reg_status <- renderUI({
    reg_msg()
  })

  filtered_profiles <- reactive({
    lst <- profiles_list()

    # Filter by search string
    search_term <- trimws(tolower(input$search_dir_name))
    if (search_term != "") {
      lst <- Filter(function(x) {
        grepl(search_term, tolower(x$name)) ||
          grepl(search_term, tolower(x$affiliation)) ||
          grepl(search_term, tolower(x$bio))
      }, lst)
    }

    # Filter by role
    role_filter <- input$filter_dir_role
    if (role_filter != "All") {
      lst <- Filter(function(x) {
        x$role == role_filter
      }, lst)
    }

    lst
  })

  output$profiles_grid_ui <- renderUI({
    lst <- filtered_profiles()

    if (length(lst) == 0) {
      return(
        div(
          style = "text-align: center; padding: 2.5rem; color: #64748b;",
          icon("users-slash", class = "fa-3x mb-3"),
          h4("No Profiles Found"),
          p("No directory entries match your current search/filter parameters.")
        )
      )
    }

    is_admin_active <- is_admin()
    logged_user_val <- logged_user()

    cards <- lapply(lst, function(p) {
      badge_class <- switch(p$role,
        "Trainer" = "badge-role-trainer",
        "Trainee" = "badge-role-trainee",
        "Support" = "badge-role-support",
        "Partner" = "badge-role-partner",
        "badge-role-trainee"
      )

      role_label <- switch(p$role,
        "Trainer" = "Trainer / Instructor",
        "Trainee" = "Trainee / Scholar",
        "Support" = "IUCN Staff",
        "Partner" = "Project Partner",
        p$role
      )

      # Initials or Photo element
      avatar_ui <- if (!is.null(p$photo)) {
        tags$img(src = p$photo, class = "profile-photo-img", alt = p$name)
      } else {
        names_split <- strsplit(p$name, "\\s+")[[1]]
        names_clean <- Filter(function(n) !tolower(n) %in% c("mr", "mr.", "ms", "ms.", "mrs", "mrs.", "dr", "dr.", "prof", "prof."), names_split)
        initials <- if (length(names_clean) >= 2) {
          paste0(substr(names_clean[1], 1, 1), substr(names_clean[length(names_clean)], 1, 1))
        } else if (length(names_clean) == 1) {
          substr(names_clean[1], 1, 2)
        } else {
          substr(p$name, 1, 2)
        }
        div(class = "profile-photo-placeholder", toupper(initials))
      }

      is_owner <- !is.null(logged_user_val) && p$id == logged_user_val$id

      div(
        class = "directory-card",
        div(class = "profile-card-header-bar"),
        div(
          class = "directory-card-body",
          div(class = "profile-photo-wrapper", avatar_ui),
          div(class = "profile-name", p$name),
          span(class = paste("profile-badge", badge_class), role_label),
          div(
            class = "profile-meta-item",
            icon("building"),
            span(p$affiliation)
          ),
          div(
            class = "profile-meta-item",
            icon("circle-nodes"),
            span(p$focus)
          ),
          div(class = "profile-bio-text", p$bio),
          if (p$email != "No Email Provided" && p$email != "") {
            tags$a(
              href = paste0("mailto:", p$email),
              class = "profile-contact-btn",
              icon("envelope"), " Contact Email"
            )
          },
          if (is_admin_active || is_owner) {
            tags$div(
              style = "display: flex; gap: 8px; width: 100%; margin-top: 0.8rem; border-top: 1px dashed #cbd5e1; padding-top: 0.8rem;",
              tags$button(
                class = "btn btn-sm btn-outline-primary",
                style = "flex: 1; padding: 0.3rem; font-size: 0.75rem;",
                onclick = sprintf("Shiny.setInputValue('dir_edit_index', %d, {priority: 'event'})", p$id),
                icon("pen"), " Edit"
              ),
              if (is_admin_active) {
                tags$button(
                  class = "btn btn-sm btn-outline-danger",
                  style = "flex: 1; padding: 0.3rem; font-size: 0.75rem;",
                  onclick = sprintf("Shiny.setInputValue('dir_delete_index', %d, {priority: 'event'})", p$id),
                  icon("trash"), " Delete"
                )
              }
            )
          }
        )
      )
    })

    div(class = "profile-grid-container", cards)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
