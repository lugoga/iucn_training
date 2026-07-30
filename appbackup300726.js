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
    datasets_metadata < - list(
        "1" = list(name = "Coastal Forest Structure", file = "theme1_coastal_forest.csv", theme = "Coastal Forest"),
        "2" = list(name = "GHG Industrial Cement (IPPU)", file = "theme2_ghg_ippu.csv", theme = "GHG IPPU"),
        "3" = list(name = "GHG Household Energy", file = "theme3_ghg_energy.csv", theme = "GHG Energy"),
        "4" = list(name = "Marine Ecology & Dolphins", file = "theme4_marine_ecology.csv", theme = "Marine Ecology"),
        "5" = list(name = "GHG Agriculture & AFOLU", file = "theme5_ghg_afolu.csv", theme = "GHG AFOLU"),
        "6" = list(name = "Plastic Waste Spatial Survey", file = "theme6_plastic_waste.csv", theme = "Plastic Waste"),
        "7" = list(name = "GHG Anaerobic Digestion (Waste)", file = "theme7_ghg_waste.csv", theme = "GHG Waste")
    )

load_dataset < - function (id) {
    meta < - datasets_metadata[[as.character(id)]]
    path < - here("data", meta$file)
    if (file.exists(path)) {
        return (read_csv(path, show_col_types = FALSE))
    }
  # Fallback to current working directory
    fallback_path < - file.path("data", meta$file)
    if (file.exists(fallback_path)) {
        return (read_csv(fallback_path, show_col_types = FALSE))
    }
    return (NULL)
}

# ---------------------------------------------------------
# UI Design System(Modern Yacht / Forest theme)
# ---------------------------------------------------------
    theme < - bs_theme(
        bootswatch = "yeti",
        primary = "#0A3B5C",      # Deep Marine Blue
  secondary = "#4A7C59",    # Coastal Forest Green
  success = "#2ECC71",
        info = "#17A2B8",
        warning = "#F39C12",
        danger = "#E74C3C",
        base_font = font_google("Inter"),
        heading_font = font_google("Outfit"),
        code_font = font_google("Fira Code")
    )

# Custom Shiny Dashboard UI
ui < - page_navbar(
    theme = theme,
    title = "IUCN Bahari Yetu Portal",
    bg = "#0A3B5C",
  
  # Custom Head Styles
  header = tags$head(
    # PWA Support Tags
    tags$link(rel = "manifest", href = "/manifest.json"),
        tags$meta(name = "theme-color", content = "#0A3B5C"),
        tags$meta(name = "mobile-web-app-capable", content = "yes"),
        tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
        tags$meta(name = "apple-mobile-web-app-status-bar-style", content = "black-translucent"),
        tags$link(rel = "apple-touch-icon", href = "/icon-192.png"),
    
    # PWA Service Worker & Auto - Reconnect / Reload on Sleep
    tags$script(HTML("
      if ('serviceWorker' in navigator) {
    window.addEventListener('load', function () {
        navigator.serviceWorker.register('/service-worker.js').then(function (reg) {
            console.log('ServiceWorker registered: ', reg.scope);
        }).catch(function (err) {
            console.log('ServiceWorker registration failed: ', err);
        });
    });
}

// Keep alive ping every 25 seconds to prevent R Shiny idle timeouts
setInterval(function () {
    if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
        Shiny.setInputValue('keep_alive_ping', new Date().getTime());
    }
}, 25000);

// Auto-reload on disconnection / server sleep
$(document).on('shiny:disconnected', function (event) {
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
    setTimeout(function () {
        window.location.reload();
    }, 2500);
});
")),

tags$style(HTML("
      body {
    background- color: #f8fafc;
color: #334155;
      }
      .navbar - brand {
    font - family: 'Outfit', sans - serif!important;
    font - weight: 800!important;
    letter - spacing: 0.5px;
}
      .hero - banner {
    background: linear - gradient(135deg, #0A3B5C 0 %, #1e3a5f 50 %, #1b4e3e 100 %);
    color: white;
    padding: 3.5rem 2rem;
    border - radius: 16px;
    margin - bottom: 2rem;
    box - shadow: 0 10px 30px rgba(10, 59, 92, 0.12);
    position: relative;
    overflow: hidden;
}
      .hero - banner h1 {
    font - family: 'Outfit', sans - serif;
    font - weight: 800;
    font - size: 2.8rem;
    margin - bottom: 0.5rem;
}
      .hero - banner p {
    font - size: 1.15rem;
    opacity: 0.9;
    max - width: 800px;
    margin - bottom: 0;
}
      .tna - card {
    background: white;
    border - radius: 12px;
    padding: 1.5rem;
    box - shadow: 0 4px 15px rgba(0, 0, 0, 0.03);
    border: 1px solid #e2e8f0;
    transition: all 0.3s ease;
    height: 100 %;
}
      .tna - card:hover {
    transform: translateY(-4px);
    box - shadow: 0 12px 25px rgba(0, 0, 0, 0.06);
    border - color: #0A3B5C;
}
      .tna - stat {
    font - size: 2.25rem;
    font - weight: 800;
    color: #0A3B5C;
    font - family: 'Outfit', sans - serif;
    line - height: 1;
    margin - bottom: 0.4rem;
}
      .tna - label {
    font - size: 0.85rem;
    font - weight: 700;
    color: #475569;
    text - transform: uppercase;
    letter - spacing: 0.5px;
}
      .tna - desc {
    font - size: 0.85rem;
    color: #64748b;
    margin - top: 0.5rem;
}
      .custom - card {
    background: white;
    border - radius: 12px;
    padding: 1.8rem;
    box - shadow: 0 4px 15px rgba(0, 0, 0, 0.03);
    border: 1px solid #e2e8f0;
    margin - bottom: 1.5rem;
}
      .section - title {
    font - family: 'Outfit', sans - serif;
    font - weight: 700;
    color: #0A3B5C;
    margin - bottom: 1.2rem;
    border - bottom: 2px solid #f1f5f9;
    padding - bottom: 0.4rem;
}
      .code - container {
    background: #0f172a;
    color: #f8fafc;
    padding: 1.2rem;
    border - radius: 8px;
    font - family: 'Fira Code', monospace;
    font - size: 0.88rem;
    position: relative;
    overflow - x: auto;
    margin - top: 0.8rem;
    box - shadow: inset 0 2px 8px rgba(0, 0, 0, 0.2);
}
      .code - container pre {
    margin: 0;
    background: transparent;
    color: inherit;
    border: none;
    padding: 0;
}
      .schedule - item {
    border - left: 3px solid #0A3B5C;
    padding - left: 1.2rem;
    margin - bottom: 1.2rem;
    position: relative;
}
      .schedule - time {
    font - weight: 700;
    color: #0A3B5C;
    font - size: 0.88rem;
}
      .schedule - title {
    font - weight: 600;
    font - size: 1.05rem;
    margin: 0.2rem 0;
    color: #1e293b;
}
      .schedule - desc {
    color: #64748b;
    font - size: 0.88rem;
}
      .wide - table, .tidy - table {
    font - size: 0.85rem;
}
      .nav - tabs.nav - link {
    font - family: 'Outfit', sans - serif;
    font - weight: 600;
}
      .badge - custom {
    background - color: #e0f2fe;
    color: #0369a1;
    font - weight: 600;
    padding: 0.3em 0.6em;
    border - radius: 4px;
}
      /* Flowchart & Educational Diagram Styles */
      .flowchart - container {
    display: flex;
    align - items: stretch;
    justify - content: space - between;
    margin: 2rem 0;
    gap: 1.5rem;
}
      .flowchart - column {
    flex: 1;
    background: white;
    border - radius: 16px;
    padding: 2rem;
    border: 1px solid #e2e8f0;
    box - shadow: 0 4px 20px rgba(0, 0, 0, 0.02);
    display: flex;
    flex - direction: column;
}
      .flowchart - column.excel {
    border - top: 5px solid #E74C3C; /* Red warning accent */
}
      .flowchart - column.coding {
    border - top: 5px solid #2ECC71; /* Green success accent */
}
      .flowchart - header {
    font - family: 'Outfit', sans - serif;
    font - weight: 700;
    font - size: 1.3rem;
    margin - bottom: 1rem;
    display: flex;
    align - items: center;
    gap: 0.6rem;
}
      .flowchart - step {
    display: flex;
    align - items: center;
    background: #f8fafc;
    padding: 1rem;
    border - radius: 10px;
    margin - bottom: 0.8rem;
    border: 1px solid #f1f5f9;
    position: relative;
}
      .flowchart - step - icon {
    width: 32px;
    height: 32px;
    border - radius: 50 %;
    display: flex;
    align - items: center;
    justify - content: center;
    font - weight: 700;
    font - size: 0.95rem;
    margin - right: 1rem;
    flex - shrink: 0;
}
      .excel.flowchart - step - icon {
    background: #fde8e8;
    color: #E74C3C;
}
      .coding.flowchart - step - icon {
    background: #e6fcf5;
    color: #2ECC71;
}
      .flowchart - step - text {
    font - size: 0.9rem;
    font - weight: 600;
    color: #1e293b;
    display: block;
}
      .flowchart - step - desc {
    font - size: 0.8rem;
    color: #64748b;
    display: block;
}
      .flowchart - arrow {
    text - align: center;
    color: #cbd5e1;
    font - size: 1.1rem;
    margin - bottom: 0.8rem;
}
      .flowchart - footer {
    margin - top: auto;
    padding - top: 1rem;
    font - size: 0.9rem;
    font - weight: 700;
    text - align: center;
    border - top: 1px solid #f1f5f9;
}
@media(max - width: 768px) {
        .flowchart - container {
        flex - direction: column;
    }
}
      /* Day Tabs README styling */
      .day - header - banner {
    background: linear - gradient(135deg, #4A7C59 0 %, #1e4e3e 100 %);
    color: white;
    padding: 1.8rem;
    border - radius: 12px;
    margin - bottom: 1.5rem;
    box - shadow: 0 4px 15px rgba(0, 0, 0, 0.03);
}
      .day - header - banner h3 {
    font - family: 'Outfit', sans - serif;
    font - weight: 700;
    margin: 0;
}
      .day - header - banner p {
    margin: 0.4rem 0 0 0;
    font - size: 0.95rem;
    opacity: 0.9;
}
      .objective - item {
    display: flex;
    align - items: flex - start;
    margin - bottom: 0.8rem;
}
      .objective - icon {
    color: #2ECC71;
    margin - right: 0.8rem;
    font - size: 1.05rem;
    margin - top: 0.15rem;
}
      .objective - text {
    font - size: 0.88rem;
    color: #475569;
    line - height: 1.4;
}
      .resource - link - btn {
    display: inline - flex;
    align - items: center;
    gap: 0.5rem;
    background: #f1f5f9;
    color: #0A3B5C;
    padding: 0.5rem 0.8rem;
    border - radius: 8px;
    font - size: 0.8rem;
    font - weight: 600;
    text - decoration: none;
    border: 1px solid #e2e8f0;
    margin - bottom: 0.5rem;
    margin - right: 0.5rem;
    transition: all 0.2s ease;
}
      .resource - link - btn:hover {
    background: #e2e8f0;
    color: #0A3B5C;
    transform: translateY(-1px);
    box - shadow: 0 2px 6px rgba(0, 0, 0, 0.05);
}
      .resource - link - btn i {
    font - size: 0.9rem;
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
            div(class = "hero-banner",
                h1("IUCN Bahari Yetu Scholarly Portal"),
                p("Accelerating research outcomes by transitioning from manual spreadsheets to automated, reproducible environmental and marine analytics pipelines in R.")
            ),

            layout_sidebar(
                sidebar = sidebar(
                    title = "Applied Statistics Logistics",
                    tags$div(style = "font-size:0.88rem; line-height: 1.6;",
                        h6("Location:", style = "font-weight:700; color:#0A3B5C; margin-top:0.5rem;"),
                        p("EDEMA Conference Hall, Morogoro, Tanzania"),
                        h6("Dates:", style = "font-weight:700; color:#0A3B5C;"),
                        p("August 3 - 8, 2026"),
                        h6("Lead Instructor:", style = "font-weight:700; color:#0A3B5C;"),
                        p("Masumbuko Semba"),
                        h6("Key Focus:", style = "font-weight:700; color:#0A3B5C;"),
                        p("Marine, Forestry, & GHG Datasets"),
                        h6("TNA Survey Report:", style = "font-weight:700; color:#0A3B5C;"),
                        tags$a(href = "#", class = "resource-link-btn", style = "text-align:center; font-size:0.8rem; padding:0.4rem; display:block;",
                            icon("file-pdf"), "Download TNA v2 PDF"
                        )
                    ),
                    hr(),
                    h5("TNA Analytics Selector", style = "font-weight:700; color:#0A3B5C; font-size:0.95rem; margin-top:0.5rem;"),
                    p("Choose a survey metric to visualize cohort bottlenecks:", style = "font-size:0.78rem; color:#64748b; margin-top:0.2rem;"),
                    selectInput("tna_chart_select", "Select Survey Data:",
                        choices = c(
                            "Time Bottlenecks (Tasks)" = "time",
                            "Skill Gap Distribution" = "skills",
                            "Prior R Experience" = "exp"
                        ), selected = "time")
                ),
        
        # Main Panel Content
        div(class = "custom-card", style = "margin-top:0;",
                    h4(class = "section-title", "Applied Statistics Overview"),
                    p("The IUCN Bahari Yetu Scholarly Training Program is built around a ", strong("Bring Your Own Data (BYOD)"), " philosophy. Over the course of five intensive days in Morogoro, scholars undergo a training path starting with core data formatting rules and progressing to advanced spatial calculations and regression modeling."),
                    p("This interactive dashboard serves as a teaching support tool. Each tab corresponds to a day of the training and demonstrates the specific tools and libraries (like ", code("tidyverse"), ", ", code("sf"), ", and ", code("testflow"), ") that scholars learn to master.")
                ),

                layout_column_wrap(
                    width = 1 / 4,
                    div(class = "tna-card", style = "padding: 1rem; border-radius: 8px;",
                        div(class = "tna-stat", style = "font-size:1.6rem;", "44.5%"),
                        div(class = "tna-label", style = "font-size:0.8rem; font-weight:700; margin-top:0.2rem;", "Excel Cleaning Time"),
                        div(class = "tna-desc", style = "font-size:0.75rem; line-height:1.4; color:#64748b;", "Research time spent on manual cleaning, copy-pasting, and sorting. Goal: reduce by 80% with code.")
                    ),
                    div(class = "tna-card", style = "padding: 1rem; border-radius: 8px;",
                        div(class = "tna-stat", style = "font-size:1.6rem;", "57.1%"),
                        div(class = "tna-label", style = "font-size:0.8rem; font-weight:700; margin-top:0.2rem;", "R Beginners"),
                        div(class = "tna-desc", style = "font-size:0.75rem; line-height:1.4; color:#64748b;", "Scholars entering training with no prior coding experience. Course starts from absolute foundations.")
                    ),
                    div(class = "tna-card", style = "padding: 1rem; border-radius: 8px;",
                        div(class = "tna-stat", style = "font-size:1.6rem;", "28"),
                        div(class = "tna-label", style = "font-size:0.8rem; font-weight:700; margin-top:0.2rem;", "Cohort Scholars"),
                        div(class = "tna-desc", style = "font-size:0.75rem; line-height:1.4; color:#64748b;", "Selected MSc & PhD candidates from SUA, UDSM, NM-AIST, and SUZA under IUCN Bahari Yetu.")
                    ),
                    div(class = "tna-card", style = "padding: 1rem; border-radius: 8px;",
                        div(class = "tna-stat", style = "font-size:1.6rem;", "5 Days"),
                        div(class = "tna-label", style = "font-size:0.8rem; font-weight:700; margin-top:0.2rem;", "Intensive Course"),
                        div(class = "tna-desc", style = "font-size:0.75rem; line-height:1.4; color:#64748b;", "Curriculum structure moving from setup (Day 1) to aggregation, graphics, GIS, and reporting.")
                    )
                ),

                div(class = "custom-card",
                    h4(class = "section-title", "Why Code? The Excel Spreadsheet Trap vs. R Loop"),
                    p("A visual comparison of manual spreadsheet manipulation (copy-paste, point-and-click) versus reproducible R programming workflows."),

                    div(class = "flowchart-container", style = "margin: 1rem 0; gap: 1rem;",
                        div(class = "flowchart-column excel", style = "padding: 1.2rem;",
                            div(class = "flowchart-header", style = "font-size: 1rem; margin-bottom: 0.8rem;",
                                icon("times-circle", style = "color:#E74C3C;"), "The Manual Excel Trap"
                            ),
                            div(class = "flowchart-step", style = "margin-bottom:0.4rem;",
                                div(class = "flowchart-step-icon", style = "width:20px; height:20px; font-size:0.8rem;", "1"),
                                div(span(class = "flowchart-step-text", style = "font-size:0.85rem;", "Copy-Paste Importing"))
                            ),
                            div(class = "flowchart-arrow", style = "margin: 0.1rem 0;", icon("arrow-down", style = "font-size:0.8rem;")),
                            div(class = "flowchart-step", style = "margin-bottom:0.4rem;",
                                div(class = "flowchart-step-icon", style = "width:20px; height:20px; font-size:0.8rem;", "2"),
                                div(span(class = "flowchart-step-text", style = "font-size:0.85rem;", "Point-and-Click Wrangling"))
                            ),
                            div(class = "flowchart-arrow", style = "margin: 0.1rem 0;", icon("arrow-down", style = "font-size:0.8rem;")),
                            div(class = "flowchart-step", style = "margin-bottom:0.4rem;",
                                div(class = "flowchart-step-icon", style = "width:20px; height:20px; font-size:0.8rem;", "3"),
                                div(span(class = "flowchart-step-text", style = "font-size:0.85rem;", "Manual Plot Design"))
                            ),
                            div(class = "flowchart-arrow", style = "margin: 0.1rem 0;", icon("arrow-down", style = "font-size:0.8rem;")),
                            div(class = "flowchart-step", style = "margin-bottom:0.4rem;",
                                div(class = "flowchart-step-icon", style = "width:20px; height:20px; font-size:0.8rem;", "4"),
                                div(span(class = "flowchart-step-text", style = "font-size:0.85rem;", "Ad-hoc Report Assembly"))
                            ),
                            div(class = "flowchart-footer", style = "color:#E74C3C; font-size:0.8rem; margin-top:0.8rem;", "Sinks 44.5% of research time!")
                        ),

                        div(class = "flowchart-column coding", style = "padding: 1.2rem;",
                            div(class = "flowchart-header", style = "font-size: 1rem; margin-bottom: 0.8rem;",
                                icon("check-circle", style = "color:#2ECC71;"), "The Reproducible R Loop"
                            ),
                            div(class = "flowchart-step", style = "margin-bottom:0.4rem;",
                                div(class = "flowchart-step-icon", style = "width:20px; height:20px; font-size:0.8rem;", "1"),
                                div(span(class = "flowchart-step-text", style = "font-size:0.85rem;", "read_csv() / here()"))
                            ),
                            div(class = "flowchart-arrow", style = "margin: 0.1rem 0;", icon("arrow-down", style = "font-size:0.8rem;")),
                            div(class = "flowchart-step", style = "margin-bottom:0.4rem;",
                                div(class = "flowchart-step-icon", style = "width:20px; height:20px; font-size:0.8rem;", "2"),
                                div(span(class = "flowchart-step-text", style = "font-size:0.85rem;", "dplyr Wrangle Pipes"))
                            ),
                            div(class = "flowchart-arrow", style = "margin: 0.1rem 0;", icon("arrow-down", style = "font-size:0.8rem;")),
                            div(class = "flowchart-step", style = "margin-bottom:0.4rem;",
                                div(class = "flowchart-step-icon", style = "width:20px; height:20px; font-size:0.8rem;", "3"),
                                div(span(class = "flowchart-step-text", style = "font-size:0.85rem;", "ggplot2 Layered Plots"))
                            ),
                            div(class = "flowchart-arrow", style = "margin: 0.1rem 0;", icon("arrow-down", style = "font-size:0.8rem;")),
                            div(class = "flowchart-step", style = "margin-bottom:0.4rem;",
                                div(class = "flowchart-step-icon", style = "width:20px; height:20px; font-size:0.8rem;", "4"),
                                div(span(class = "flowchart-step-text", style = "font-size:0.85rem;", "Quarto (.qmd) Rendering"))
                            ),
                            div(class = "flowchart-footer", style = "color:#2ECC71; font-size:0.8rem; margin-top:0.8rem;", "Updates instantly on data changes!")
                        )
                    )
                ),

                div(class = "custom-card",
                    h4(class = "section-title", "Training Needs Assessment Survey Results"),
                    plotOutput("tna_plot", height = "320px")
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
            div(class = "day-header-banner",
                h3("Day 1: Foundations of R, Modern IDE Setup, & Tidy Data Principles"),
                p("Transitioning from manual spreadsheet manipulation to reproducible, programmatic workflows in R.")
            ),

            fluidRow(
                column(width = 7,
                    div(class = "custom-card",
                        h4(class = "section-title", "Daily Schedule"),
                        div(class = "schedule-item",
                            div(class = "schedule-time", "09:00 - 10:30"),
                            div(class = "schedule-title", "Session 1: IDE Setup & Projects"),
                            div(class = "schedule-desc", "Positron/VS Code interface, RStudio Projects, Directory structure (/data, /scripts, /outputs), the here package.")
                        ),
                        div(class = "schedule-item",
                            div(class = "schedule-time", "11:00 - 12:30"),
                            div(class = "schedule-title", "Session 2: R Syntax Foundations"),
                            div(class = "schedule-desc", "Objects, vectors, assignment operator (<-), functions, data frames.")
                        ),
                        div(class = "schedule-item",
                            div(class = "schedule-time", "14:00 - 15:30"),
                            div(class = "schedule-title", "Session 3: Tidy Data & Import"),
                            div(class = "schedule-desc", "Tidy principles, importing using readr::read_csv() and readxl::read_excel().")
                        ),
                        div(class = "schedule-item",
                            div(class = "schedule-time", "15:30 - 17:00"),
                            div(class = "schedule-title", "Hands-On Lab 1 (BYOD)"),
                            div(class = "schedule-desc", "Set up project directories and import active thesis data.")
                        )
                    )
                ),
                column(width = 5,
                    div(class = "custom-card",
                        h4(class = "section-title", "Learning Objectives"),
                        div(class = "objective-item",
                            icon("check-circle", class= "objective-icon"),
                            div(class = "objective-text", "Navigate the RStudio/Positron IDE interface.")
                        ),
                        div(class = "objective-item",
                            icon("check-circle", class= "objective-icon"),
                            div(class = "objective-text", "Establish a self-contained project directory with clean folder structures.")
                        ),
                        div(class = "objective-item",
                            icon("check-circle", class= "objective-icon"),
                            div(class = "objective-text", "Understand R syntax basics (objects, functions, vectors, and data frames).")
                        ),
                        div(class = "objective-item",
                            icon("check-circle", class= "objective-icon"),
                            div(class = "objective-text", "Apply the three core rules of Tidy Data.")
                        ),
                        div(class = "objective-item",
                            icon("check-circle", class= "objective-icon"),
                            div(class = "objective-text", "Import raw tabular datasets (.csv and .xlsx) into R using relative paths.")
                        )
                    ),
                    div(class = "custom-card",
                        h4(class = "section-title", "Packages & Resources"),
                        h6("Required Setup Commands:", style = "font-weight:600; color:#475569; margin-top:0;"),
                        div(class = "code-container", style = "margin-top:0.3rem; margin-bottom:1rem; padding:0.8rem;",
                            pre("install.packages(c(\"tidyverse\", \"here\", \"readxl\"))")
                        ),
                        h6("Recommended Readings:", style = "font-weight:600; color:#475569; margin-top:0.5rem;"),
                        tags$a(href = "https://r4ds.hadley.nz/workflow-scripts", target = "_blank", class= "resource-link-btn",
                            icon("book-open"), "Workflow: Projects (R4DS)"
                        ),
                        tags$a(href = "https://www.jstatsoft.org/article/view/v059i10", target = "_blank", class= "resource-link-btn",
                            icon("file-alt"), "Tidy Data Paper (Wickham)"
                        )
                    )
                )
            ),

            div(class = "custom-card",
                h4(class = "section-title", "Interactive Lab: Tidy Data Reshaping (pivot_longer)"),
                p("Tidy data requires: (1) columns as variables, (2) rows as observations, and (3) cells as individual values. Excel sheets are often created in 'wide' layout (messy for plotting). Customize the values below to pivot the wide dataset into a clean, analysis-ready format."),

                layout_sidebar(
                    sidebar = sidebar(
                        title = "Pivoting Parameters",
                        selectizeInput("d1_pivot_cols", "Columns to Pivot:",
                            choices = c("CoralCover_2024", "CoralCover_2025", "CoralCover_2026"),
                            selected = c("CoralCover_2024", "CoralCover_2025", "CoralCover_2026"),
                            multiple = TRUE),
                        textInput("d1_names_to", "names_to (Year column):", "Year"),
                        textInput("d1_values_to", "values_to (Measurement column):", "PercentCover"),
                        textInput("d1_names_prefix", "names_prefix (strip text):", "CoralCover_"),
                        checkboxInput("d1_drop_na", "Drop Missing Values (drop_na)", FALSE)
                    ),

                    fluidRow(
                        column(width = 6,
                            h5("Messy / Wide Format (Excel style)", style = "color:#E74C3C; font-weight:600;"),
                            tableOutput("wide_table_view")
                        ),
                        column(width = 6,
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
            div(class = "day-header-banner",
                h3("Day 2: Data Wrangling, Joining, and Reshaping"),
                p("Transforming raw field data into clean, analysis-ready formats using tidyverse pipelines.")
            ),

            fluidRow(
                column(width = 7,
                    div(class = "custom-card",
                        h4(class = "section-title", "Daily Schedule"),
                        div(class = "schedule-item",
                            div(class = "schedule-time", "09:00 - 10:30"),
                            div(class = "schedule-title", "Session 1: Single-Table Wrangling"),
                            div(class = "schedule-desc", "Pipes, filter() for rows, select() for columns, and mutate() for calculating new variables.")
                        ),
                        div(class = "schedule-item",
                            div(class = "schedule-time", "11:00 - 12:30"),
                            div(class = "schedule-title", "Session 2: Group Summaries & Aggregation"),
                            div(class = "schedule-desc", "Grouping datasets using group_by(), summarizing with functions like mean(), sd(), n().")
                        ),
                        div(class = "schedule-item",
                            div(class = "schedule-time", "14:00 - 15:30"),
                            div(class = "schedule-title", "Session 3: Relational Joins & Reshaping"),
                            div(class = "schedule-desc", "Joining tables, appending rows, pivoting data long and wide.")
                        ),
                        div(class = "schedule-item",
                            div(class = "schedule-time", "15:30 - 17:00"),
                            div(class = "schedule-title", "Hands-On Lab 2 (BYOD)"),
                            div(class = "schedule-desc", "Clean, join, and summarize active research datasets.")
                        )
                    )
                ),
                column(width = 5,
                    div(class = "custom-card",
                        h4(class = "section-title", "Learning Objectives"),
                        div(class = "objective-item",
                            icon("check-circle", class= "objective-icon"),
                            div(class = "objective-text", "Chain R commands efficiently using the pipe operator ( |> or %>% ).")
                        ),
                        div(class = "objective-item",
                            icon("check-circle", class= "objective-icon"),
                            div(class = "objective-text", "Subset and transform datasets using dplyr core verbs (filter, select, mutate).")
                        ),
                        div(class = "objective-item",
                            icon("check-circle", class= "objective-icon"),
                            div(class = "objective-text", "Summarize complex datasets grouped by key factors using group_by and summarize.")
                        ),
                        div(class = "objective-item",
                            icon("check-circle", class= "objective-icon"),
                            div(class = "objective-text", "Merge multiple data sheets using joins (left_join, bind_rows).")
                        ),
                        div(class = "objective-item",
                            icon("check-circle", class= "objective-icon"),
                            div(class = "objective-text", "Reshape datasets between wide and long layouts using pivot_longer and pivot_wider.")
                        )
                    ),
                    div(class = "custom-card",
                        h4(class = "section-title", "Packages & Resources"),
                        h6("Required Setup Commands:", style = "font-weight:600; color:#475569; margin-top:0;"),
                        div(class = "code-container", style = "margin-top:0.3rem; margin-bottom:1rem; padding:0.8rem;",
                            pre("library(tidyverse)")
                        ),
                        h6("Recommended Readings:", style = "font-weight:600; color:#475569; margin-top:0.5rem;"),
                        tags$a(href = "https://r4ds.hadley.nz/data-transform", target = "_blank", class= "resource-link-btn",
                            icon("book-open"), "Data Transformation (R4DS)"
                        ),
                        tags$a(href = "https://r4ds.hadley.nz/data-import", target = "_blank", class= "resource-link-btn",
                            icon("book-open"), "Data Import (R4DS)"
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
                        ), selected = "1"),

                    selectizeInput("d2_select_cols", "Select Columns to Retain:",
                        choices = NULL, multiple = TRUE),

                    uiOutput("d2_filter_ui"),

                    selectInput("d2_arrange_var", "Arrange By (Sort Column):", choices = c("None" = "")),

                    checkboxInput("d2_do_summary", "Enable Group Summary", FALSE),

                    conditionalPanel(
                        condition = "input.d2_do_summary == true",
                        selectInput("d2_group_var", "Group By:", choices = NULL),
                        selectInput("d2_summary_var", "Summary Target Variable:", choices = NULL),
                        selectInput("d2_summary_fun", "Summary Statistic:",
                            choices = c("Mean" = "mean", "Maximum" = "max", "Minimum" = "min", "Count" = "n"))
                    ),

                    hr(),
                    uiOutput("d2_row_metric")
                ),

                div(class = "custom-card",
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
            div(class = "day-header-banner",
                h3("Day 3: Publication-Ready Graphics with ggplot2"),
                p("Mastering the Grammar of Graphics to generate high-resolution figures for journal submission.")
            ),

            fluidRow(
                column(width = 7,
                    div(class = "custom-card",
                        h4(class = "section-title", "Daily Schedule"),
                        div(class = "schedule-item",
                            div(class = "schedule-time", "09:00 - 10:30"),
                            div(class = "schedule-title", "Session 1: Grammar of Graphics & Geoms"),
                            div(class = "schedule-desc", "Data binding, aesthetic mappings (aes), geoms (geom_point, geom_boxplot, geom_line).")
                        ),
                        div(class = "schedule-item",
                            div(class = "schedule-time", "11:00 - 12:30"),
                            div(class = "schedule-title", "Session 2: Styling & Themes"),
                            div(class = "schedule-desc", "Theme modifications (theme_classic, theme_minimal), scale adjustments, legends, and faceting.")
                        ),
                        div(class = "schedule-item",
                            div(class = "schedule-time", "14:00 - 15:30"),
                            div(class = "schedule-title", "Session 3: Exporting to Journals"),
                            div(class = "schedule-desc", "Resolution control, vector PDF/TIFF export, pixel sizes, and ggsave().")
                        ),
                        div(class = "schedule-item",
                            div(class = "schedule-time", "15:30 - 17:00"),
                            div(class = "schedule-title", "Hands-On Lab 3 (BYOD)"),
                            div(class = "schedule-desc", "Create and refine a publication-quality figure using active thesis datasets.")
                        )
                    )
                ),
                column(width = 5,
                    div(class = "custom-card",
                        h4(class = "section-title", "Learning Objectives"),
                        div(class = "objective-item",
                            icon("check-circle", class= "objective-icon"),
                            div(class = "objective-text", "Understand the components of the Grammar of Graphics (Data, Aesthetics, Geoms).")
                        ),
                        div(class = "objective-item",
                            icon("check-circle", class= "objective-icon"),
                            div(class = "objective-text", "Choose and construct appropriate plots (scatters, boxes, histograms, bar charts).")
                        ),
                        div(class = "objective-item",
                            icon("check-circle", class= "objective-icon"),
                            div(class = "objective-text", "Apply customized color scales (e.g., Viridis, ColorBrewer) and themes.")
                        ),
                        div(class = "objective-item",
                            icon("check-circle", class= "objective-icon"),
                            div(class = "objective-text", "Implement panel layouts using facet_wrap and facet_grid.")
                        ),
                        div(class = "objective-item",
                            icon("check-circle", class= "objective-icon"),
                            div(class = "objective-text", "Save plots with precise resolutions and dimensions using ggsave().")
                        )
                    ),
                    div(class = "custom-card",
                        h4(class = "section-title", "Packages & Resources"),
                        h6("Required Setup Commands:", style = "font-weight:600; color:#475569; margin-top:0;"),
                        div(class = "code-container", style = "margin-top:0.3rem; margin-bottom:1rem; padding:0.8rem;",
                            pre("library(tidyverse)")
                        ),
                        h6("Recommended Readings:", style = "font-weight:600; color:#475569; margin-top:0.5rem;"),
                        tags$a(href = "https://r4ds.hadley.nz/data-visualize", target = "_blank", class= "resource-link-btn",
                            icon("book-open"), "Data Visualization (R4DS)"
                        ),
                        tags$a(href = "https://ggplot2.tidyverse.org/", target = "_blank", class= "resource-link-btn",
                            icon("external-link-alt"), "ggplot2 Documentation"
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
                        ), selected = "1"),

                    selectInput("d3_x", "X Variable (Aesthetic):", choices = NULL),
                    selectInput("d3_y", "Y Variable (Aesthetic):", choices = NULL),
                    selectInput("d3_color", "Color Variable (Optional):", choices = c("None" = "")),
                    selectInput("d3_facet", "Facet Panel Variable (Optional):", choices = c("None" = "")),

                    selectInput("d3_geom", "Plot Type (Geom):",
                        choices = c("Scatter Plot" = "point", "Boxplot" = "boxplot", "Bar Chart" = "bar", "Line Plot" = "line")),

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
                        choices = c("Classic" = "theme_classic", "Minimal" = "theme_minimal", "Black & White" = "theme_bw", "Light" = "theme_light")),

                    selectInput("d3_palette", "Color Palette:",
                        choices = c("Default" = "default", "Viridis" = "viridis", "ColorBrewer (Set2)" = "set2", "ColorBrewer (Dark2)" = "dark2")),

                    hr(),
                    h5("Export Parameters:"),
                    sliderInput("d3_width", "Width (inches):", min = 4, max = 12, value = 7, step = 0.5),
                    sliderInput("d3_height", "Height (inches):", min = 3, max = 10, value = 5, step = 0.5),
                    sliderInput("d3_dpi", "DPI Resolution:", min = 72, max = 400, value = 150, step = 10),
                    downloadButton("d3_download", "Download Publication Figure", class = "btn-success", style = "width:100%;")
                ),

                div(class = "custom-card",
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
        # Sub - tab: Syllabus Overview
        nav_panel(
                title = "Track Overview & Schedule",
                fluidPage(
                    br(),
                    div(class = "day-header-banner",
                        h3("Day 4: Domain-Specific Modules (Elective Tracks)"),
                        p("Cohort splits into parallel tracks covering Spatial GIS mapping and Advanced Statistical modeling.")
                    ),

                    fluidRow(
                        column(width = 7,
                            div(class = "custom-card",
                                h4(class = "section-title", "Daily Schedule"),
                                div(class = "schedule-item",
                                    div(class = "schedule-time", "09:00 - 10:30"),
                                    div(class = "schedule-title", "Session 1: Foundations"),
                                    div(class = "schedule-desc", "Track A: Vector GIS shapefiles using sf, projections, and cartography. / Track B: Hypothesis testing (t-tests, ANOVA) using testflow and rstatix.")
                                ),
                                div(class = "schedule-item",
                                    div(class = "schedule-time", "11:00 - 12:30"),
                                    div(class = "schedule-title", "Session 2: Advanced Operations"),
                                    div(class = "schedule-desc", "Track A: Raster calculations with terra, DEM extractions. / Track B: Multiple regression lm(), coefficients, diagnostics.")
                                ),
                                div(class = "schedule-item",
                                    div(class = "schedule-time", "14:00 - 15:30"),
                                    div(class = "schedule-title", "Session 3: Elective Practice"),
                                    div(class = "schedule-desc", "Hands-on lab 4-A (GIS maps and raster values) and lab 4-B (statistical modeling on environmental data).")
                                ),
                                div(class = "schedule-item",
                                    div(class = "schedule-time", "15:30 - 17:00"),
                                    div(class = "schedule-title", "Joint Synthesis & Presentations"),
                                    div(class = "schedule-desc", "Cohorts merge back to present maps, regression matrices, and model diagnostic parameters.")
                                )
                            )
                        ),
                        column(width = 5,
                            div(class = "custom-card",
                                h4(class = "section-title", "Learning Objectives"),
                                div(class = "objective-item",
                                    icon("check-circle", class= "objective-icon"),
                                    div(class = "objective-text", "Read vector shapefiles using sf and plot map layers.")
                                ),
                                div(class = "objective-item",
                                    icon("check-circle", class= "objective-icon"),
                                    div(class = "objective-text", "Perform spatial coordinate transformations (CRS 4326 to CRS 32737).")
                                ),
                                div(class = "objective-item",
                                    icon("check-circle", class= "objective-icon"),
                                    div(class = "objective-text", "Manage raster DEM models using terra and extract values.")
                                ),
                                div(class = "objective-item",
                                    icon("check-circle", class= "objective-icon"),
                                    div(class = "objective-text", "Execute t-tests, ANOVA comparisons, and regression models.")
                                ),
                                div(class = "objective-item",
                                    icon("check-circle", class= "objective-icon"),
                                    div(class = "objective-text", "Evaluate regression assumptions and model diagnostic plots.")
                                )
                            ),
                            div(class = "custom-card",
                                h4(class = "section-title", "Required Setup Packages"),
                                h6("Track A (Spatial):", style = "font-weight:600; color:#475569; margin-top:0;"),
                                div(class = "code-container", style = "margin-top:0.3rem; margin-bottom:0.8rem; padding:0.6rem; font-size:0.75rem;",
                                    pre("install.packages(c(\"sf\", \"terra\", \"tidyterra\"))")
                                ),
                                h6("Track B (Stats):", style = "font-weight:600; color:#475569; margin-top:0.2rem;"),
                                div(class = "code-container", style = "margin-top:0.3rem; margin-bottom:0.8rem; padding:0.6rem; font-size:0.75rem;",
                                    pre("install.packages(c(\"testflow\", \"car\", \"flextable\", \"rstatix\"))")
                                ),
                                h6("Recommended Readings:", style = "font-weight:600; color:#475569; margin-top:0.4rem;"),
                                tags$a(href = "https://r.geocompx.org/", target = "_blank", class= "resource-link-btn",
                                    icon("globe"), "Geocomputation with R"
                                ),
                                tags$a(href = "https://www.modernstatisticswithr.com/", target = "_blank", class= "resource-link-btn",
                                    icon("calculator"), "Modern Stats with R"
                                )
                            )
                        )
                    )
                )
            ),
        
        # Sub - tab: Track A
        nav_panel(
                title = "Track A: Spatial Data & GIS",
                fluidPage(
                    br(),
                    div(class = "custom-card",
                        h4(class = "section-title", "Track A: GIS Interactive Playground"),
                        p("This playground implements vector GIS mapping. Adjust the slider to filter plastic cleanup stations and view coordinates mapped via Leaflet.")
                    ),

                    fluidRow(
                        column(width = 8,
                            div(class = "custom-card",
                                h5("Interactive Plastic Waste Coastal Map", style = "color:#0A3B5C; font-weight:600;"),
                                leafletOutput("d4_spatial_map", height = "500px")
                            )
                        ),
                        column(width = 4,
                            div(class = "custom-card",
                                h5("Filter Sites", style = "color:#0A3B5C; font-weight:600;"),
                                sliderInput("d4_plastic_min", "Min Macroplastics Count:", min = 0, max = 500, value = 50),
                                hr(),
                                h5("Spatial Grid Selector (CRS)", style = "color:#0A3B5C; font-weight:600;"),
                                selectInput("d4_crs_proj", "Choose Projected System:",
                                    choices = c(
                                        "Tanzania UTM Zone 37S (EPSG:32737)" = "32737",
                                        "Arc 1960 / UTM Zone 37S (EPSG:21037)" = "21037",
                                        "Web Mercator (EPSG:3857)" = "3857"
                                    ), selected = "32737"),
                                hr(),
                                h5("Distance Calculator to EDEMA", style = "color:#0A3B5C; font-weight:600;"),
                                selectInput("d4_target_site", "Select Target Station:", choices = NULL),
                                uiOutput("d4_distance_report"),
                                hr(),
                                h5("Accommodation Route Calculator", style = "color:#0A3B5C; font-weight:600;"),
                                fluidRow(
                                    column(width = 6,
                                        numericInput("d4_accom_lat", "Latitude:", value = -6.8164, min = -7.2, max = -6.5, step = 0.0001)
                                    ),
                                    column(width = 6,
                                        numericInput("d4_accom_lon", "Longitude:", value = 37.6545, min = 37.4, max = 37.9, step = 0.0001)
                                    )
                                ),
                                selectInput("d4_travel_mode", "Mode of Transport:",
                                    choices = c("Driving (Car)" = "driving", "Walking (Foot)" = "foot")),
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
        
        # Sub - tab: Track B
        nav_panel(
                title = "Track B: Statistical Modeling",
                fluidPage(
                    br(),
                    div(class = "custom-card",
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
                                )),

                            conditionalPanel(
                                condition = "input.d4_model_type == 'ttest'",
                                selectInput("d4_ttest_response", "Numeric Response Variable:",
                                    choices = c("Soil Organic Carbon (%)" = "soil_organic_carbon_pct", "Crop Yield (t/ha)" = "yield_tonnes_ha")),
                                selectInput("d4_ttest_group", "Grouping Factor:",
                                    choices = c("Crop Type" = "crop_type", "District" = "district"))
                            ),
                            conditionalPanel(
                                condition = "input.d4_model_type == 'regression'",
                                selectInput("d4_reg_response", "Response Variable (Y):",
                                    choices = c("Tree Height (m)" = "height_m", "Crop Yield (t/ha)" = "yield_tonnes_ha")),
                                selectizeInput("d4_reg_predictors", "Predictor Variables (X):",
                                    choices = NULL, multiple = TRUE),
                                hr(),
                                uiOutput("d4_prediction_inputs")
                            ),

                            actionButton("d4_run_model", "Run Model Analysis", class = "btn-primary", style = "width:100%;")
                        ),

                        div(class = "custom-card",
                            h5("Statistical & Diagnostic Outputs", style = "color:#0A3B5C; font-weight:600;"),
                            p("Click 'Run Model Analysis' to update calculations and assumptions."),

                            tabsetPanel(
                                tabPanel("Report Summary",
                                    verbatimTextOutput("d4_model_summary")
                                ),
                                tabPanel("Model Assumptions",
                                    plotOutput("d4_model_diagnostics", height = "400px")
                                ),
                                tabPanel("Assumptions Scorecard",
                                    br(),
                                    uiOutput("d4_assumptions_scorecard")
                                ),
                                tabPanel("Predictive Tool",
                                    br(),
                                    uiOutput("d4_prediction_card")
                                ),
                                tabPanel("R Code Block",
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
            div(class = "day-header-banner",
                h3("Day 5: Reproducible Quarto Reports & Project Delivery"),
                p("Integrating code, tables, figures, and narratives into automated PDF, HTML, and Word formats.")
            ),

            fluidRow(
                column(width = 7,
                    div(class = "custom-card",
                        h4(class = "section-title", "Daily Schedule"),
                        div(class = "schedule-item",
                            div(class = "schedule-time", "09:00 - 10:30"),
                            div(class = "schedule-title", "Session 1: Quarto Foundations"),
                            div(class = "schedule-desc", "YAML frontmatter, Markdown syntax, embedding code chunks, and rendering reports.")
                        ),
                        div(class = "schedule-item",
                            div(class = "schedule-time", "11:00 - 12:30"),
                            div(class = "schedule-title", "Session 2: Formatting Options"),
                            div(class = "schedule-desc", "Cross-referencing figures and tables, chunk control options (echo, warning), Word/PDF templates.")
                        ),
                        div(class = "schedule-item",
                            div(class = "schedule-time", "14:00 - 16:30"),
                            div(class = "schedule-title", "Session 3: Cohort Presentations"),
                            div(class = "schedule-desc", "Final Theme Table presentations of reproducible R data workflows.")
                        ),
                        div(class = "schedule-item",
                            div(class = "schedule-time", "16:30 - 17:00"),
                            div(class = "schedule-title", "Applied Statistics Wrap-up"),
                            div(class = "schedule-desc", "Distribution of certificates, post-training feedback survey, and closing remarks.")
                        )
                    )
                ),
                column(width = 5,
                    div(class = "custom-card",
                        h4(class = "section-title", "Learning Objectives"),
                        div(class = "objective-item",
                            icon("check-circle", class= "objective-icon"),
                            div(class = "objective-text", "Construct and compile a Quarto document (.qmd).")
                        ),
                        div(class = "objective-item",
                            icon("check-circle", class= "objective-icon"),
                            div(class = "objective-text", "Style reports using Markdown headers, lists, links, and math equations.")
                        ),
                        div(class = "objective-item",
                            icon("check-circle", class= "objective-icon"),
                            div(class = "objective-text", "Configure code chunk parameters (echo, warning, message, tbl-cap, fig-cap).")
                        ),
                        div(class = "objective-item",
                            icon("check-circle", class= "objective-icon"),
                            div(class = "objective-text", "Render documents directly to Microsoft Word, PDF, or HTML formats.")
                        ),
                        div(class = "objective-item",
                            icon("check-circle", class= "objective-icon"),
                            div(class = "objective-text", "Apply professional styling using document layout presets.")
                        )
                    ),
                    div(class = "custom-card",
                        h4(class = "section-title", "Packages & Resources"),
                        h6("Required Setup Commands:", style = "font-weight:600; color:#475569; margin-top:0;"),
                        div(class = "code-container", style = "margin-top:0.3rem; margin-bottom:1rem; padding:0.8rem;",
                            pre("library(tidyverse)\nlibrary(flextable)\nlibrary(knitr)")
                        ),
                        h6("Recommended Readings:", style = "font-weight:600; color:#475569; margin-top:0.5rem;"),
                        tags$a(href = "https://quarto.org/", target = "_blank", class= "resource-link-btn",
                            icon("external-link-alt"), "Quarto Official Website"
                        ),
                        tags$a(href = "https://www.markdownguide.org/", target = "_blank", class= "resource-link-btn",
                            icon("book"), "Markdown Style Guide"
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
                        choices = c("cosmo", "flatly", "sandstone", "united", "lux", "slate")),
                    selectInput("d5_code_fold", "Code Folding:",
                        choices = c("Show Code (none)" = "none", "Fold Code (true)" = "true", "Hide Code (false)" = "false")),
                    checkboxInput("d5_toc_input", "Include Table of Contents", TRUE),
                    hr(),
                    h5("Export Deliverables:"),
                    downloadButton("d5_download_qmd", "Download QMD Template", class = "btn-success", style = "width:100%;"),
                    br(),
                    downloadButton("d5_download_pdf", "Download Mock Rendered PDF", class = "btn-primary", style = "width:100%;"),
                    br(),
                    downloadButton("d5_download_templates", "Download Script Templates (.ZIP)", class = "btn-info", style = "width:100%;")
                ),

                div(class = "custom-card",
                    h4(class = "section-title", "Quarto Source Code Preview"),
                    p("Below is an interactive view of a Quarto report template configured with chunk headers."),
                    uiOutput("d5_preview_ui")
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
            div(class = "day-header-banner",
                h3("Bahari Yetu Knowledge Quiz"),
                p("Test your understanding of the R coding, plotting, mapping, and modeling concepts covered throughout the training.")
            ),

            fluidRow(
                column(width = 3,
                    div(class = "custom-card",
                        h4(class = "section-title", "Quiz Navigator"),
                        selectInput("quiz_day", "Choose a Day:",
                            choices = c(
                                "Day 1: Foundations & Tidy Data" = "day1",
                                "Day 2: Data Wrangling (dplyr)" = "day2",
                                "Day 3: ggplot2 Graphics" = "day3",
                                "Day 4: Spatial GIS & Modeling" = "day4",
                                "Day 5: Reproducible Quarto" = "day5"
                            )),
                        selectInput("quiz_question_num", "Choose a Question:",
                            choices = c(
                                "Question 1" = "q1",
                                "Question 2" = "q2",
                                "Question 3" = "q3",
                                "Question 4" = "q4",
                                "Question 5" = "q5"
                            )),
                        hr(),
                        uiOutput("quiz_score_ratio")
                    )
                ),
                column(width = 5,
                    div(class = "custom-card", style = "min-height: 400px; display: flex; flex-direction: column; justify-content: space-between;",
                        div(
                            h4(class = "section-title", "Active Question"),
                            uiOutput("quiz_question_ui")
                        ),
                        div(style = "margin-top: 1.5rem;",
                            actionButton("quiz_submit", "Submit Answer", class = "btn-primary", style = "width:100%;")
                        )
                    )
                ),
                column(width = 4,
                    uiOutput("quiz_feedback_ui")
                )
            )
        )
    ),
  
  # ---------------------------------------------------------
  # TAB: Facilitator
  # ---------------------------------------------------------
    nav_panel(
        title = "Facilitator",
        icon = icon("chalkboard-teacher"),
        fluidPage(
            fluidRow(
                column(width = 4,
                    div(class = "custom-card", style = "text-align: center; padding-top: 2rem;",
            # Icon as placeholder for facilitator portrait
            tags$div(
                        style = "width: 120px; height: 120px; border-radius: 60px; background-color: #0A3B5C; color: white; display: inline-flex; align-items: center; justify-content: center; font-size: 3rem; margin-bottom: 1.5rem; box-shadow: 0 4px 10px rgba(0,0,0,0.15);",
                        icon("user-tie")
                    ),
    h4("Masumbuko Semba", style = "font-family: 'Outfit', sans-serif; font-weight:700; color:#0A3B5C; margin-bottom: 0.2rem;"),
    p("Lead Instructor", style = "color:#64748b; font-weight:600; text-transform:uppercase; font-size:0.85rem; letter-spacing:0.5px;"),
    hr(),
    p("Masumbuko is an oceanographer, data scientist, and lead instructor for the Bahari Yetu Scholarly Training Program.", style = "font-size:0.9rem; text-align:justify; color: #475569;"),
    p("He specializes in R programming, spatial analytics (GIS), and climate models, helping scholars transform field data into scholarly reports.", style = "font-size:0.9rem; text-align:justify; color: #475569;")
)
        ),

column(width = 8,
    div(class = "custom-card",
        h4(class = "section-title", "Training Venue & Location Map"),
        p("The training is held at the ", strong("EDEMA Conference Hall"), " in Morogoro, Tanzania. Toggle map basemaps via the control panel. Enter your accommodation coordinates below to calculate paths and estimate travel times dynamically."),

        leafletOutput("venue_map", height = "400px"),

        br(),
            
            # Interactive Route Planner
            fluidRow(
            column(width = 6,
                div(style = "background: #f8fafc; border: 1px solid #e2e8f0; padding: 1.2rem; border-radius: 10px; height: 100%;",
                    h5("Accommodation Route Calculator", style = "font-weight:700; color:#0A3B5C; margin-top:0; margin-bottom:1rem;"),

                    fluidRow(
                        column(width = 6,
                            numericInput("accom_lat", "Latitude:", value = -6.8164, min = -7.2, max = -6.5, step = 0.0001)
                        ),
                        column(width = 6,
                            numericInput("accom_lon", "Longitude:", value = 37.6545, min = 37.4, max = 37.9, step = 0.0001)
                        )
                    ),

                    selectInput("travel_mode", "Mode of Transport:",
                        choices = c("Driving (Car)" = "driving", "Walking (Foot)" = "foot")),

                    actionButton("calculate_route", "Find Path to Edema", class = "btn-primary", style = "width:100%;", icon = icon("route")),
                    p("Real-time network estimation powered by OSRM.", style = "font-size:0.75rem; color:#94a3b8; margin-top:0.5rem; margin-bottom:0;")
                )
            ),
            column(width = 6,
                uiOutput("route_summary")
            )
        ),

        br(),
        h5("General Logistics checklist:", style = "color:#0A3B5C; font-weight:600;"),
        layout_column_wrap(
            width = 1 / 2,
            tags$ul(style = "padding-left:1.2rem; font-size: 0.9rem; line-height: 1.6;",
                tags$li("Accommodation check-in opens at 14:00 Sunday."),
                tags$li("Laptops must have R 4.6.1 + IDE installed."),
                tags$li("Bring active raw field measurements for Friday's session.")
            ),
            tags$ul(style = "padding-left:1.2rem; font-size: 0.9rem; line-height: 1.6;",
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

# ---------------------------------------------------------
# SERVER SIDE LOGIC
# ---------------------------------------------------------
    server < - function (input, output, session) {
  
  # ---------------------------------------------------------
  # Day 1: Tidy Data Pivot Demo
  # ---------------------------------------------------------
  # Simulated wide dataset
        wide_data < - tibble(
            Site = c("Changuu Reef", "Bawe Reef", "Chumbe Sanctuary"),
            CoralCover_2024 = c(42.5, 31.2, 58.9),
            CoralCover_2025 = c(40.1, 29.8, 61.2),
            CoralCover_2026 = c(37.4, 28.5, 63.4)
        )

        output$wide_table_view < - renderTable({
            wide_data
        }, striped = TRUE, bordered = TRUE, align = 'c')

        output$tidy_table_view < - renderTable({
            cols<- input$d1_pivot_cols
    if (length(cols) == 0) return (data.frame(Message = "Select columns to pivot"))

        res < - wide_data |>
            pivot_longer(
                cols = all_of(cols),
                names_to = input$d1_names_to,
                names_prefix = input$d1_names_prefix,
                values_to = input$d1_values_to
            )

        if (input$d1_drop_na) {
            res < - res |> drop_na(all_of(input$d1_values_to))
        }

        res
    }, striped = TRUE, bordered = TRUE, align = 'c')

output$d1_code_output < - renderUI({
    cols_str<- paste(sprintf('"%s"', input$d1_pivot_cols), collapse = ", ")
    drop_str < - if (input$d1_drop_na) sprintf(" |>\n  drop_na(%s)", input$d1_values_to) else ""

code_text < - sprintf(
    "library(tidyverse)

# Pivot the wide dataset to a tidy long dataset
tidy_data < - wide_data |>
    pivot_longer(
        cols = c(% s),
        names_to = \"%s\",
    names_prefix = \"%s\",
    values_to = \"%s\"
    ) % s",
    cols_str, input$d1_names_to, input$d1_names_prefix, input$d1_values_to, drop_str
)

div(class = "code-container", pre(code_text))
  })
  
  # ---------------------------------------------------------
  # Day 2: Wrangling Playground
  # ---------------------------------------------------------
  # Reactive loader for selected dataset
  d2_raw_data < - reactive({
    load_dataset(input$d2_dataset)
})
  
  # Update column selection options based on chosen dataset
observe({
    df<- d2_raw_data()
    if (!is.null(df)) {
    updateSelectizeInput(session, "d2_select_cols",
        choices = colnames(df),
        selected = colnames(df))
}
  })
  
  # Render a dynamic slider filter based on numeric column
output$d2_filter_ui < - renderUI({
    df<- d2_raw_data()
    if (is.null(df)) return (NULL)
    
    # Find first numeric column to slide on
num_cols < - sapply(df, is.numeric)
num_col_names < - names(num_cols)[num_cols]

if (length(num_col_names) == 0) return (NULL)

target_col < - num_col_names[1]
min_val < - min(df[[target_col]], na.rm = TRUE)
max_val < - max(df[[target_col]], na.rm = TRUE)
    
    # Show slider
sliderInput("d2_filter_range",
    label = sprintf("Filter Range of %s:", target_col),
    min = floor(min_val),
    max = ceiling(max_val),
    value = c(floor(min_val), ceiling(max_val)))
  })
  
  # Update group by / summary choices
observe({
    df<- d2_raw_data()
    if (!is.null(df)) {
    char_cols < - colnames(df)[sapply(df, function (x) is.character(x) || is.factor(x))]
    num_cols < - colnames(df)[sapply(df, is.numeric)]

    updateSelectInput(session, "d2_group_var", choices = char_cols)
    updateSelectInput(session, "d2_summary_var", choices = num_cols)
    updateSelectInput(session, "d2_arrange_var", choices = c("None" = "", colnames(df)))
}
  })
  
  # Wrangled calculations
d2_wrangled_result < - reactive({
    df<- d2_raw_data()
    if (is.null(df)) return (NULL)
    
    # 1. Select
selected_cols < - input$d2_select_cols
if (length(selected_cols) > 0) {
    df < - df |> select(all_of(selected_cols))
}
    
    # 2. Filter
num_cols < - sapply(df, is.numeric)
num_col_names < - names(num_cols)[num_cols]
if (length(num_col_names) > 0 && !is.null(input$d2_filter_range)) {
    target_col < - num_col_names[1]
    df < - df[df[[target_col]] >= input$d2_filter_range[1] & df[[target_col]] <= input$d2_filter_range[2], ]
}
    
    # 3. Summarize
if (input$d2_do_summary) {
    g_var < - input$d2_group_var
    s_var < - input$d2_summary_var
    s_fun < - input$d2_summary_fun

    if (!is.null(g_var) && g_var != "" && !is.null(s_var) && s_var != "") {
        # Dynamically build summary
        if (s_fun == "mean") {
            df < - df |> group_by(.data[[g_var]]) |> summarize(Mean = round(mean(.data[[s_var]], na.rm = TRUE), 2))
        } else if (s_fun == "max") {
            df < - df |> group_by(.data[[g_var]]) |> summarize(Max = max(.data[[s_var]], na.rm = TRUE))
        } else if (s_fun == "min") {
            df < - df |> group_by(.data[[g_var]]) |> summarize(Min = min(.data[[s_var]], na.rm = TRUE))
        } else {
            df < - df |> group_by(.data[[g_var]]) |> summarize(Count = n())
        }
    }
}
    
    # 4. Arrange(Sort)
if (!is.null(input$d2_arrange_var) && input$d2_arrange_var != "None" && input$d2_arrange_var != "") {
    df < - df |> arrange(.data[[input$d2_arrange_var]])
}

return (df)
  })
  
  # Code generator
output$d2_code_output < - renderUI({
    meta<- datasets_metadata[[as.character(input$d2_dataset)]]
    
    select_str < - ""
    if (length(input$d2_select_cols) > 0) {
    select_str < - sprintf("  select(%s) |>\n", paste(input$d2_select_cols, collapse = ", "))
}

filter_str < - ""
df_raw < - d2_raw_data()
if (!is.null(df_raw)) {
    num_cols < - sapply(df_raw, is.numeric)
    num_col_names < - names(num_cols)[num_cols]
    if (length(num_col_names) > 0 && !is.null(input$d2_filter_range)) {
        filter_str < - sprintf("  filter(%s >= %s & %s <= %s) |>\n",
            num_col_names[1], input$d2_filter_range[1],
            num_col_names[1], input$d2_filter_range[2])
    }
}

sum_str < - ""
if (input$d2_do_summary && input$d2_group_var != "" && input$d2_summary_var != "") {
    if (input$d2_summary_fun == "mean") {
        fun_str < - sprintf("mean(%s, na.rm = TRUE)", input$d2_summary_var)
    } else if (input$d2_summary_fun == "max") {
        fun_str < - sprintf("max(%s, na.rm = TRUE)", input$d2_summary_var)
    } else if (input$d2_summary_fun == "min") {
        fun_str < - sprintf("min(%s, na.rm = TRUE)", input$d2_summary_var)
    } else {
        fun_str < - "n()"
    }
    sum_str < - sprintf("  group_by(%s) |>\n  summarize(Value = %s) |>\n", input$d2_group_var, fun_str)
}

arrange_str < - ""
if (!is.null(input$d2_arrange_var) && input$d2_arrange_var != "None" && input$d2_arrange_var != "") {
    arrange_str < - sprintf("  arrange(%s) |>\n", input$d2_arrange_var)
}
    
    # Combine pipelines
pipeline < - paste0(select_str, filter_str, sum_str, arrange_str)
    # Strip trailing " |>\n"
if (grepl(" \\|>\n$", pipeline)) {
    pipeline < - gsub(" \\|>\n$", "", pipeline)
}

code_text < - sprintf("library(tidyverse)\nlibrary(here)\n\n# Load raw theme dataset\ndf <- read_csv(here(\"data\", \"%s\"))\n\ndf_wrangled <- df |>\n%s",
    meta$file, pipeline)

div(class = "code-container", pre(code_text))
  })
  
  # Row retention metric output
output$d2_row_metric < - renderUI({
    df_raw<- d2_raw_data()
    df_wrangled < - d2_wrangled_result()
    if (is.null(df_raw) || is.null(df_wrangled)) return (NULL)

n_raw < - nrow(df_raw)
n_wrangled < - nrow(df_wrangled)
pct < - round((n_wrangled / n_raw) * 100)

div(style = "font-size:0.82rem; color:#475569; margin-top:0.5rem;",
    div(style = "display:flex; justify-content:space-between; font-weight:700; margin-bottom:0.2rem;",
        span("Row Retention:"),
        span(sprintf("%d / %d (%d%%)", n_wrangled, n_raw, pct))
    ),
    div(class= "progress", style = "height:6px; background-color:#e2e8f0; border-radius:4px; overflow:hidden;",
        div(class= "progress-bar", style = sprintf("width: %d%%; background-color: #2ECC71; height:100%%; transition: width 0.3s ease;", pct))
    )
)
  })

output$d2_table_output < - renderDT({
    res<- d2_wrangled_result()
    if (is.null(res)) return (NULL)
datatable(res, options = list(pageLength = 8, scrollX = TRUE), class = 'cell-border stripe')
  })
  
  # ---------------------------------------------------------
  # Day 3: ggplot2 Builder
  # ---------------------------------------------------------
    d3_raw_data < - reactive({
        load_dataset(input$d3_dataset)
    })
  
  # Update X, Y, Color, and Facet choices
observe({
    df<- d3_raw_data()
    if (!is.null(df)) {
    updateSelectInput(session, "d3_x", choices = colnames(df), selected = colnames(df)[1])
    updateSelectInput(session, "d3_y", choices = colnames(df), selected = colnames(df)[if (ncol(df) > 1) 2 else 1])

    char_cols < - colnames(df)[sapply(df, function (x) is.character(x) || is.factor(x))]
    updateSelectInput(session, "d3_color", choices = c("None" = "", char_cols))
    updateSelectInput(session, "d3_facet", choices = c("None" = "", char_cols))
}
  })
  
  # Construct ggplot reactively
d3_plot_obj < - reactive({
    df<- d3_raw_data()
    if (is.null(df) || is.null(input$d3_x) || is.null(input$d3_y)) return (NULL)
    
    # Build plot mapping
if (input$d3_color != "") {
    p < - ggplot(df, aes_string(x = input$d3_x, y = input$d3_y, color = input$d3_color, fill = input$d3_color))
} else {
    p < - ggplot(df, aes_string(x = input$d3_x, y = input$d3_y))
}
    
    # Add Geoms
if (input$d3_geom == "point") {
    p < - p + geom_point(size = 3.5, alpha = 0.8)
} else if (input$d3_geom == "boxplot") {
    p < - p + geom_boxplot(alpha = 0.7, width = 0.5, outlier.shape = NA)
    if (input$d3_add_jitter) {
        p < - p + geom_jitter(width = 0.15, alpha = 0.5, size = 1.8)
    }
} else if (input$d3_geom == "bar") {
    p < - p + geom_col(alpha = 0.8, width = 0.6)
    if (input$d3_add_jitter) {
        p < - p + geom_jitter(width = 0.15, alpha = 0.5, size = 1.8)
    }
} else if (input$d3_geom == "line") {
    p < - p + geom_line(size = 1.2) + geom_point(size = 2.5)
}
    
    # Apply theme with custom base font size
b_size < - input$d3_base_size
if (input$d3_theme == "theme_classic") {
    p < - p + theme_classic(base_size = b_size)
} else if (input$d3_theme == "theme_minimal") {
    p < - p + theme_minimal(base_size = b_size)
} else if (input$d3_theme == "theme_bw") {
    p < - p + theme_bw(base_size = b_size)
} else if (input$d3_theme == "theme_light") {
    p < - p + theme_light(base_size = b_size)
}
    
    # Apply color palette
if (input$d3_color != "") {
    if (input$d3_palette == "viridis") {
        p < - p + scale_color_viridis_d() + scale_fill_viridis_d()
    } else if (input$d3_palette == "set2") {
        p < - p + scale_color_brewer(palette = "Set2") + scale_fill_brewer(palette = "Set2")
    } else if (input$d3_palette == "dark2") {
        p < - p + scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2")
    } else {
        p < - p + scale_color_manual(values = c("#0A3B5C", "#4A7C59", "#E74C3C", "#F39C12", "#9B59B6")) +
            scale_fill_manual(values = c("#0A3B5C", "#4A7C59", "#E74C3C", "#F39C12", "#9B59B6"))
    }
} else {
    if (input$d3_geom == "point") {
        p < - p + scale_color_manual(values = "#0A3B5C")
    } else {
        p < - p + scale_fill_manual(values = "#0A3B5C")
    }
}
    
    # Add faceting
if (!is.null(input$d3_facet) && input$d3_facet != "None" && input$d3_facet != "") {
    p < - p + facet_wrap(as.formula(paste("~", input$d3_facet)))
}
    
    # Premium text adjustments
p < - p + theme(
    text = element_text(family = "Inter"),
    plot.title = element_text(family = "Outfit", face = "bold", color = "#0A3B5C"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
)

title_text < - if (input$d3_title != "") input$d3_title else sprintf("Publication Quality Analysis: %s", datasets_metadata[[as.character(input$d3_dataset)]]$name)
sub_text < - if (input$d3_subtitle != "") input$d3_subtitle else sprintf("Plotted using ggplot2 | %s geom", input$d3_geom)
x_label < - if (input$d3_xlab != "") input$d3_xlab else input$d3_x
y_label < - if (input$d3_ylab != "") input$d3_ylab else input$d3_y

p < - p + labs(
    title = title_text,
    subtitle = sub_text,
    x = x_label,
    y = y_label
)

return (p)
  })

output$d3_plot < - renderPlot({
    p<- d3_plot_obj()
    if (!is.null(p)) p
  })
  
  # Code output
output$d3_code_output < - renderUI({
    meta<- datasets_metadata[[as.character(input$d3_dataset)]]
    
    color_aes < - ""
    color_scale < - ""
    if (input$d3_color != "") {
    color_aes < - sprintf(", color = %s, fill = %s", input$d3_color, input$d3_color)

    if (input$d3_palette == "viridis") {
        color_scale < - "  scale_color_viridis_d() +\n  scale_fill_viridis_d() +\n"
    } else if (input$d3_palette == "set2") {
        color_scale < - "  scale_color_brewer(palette = \"Set2\") +\n  scale_fill_brewer(palette = \"Set2\") +\n"
    } else if (input$d3_palette == "dark2") {
        color_scale < - "  scale_color_brewer(palette = \"Dark2\") +\n  scale_fill_brewer(palette = \"Dark2\") +\n"
    }
}

jitter_str < - ""
if (input$d3_add_jitter && input$d3_geom %in% c("boxplot", "bar")) {
    jitter_str < - " +\n  geom_jitter(width = 0.15, alpha = 0.5, size = 1.8)"
}

geom_str < - switch (input$d3_geom,
"point" = "geom_point(size = 3.5, alpha = 0.8)",
"boxplot" = sprintf("geom_boxplot(alpha = 0.7, width = 0.5, outlier.shape = NA)%s", jitter_str),
"bar" = sprintf("geom_col(alpha = 0.8, width = 0.6)%s", jitter_str),
"line" = "geom_line(size = 1.2) + geom_point(size = 2.5)")
    
    facet_str < - ""
if (!is.null(input$d3_facet) && input$d3_facet != "None" && input$d3_facet != "") {
    facet_str < - sprintf("  facet_wrap(~%s) +\n", input$d3_facet)
}

title_val < - if (input$d3_title != "") input$d3_title else "Publication Quality Analysis"
x_val < - if (input$d3_xlab != "") input$d3_xlab else input$d3_x
y_val < - if (input$d3_ylab != "") input$d3_ylab else input$d3_y

code_text < - sprintf(
    "library(tidyverse)
library(here)

# Load data
df < - read_csv(here(\"data\", \"%s\"))

# Plot figure
p < - ggplot(df, aes(x = % s, y = % s % s)) +
  % s +
  % s % s % s(base_size = % d) +
        labs(
            title = \"%s\",
    x = \"%s\",
    y = \"%s\"
        )

# Save high - res plot for journal submission
ggsave(\"outputs/figure1.tiff\", plot = p, width = %s, height = %s, dpi = %s)",
            meta$file, input$d3_x, input$d3_y, color_aes, geom_str, color_scale, facet_str, input$d3_theme, input$d3_base_size,
            title_val, x_val, y_val, input$d3_width, input$d3_height, input$d3_dpi
        )
    
    div(class = "code-container", pre(code_text))
  })
  
  # Download handler
output$d3_download < - downloadHandler(
    filename = function () {
        paste0("figure_", Sys.Date(), ".tiff")
    },
    content = function (file) {
        ggsave(file, plot = d3_plot_obj(),
            width = input$d3_width,
            height = input$d3_height,
            dpi = input$d3_dpi,
            device = "tiff")
    }
)
  
  # ---------------------------------------------------------
  # Day 4: Track A(Spatial Map)
  # ---------------------------------------------------------
    d4_spatial_data < - reactive({
        load_dataset("6") # Theme 6: Plastic Waste
    })
  
  # Populate target stations select dynamically
observe({
    df<- d4_spatial_data()
    if (!is.null(df)) {
    updateSelectInput(session, "d4_target_site", choices = df$station_id)
}
  })

output$d4_spatial_map < - renderLeaflet({
    df<- d4_spatial_data()
    if (is.null(df)) return (NULL)
    
    # Filter
df_filt < - df |> filter(macroplastics_count >= input$d4_plastic_min)
    
    # Reproject coordinates on - the - fly to chosen CRS
crs_target < - as.numeric(input$d4_crs_proj)
crs_label < - switch (input$d4_crs_proj,
"32737" = "UTM 37S (EPSG:32737)",
"21037" = "Arc 1960 Zone 37S (EPSG:21037)",
"3857" = "Web Mercator (EPSG:3857)")
    
    if (nrow(df_filt) > 0) {
    df_sf < - st_as_sf(df_filt, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
    df_proj < - st_transform(df_sf, crs = crs_target)
    proj_coords < - st_coordinates(df_proj)
    df_filt$proj_x < - round(proj_coords[, 1], 1)
    df_filt$proj_y < - round(proj_coords[, 2], 1)
} else {
    df_filt$proj_x < - numeric(0)
    df_filt$proj_y < - numeric(0)
}

pal < - colorNumeric(
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
        radius = ~sqrt(macroplastics_count) * 1.5,
        color = ~pal(macroplastics_count),
        stroke = TRUE, fillOpacity = 0.8,
        weight = 1.5,
        popup = ~sprintf("<strong>Station:</strong> %s<br/><strong>Location:</strong> %s<br/><strong>Macroplastics Count:</strong> %d<br/><strong>GPS Coord:</strong> %.4f, %.4f<br/><strong>Grid:</strong> %s<br/><strong>X:</strong> %.1f, <strong>Y:</strong> %.1f",
            station_id, location, macroplastics_count, lat, lon, crs_label, proj_x, proj_y)
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
    start_lat<- input$d4_accom_lat
    start_lon < - input$d4_accom_lon
    mode < - input$d4_travel_mode
    
    if (is.null(start_lat) || is.null(start_lon)) return ()
    
    # Query OSRM API(http://router.project-osrm.org/route/v1/mode/lon1,lat1;lon2,lat2)
    # Target: Edema Hall coordinates(37.660060809193034, -6.801425980395493)
    url < - sprintf("http://router.project-osrm.org/route/v1/%s/%f,%f;37.660060809193034,-6.801425980395493?overview=full&geometries=geojson",
        mode, start_lon, start_lat)
    
    res < - tryCatch({
            jsonlite:: fromJSON(url)
        }, error = function (e) {
            NULL
        })
    
    if (is.null(res) || res$code != "Ok") {
    output$d4_route_summary < - renderUI({
        div(class= "alert alert-danger", style="margin-top:0.5rem; font-size:0.8rem;",
        "Error: Could not retrieve route. Check connection or start location.")
})
return ()
    }
    
    # Parse OSRM response details
dist_km < - round(res$routes$distance / 1000, 2)
duration_min < - round(res$routes$duration / 60, 1)
    
    # Get geometry path coordinates(N x 2 matrix: Lon, Lat)
coords < - res$routes$geometry$coordinates[[1]]
lons < - coords[, 1]
lats < - coords[, 2]

output$d4_route_summary < - renderUI({
    mode_icon<- if (mode == "driving") "car" else "walking"
mode_label < - if (mode == "driving") "Driving" else "Walking"

div(style = "margin-top:0.5rem; border-left: 3px solid #4A7C59; background: #f8fafc; padding: 0.6rem; border-radius: 4px;",
    div(style = "font-size:0.75rem; font-weight:700; color:#475569;", sprintf("OSRM Path (%s):", mode_label)),
    div(style = "display:flex; justify-content:space-between; margin-top:0.2rem;",
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
output$d4_distance_report < - renderUI({
    df<- d4_spatial_data()
    req(input$d4_target_site)
    if (is.null(df)) return (NULL)

target_row < - df |> filter(station_id == input$d4_target_site)
if (nrow(target_row) == 0) return (NULL)
    
    # EDEMA location
edema_sf < - st_sfc(st_point(c(37.660060809193034, -6.801425980395493)), crs = 4326)
target_sf < - st_sfc(st_point(c(target_row$lon[1], target_row$lat[1])), crs = 4326)
    
    # Calculate distance using projected coordinates
dist_val < - as.numeric(st_distance(st_transform(edema_sf, crs = 32737), st_transform(target_sf, crs = 32737)))
dist_km < - round(dist_val / 1000, 2)

div(class = "custom-card", style = "border-left: 5px solid #0A3B5C; background: #f8fafc; padding: 0.8rem; box-shadow:none; margin-top:0.5rem;",
    div(style = "font-size:0.75rem; font-weight:700; color:#64748b;", "Projected Distance to EDEMA:"),
    div(style = "font-size:1.4rem; font-weight:800; color:#0A3B5C; margin:0.2rem 0;", sprintf("%.2f km", dist_km)),
    div(style = "font-size:0.72rem; color:#475569;", sprintf("Station %s (%s)", target_row$station_id[1], target_row$location[1]))
)
  })
  
  # Dynamic Spatial Code Output
output$d4_spatial_code_ui < - renderUI({
    crs_target<- input$d4_crs_proj
    
    code_text < - sprintf(
        "library(sf)
library(tidyverse)

# 1. Convert tabular data to spatial simple feature(WGS 84)
clean_sites_sf < - st_as_sf(
            cleanup_sites,
            coords = c(\"lon\", \"lat\"), 
  crs = 4326
            )

# 2. Transform coordinate reference system(CRS)
clean_sites_projected < - st_transform(
                clean_sites_sf,
                crs = % s
            )", crs_target)
    
    div(class = "code-container", pre(code_text))
  })
  
  # Update predictors list and dynamically render prediction sliders
  observe({
                df1<- load_dataset("1")
    df5 < - load_dataset("5")
    
    if (input$d4_model_type == "regression") {
    if (input$d4_reg_response == "height_m" && !is.null(df1)) {
        updateSelectizeInput(session, "d4_reg_predictors",
            choices = c("dbh_cm"),
            selected = c("dbh_cm"))
    } else if (input$d4_reg_response == "yield_tonnes_ha" && !is.null(df5)) {
        updateSelectizeInput(session, "d4_reg_predictors",
            choices = c("fertilizer_nitrogen_kg_ha", "soil_organic_carbon_pct"),
            selected = c("fertilizer_nitrogen_kg_ha", "soil_organic_carbon_pct"))
    }
}
  })
  
  # Dynamic prediction inputs in sidebar
output$d4_prediction_inputs < - renderUI({
    req(input$d4_model_type == "regression")
preds < - input$d4_reg_predictors
resp < - input$d4_reg_response
if (length(preds) == 0) return (NULL)

df < - if (resp == "height_m") load_dataset("1") else load_dataset("5")
if (is.null(df)) return (NULL)

slider_list < - lapply(preds, function (col) {
    min_val < - min(df[[col]], na.rm = TRUE)
    max_val < - max(df[[col]], na.rm = TRUE)
    mean_val < - mean(df[[col]], na.rm = TRUE)

    label_map < - c(
        "dbh_cm" = "Diameter at Breast Height (DBH, cm)",
        "fertilizer_nitrogen_kg_ha" = "Nitrogen fertilizer (kg/ha)",
        "soil_organic_carbon_pct" = "Soil Organic Carbon (%)"
    )

    label < - if (col %in% names(label_map)) label_map[[col]] else col

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
  model_results < - eventReactive(input$d4_run_model, {
    if(input$d4_model_type == "ttest") {
        df<- load_dataset("5")
      if (is.null(df)) return (list(error = "Dataset not found"))

resp < - input$d4_ttest_response
grp < - input$d4_ttest_group

res < - tryCatch({
    comp<- test_t_two_sample(data = df, response = resp, group = grp)
        list(type = "ttest", obj = comp, method = "testflow")
      }, error = function (e) {
        stat_t < - df |> t_test(as.formula(paste(resp, "~", grp)), detailed = TRUE)
        list(type = "ttest_fallback", obj = stat_t, method = "rstatix", error = e$message)
    })

return (res)
    } else {
    resp < - input$d4_reg_response
    preds < - input$d4_reg_predictors

    if (length(preds) == 0) return (list(error = "No predictors chosen"))

    df < - if (resp == "height_m") load_dataset("1") else load_dataset("5")
    if (is.null(df)) return (list(error = "Dataset not found"))

    formula_str < - paste(resp, "~", paste(preds, collapse = " + "))
    model_fit < - lm(as.formula(formula_str), data = df)

    return (list(type = "regression", obj = model_fit))
}
  }, ignoreNULL = FALSE)

output$d4_model_summary < - renderPrint({
    res<- model_results()
    if (!is.null(res$error) && is.null(res$obj)) {
    cat("Error running model: ", res$error, "\n")
    return ()
}

if (res$type == "ttest") {
    tryCatch({
        report(res$obj)
    }, error = function (e) {
        print(res$obj)
    })
} else if (res$type == "ttest_fallback") {
    cat("Warning: testflow run failed. Outputting rstatix fallback table:\n\n")
    print(res$obj)
} else if (res$type == "regression") {
    print(summary(res$obj))
}
  })

output$d4_model_diagnostics < - renderPlot({
    res<- model_results()
    if (is.null(res) || !is.null(res$error)) return (NULL)

if (res$type == "ttest") {
    tryCatch({
        plot(res$obj)
    }, error = function (e) {
        df < - load_dataset("5")
        boxplot(as.formula(paste(input$d4_ttest_response, "~", input$d4_ttest_group)), data = df,
            col = c("#0A3B5C", "#4A7C59"), main = "Comparison Boxplot")
    })
} else if (res$type == "ttest_fallback") {
    df < - load_dataset("5")
    boxplot(as.formula(paste(input$d4_ttest_response, "~", input$d4_ttest_group)), data = df,
        col = c("#0A3B5C", "#4A7C59"), main = "Comparison Boxplot")
} else if (res$type == "regression") {
    par(mfrow = c(1, 2))
    plot(res$obj, which = 1: 2, col = "#0A3B5C")
    par(mfrow = c(1, 1))
}
  })
  
  # Assumptions Scorecard(Normality Test / Diagnostic metrics)
output$d4_assumptions_scorecard < - renderUI({
    res<- model_results()
    if (is.null(res) || !is.null(res$error)) return (p("Run model analysis to view assumptions scorecard.", style = "color:#64748b; font-style:italic;"))

if (res$type == "regression") {
    model_fit < - res$obj
    resids < - residuals(model_fit)
    shapiro_p < - tryCatch(shapiro.test(resids)$p.value, error = function (e) NA)
      
      # Check BP test(homoscedasticity)
    bp_p < - tryCatch(car:: ncvTest(model_fit)$p, error = function (e) NA)

    norm_alert < - if (!is.na(shapiro_p) && shapiro_p >= 0.05) {
        div(class= "alert alert-success", style = "border-left:5px solid #2ECC71;",
            h5(style = "color:#27ae60; font-weight:700; margin-top:0;", icon("check-circle"), "Residual Normality Met"),
            p(sprintf("Shapiro-Wilk test p-value = %.4f (p >= 0.05). Residuals appear normally distributed.", shapiro_p))
        )
    } else {
        div(class= "alert alert-danger", style = "border-left:5px solid #E74C3C;",
            h5(style = "color:#c0392b; font-weight:700; margin-top:0;", icon("exclamation-triangle"), "Residual Normality Violated"),
            p(sprintf("Shapiro-Wilk test p-value = %.4f (p < 0.05). Consider log-transforming response variables.", shapiro_p))
        )
    }

    var_alert < - if (!is.na(bp_p) && bp_p >= 0.05) {
        div(class= "alert alert-success", style = "border-left:5px solid #2ECC71;",
            h5(style = "color:#27ae60; font-weight:700; margin-top:0;", icon("check-circle"), "Homoscedasticity Met"),
            p(sprintf("Score test for non-constant error variance p-value = %.4f (p >= 0.05). Constant variance holds.", bp_p))
        )
    } else {
        div(class= "alert alert-danger", style = "border-left:5px solid #E74C3C;",
            h5(style = "color:#c0392b; font-weight:700; margin-top:0;", icon("exclamation-triangle"), "Heteroscedasticity Detected"),
            p(sprintf("Score test p-value = %.4f (p < 0.05). Standard errors may be biased; check Scale-Location plot.", bp_p))
        )
    }

    tagList(norm_alert, br(), var_alert)

} else {
      # T - Test normality
    df < - load_dataset("5")
    resp < - input$d4_ttest_response
    grp < - input$d4_ttest_group

    shapiro_res < - tryCatch({
        p_vals<- df |> group_by(.data[[grp]]) |> summarize(p = shapiro.test(.data[[resp]])$p.value)
        p_vals
      }, error = function (e) NULL)

if (is.null(shapiro_res)) {
    return (p("Shapiro-Wilk test could not be calculated (requires between 3 and 5000 samples per group)."))
}

alerts < - lapply(1: nrow(shapiro_res), function (i) {
    group_name < - shapiro_res[[1]][i]
    p_val < - shapiro_res$p[i]

    if (p_val >= 0.05) {
        div(class= "alert alert-success", style = "border-left:5px solid #2ECC71; margin-bottom:0.8rem;",
            h6(style = "color:#27ae60; font-weight:700; margin:0;",
                icon("check-circle"), sprintf("Group [%s]: Normality Met (p = %.4f)", group_name, p_val))
        )
    } else {
        div(class= "alert alert-danger", style = "border-left:5px solid #E74C3C; margin-bottom:0.8rem;",
            h6(style = "color:#c0392b; font-weight:700; margin:0;",
                icon("exclamation-triangle"), sprintf("Group [%s]: Normality Violated (p = %.4f)", group_name, p_val))
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
output$d4_prediction_card < - renderUI({
    res<- model_results()
    if (is.null(res) || res$type != "regression") {
    return (p("Prediction tools are only available for Multiple Linear Regression models.", style = "color:#64748b; font-style:italic;"))
}

model_fit < - res$obj
preds < - input$d4_reg_predictors
    
    # Collect slider inputs
newdata < - list()
for (col in preds) {
    val < - input[[paste0("pred_val_", col)]]
    if (is.null(val)) return (p("Set values on the left sidebar to generate prediction outputs."))
    newdata[[col]] < - val
}

newdata_df < - as.data.frame(newdata)
pred_res < - tryCatch({
    predict(model_fit, newdata = newdata_df, interval = "confidence")
}, error = function (e) NULL)

if (is.null(pred_res)) return (p("Error calculating predictions."))

fit_val < - round(pred_res[1, "fit"], 2)
lwr_val < - round(pred_res[1, "lwr"], 2)
upr_val < - round(pred_res[1, "upr"], 2)

y_name < - switch (input$d4_reg_response,
"height_m" = "Tree Height (meters)",
"yield_tonnes_ha" = "Crop Yield (tonnes/ha)")
    
    div(class = "custom-card", style = "border-left: 5px solid #0A3B5C; padding: 1.5rem;",
    h4(style = "color:#0A3B5C; font-weight:700; margin-top:0;", icon("calculator"), "Live Predictive Estimate"),
    p(sprintf("Based on regression coefficients fitted from historical theme data, the predicted value for <strong>%s</strong> is:", y_name)),
    div(style = "font-size: 2.8rem; font-weight: 800; color: #0A3B5C; margin: 1rem 0;", sprintf("%.2f", fit_val)),
    div(style = "font-size: 0.95rem; font-weight: 700; color: #475569;",
        icon("info-circle"), sprintf("95%% Confidence Interval: [%.2f to %.2f]", lwr_val, upr_val)
    )
)
  })
  
  # Code output for Modeling
  output$d4_model_code < - renderUI({
    code_text<- ""
    if (input$d4_model_type == "ttest") {
        code_text < - sprintf(
            "library(tidyverse)\nlibrary(testflow)\nlibrary(here)\n\n# Load Agriculture AFOLU dataset\nstudy_data <- read_csv(here(\"data\", \"theme5_ghg_afolu.csv\"))\n\n# Run two-sample t-test comparison\ncomp <- test_t_two_sample(\n  data = study_data,\n  response = \"%s\",\n  group = \"%s\"\n)\n\n# Render scholarly report details\nreport(comp)\n\n# Show assumptions plots\nplot(comp)",
            input$d4_ttest_response, input$d4_ttest_group
        )
    } else {
        resp < - input$d4_reg_response
        preds < - input$d4_reg_predictors
        ds < - if (resp == "height_m") "theme1_coastal_forest.csv" else "theme5_ghg_afolu.csv"

        code_text < - sprintf(
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
d5_generate_qmd_content < - reactive({
    title_val<- input$d5_title_input
    sub_val < - input$d5_subtitle_input
    author_val < - input$d5_author_input
    theme_val < - input$d5_theme_input
    toc_val < - if (input$d5_toc_input) "true" else "false"

fold_line < - ""
if (input$d5_code_fold == "true") {
    fold_line < - "\n    code-fold: true"
} else if (input$d5_code_fold == "false") {
    fold_line < - "\n    code-fold: false"
}

qmd_txt < - sprintf(
    "---
title: \"%s\"
subtitle: \"%s\"
author: \"%s\"
date: today
format:
    html:
    toc: % s
    theme: % s % s
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
We convert the tabular dataset to a coordinate - aware simple feature spatial object.

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

output$d5_preview_ui < - renderUI({
    qmd_text<- d5_generate_qmd_content()
    div(class = "code-container", style = "max-height: 450px; overflow-y: auto;",
        pre(qmd_text)
    )
  })

output$d5_download_qmd < - downloadHandler(
    filename = function () {
        "custom_scholarly_report.qmd"
    },
    content = function (file) {
        writeLines(d5_generate_qmd_content(), file)
    }
)

output$d5_download_pdf < - downloadHandler(
    filename = function () {
        "reproducible_scholarly_report.pdf"
    },
    content = function (file) {
        pdf_path < - here("TNA_report_v2.pdf") # Serve the available PDF as a simulated render
        if (file.exists(pdf_path)) {
            file.copy(pdf_path, file)
        } else {
        # Create empty file
            writeLines("PDF render mock", file)
        }
    }
)
  
  # Dynamic templates download handler(.ZIP)
output$d5_download_templates < - downloadHandler(
    filename = function () {
        "bahari_yetu_templates.zip"
    },
    content = function (file) {
      # Create temporary folder for template scripts
      temp_dir < - file.path(tempdir(), "templates")
      dir.create(temp_dir, showWarnings = FALSE)
      
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
      
      # Zip files
        files_to_zip < - list.files(temp_dir, full.names = TRUE)
        zip(file, files = files_to_zip, flags = "-j")
    }
)
  
  # ---------------------------------------------------------
  # Home Tab: TNA Data Explorer Charts
  # ---------------------------------------------------------
    tna_time_data < - tibble(
        Task = c("Data Import / Formatting", "Data Cleaning", "Data Analysis / Stats", "Writing / Reporting", "Fieldwork"),
        Hours_Pct = c(18.5, 44.5, 15.0, 12.0, 10.0)
    )

tna_skill_data < - tibble(
    Category = rep(c("Tidy Data", "dplyr Wrangling", "ggplot2 Graphics", "GIS / sf Mapping", "Quarto Reports"), each = 3),
    Level = rep(c("Beginner", "Intermediate", "Advanced"), 5),
    Percentage = c(
        60, 30, 10,  # Tidy
      57, 33, 10,  # dplyr
      70, 25, 5,   # ggplot
      85, 12, 3,   # GIS
      90, 8, 2     # Quarto
    )
)

tna_exp_data < - tibble(
    Experience = c("Never used R", "Used R once/twice", "Intermediate user", "Regularly write R"),
    Percentage = c(57.1, 28.6, 11.4, 2.9)
)

output$tna_plot < - renderPlot({
    chart<- input$tna_chart_select
    
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
    tna_skill_data$Level < - factor(tna_skill_data$Level, levels = c("Beginner", "Intermediate", "Advanced"))
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
  # Server Quiz Logic(25 Questions)
quiz_answers < - list(
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
quiz_state < - reactiveValues(
    submitted = character(),
    correct_count = 0,
    score_status = list(),
    current_feedback = NULL
)

output$quiz_question_ui < - renderUI({
    day_key<- input$quiz_day
    q_key < - input$quiz_question_num
    q_data < - quiz_answers[[day_key]][[q_key]]
    
    radioButtons("quiz_choice", label = q_data$q,
        choices = q_data$choices, selected = character(0))
  })

observeEvent(input$quiz_submit, {
    day_key<- input$quiz_day
    q_key < - input$quiz_question_num
    q_data < - quiz_answers[[day_key]][[q_key]]
    user_choice < - input$quiz_choice
    
    if (is.null(user_choice) || user_choice == "") {
    quiz_state$current_feedback < - div(class= "alert alert-warning", "Please select an answer option first.")
    return ()
}

is_correct < - (user_choice == q_data$correct)
combined_key < - paste(day_key, q_key, sep = "_")
    
    # Check if already counted in score
if (!(combined_key %in% quiz_state$submitted)) {
    quiz_state$submitted < - c(quiz_state$submitted, combined_key)
    quiz_state$score_status[[combined_key]] < - is_correct
    if (is_correct) {
        quiz_state$correct_count < - quiz_state$correct_count + 1
    }
}

if (is_correct) {
    quiz_state$current_feedback < - div(class = "custom-card", style = "border-left: 5px solid #2ECC71; background: #f0fdf4; padding: 1.5rem; min-height: 400px;",
        h4(style = "color:#2ECC71; font-weight:700; margin-top:0;", icon("check-circle"), "Correct!"),
        p(q_data$explanation, style = "color:#1e293b; font-size:0.95rem; margin-bottom:0;")
    )
} else {
    quiz_state$current_feedback < - div(class = "custom-card", style = "border-left: 5px solid #E74C3C; background: #fef2f2; padding: 1.5rem; min-height: 400px;",
        h4(style = "color:#E74C3C; font-weight:700; margin-top:0;", icon("times-circle"), "Incorrect Answer"),
        p("That is not correct. Try studying the curriculum outline or reviewing the day tab, then try again!", style = "color:#1e293b; font-size:0.95rem; margin-bottom:0;")
    )
}
  })
  
  # Clear feedback when switching questions
observe({
    input$quiz_day
    input$quiz_question_num
    quiz_state$current_feedback<- NULL
  })

output$quiz_feedback_ui < - renderUI({
    if(is.null(quiz_state$current_feedback)) {
    div(class= "custom-card", style = "min-height:400px; display:flex; align-items:center; justify-content:center; border-style:dashed; border-color:#cbd5e1; background:transparent;",
        div(style = "text-align:center; color:#94a3b8;",
            icon("lightbulb", style = "font-size:3rem; margin-bottom:1rem;"),
            p("Select an answer and submit to view explanations.")
        )
    )
} else {
    quiz_state$current_feedback
}
  })

render_svg_gauge < - function (score, total) {
    if (total == 0) {
        pct < - 0
        label < - "0%"
        gradient_id < - "gradient-gray"
        glow_color < - "#94a3b8"
        score_text < - "Quiz Not Started"
    } else {
        pct < - round((score / total) * 100)
        label < - sprintf("%d%%", pct)
        gradient_id < - if (pct >= 80) "gradient-green" else if (pct >= 50) "gradient-yellow" else "gradient-red"
        glow_color < - if (pct >= 80) "#10B981" else if (pct >= 50) "#F59E0B" else "#EF4444"
        score_text < - sprintf("Score: %d / %d Correct", score, total)
    }

    circumference < - 314.16
    dashoffset < - circumference * (1 - pct / 100)

    HTML(sprintf('
        < div style = "display: flex; flex-direction: column; align-items: center; justify-content: center; padding: 0.5rem 0;" >
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
      </div >
        ', gradient_id, glow_color, gradient_id, dashoffset, gradient_id, label, score_text))
  }

output$quiz_score_ratio < - renderUI({
    render_svg_gauge(quiz_state$correct_count, length(quiz_state$submitted))
  })
  
  # ---------------------------------------------------------
  # Facilitator Tab: Maps & Info
  # ---------------------------------------------------------
    output$venue_map < - renderLeaflet({
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
    start_lat<- input$accom_lat
    start_lon < - input$accom_lon
    mode < - input$travel_mode
    
    if (is.null(start_lat) || is.null(start_lon)) return ()
    
    # Query OSRM API(http://router.project-osrm.org/route/v1/mode/lon1,lat1;lon2,lat2)
    # Target: Edema Hall coordinates(37.660060809193034, -6.801425980395493)
    url < - sprintf("http://router.project-osrm.org/route/v1/%s/%f,%f;37.660060809193034,-6.801425980395493?overview=full&geometries=geojson",
        mode, start_lon, start_lat)
    
    res < - tryCatch({
            jsonlite:: fromJSON(url)
        }, error = function (e) {
            NULL
        })
    
    if (is.null(res) || res$code != "Ok") {
    output$route_summary < - renderUI({
        div(class= "alert alert-danger", style="margin-top:0;",
        "Error: Could not retrieve route coordinates from OSRM. Please check your internet connection or start location.")
})
return ()
    }
    
    # Parse OSRM response details
dist_km < - round(res$routes$distance / 1000, 2)
duration_min < - round(res$routes$duration / 60, 1)
    
    # Get geometry path coordinates(N x 2 matrix: Lon, Lat)
coords < - res$routes$geometry$coordinates[[1]]
lons < - coords[, 1]
lats < - coords[, 2]

output$route_summary < - renderUI({
    mode_icon<- if (mode == "driving") "car" else "walking"
mode_label < - if (mode == "driving") "Driving (Car)" else "Walking (Foot)"

div(class = "custom-card", style = "margin-top:0; border-left: 5px solid #0A3B5C; background: #f8fafc; padding: 1.2rem; height:100%; box-shadow:none;",
    h5(style = "margin-top:0; font-weight:700; color:#0A3B5C;", "Calculated Route:"),
    fluidRow(
        column(width = 12,
            div(style = "display:flex; align-items:center; gap:0.8rem; margin-bottom:1rem;",
                icon(mode_icon, style = "font-size:1.5rem; color:#4A7C59;"),
                span(style = "font-weight:600; color:#334155; font-size:0.95rem;", mode_label)
            )
        )
    ),
    fluidRow(
        column(width = 6,
            div(
                div(style = "font-size:1.5rem; font-weight:800; color:#0A3B5C; line-height:1.2;", sprintf("%.2f km", dist_km)),
                div(style = "font-size:0.75rem; font-weight:600; color:#64748b;", "Distance")
            )
        ),
        column(width = 6,
            div(
                div(style = "font-size:1.5rem; font-weight:800; color:#0A3B5C; line-height:1.2;", sprintf("%.1f min", duration_min)),
                div(style = "font-size:0.75rem; font-weight:600; color:#64748b;", "Est. Travel Time")
            )
        )
    )
)
    })
    
    # Update map elements
leafletProxy("venue_map") |>
    clearGroup("route_path") |>
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
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
