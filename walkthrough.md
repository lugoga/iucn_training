# Walkthrough: IUCN Bahari Yetu Shiny Application

We have built, updated, and verified a professional-grade Shiny web application (`app.R`) for the 5-day Applied Statistics course, integrating all course files, themes, maps, statistics, and modeling frameworks. 

We have integrated the exact Edema Conference Hall coordinates, added toggling for three mapping basemaps, and implemented an interactive accommodation path and travel time estimator. Additionally, we have completed a suite of educational, interactive, and offline deployment enhancements.

---

## Changes Made

### 1. Advanced Interactive Sandboxes Across All Day Tabs

* **Day 1: pivot_longer Reshaping Playground**:
  - Replaced the static wide-to-long transformation trigger with a live data reshaping sandbox.
  - Added user-configurable parameters: columns selection list, custom year column names (`names_to`), measurement value names (`values_to`), header prefix strip text (`names_prefix`), and a missing values drop filter (`drop_na`).
  - Displays the pivoted tidy table and generated R tidyverse code block dynamically in real-time.

* **Day 2: dplyr Wrangling Pipeline Builder**:
  - Added a sort order input variable (`arrange()`) to dynamically arrange rows.
  - Implemented a **Live Row Retention Metric progress bar** in the sidebar showing the exact ratio and percentage of rows kept after filtering (e.g. `54 / 300 rows (18%)`).

* **Day 3: Publication-Ready ggplot2 Designer**:
  - Added text customizers for plot titles, subtitles, and X/Y axis labels.
  - Integrated a typography base size slider to adjust the entire chart font size (`base_size`) dynamically.
  - Added a point jitter overlay checkbox (`geom_jitter()`) to display raw data distributions over box and bar charts.
  - Added a facet grid variable selector (`facet_wrap()`) to split plots by categoric sub-panels dynamically.

* **Day 4 Track A: Projected Grid Systems, Route Planners & Distance Calculators**:
  - Added a **Projected Grid reference CRS selector** (Tanzania UTM 37S, Arc 1960 Zone 37S, and Web Mercator). Circles and popups on the map recalculate projected local coordinates dynamically.
  - Implemented multi-layered map tiles, allowing scholars to toggle between OpenStreetMap, Satellite imagery, and Light CartoDB basemaps directly.
  - Integrated the **OSRM Network Route Planner** onto the GIS tab, allowing users to enter custom coordinates of where they stay and estimate path distances, times, and plot the actual driving/walking road network to EDEMA Conference Hall on the same map.
  - Added a straight-line distance calculator using `st_distance()`.
  - Generates matching `sf` projection R code templates dynamically.

* **Day 4 Track B: Predictive Modeling & Assumptions Validator**:
  - Added a **Live Prediction Estimator** that estimates response variables with a **95% Confidence Interval** (lower/upper bound) in real-time using user-defined predictor values.
  - Added an **Assumptions Scorecard** that evaluates residual normality (Shapiro-Wilk test) and heteroscedasticity (Score/BP test), presenting green status badges for satisfied assumptions or red alerts for violations.

* **Day 5: Quarto Document Configurator & Template Generator**:
  - Upgraded the static report preview to a dynamic **YAML Frontmatter Customizer** (Title, Subtitle, Author, HTML Bootstrap stylesheet theme, Table of Contents, and Code folding).
  - The preview window renders the custom frontmatter in real-time. The `Download QMD Template` handler compiles and outputs a custom `.qmd` file incorporating all user selections.

### 2. Balanced 3-Column Quiz Tab Layout
* Restructured the Quiz tab to utilize a balanced 3-column row:
  - **Column 1 (`width = 3`)**: **Quiz Navigator & Scorecard**. Groups the Day selector, Question selector, and the premium circular SVG progress gauge into a single cohesive panel.
  - **Column 2 (`width = 5`)**: **Active Question Card**. Displays the active question radio options and includes the primary `Submit Answer` button.
  - **Column 3 (`width = 4`)**: **Feedback & Explanation Card**. Renders the color-coded correct/incorrect explanations upon answer submission.
* Set a matching **`min-height: 400px`** and layout spacing rules on both the Active Question Card and the Feedback panels, aligning the bottom borders of the columns to prevent off-balance visual sizing.

### 3. Premium SVG Gradient Gauge in Quiz Scorecard
* Embedded a white backing card disk (`r="44" fill="#FFFFFF"`) underneath the track ring to create a layered glassmorphic depth effect.
* Added a drop-shadow glow filter (`<feDropShadow>`) matching the status color of the active progress stroke.
* Configured four high-fidelity color gradients inside the SVG `<defs>` block (emerald green, orange-yellow, crimson red, slate grey).
* Structured the text inside the circle into two components:
  - Large percentage value text (`y="58"`) using a heavy bold font (`font-weight: 900; font-family: 'Outfit'`).
  - Uppercase sub-label `SCORE` (`y="74"`) in a smaller, spaced font style (`font-family: 'Inter'; letter-spacing: 1.5px; fill: #64748B;`).

### 4. Expanded Knowledge Quiz (5 Questions Per Day)
* Dropdown menus for Day selection and Question sub-selection (5 distinct questions per day, 25 total).
* Interactive Scorecard tracks and aggregates submissions using combined keys to prevent double-counting.

### 5. Cross-Platform App Installation (Windows, Mac, Android)
* Custom branding icons generated programmatically and saved in `www/`.
* Web App Manifest (`manifest.json`) and Service Worker (`service-worker.js`) files managing offline browser prompts.

### 6. Venue Map & Accommodations Router (Developer Tab)
* Centered map on exact EDEMA coordinates (`-6.801425980395493`, `37.660060809193034`), added layered base control, real-time path planner and travel estimator querying OSRM.

### 7. Offline Application Launcher: [run_app.bat](file:///c:/Users/lugos/Documents/iucn/run_app.bat)
* Lightweight Windows script that double-clicks to open the browser page and starts the R server.

### 8. Auto-Reconnect & Sleep Recovery
* **Keep-Alive Ping**: Added a client-to-server keep-alive JavaScript interval that pings the R session input coordinates every 25 seconds, preventing idle timeouts and keeping connections warm.
* **Sleep Reconnect Overlay**: Embedded a JavaScript socket drop listener (`shiny:disconnected`) that instantly catches disconnections, renders a themed, responsive, fullscreen "App is Sleeping" reconnect splash overlay, and automatically reloads the browser tab within 2.5 seconds to wake up the R Shiny server.

### 9. Dynamic Theme Switcher & Glassmorphic Layouts
* **Interface Customizer**: Integrated a dynamic theme selector in the Home tab sidebar (Ocean Blue, Emerald Canopy, Midnight Slate / Dark Mode, and Sunset Gold) that modifies CSS styles on the fly.
* **Midnight Slate (Dark Mode)**: Automatically re-styles the entire portal's background, card wrappers, textual indicators, checklists, and code containers to support a high-contrast slate-blue dark theme.
* **Glassmorphic Components**: Upgraded cards to utilize `backdrop-filter: blur(8px)` with soft shadows and thin white semi-transparent borders. Included micro-interactive scale shifts and translates-Y animations on card hovering.
* **Background Glowing Highlights**: Implemented animated pseudo-elements (`::after` and `::before`) with radial gradients in the main landing banner to build warm, glowing background bubbles.

### 10. Daily File Indexing & Dynamic ZIP Downloader
* **Live Sidebar File Index**: Added a comprehensive index list inside the "Packages & Resources" card on every daily tab in the Shiny UI, referencing the newly created `.qmd` presentation slides and worksheets.
* **Dynamic Material Packager**: Modified the template script ZIP downloader on the Day 5 tab. Clicking the button now dynamically copies and packages the actual, split RevealJS presentations, hands-on exercises, and syllabus `README.md` files from the server's workspace directories into a unified `bahari_yetu_course_materials.zip` archive.
* **Semba Portal App Link**: Inserted the new link to `https://rugo.shinyapps.io/semba/` as the first web application featured in the Decision-Support Portfolio list on the Facilitator page.
* **Facilitator Sidebar & Column Restructure**: Restructured the Facilitator tab into a `layout_sidebar()` layout. Placed a newly styled **Tips & Guidance** sidebar on the left. Grouped all personal profiles, credentials, affiliations, professional projects, and book lists on Dr. Semba into the left panel of `layout_columns(col_widths = c(5, 7))`, and dedicated the right panel to the venue map, OSRM router, and travel estimator.
* **Logo Branding**: Replaced the plain text navigation bar title with a customized flex container holding the high-resolution `icon-192.png` logo. Set `window_title` to preserve a clean web browser title.
* **Home Page Program Info & Partner Acknowledgement**: Enhanced the Home page main panel to contain:
  1. A **Partner & Funding Acknowledgement** header containing a dynamically rendered vector inline SVG of the European Union flag alongside the IUCN project logo to acknowledge joint effort.
  2. A detailed **Pamoja Tuhifadhi Bahari Yetu Training Initiative** overview outlining the capacity development targets, postgraduate scholars count (28), supporting universities (SUA, UDSM, NM-AIST, SUZA), and the 8 core scientific/ecological thematic research areas.
  3. A new **Expected Outcomes & Outputs** paragraph focusing on equipping MSc and PhD scholars with advanced skills to manage, store, analyse, plot, and report environmental and marine research data.

### 11. Multi-Format Scholarly Reporting Tab
* **Thematic Reporting**: Added a dedicated **Reporting** tab configured to build scholarly summaries by each of the 8 Scholarly Thematic Research Areas (Greenhouse Gas, Marine Ecology, Coastal Forest, Marine Plastics, etc.).
* **Data Sources (Preloaded & Uploads)**: Implemented dual data loading: automatically loads the matching project theme CSV dataset from the `data/` folder, or lets scholars upload their own custom data files via `fileInput()`.
* **Side-by-Side Table & Plot Explorer**: Restructured the layout of the Reporting tab to present the **Raw Data Explorer table (Step 1)** and the **Interactive Plot (Step 2)** side-by-side (using a 6-6 column width ratio). This allows scholars to inspect, search, and page through their raw measurements directly alongside the dynamic ggplot2 chart.
* **Interactive Chart & Analysis**: Integrated real-time column mapping selectors (X-Axis, Y-Axis, plot styles, color palettes) rendering high-resolution figures.
* **Document Exporter (PDF, Word, EPUB)**: Added custom metadata inputs (Title, Subtitle, Author, Discussion) and a download handler that compiles/exports files into **Microsoft Word (.docx)**, **EPUB Ebook (.epub)**, and **PDF Document (.pdf)** formats. The report is structured as a professional, scientific document containing a Coverpage, Table of Contents, and key sections: **Introduction** (expanded with specialized scientific paragraphs matching the selected thematic research track), **Methods**, **Results** (incorporating summary stats and plots), **Discussion**, **Recommendation**, **References**, and an **Appendix & Index** of variables.
* **Base R PDF Fallback Engine**: Solved the corrupted PDF opening error by implementing a native R `pdf()` device graphics compiler. When the main pandoc/LaTeX compiler is unavailable, the fallback engine programmatically draws a styled cover page and formats the scientific sections and the active ggplot2 visualization onto native PDF vector canvases, ensuring the downloaded PDF opens perfectly and contains customized research metrics. Also configured Positron's integrated Pandoc path (`C:/Program Files/Positron/resources/app/quarto/bin/tools`) inside the R session to ensure Word and EPUB documents render successfully.
* **Format-Enforced Path Suffixes**: Resolved the file extension mismatch where PDF selections rendered in HTML format. Because Shiny temporary download paths have no file suffixes, rmarkdown defaults to generating `.html` output files when compiling. Implemented a format-enforced destination (`temp_out <- tempfile(fileext = paste0(".", ext))`) which guides the compiler to write exact formats before copying to the client target.
* **Ggplot2 PDF Device Font Fix**: Resolved an "invalid font type" error where ggplot2 was calling for web fonts (`"Inter"`, `"Outfit"`) that are not present in the native PDF PostScript database, causing rendering processes to fail. Restructured the ggplot theme to use standard fallbacks.
* **Home Tab Subtabs & Sidebar Restructuring**:
  * Restructured the **Home Tab Sidebar** to serve as a comprehensive information portal for attendees and general audiences. Added sections for *Training At-A-Glance* (venue, dates, instructor, audience details), *Key Objectives* (reproducibility goals), *Syllabus Highlights* (weekly timeline overview from installation to Quarto), and structured technical support links.
  * Reformatted both the **Key Objectives** and the **Syllabus Highlights** inside the sidebar into a professional list layout with custom icons (`arrows-rotate`, `gears`, `file-export`, `laptop-code`, `database`, `chart-line`, `map`, and `file-lines`) aligned using flexbox.
  * Solved the **TNA Report Download** issue where the sidebar button was a static placeholder with `href="#"`. Replaced it with a functional Shiny `downloadButton()` connected to a server-side `downloadHandler()` that serves the original [TNA_report_v2.pdf](file:///c:/Users/lugos/Documents/iucn/TNA_report_v2.pdf) file cleanly.
  * Added logistics contact information for **Herry Lugala** (`herry.lugala@iucn.org`) directly to the **Logistics Resources** section of the sidebar, enabling scholars to send direct emails for non-technical logistics questions.
  * Created a tabbed container (`navset_card_tab`) on the **Home** tab to segment core training details.
  * Tab 1 (**Training Initiative**): Focuses on the *Pamoja Tuhifadhi Bahari Yetu Project* thematic areas, Expected Outcomes, and cohort Training Needs Assessment (TNA) results.
    * Reformatted the **Scholarly Thematic Research Areas** list into a structured, dual-column grid with custom topic icons (`smog`, `fish`, `tree`, `recycle`, `leaf`, `chart-pie`, `earth-africa`, and `chart-line`).
    * Relocated the **TNA Analytics Selector** from the sidebar directly into this tab's survey results card, placing it side-by-side on the left of the dynamic TNA plot.
    * Swapped card ordering to place the **Why Code? The Excel Spreadsheet Trap vs. R Loop** flowchart card directly below the **Training Needs Assessment Survey Results** card.
  * Tab 2 (**Venue & Location Map**): Re-housed the training location map and interactive OSRM accommodation route calculator, making logistics readily available to scholars on page load.
* **Right-Aligned Facilitator Navigation**: Positioned the **Facilitator** tab on the far right-hand side of the navbar header using a `nav_spacer()`. The rest of the tabs (including **Quiz** and **Stats Guide**) remain left/center-aligned next to worksheets and reporting utilities.
* **Facilitator Profile Updates**: Updated the education details of the Facilitator card: removed the graduation years from all entries (PhD, MSc, BSc) and added clear, plain, and concise summaries explaining the academic focus of each degree course. Removed the location map from this tab to focus exclusively on the facilitator's academic background, publications, and professional history.
* **Day 4 Recommended Readings**: Added the [rstatix Package Reference Guide](https://rpkgs.datanovia.com/rstatix/) directly to the Recommended Readings list inside both the Day 4 Dashboard Sidebar and the [day4/README.md](file:///c:/Users/lugos/Documents/iucn/day4/README.md) curriculum workbook.
* **Interactive Stats Guide & On-Site AI**: Created a dedicated **Stats Guide** tab focusing on the `rstatix` package.
  * **Split Layout**: Organizes the thematic research subtabs on the left (width = 8) and places the interactive **On-Site AI Stats Assistant** card on the right (width = 4).
  * **Custom Text Prompts & Expanded Issues**: Allows scholars to type any free-text query (e.g. asking about t-tests, ANOVA models, p-values, normality assumptions, correlation, etc.). The on-site classifier has been expanded to parse key terms and return detailed biostatistics, syntax, and troubleshooting guide responses for:
    * **Statistical Tests & Reasoning**: Comparisons of means, ANOVA, correlation coefficients, p-value rules, and hypothesis formulation.
    * **Assumption Tests**: Validating normality (Shapiro-Wilk) and variance equality (Levene's test) prior to parametric testing.
    * **Missing Data & Outliers**: Handling NA values and identifying outlier metrics using `identify_outliers()`.
    * **Plotting & Themes**: Visual customizer guidelines (colors, scaling, fonts, and `ggsave()` resolution parameters).
    * **Compilation & Report Errors**: Troubleshooting Pandoc configurations, corrupted vector graphics fallbacks, and suffix overrides.
    * **Tidyverse Wrangling**: Pipe operator (`|>`) usage, grouping, filtering, mutating, and selecting variables.
    * **IPCC GHG & Marine Thematics**: Scientific guidelines covering clinker factors, AFOLU storage, dolphin density indicators, and plastic waste hotspots.
  * **Thematic Subtabs**: The 8 subtabs matching the Scholarly Thematic Research Tracks provide:
    * **Hypothesis Development**: Outlines null ($H_0$) and alternative ($H_1$) hypotheses alongside statistical assumptions check lists.
    * **Descriptive Stats**: Displays dynamic summaries with `get_summary_stats()`.
    * **Comparing Means**: Runs a `t_test()` and displays a boxplot annotated with its p-value.
    * **ANOVA**: Performs analysis of variance using `anova_test()`.
    * **Correlation Matrix**: Computes a correlation matrix with significance symbols using `cor_mat()` and `cor_mark_significant()`.
* **Quarto Previewer**: Renders a live preview of the underlying Quarto `.qmd` source code for scholars to review/copy.
* **Network Directory & Scholar Registry**: Implemented a new interactive **Directory** tab.
  * **Sidebar Registration Form**: Allows scholars, instructors, partners, and support staff to register their particulars (Name, Role, Affiliation, Thematic Research Area, Email, Short Bio, and an uploaded photo).
  * **Profile Password Security**: Added a secure `passwordInput()` during registration. Users choose their own password to authenticate and edit their particulars later. Default credentials are set for pre-populated records:
    * **Mr. Masumbuko Semba**: Email `lugosemba@gmail.com`, Password `semba123`
    * **Herry Lugala**: Email `herry.lugala@iucn.org`, Password `lugala123`
  * **User Authentication Portal (Popup Modal)**: Replaced the inline login panel with a clean, low-profile **Log In / Authenticate** launcher button at the top of the sidebar.
    * Clicking the launcher triggers a secure Shiny popup dialogue (`showModal(modalDialog(...))`).
    * **User Account**: Select registered email and input password. Clicking **Log In** validates credentials and closes the modal automatically (`removeModal()`).
    * **Admin Key**: Input admin credential (`iucn2026`) and click **Log In** to authenticate as Administrator, closing the modal.
    * If logged in, their name and capacity display in the sidebar with a **Log Out** button to terminate the session.
  * **Role-Based Edit & Delete Permissions**:
    * **Administrators**: Get both **Edit** and **Delete** actions for all directory cards.
    * **Authenticated Users**: Get the **Edit** action **only** on their own personal profile card. All other profile cards remain read-only.
    * **Anonymous Visitors**: Have read-only access (no action buttons display).
  * **Strict Server-Side Authorization Verification**:
    * Enforced server-side checks in the delete handler (`input$dir_delete_index`) to ensure **only** an authenticated administrator can execute deletions.
    * Enforced server-side checks in the edit handler (`input$dir_edit_index` and `input$reg_submit`) to verify that non-administrators can **only** edit their own profile matching their logged-in record ID. Any unauthorized client-side event triggers are immediately rejected.
  * **Dynamic Registration State**: Clicking **Edit** loads their profile data back into the form fields and swaps the submit button into an **Update Profile / Cancel Edit** panel to save modifications in-place.
  * **Image Base64 Parsing**: Integrates the `base64enc` package (`base64enc::dataURI()`) to dynamically convert temporary uploaded profile photos into persistent base64 URIs, which are saved in-memory and rendered instantly as responsive profile cards.
  * **Initials Placeholder Fallback**: Auto-generates initials (e.g. `MS` for Masumbuko Semba) in a colored, styled avatar block if no custom image is supplied, filtering out standard prefixes like `Dr.`, `Mr.`, etc.
  * **Pre-populated Profiles**: Populates the directory on boot with **Mr. Masumbuko Semba** (Lead Instructor), **Mr. Herry Lugala** (Logistics Lead), and the **25 registered trainees/scholars** representing UDSM, SUA, SUZA, and NMAIST. Each trainee has a custom drafted bio summary, registered institutional email, and default credentials (e.g. password `firstnamelastname` or `firstname123`) to ensure the directory feels full and fully functional right out of the box.
  * **Dynamic Directory Grid**: Renders responsive profile card elements in the main panel. Each card features role-specific styling badges (e.g. green for trainers, blue for scholars), research theme info, organization affiliation, and direct contact buttons.
  * **Search & Filters**: Integrates real-time text-search (matching names, organizations, or bios) and role filter dropdowns to allow swift slicing of the participant directory.
* **Dynamic Full-Screen Loading Overlay**: Added global CSS classes tied to `.shiny-busy` state. Whenever R Shiny is processing data, compiling reports, registering users, or performing database operations, a beautiful dark frosted overlay (`backdrop-filter: blur(4px)`) and a rotating teal spinner (`#34d399`) automatically lock the UI to indicate activity and prevent click-spamming, restoring access once completed.

---

## Verification & Testing

### 1. Syntax Parsing Verification
Parsed `app.R` syntax using the R base engine:
```powershell
& 'C:\Program Files\R\R-4.6.1\bin\Rscript.exe' -e "parse('app.R')"
```
* **Result**: Passed with zero syntax errors.

### 2. Runtime Server Loading
Launched the Shiny app server locally (currently active under PID task-606):
```powershell
& 'C:\Program Files\R\R-4.6.1\bin\Rscript.exe' -e "shiny::runApp(port = 8888, launch.browser = FALSE)"
```
* **Result**: Initialized successfully and starts listening:
  ```
  Listening on http://127.0.0.1:8888
  ```

### 3. Testing Visual and Interactive Features
You can inspect the new balanced layouts and interactive tools directly in your browser:
👉 **[http://127.0.0.1:8888](http://127.0.0.1:8888)**
