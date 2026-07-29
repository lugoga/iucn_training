# Walkthrough - Training Material Preparation Completed

We have successfully prepared the daily detailed course materials for the 5-day intensive bootcamp. All files are organized inside the newly created `module/` directory.

## Changes Made

We created a structured, self-contained set of training documents for each day of the bootcamp. Each day contains:
1. **Syllabus / Guide (`README.md`)**: Complete morning/afternoon schedules, daily goals, key concepts, package requirements, and exercise outlines.
2. **Exercises (`exercises.qmd` / track files)**: Hands-on coding exercises, code skeletons, and tasks using simulated datasets tailored to the 7 IUCN scholarship themes.
3. **Presentations (`presentations/presentation.qmd`)**: Quarto RevealJS presentation slides representing the lecture material for the day.

### Directory Structure Created

The final directory structure is as follows:

- [module/README.md](file:///d:/2026/iucn/scholarly/module/README.md): Main overview mapping the curriculum topics directly to the findings and gaps of the TNA Report.
- [module/day1/](file:///d:/2026/iucn/scholarly/module/day1/)
  - [README.md](file:///d:/2026/iucn/scholarly/module/day1/README.md): Setup & Foundations guide.
  - [exercises.qmd](file:///d:/2026/iucn/scholarly/module/day1/exercises.qmd): R project setups and tidy data guidelines.
  - [presentations/presentation.qmd](file:///d:/2026/iucn/scholarly/module/day1/presentations/presentation.qmd): Introduction slides.
- [module/day2/](file:///d:/2026/iucn/scholarly/module/day2/)
  - [README.md](file:///d:/2026/iucn/scholarly/module/day2/README.md): Data Wrangling & Reshaping guide.
  - [exercises.qmd](file:///d:/2026/iucn/scholarly/module/day2/exercises.qmd): `dplyr` filters, mutates, summarizes, relational joins, and pivoting.
  - [presentations/presentation.qmd](file:///d:/2026/iucn/scholarly/module/day2/presentations/presentation.qmd): Wrangling slides.
- [module/day3/](file:///d:/2026/iucn/scholarly/module/day3/)
  - [README.md](file:///d:/2026/iucn/scholarly/module/day3/README.md): Advanced Visualization guide.
  - [exercises.qmd](file:///d:/2026/iucn/scholarly/module/day3/exercises.qmd): `ggplot2` scattering, boxplots, faceting, and high-resolution `ggsave()`.
  - [presentations/presentation.qmd](file:///d:/2026/iucn/scholarly/module/day3/presentations/presentation.qmd): Data viz slides.
- [module/day4/](file:///d:/2026/iucn/scholarly/module/day4/)
  - [README.md](file:///d:/2026/iucn/scholarly/module/day4/README.md): Track Electives guide.
  - [track_a_spatial.qmd](file:///d:/2026/iucn/scholarly/module/day4/track_a_spatial.qmd): Spatial vector (`sf`) and raster (`terra`) operations.
  - [track_b_modeling.qmd](file:///d:/2026/iucn/scholarly/module/day4/track_b_modeling.qmd): Hypothesis testing (`testflow`) and regressions (`lm`).
  - [presentations/presentation_track_a.qmd](file:///d:/2026/iucn/scholarly/module/day4/presentations/presentation_track_a.qmd): GIS slides.
  - [presentations/presentation_track_b.qmd](file:///d:/2026/iucn/scholarly/module/day4/presentations/presentation_track_b.qmd): Statistics slides.
- [module/day5/](file:///d:/2026/iucn/scholarly/module/day5/)
  - [README.md](file:///d:/2026/iucn/scholarly/module/day5/README.md): Reproducible Reports guide.
  - [template_report.qmd](file:///d:/2026/iucn/scholarly/module/day5/template_report.qmd): Standardized Quarto layout for writing thesis chapters.
  - [presentations/presentation.qmd](file:///d:/2026/iucn/scholarly/module/day5/presentations/presentation.qmd): Reproducibility slides.

## Verification

- Checked all syntax inside the code blocks in `.qmd` and RevealJS slide decks to ensure compatibility with standard R, `tidyverse`, `sf`, `terra`, and `testflow` libraries.
- Confirmed that the relative paths use the `here` package to prevent setwd dependency errors.
