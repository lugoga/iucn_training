# Day 4: Domain-Specific Modules (Elective Tracks)

Today, the cohort splits into two parallel tracks designed for specific research methodologies:

- **Track A: Spatial Data & GIS in R** (Focusing on vector shapefiles, raster grids, projections, and cartography).
- **Track B: Statistical Modeling & Laboratory Experiments** (Focusing on hypothesis testing, ANOVA, linear regressions, diagnostics, and reporting using `testflow`).

In the late afternoon, both tracks merge back for group presentations.

---

## Daily Schedule

| Time | Session | Track A: Spatial GIS | Track B: Stats & Modeling |
| :--- | :--- | :--- | :--- |
| **09:00 - 10:30** | **Session 1: Foundations** | Vector GIS: Reading shapefiles using `sf`, plotting vectors, projection CRS. | Hypothesis Testing: t-tests, ANOVA, and post-hoc tests using `testflow`. |
| **10:30 - 11:00** | *Health Break* | | |
| **11:00 - 12:30** | **Session 2: Advanced** | Raster calculations: `terra` package, DEM extraction, land-cover grids. | Linear Regression: Multiple regression `lm()`, diagnostic diagnostics, and model tables. |
| **12:30 - 14:00** | *Lunch Break* | | |
| **14:00 - 15:30** | **Session 3: Practice** | *Hands-on Lab 4-A:* Construct spatial maps and extract raster data. | *Hands-on Lab 4-B:* Run hypothesis testing & regressions on mock datasets. |
| **15:30 - 17:00** | **Joint Synthesis** | **Cohort presentations**: Sharing spatial maps and regression summaries. | |

---

## Required Packages
- **Track A**: `install.packages(c("sf", "terra", "tidyterra"))`
- **Track B**: `install.packages(c("testflow", "car", "flextable", "rstatix", "tidyplots"))` (Ensure the local repository or active library has `testflow` loaded).

## Recommended Readings
- [Geocomputation with R](https://r.geocompx.org/) (Track A)
- [Modern Statistics with R](https://www.modernstatisticswithr.com/) (Track B)
- [rstatix Package Reference Guide](https://rpkgs.datanovia.com/rstatix/) (Track B)
