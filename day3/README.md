# Day 3: Publication-Ready Graphics

Today focuses on building and customizing visualization pipelines in R. We will master the Grammar of Graphics using `ggplot2`, customize labels/themes, use multi-panel layout faceting, and export high-resolution (300+ DPI) figures for journal submission.

## Objectives
By the end of today's sessions, you will be able to:
1. Understand the components of the Grammar of Graphics (Data, Aesthetics, Geoms).
2. Choose and construct appropriate plots (scatters, boxes, histograms, bar charts).
3. Apply customized color scales (e.g., Viridis, ColorBrewer) and themes.
4. Implement panel layouts using `facet_wrap` and `facet_grid`.
5. Save plots with precise resolutions and dimensions using `ggsave()`.

---

## Daily Schedule

| Time | Session | Key Concepts Covered |
| :--- | :--- | :--- |
| **09:00 - 10:30** | **Session 1: Grammar of Graphics & Geoms** | Data binding, aesthetic mappings (`aes`), geoms (`geom_point`, `geom_boxplot`, `geom_line`). |
| **10:30 - 11:00** | *Health Break* | |
| **11:00 - 12:30** | **Session 2: Styling & Faceting** | Theme modifications (`theme_classic`, `theme_minimal`), scale adjustments, legends, and faceting. |
| **12:30 - 14:00** | *Lunch Break* | |
| **14:00 - 15:30** | **Session 3: Exporting to Journals** | Resolution control, vector PDF/TIFF export, pixel sizes. |
| **15:30 - 17:00** | **Hands-On Lab 3 (BYOD)** | Create and refine a publication-quality figure using active thesis datasets. |

---

## Recommended Packages
Make sure your tidyverse library is active:
```r
library(tidyverse)
```
