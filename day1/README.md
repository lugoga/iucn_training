# Day 1: Foundations of R, Modern IDE Setup, & Tidy Data Principles

Welcome to the first day of the IUCN Bahari Yetu data management training course. Today, we transition from manual spreadsheet manipulation to reproducible, programmatic workflows using R.

## Objectives
By the end of today's sessions, you will be able to:
1. Navigate the RStudio/Positron IDE interface.
2. Establish a self-contained project directory with clean folder structures.
3. Understand R syntax basics (objects, functions, vectors, and data frames).
4. Apply the three core rules of **Tidy Data**.
5. Import raw tabular datasets (`.csv` and `.xlsx`) into R using relative file paths.

---

## Daily Schedule

| Time | Session | Key Concepts Covered |
| :--- | :--- | :--- |
| **09:00 - 10:30** | **Session 1: IDE Setup & Projects** | Positron/VS Code interface, RStudio Projects, Directory structure (`/data`, `/scripts`, `/outputs`), the `here` package. |
| **10:30 - 11:00** | *Health Break* | |
| **11:00 - 12:30** | **Session 2: R Syntax Foundations** | Objects, vectors, assignment operator (`<-`), functions, data frames. |
| **12:30 - 14:00** | *Lunch Break* | |
| **14:00 - 15:30** | **Session 3: Tidy Data & Import** | Tidy principles, importing using `readr::read_csv()` and `readxl::read_excel()`. |
| **15:30 - 17:00** | **Hands-On Lab 1 (BYOD)** | Set up project directories and import active thesis data. |

---

## Key Setup Commands
Ensure you have the required packages loaded:
```r
install.packages(c("tidyverse", "here", "readxl"))
```

## Recommended Reading & Resources
- R for Data Science (2e) - [Workflow: projects](https://r4ds.hadley.nz/workflow-scripts)
- Tidy Data paper by Hadley Wickham - [Journal of Statistical Software](https://www.jstatsoft.org/article/view/v059i10)
