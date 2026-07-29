# Day 2: Data Wrangling, Joining, and Reshaping

Today focuses on transforming raw data into analysis-ready formats. We will learn how to filter rows, select columns, create new metrics, calculate group summaries, join multiple data tables, and pivot tables.

## Objectives
By the end of today's sessions, you will be able to:
1. Chain R commands efficiently using the pipe operator (`|>` or `%>%`).
2. subset and transform datasets using `dplyr` core verbs (`filter`, `select`, `mutate`).
3. Summarize complex datasets grouped by key factors using `group_by` and `summarize`.
4. Merge multiple data sheets using joins (`left_join`, `bind_rows`).
5. Reshape datasets between wide and long layouts using `pivot_longer` and `pivot_wider`.

---

## Daily Schedule

| Time | Session | Key Concepts Covered |
| :--- | :--- | :--- |
| **09:00 - 10:30** | **Session 1: Single-Table Wrangling** | Pipes, `filter()` for rows, `select()` for columns, and `mutate()` for calculating new variables. |
| **10:30 - 11:00** | *Health Break* | |
| **11:00 - 12:30** | **Session 2: Group Summaries & Aggregation** | Grouping datasets using `group_by()`, summarizing with functions like `mean()`, `sd()`, `n()`. |
| **12:30 - 14:00** | *Lunch Break* | |
| **14:00 - 15:30** | **Session 3: Relational Joins & Pivoting** | Joining tables, appending rows, pivoting data long and wide. |
| **15:30 - 17:00** | **Hands-On Lab 2 (BYOD)** | Clean, join, and summarize active research datasets. |

---

## Recommended Packages
Make sure your tidyverse library is active:
```r
library(tidyverse)
```
