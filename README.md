# IUCN Bahari Yetu Scholarly Training Modules

This directory contains the training modules, presentations, and exercises designed for the 5-Day Intensive "Bring Your Own Data" (BYOD) Bootcamp.

> **Important Details**  
> * **Dates**: Monday, 3 - 8, August 2026  
> * **Location**: Morogoro, Tanzania  
> * **Venue**: EDEMA Conference Hall  
> * **Facilitator**: Masumbuko Semba
>


## Curriculum Informed by the Training Needs Assessment (TNA)


The design and content of these modules are directly tailored to address the baseline skills, software preferences, time bottlenecks, and specific competency gaps identified in the [TNA Report](TNA_report_v2.pdf). 

Key findings from the TNA that shaped this curriculum include:
1. **The Excel Bottleneck**: Scholars spend **44.5% of their total research time** on manual data cleaning and formatting. 
   * *Curriculum Response*: **Day 1** (Tidy Data principles) and **Day 2** (`dplyr` and `tidyr` data wrangling) focus on automating these steps, aiming to reduce cleaning time by up to 80%.
2. **Minimal R Experience**: **57.1%** of the scholars have never coded in R, and **28.6%** can only run others' scripts.
   * *Curriculum Response*: **Day 1** starts from the absolute basics, covering IDE setups, project directory design, variables, and vectors.
3. **Advanced Stats & Modeling Gaps**: The highest skill gap was recorded for *Advanced Statistics* (Gap: 2.95/5.00).
   * *Curriculum Response*: **Day 4 (Track B)** introduces standardized hypothesis testing (t-tests, ANOVA) and regression models using the `testflow` and `rstatix` packages.
4. **GIS and Spatial Demands**: Many scholars work on marine ecology, plastic waste transport, and forestry.
   * *Curriculum Response*: **Day 4 (Track A)** offers a dedicated elective on spatial vector data (`sf`) and raster operations (`terra`).
5. **Reproducibility Deficits**: Over **70%** of scholars are unfamiliar with version control (Git) and automated reports.
   * *Curriculum Response*: **Day 5** introduces Quarto (`.qmd`) templates that compile code, tables, and text directly to Word or PDF thesis chapters.

---

## Folder Directory

| Directory | Topic | Key Files |
| :--- | :--- | :--- |
| **[Day 1](./day1/)** | Foundations & Tidy Data | [README.md](day1/README.md), [exercises.qmd](day1/exercises.qmd), [presentation.qmd](day1/presentations/presentation.qmd) |
| **[Day 2](./day2/)** | Wrangling & Reshaping | [README.md](day2/README.md), [exercises.qmd](day2/exercises.qmd), [presentation.qmd](day2/presentations/presentation.qmd) |
| **[Day 3](./day3/)** | Graphics with `ggplot2` | [README.md](day3/README.md), [exercises.qmd](day3/exercises.qmd), [presentation.qmd](day3/presentations/presentation.qmd) |
| **[Day 4](./day4/)** | Specialized Elective Tracks | [README.md](day4/README.md), [track_a_spatial.qmd](day4/track_a_spatial.qmd), [track_b_modeling.qmd](day4/track_b_modeling.qmd), [presentations/](day4/presentations/) |
| **[Day 5](./day5/)** | Reproducible Quarto Reports | [README.md](day5/README.md), [template_report.qmd](day5/template_report.qmd), [presentation.qmd](day5/presentations/presentation.qmd) |
