# A Second Introduction to Formal Political Economy

**Instructor:** Jacob Edenhofer

**Institution:** University of Oxford – Nuffield College

**Term:** Trinity Term 2026

**Contact:** [jacob.edenhofer@nuffield.ox.ac.uk](mailto:jacob.edenhofer@nuffield.ox.ac.uk)

## Course Overview

This course offers a formal treatment of foundational models in political economy, with a focus on democratic institutions — elections, political parties, and interest groups. It aims both to deepen theoretical understanding and to draw out the real-world implications of these models. The course is designed for students with little prior exposure to formal theory, and devotes significant time to the intuitions behind the formal arguments rather than to technical derivations alone.

This is the second iteration of the course. The 2025 version is archived at <https://github.com/jacob-edenhofer/PolEcon-Oxford2025>.

---

## Repository Structure

```
.
├── 01 Readings/           # Required and optional readings, organised by week
├── 02 Slides/             # Weekly lecture slides (PDF)
├── 03 Simulations/        # R scripts and Jupyter notebooks for figures and simulations
│   └── Custom_scripts/    # Shared theme, packages, and helper functions
├── 04 Assignments/        # Weekly assignment sheets and reference solutions
│   └── Solutions/         # Reference solutions in R and Python
├── Syllabus_2026.pdf      # Full course syllabus
└── README.md              # This file
```

> **A note on the readings folder.** All required and recommended readings are organised by week under `01 Readings/`, with full bibliographic details in the syllabus. These materials are made available for educational use by enrolled students; please refer to the original publishers for any redistribution rights.

---

## Weekly Breakdown

| Week | Topic                                              | Key models and themes                                                                  |
|:----:|----------------------------------------------------|----------------------------------------------------------------------------------------|
|  01  | Electoral accountability                           | Voter control; selection vs. sanctioning; Ferejohn (1986), Fearon (1999)               |
|  02  | Party competition                                  | Downsian model and extensions; valence; probabilistic voting                           |
|  03  | Distributive politics and electoral rules          | Core-vs-swing-voter debate; formal models of electoral systems                         |
|  04  | Interest groups and lobbying                       | Vote-buying, informational lobbying, political connections                             |
|  05  | Bonus: democratic backsliding                      | Strategic erosion of democratic norms; self-enforcing democracy                        |

> **Formative assessment.** Students may choose between weekly short essays, structured exercises on the model of the week, or simulation extensions. Assignment briefs are in `04 Assignments/`.

---

## How to Use this Repository

- **Slides.** All lecture slides as PDFs are in `02 Slides/`.
- **Simulations.** Each week's figures are reproducible from the scripts in `03 Simulations/WeekX/`. Generated figures are stored in the `Figures/` subfolder.
- **Custom scripts.** Shared package loading, plot themes, and helper functions live in `03 Simulations/Custom_scripts/`. Scripts in each week's folder source these via `here::here(...)` so paths work from anywhere in the project.
- **Assignments.** Each week's brief is in `04 Assignments/WeekX/AssignmentSheet.pdf`, with reference solutions (R and Python where applicable) in the same folder under `Solutions/`.

---

## Reproducing the Figures

Most figures in the slides are produced by R scripts in `03 Simulations/WeekX/`. To run them:

1. Open the R project file (`PolEcon-Oxford2026.Rproj`) at the repository root in RStudio.
2. Run any script in `03 Simulations/WeekX/`. The first run will install the packages listed in `03 Simulations/Custom_scripts/custom_packages.R` via `pacman::p_load`.
3. Figures are written to `03 Simulations/WeekX/Figures/`.

For Python notebooks, open them in JupyterLab or VS Code; the dependencies are listed at the top of each notebook.

---

## Requirements

- **Pre-requisites.** None strictly required, though having taken the formal-theory class in Hilary term is helpful. The course assumes only basic linear algebra, calculus, and probability.
- **Tools.** R (with RStudio recommended); optionally Python with Jupyter for some simulations. The R scripts use the `pacman` package for dependency management.

---

## Acknowledgements

This course builds on the formal political-economy literature surveyed in Persson & Tabellini (2002), Gehlbach (2021), and Osborne (2025). Specific debts to colleagues, students, and prior versions of the course are acknowledged in the relevant lecture slides.

---

## Licence

Course materials authored by Jacob Edenhofer are released under [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/). Cited papers, datasets, and any third-party material retain their original licences and are not covered by this notice.
