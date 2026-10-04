# Assignment 2.1 — School composition

**Mariia Steeghs-Turchina · DV7**

Research question: is the neighbourhood composition of a primary school's
new intake associated with that of its existing pupils?

This branch contains the data exploration and decisions for Part 1.
The notebook loads its packages explicitly before sourcing the documented
functions in `R/`.

- `R/`: download, prepare, and plot the data.
- `scripts/`: setup, downloads, checks, and source copying.
- `report/`: the R Markdown notebook.
- `data/raw/`: downloaded data, excluded from Git.

## Run

From the project root (R and [Quarto](https://quarto.org/docs/get-started/) required):

```sh
Rscript scripts/setup.R
Rscript scripts/check.R
quarto render
```

Setup restores pinned packages and enables the pre-commit hook for this
checkout. The hook checks staged R, Rmd, and Qmd files for lint and formatting.
R code uses four-space indentation, enforced by the formatter and linter.
Fix formatting with `Rscript scripts/check.R --fix`, then review and stage.

Rendering downloads four public DUO/CBS files once and creates the website
in `build/`. Use `quarto preview` to browse locally.

To download data separately, run `Rscript scripts/download.R`. The notebook
also calls this script; reusable download functions stay in `R/download.R`.
Existing downloads are reused. Data and generated HTML stay out of Git.
