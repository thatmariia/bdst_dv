# Assignment 2.1 — School composition

**DV7**

Is the neighbourhood composition of a primary school's
new intake associated with that of its existing pupils?

## Quickstart

From the project root (R and [Quarto](https://quarto.org/docs/get-started/) required):

```sh
Rscript scripts/setup.R
Rscript scripts/check.R
quarto render
```

Setup restores pinned packages and enables Git hooks for this checkout.
The pre-commit hook checks staged R, Rmd, and Qmd files for lint and formatting.
Fix formatting with `Rscript scripts/check.R --fix`.

Rendering downloads four public DUO/CBS files if needed, creates the website in
`build/`, and writes each report HTML beside its `.Rmd` in `report/`.

To download data separately, run `Rscript scripts/download.R`. The notebook
also calls this script; reusable download functions stay in `R/download.R`.
Existing downloads are reused.

Architecture:

- `R/`: download, prepare, and plot the data.
- `scripts/`: setup, downloads, checks, and source copying.
- `report/`: the R Markdown notebook.
- `data/raw/`: downloaded data, excluded from Git.