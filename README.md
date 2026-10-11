# School composition

**DV7**

What share of Dutch secondary school pupils is eligible for an upward revision of the teacher's advice after receiving the test recommendation, and how much does each error source contribute to that share?


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

Rendering downloads the public data if needed and writes the report HTML beside
its `.qmd` in `report/`. Commit that HTML: on push, GitHub Actions checks lint
and style and publishes it as the GitHub Pages site.

Existing downloads are reused.

Architecture:

- `R/data/`: download the sources, tidy them, and check them against each
  other (`get-data.R` is the entry point).
- `R/exploration/`: datasets and plots of the exploratory process.
- `R/national-model/`: fitting the model, separating the errors, the decomposition, and
  its sensitivity to the anchor, with their plots.
- `R/school-model/`: school-level fitting (cached in `data/derived/`) and
  aggregated school results, using the national model parameters.
- `R/final/`: the eligibility explorer, using the national and school results.
- `R/plotting/`: shared colours, theme, and interactive plot helpers.
- `report/`: the Quarto report; one file per section in `report/sections/`.
- `data/raw/`: downloaded data, excluded from Git.
- `data/derived/`: cached results of slow computations, excluded from Git.
- `data/reports/`: tables parsed from the CvTE, Cito, and DUO PDF reports,
  kept in Git. They were made once by the (gitignored) code in `R/data/one-off/`.