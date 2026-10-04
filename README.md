# Assignment 2.1 — School composition

**Mariia Steeghs-Turchina · DV7 · solo project**

Research question: is the neighbourhood composition of a primary school's
new intake associated with that of its existing pupils?

This branch contains the data exploration and decisions for Part 1.
The notebook loads its packages explicitly before sourcing the documented
functions in `R/`.

- `R/`: download, prepare, and plot the data.
- `scripts/`: setup, downloads, checks, and source copying.
- `report/`: the R Markdown submission.
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
in `_site/`, including `report/DV-Assignment2-Part1-DV7.html`. Submit that
HTML and the original `.Rmd` to Canvas. Use `quarto preview` to browse locally.

To download data separately, run `Rscript scripts/download.R`. The notebook
also calls this script; reusable download functions stay in `R/download.R`.
Existing downloads are reused. Data and generated HTML stay out of Git.

## GitHub Pages

The workflow checks, renders with Quarto 1.10.18, and publishes on pushes to `main` or
`ass-2-1` in a single job.
The landing page lists every `.Rmd` in `report/` with an HTML report link,
an R Markdown download. Quarto provides navigation, search, and a GitHub link.

Before the first push, set **Settings → Pages → Source → GitHub Actions**
and allow the publishing branch in the `github-pages` environment if it
has a branch restriction. No access token or committed HTML is needed.
After a successful deployment:
[reports](https://thatmariia.github.io/bdst_dv/).

[Assignment instructions](https://ann1ejohansson.github.io/data-visualization-2026/assignments/assignment-2.html)
· [GitHub Pages setup](https://docs.github.com/en/pages/getting-started-with-github-pages/using-custom-workflows-with-github-pages)
