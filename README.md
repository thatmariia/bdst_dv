# Assignment 2.1 — School composition

**Mariia Steeghs-Turchina · DV7 · solo project**

Research question: is the neighbourhood composition of a primary school's
new intake associated with that of its existing pupils?

This branch contains the data exploration and decisions for Part 1.

- `R/`: download, prepare, and plot the data.
- `scripts/`: setup, checks, and rendering.
- `report/`: the R Markdown submission.
- `data/raw/`: downloaded data, excluded from Git.

## Run

From the project root (R and Pandoc, or RStudio, required):

```sh
Rscript scripts/setup.R
Rscript scripts/check.R
Rscript tests/check-data.R
Rscript scripts/render.R
```

Setup restores pinned packages and enables the pre-commit hook for this
checkout. The hook checks the staged R/Rmd files for lint and formatting;
fix formatting with `Rscript scripts/check.R --fix`, then review and stage.

Rendering downloads four public DUO/CBS files once and creates
`report/DV-Assignment2-Part1-DV7.html`. The `.Rmd` and `.html` are the Canvas
submission. You can also use RStudio's Knit button. Data and generated HTML
stay out of Git; package installation is separate from knitting.

## GitHub Pages

The workflow checks, knits, and publishes reports on pushes to `main` or
`codex/assignment-2-1`. Pull requests to `main` build without deploying.

Before the first push, set **Settings → Pages → Source → GitHub Actions**
and allow the publishing branch in the `github-pages` environment if it
has a branch restriction. No access token or committed HTML is needed.
After a successful deployment:
[reports](https://thatmariia.github.io/bdst_dv/).

[Assignment instructions](https://ann1ejohansson.github.io/data-visualization-2026/assignments/assignment-2.html)
· [GitHub Pages setup](https://docs.github.com/en/pages/getting-started-with-github-pages/using-custom-workflows-with-github-pages)
