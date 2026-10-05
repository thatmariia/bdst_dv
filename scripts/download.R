# Run from the project root, or source from the notebook, to cache its data.
library(jsonlite)
library(readr)
library(dplyr)

source("R/download.R")
paths <- download_data()
