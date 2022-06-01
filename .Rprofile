# package development
if (interactive()) {
  suppressPackageStartupMessages(require(devtools))
}

# dependency management
options(renv.settings.snapshot.type = "explicit")
source("renv/activate.R")

# R sources files
file.symlink("R/", "inst/src/R")

