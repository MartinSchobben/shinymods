
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinymods

<!-- badges: start -->

[![R-CMD-check](https://github.com/MartinSchobben/shinymods/workflows/R-CMD-check/badge.svg)](https://github.com/MartinSchobben/shinymods/actions)
[![Codecov test
coverage](https://codecov.io/gh/MartinSchobben/shinymods/branch/master/graph/badge.svg)](https://app.codecov.io/gh/MartinSchobben/shinymods?branch=master)
[![Project Status: Concept – Minimal or no implementation has been done
yet, or the repository is only intended to be a limited example, demo,
or
proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![license](https://img.shields.io/github/license/mashape/apistatus.svg)](https://choosealicense.com/licenses/mit/)
[![Last-changedate](https://img.shields.io/badge/last%20change-2022--06--01-yellowgreen.svg)](/commits/master)
<!-- badges: end -->

The goal of shinymods is to package some convenient Shiny (Chang et al.
2021) modules that are often used to build an app. Some of the modules
are inspired by the book [Mastering
Shiny](https://mastering-shiny.org/index.html) (Wickham 2020).

You can also vendor the source R code to your own project to prevent
having excessive dependencies.

## Credits

This R (R Core Team 2022) package is intended to help you construct
Shiny (Chang et al. 2021) applications. Some of the modules are derived
from examples in the book *Mastering Shiny: Build Interactive Apps,
Reports, and Dashboards Powered by R* by Wickham (2020).

## Installation

You can install the development version of shinymods from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MartinSchobben/shinymods")
```

## Example

This is a basic example of a shiny app that requires filtering based on
the variables in a `dataframe`, where the controllers are conditional to
the value ranges of that particular variable.

``` r
library(shiny)
library(shinymods)
## basic example code

ui <- fluidPage(
        sidebarLayout(
          sidebarPanel(
            dataset_ui("test", filter = is.data.frame),
            filter_ui("test")
          ),
          mainPanel(
            tableOutput("table")
          )
        )
      )


server <- function(input, output, session) {
  dt <- dataset_server("test")
  ft <- filter_server(dt, "test", shinyjs = FALSE)
  output$table <- renderTable({req(ft()); ft()})
}

shinyApp(ui, server)
```

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-shiny" class="csl-entry">

Chang, Winston, Joe Cheng, JJ Allaire, Carson Sievert, Barret Schloerke,
Yihui Xie, Jeff Allen, Jonathan McPherson, Alan Dipert, and Barbara
Borges. 2021. *Shiny: Web Application Framework for r*.
<https://shiny.rstudio.com/>.

</div>

<div id="ref-rversion" class="csl-entry">

R Core Team. 2022. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

<div id="ref-Wickham2020" class="csl-entry">

Wickham, Hadley. 2020. *Mastering Shiny: Build Interactive Apps,
Reports, and Dashboards Powered by r*. O’Reilly Media, Inc.
<https://https://mastering-shiny.org//>.

</div>

</div>
