# APCharts
Shiny app that dynamically creates SPC Charts using an uploaded file.

Requires my forked version of qcc which can be installed here:

    devtools::install_github("bensker/qcc", build_vignettes = TRUE)

The follow packages must be installed prior to running:

   library(devtools)
   library(shiny)
   library(DT)
   library(tidyverse)
   library(markdown)
