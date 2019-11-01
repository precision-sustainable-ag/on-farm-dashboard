library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

# helpers ----
# a shorthand for the {purrr} error catchers
gracefully <- function(expr, otherwise = NA) {
  possibly(identity, otherwise = otherwise)(expr)
}


source("secret.R")

source("module_biomass.R")
source("module_soil_water.R")
source("module_yield.R")




