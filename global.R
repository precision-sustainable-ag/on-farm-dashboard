library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

library(RPostgres)

# helpers ----
# connect to the DB inside each promise or reactive
make_con <- function(host = pg_host) {
  dbConnect(
    Postgres(),
    dbname = pg_dbname,
    host = host,
    port = pg_port,
    user = pg_user,
    password = pg_password,
    sslmode = "require",
    application_name = "on_farm_dashboard_v2020.08"
  )
}


# a shorthand for the {purrr} error catchers
gracefully <- function(expr, otherwise = NA) {
  possibly(identity, otherwise = otherwise)(expr)
}



source("secret.R")

source("module_biomass.R")
source("module_soil_water.R")
source("module_yield.R")




