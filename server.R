library(shiny)
library(shinyWidgets)
library(dplyr)
library(googlesheets)
library(ggplot2)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(glue)
library(purrr)
library(leaflet)

library(dbplyr)
library(RPostgres)
con <- dbConnect(
  Postgres(),
  dbname = pg_dbname,
  host = pg_host,
  port = pg_port,
  user = pg_user,
  password = pg_password,
  sslmode = "require"
)


function(input, output, session) {
  
  start_sites <- full_join(
    tbl(con, "site_information") %>% 
      select(code, year, state, longitude, latitude, producer_id),
    tbl(con, "producer_ids") %>% 
      select(producer_id, last_name)
  ) %>% collect()


  # biomass ----
   
  in_field_biomass <-
    tbl(con, "in_field_biomass") %>% 
    select(code, subplot, fresh_wt_a, fresh_wt_b, legumes_40) %>% 
    collect() %>% 
    gather(key = subsample, value = fresh_wt, fresh_wt_a:fresh_wt_b) %>% 
    mutate(subsample = str_to_upper(str_sub(subsample, start = -1, end = -1)))
  
  decomp_biomass <- 
    tbl(con, "decomp_biomass") %>% 
    filter(time == 0) %>% 
    mutate(dry_to_fresh_ratio = dry_biomass_wt/fresh_biomass_wt) %>% 
    filter(dry_to_fresh_ratio < 1) %>% 
    select(code, subplot, subsample, dry_to_fresh_ratio, percent_n) %>% 
    collect()
  
  biomass <- 
    full_join(in_field_biomass, decomp_biomass) %>% 
    mutate(
      dry_kg_ha = 10*fresh_wt*dry_to_fresh_ratio,
      n_kg_ha = 0.01*percent_n * dry_kg_ha
      )
  
  biomass_sites <- 
    full_join(biomass, start_sites) 
  
  # yield
  yield <- tbl(con, "yield") %>% 
    collect() %>% 
    full_join(start_sites) %>% 
    mutate(
      bu_ac = harvest_wt * (43560 / (2.5*20)) * 2.2 / (56),
      N = case_when(
        subplot == 3 ~ "+N",
        T ~ "-N"
        )
      ) %>% 
    filter(!is.na(treatment))
  

  # sensor summaries 
  soil_water <- 
    gs_key(soil_water_gs_key, lookup = F) %>% 
    gs_read(col_types = "cccTnn")
  

  

# output$fieldinfo ----
  output$fieldinfo <- renderUI({
    if (is.null(start_sites) | input$lastname == "") return(NULL)
    
    choices <- 
      start_sites %>% 
      filter(
        str_detect(
          str_to_lower(last_name), 
          str_to_lower(input$lastname)
          )
        ) %>% 
      pull(code) %>% 
      unique()
      
    
    checkboxGroupButtons(
      "fieldinfo", "Field ID:",
      choices = choices,
      selected = choices,
      individual = T, status = "outline-primary"
    )
  })
  
  # latlongs ----

  
  latlongs_grower <- reactive({
    if (is.null(start_sites)) return(NULL)
    
    start_sites %>% 
      filter(
        str_detect(
          str_to_lower(last_name), 
          str_to_lower(input$lastname)
          )
        ) 
  })
  
  # output$ll_error ----
  output$ll_error <- renderUI({
    if (is.null(latlongs_grower())) {
      return(
        tags$small("Loading field locations...", class = "text-muted")
        )
      }
    
    if (any(is.na(latlongs_grower()$latitude))) {
      return(
        tags$small(
          "Some field GPS coordinates aren't available yet.",  
          class = "text-muted", style = "float: right;"
          )
        )
    } else {
      return(NULL)
    }
  })
  
  # basemap ----
  basemap <- 
    leaflet(options = leafletOptions(attributionControl = F)) %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      addProviderTiles(providers$Stamen.TonerLabels, 
                       options = tileOptions(opacity = 0.5)) %>% 
      fitBounds(-74.17, 39.96, -84.9,  32.15)
  
  # output$map ----
  output$map <- renderLeaflet({
    trash <- input$reset
    
    df <- latlongs_grower() %>% filter(!is.na(latitude))
    
    if (nrow(df) > 0 & input$lastname != "") {
      basemap %>% clearBounds() %>% 
      addCircleMarkers(
        data = df,
        ~longitude, ~latitude, label = ~code,
        labelOptions = labelOptions(
          noHide = T, 
          offset = c(0,15),
          opacity = 0.65, 
          style = list(
            "color" = "black", 
            "font-size" = "18px", 
            "font-weight" = "900"
            )
          ),
        clusterOptions = markerClusterOptions(
          spiderLegPolylineOptions = list(color = "#ffffff", weight = 2.5),
          style = list("font-weight" = "900")
          ),
        color = "white", opacity = 0.8
        )
    } else {
      basemap %>%  
        addCircleMarkers(
          data = df,
          ~longitude, ~latitude, 
          clusterOptions = markerClusterOptions(
            spiderLegPolylineOptions = list(color = "#ffffff", weight = 2.5),
            style = list("font-weight" = "900")
          ),
          color = "white", opacity = 0  
        ) %>% 
        addCircleMarkers(
          data = latlongs_grower(),
          ~longitude, ~latitude,
          fill = F, radius = 5,
          color = "white", opacity = 0.5
        )
      }
  
  })
  
  
  
  
  # output$fresh_text_summary ----
  output$fresh_text_summary <- renderUI(
    fresh_pseudomodule(biomass_sites, input$fieldinfo, input$lastname)
    )
  
  # output$dry_text_summary ----
  output$dry_text_summary <- renderUI(
    dry_pseudomodule(biomass_sites, input$fieldinfo, input$lastname)
  )
  
  # output$dry_plot ----
  output$dry_plot <- renderPlot(
    dry_plotter_module(biomass_sites, input$fieldinfo, input$lastname)
  )
  
  # Walk over soil water module ----
  observeEvent(
    input$fieldinfo, 
    walk(
      input$fieldinfo, 
      ~callModule(water_boxer, id = .x, inputcode = .x, data = soil_water)
      )
    )
  
  # output$waterbox ----
  output$waterbox <- renderUI(
    map(input$fieldinfo, water_boxerUI) %>% tagList()
  )

  # Walk over yield module ----
  observeEvent(
    input$fieldinfo, 
    walk(
      input$fieldinfo, 
      ~callModule(yield_boxer, id = .x, inputcode = .x, data = yield)
      )
    )
  
  # output$yieldbox ----
  output$yieldbox <- renderUI(
    map(input$fieldinfo, yield_boxerUI) %>% tagList()
    )

  
  # output$yield_plot ----
  output$yield_plot <- renderPlot(
    yield_plotter(yield, input$fieldinfo, input$lastname)
  )
}