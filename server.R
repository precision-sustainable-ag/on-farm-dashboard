library(shiny)
library(shinyWidgets)
library(dplyr)
library(googlesheets)
library(ggplot2)
library(tidyr)
library(stringr)
library(lubridate)
library(glue)
library(purrr)
library(leaflet)

library(promises)
library(future)
plan(multisession)

httr::set_config(httr::config(http_version = 0))

library(dbplyr)
library(RPostgres)
make_con <- function(host = pg_host) {
  dbConnect(
    Postgres(),
    dbname = pg_dbname,
    host = host,
    port = pg_port,
    user = pg_user,
    password = pg_password,
    sslmode = "require",
    application_name = "on_farm_dashboard_v2020.05"
  )
}


function(input, output, session) {
  # Resolve IP of DB once to speed up connections
  # first element in case it returns both ipv4 and ipv6
  host_ip <- iptools::hostname_to_ip(pg_host)[[1]][1]
  
  # Data loading section ----
  # _ site list ----
  start_sites_promise <- future({
    con <- make_con(host_ip)
    on.exit(dbDisconnect(con))
    
    full_join(
      tbl(con, "site_information") %>% 
        select(code, year, state = affiliation, longitude, latitude, producer_id),
      tbl(con, "producer_ids") %>% 
        select(producer_id, last_name),
      by = "producer_id"
    ) %>% collect() %>% 
      group_by(last_name) %>% 
      arrange(year) %>% 
      mutate(prettyid = sprintf("%02d", row_number())) %>% 
      ungroup()
  })
  start_sites <- reactiveVal()
  start_sites_promise %...>% start_sites()

  # _ biomass ----
   
  in_field_biomass_promise <- future({
    con <- make_con(host_ip)
    on.exit(dbDisconnect(con))
    
    tbl(con, "biomass_in_field") %>% 
      select(code, subplot, fresh_wt_a, fresh_wt_b, legumes_40) %>% 
      # TODO drop legumes_40 because of splitted tables?
      collect() %>% 
      mutate_all(~na_if(., -999)) %>% 
      gather(key = subsample, value = fresh_wt, fresh_wt_a:fresh_wt_b) %>% 
      mutate(subsample = str_to_upper(str_sub(subsample, start = -1, end = -1))) 
  })

  
  decomp_biomass_promise <- future({
    con <- make_con(host_ip)
    on.exit(dbDisconnect(con))
    
    full_join(
      tbl(con, "decomp_biomass_dry") %>%  
        filter(time == 0) %>% 
        select(code, subplot, subsample, dry_biomass_wt),
      tbl(con, "decomp_biomass_fresh") %>%  
        filter(time == 0) %>% 
        select(code, subplot, subsample, fresh_biomass_wt),
      by = c("code", "subplot", "subsample")
      # TODO subtract out empty_bag_wt? from both fresh and dry?
    ) %>% 
      full_join(
        tbl(con, "decomp_biomass_cn") %>% 
          filter(time == 0) %>% 
          select(code, subplot, subsample, percent_n),
        by = c("code", "subplot", "subsample")
      ) %>% 
      mutate(dry_to_fresh_ratio = dry_biomass_wt/fresh_biomass_wt) %>% 
      filter(dry_to_fresh_ratio <= 1, dry_to_fresh_ratio > 0) %>% 
      select(code, subplot, subsample, dry_to_fresh_ratio, percent_n) %>% 
      collect() %>% 
      mutate_all(~na_if(., -999))
  })


  # the two promises here can be combined into a single future block
  # saves one DB connection
  # could also be combined with biomass_sites, but saves no conns
  biomass_promise <- promise_all(
    in_field_biomass_promise, 
    decomp_biomass_promise
    ) %...>% {
      full_join(
        .[[1]], .[[2]], 
        by = c("code", "subplot", "subsample")
        ) %>% 
        mutate(
          dry_kg_ha = 10*fresh_wt*dry_to_fresh_ratio,
          n_kg_ha = 0.01*percent_n * dry_kg_ha
        ) 
    }

  
  biomass_sites_promise <- promise_all(
    biomass_promise, 
    start_sites_promise
    ) %...>% {
    full_join(.[[1]], .[[2]], by = "code")
  }
  biomass_sites <- reactiveVal()
  biomass_sites_promise %...>% biomass_sites()
  
  
  
  # TODO update to separated corn/soy/cotton tables
  # _ yield ----
  yield_corn_tbl_promise <- future({
    con <- make_con(host_ip)
    on.exit(dbDisconnect(con))
    
    tbl(con, "yield_corn") %>% 
      collect() 
    })
  
  yield_corn_promise <- promise_all(
    yield_corn_tbl_promise, 
    start_sites_promise
    ) %...>% {
      full_join(.[[1]], .[[2]], by = "code") %>% 
      mutate_all(~na_if(., -999)) %>% 
      mutate(
        bu_ac_dry = fresh_harvest_wt * (43560 / (2.5*20)) * 2.2 / (56),
        bu_ac = bu_ac_dry * (100 - moisture_1) / (100 - 15),
        N = case_when(
          subplot == 3 ~ "+N",
          T ~ "-N"
        )
      ) %>% 
      filter(!is.na(treatment))
  }
  
  yield_corn <- reactiveVal()
  yield_corn_promise %...>% yield_corn()
  

  # _ sensor summaries ----
  # TODO update to use DB instead
  soil_water_promise <- future({
    gs_key(soil_water_gs_key, lookup = F) %>%
      gs_read(col_types = "cccTnn")
  })
  soil_water <- reactiveVal()
  soil_water_promise %...>% soil_water()

  
# User filtering section ----

# _ lastname URL query handler
  observe({
    q <- getQueryString() %>% purrr::map(utils::URLdecode)
    updateTextInput(session, "lastname", value = q[["lastname"]] %||% "")
  })
  
  observeEvent(input$lastname, {
    qs <- paste0("?lastname=", utils::URLencode(input$lastname))
    updateQueryString(qs, mode = "replace")
  })
  
# _ output$fieldinfo ----
  output$fieldinfo <- renderUI({
    req(start_sites())
    req(input$lastname)
    
    choices <- 
      start_sites() %>% 
      filter(
        str_detect(
          str_to_lower(last_name), 
          str_to_lower(input$lastname)
          )
        ) %>% 
      select(code, prettyid)
      
    lab <- paste0(
      "Field ID:", 
      tags$small(
        "    click any to hide", 
        class = 'text-muted',
        style = 'white-space: pre;'
      )
    )
    
    checkboxGroupButtons(
      "fieldinfo", 
      HTML(lab),
      selected = choices$code,
      individual = T, 
      status = "outline-primary",
      choiceNames = choices$prettyid,
      choiceValues = choices$code
    )
  })
  
  # _ latlongs ----

  
  latlongs_grower <- reactive({
    req(start_sites())
    
    start_sites() %>% 
      filter(
        str_detect(
          str_to_lower(last_name), 
          str_to_lower(input$lastname)
          ),
        !is.na(latitude)
        ) 
  })
  
  # _ output$ll_error ----
#  output$ll_error <- renderUI({
    # req(latlongs_grower())
    # 
    # if (is.null(latlongs_grower())) {
    #   return(
    #     tags$small("Loading field locations...", class = "text-muted")
    #     )
    #   }
    # 
    # if (any(is.na(latlongs_grower()$latitude))) {
    #   return(
    #     tags$small(
    #       "Some field GPS coordinates aren't available yet.",  
    #       class = "text-muted", style = "float: right;"
    #       )
    #     )
    # } else {
    #   return(NULL)
    # }
#  })
  
  # Mapping section ----
  # _ basemap ----
  basemap <- 
    leaflet(options = leafletOptions(attributionControl = F)) %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      addProviderTiles(
        providers$Stamen.TonerLabels, 
        options = tileOptions(opacity = 0.5)
        ) %>% 
      fitBounds(-74.17, 39.96, -84.9,  32.15)
  
  # _ output$map ----
  output$map <- renderLeaflet({
    trash <- input$reset
    req(latlongs_grower())
    
    df <- latlongs_grower() %>% filter(!is.na(latitude))
    
    if (nrow(df) > 0 & input$lastname != "") {
      basemap %>% clearBounds() %>% 
      addCircleMarkers(
        data = df,
        ~longitude, ~latitude, label = ~prettyid,
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
  
  
  # Tab content modules ----

  # _ output$fresh_text_summary ----
  output$fresh_text_summary <- renderUI(
    fresh_pseudomodule(biomass_sites(), input$fieldinfo, input$lastname)
    )

  # _ output$dry_text_summary ----
  output$dry_text_summary <- renderUI(
    dry_pseudomodule(biomass_sites(), input$fieldinfo, input$lastname)
  )

  # _ output$dry_plot ----
  output$dry_plot <- renderPlot({
    req(biomass_sites())
    dry_plotter_module(biomass_sites(), input$fieldinfo, input$lastname)
  })

  # _ Walk over soil water module ----
  observeEvent(
    input$fieldinfo,
    walk(
      input$fieldinfo,
      ~callModule(water_boxer, id = .x, inputcode = .x, data = soil_water())
      )
    )

  # _ output$waterbox ----
  output$waterbox <- renderUI({
    req(input$fieldinfo)
    map(input$fieldinfo, water_boxerUI) %>% tagList()
  })

  # _ Walk over yield module ----
  observeEvent(
    input$fieldinfo,
    walk(
      input$fieldinfo,
      ~callModule(yield_boxer, id = .x, inputcode = .x, data = yield_corn())
      )
    )

  # _ output$yieldbox ----
  output$yieldbox <- renderUI({
    req(input$fieldinfo)
    map(input$fieldinfo, yield_boxerUI) %>% tagList()
    })


  # _ output$yield_plot ----
  output$yield_plot <- renderPlot({
    req(yield_corn())
    yield_plotter(yield_corn(), input$fieldinfo, input$lastname)
  })
}