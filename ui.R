library(shiny)
library(leaflet)


map_attribution <- tags$small(
  icon("copyright"), "ESRI,", 
  icon("copyright"), "OpenStreetMap, Stamen", 
  icon("creative-commons"), "BY3.0",
  style = "float: right;"
)

footer <- fluidRow(
  column(
    12,
    hr(),
    tags$h2(
      tags$img(
        src = "PSAlogo-text.png", 
        style = "height: 2em; padding-left: 2em; padding-right: 2em;"
      ),
      tags$a(
        href = "http://www.precisionsustainableag.org", 
        "precisionsustainableag.org"
      ),
      style = "text-align: center;"
    ),
    hr()
  )
)


fluidPage(
  theme = "bootstrap.css",
  
  tags$head(
    tags$style(
      "div#dry_plot {height:85vh !important;}",
      "div#yield_plot {height:85vh !important;}",
      "h1{font-size:3rem !important;}",
      "h2{font-size:2.5rem !important;}",
      "body{font-size:160% !important;}",
      ".btn{font-size:120% !important;}", # font-family: monospace, monospace;
      ".form-control{font-size:100% !important;}",
      ".row:before, .row:after{ display: inline-block !important}" # Safari fix
    ),
    tags$link(rel="apple-touch-icon", href="apple-touch-icon.png"),
    tags$link(rel="icon", type="image/png", sizes="32x32", href="favicon-32x32.png"),
    tags$link(rel="icon", type="image/png", sizes="16x16", href="favicon-16x16.png"),
    tags$link(rel="manifest", href="site.webmanifest")
  ),
  
  title = "PSA On-farm trial report",
  
  titlePanel(
    div(
      tags$img(src = "PSAlogo-only.png", style = "height: 1.5em; vertical-align: middle;"),
      "On-farm trial report",
      style = "padding-top: 0.5em;"
      )
    ),
  tags$hr(),

  fluidRow(
    column(
      4,
      wellPanel(
        textInput(
          'lastname', 
          label = NULL, 
          placeholder = "Enter your last name"
        ),
        uiOutput('fieldinfo'),
        uiOutput('missingerror'),
        uiOutput('ll_error'),
        leafletOutput("map") %>% 
          shinycssloaders::withSpinner(type = 6, color = "#158cba"),
        map_attribution,
        actionButton(
          "reset", 
          tags$small("Reset View"), 
          icon = icon("refresh", class = "fa-xs"),
          class = "btn btn-success btn-sm"
        )
      ),
      uiOutput("fresh_text_summary"),
      tags$br()
    ),
    
    column(
      8,    
      tabsetPanel(
        tabPanel(
          "Cover biomass", tags$br(),  
          column(6, plotOutput('dry_plot')),
          column(5, uiOutput("dry_text_summary"))
        ),
        tabPanel(
          "Soil water", tags$br(),
          conditionalPanel("input.lastname", uiOutput("waterbox"))
        ),
        tabPanel(
          "Corn yield", tags$br(),
          column(6, conditionalPanel("input.lastname", uiOutput("yieldbox"))),
          column(6, plotOutput('yield_plot'))
        )
      )
    )
  ),
  
  footer
  
)