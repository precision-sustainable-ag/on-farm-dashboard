# generate box elements
water_boxer <- function(input, output, session, inputcode, data) {
  if (is.null(data)) return(NULL)
  
  data_at_inputcode <- data %>% filter(code == inputcode) 
  pal <- scales::hue_pal(h.start = 30, l = 60)(3)[1:2]
  
  output$showWaterBox <- reactive(nrow(data_at_inputcode))
  outputOptions(output, "showWaterBox", suspendWhenHidden = FALSE)
  
  output$surplus <- renderPlot({
    
    data_wide <- data_at_inputcode %>%  
      select(-vwc) %>% 
      spread(key = trt, value = inches) %>%  
      mutate(surplus = gracefully(c-b)) %>% 
      filter(!is.na(surplus))
    
    rng_max <- max(abs(range(data_wide$surplus, na.rm = T)))
    
    if (
      !is.finite(rng_max) | 
      nrow(data_at_inputcode) == 0 | 
      nrow(data_wide) == 0 | 
      length(unique(data_at_inputcode$trt)) != 2
      ) {
      output$errsurplus <- renderText("Some data is missing. Check back soon.")
      return(NULL)
    }
    
    data_wide %>% 
      ggplot(aes(d, surplus)) + 
      geom_col(
        aes(fill = factor(surplus>=0)), 
        show.legend = F, 
        width = 60*60*24,
        na.rm = T
        ) +
      scale_y_continuous(
        "Saved water, inches", labels = abs, 
        limits = c(-1,1)*rng_max,
        sec.axis = dup_axis(name = " Cover crop    No cover   ")) + # ← →
      scale_fill_manual(values = set_names(pal, c("FALSE", "TRUE"))) +
      theme_minimal() +
      labs(x = NULL) +
      theme(axis.title = element_text(size = rel(1.2)),
            axis.text = element_text(size = rel(1.2)))
    
    
  })
  
  output$fieldcap <- renderPlot({
    if (nrow(data_at_inputcode) == 0) {
      output$errfieldcap <- renderText("Some data is missing. Check back soon.")
      return(NULL)
    } 
    
    data_at_inputcode %>% group_by(code, trt) %>% 
      mutate(count_days_backwards = row_number(-as.numeric(d))) %>% 
      ungroup() %>% 
      #filter(count_days_backwards < 7) %>% 
      group_by(code, trt) %>% 
      summarise(pctoffc = mean(inches, na.rm = T)/(3.5*3.28)) %>% 
      ggplot(aes(0, pctoffc, fill = trt)) + 
      geom_col(position = position_dodge(width = 1), show.legend = F, na.rm = T) +
      geom_hline(yintercept = 0:1, size = 1) +
      scale_y_continuous(
        "% of soil water field capacity",
        breaks = (0:4)/4,
        labels = scales::percent
      ) +
      scale_x_continuous(NULL, breaks = NULL) +
      scale_fill_manual(values = set_names(pal, c("b", "c"))) +
      theme_minimal() +
      theme(
        axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        axis.text.y = element_blank()
        ) +
      coord_flip()
    
  })
  
  output$waterhead <- renderUI({
    tags$strong(inputcode)
  })
  
  output$watertext <- renderUI({
    if (nrow(data_at_inputcode) == 0) return(NULL)
    
    div(
      "In both these graphs", 
      span(tags$strong("green"), style = glue::glue("color:{pal[2]};")),
      "refers to the cover crop, and", 
      span(tags$strong("orange-brown"), style = glue::glue("color:{pal[1]};")),
      "refers to the bare ground.", 
      tags$strong("Field capacity"), 
      "is used here to mean the approximate amount of",
      "water your soil can hold (without being saturated) in the top 3 feet.",
      "This graph is averaged over time, representing the whole season.",
      tags$strong("Saved water"), 
      "is how many more inches of water (in the top 3 feet)",
      "one plot had relative to the other each day."
    )
  })
}

# layout box: don't forget ns() inside *Output(...)
water_boxerUI <- function(id) {
  ns <- NS(as.character(id))
  
  conditionalPanel(
    "output.showWaterBox",
    div(
      div(
        htmlOutput(ns("waterhead")),
        class = "card-header"
      ),
      div(
        fluidRow(
          column(
            5, textOutput(ns("errfieldcap")), 
            plotOutput(ns("fieldcap"), height = "20vh"),  tags$br(),
            htmlOutput(ns("watertext"))
          ),
          column(
            7, textOutput(ns("errsurplus")),
            plotOutput(ns("surplus"), height = "35vh")
          )
        ),
        class = "card-body"
      ),
      class = "card border-primary mb-3"
    ), 
    ns = ns
  )
  
}
