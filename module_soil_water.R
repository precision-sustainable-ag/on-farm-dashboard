# get soil water summary for selected sites
water_summary_extractor_SQL <- function(fields = "") {
  
  if (!length(fields) || any(nchar(fields) == 0)) { return(NULL) }
  
  con <- make_con()
  on.exit(dbDisconnect(con))
  
  fieldlist <- stringr::str_c("\'", fields, "\'", collapse = ",")
  
  install_queries <- purrr::map(
    c("bare_node_serial_no", "cover_node_serial_no"),
    ~glue::glue(
      '
    SELECT 
      "code", "subplot", "serial",
      \'{substr(.x, 1, 1)}\' AS "trt",
      "time_begin", 
      COALESCE("time_end", "possible_time_end", "end_of_year") AS "time_end"
    FROM (
      SELECT 
        "code", "subplot", "serial", 
        "time_begin", "time_end", "possible_time_end", 
        CAST(CONCAT(EXTRACT(year FROM "time_begin"), \'-11-01 12:00:00\') AS TIMESTAMP) AS "end_of_year"
      FROM (
        SELECT 
          "code", "subplot", "serial", 
          "time_begin", "time_end", 
          LEAD("time_begin", 1, NULL) OVER (PARTITION BY "serial" ORDER BY "code", "time_begin") AS "possible_time_end"
        FROM (
          SELECT 
            "code", "subplot", 
            "{.x}" AS "serial", 
            "time_begin", "time_end"
          FROM "wsensor_install" 
            WHERE ("code" IN ({fieldlist}))
            ORDER BY "code", "time_begin"
        ) AS filtered_subquery 
      ) AS filled_subquery
    ) AS guessed_subquery
    '
    )
  )
  
  
  purrr::map(
    install_queries,
    ~tbl(con, sql(.x))
  ) %>% 
    {union_all(.[[1]], .[[2]])} %>% 
    compute("temp_installs")
  
  
  
  subsets <- tbl(con, sql(
    '
      SELECT * FROM water_sensor_data
      LEFT JOIN temp_installs
      ON (
        "timestamp" > time_begin 
        AND "timestamp" < time_end 
        AND node_serial_no = serial
      )
      '
  )
  )
  
  
  subsets %>% 
    filter(!is.na(code)) %>% 
    mutate(d = as.Date(timestamp)) %>% 
    filter(between(vwc, 0, 100)) %>% 
    group_by(code, trt, d) %>% 
    summarise(
      vwc = mean(vwc, na.rm = T)
    ) %>% 
    mutate(
      inches = vwc / 2.54
    ) %>% 
    arrange(d) %>% 
    ungroup() %>% 
    mutate(d = as_datetime(d)) %>% 
    collect()
}













# generate box elements
water_boxer <- function(input, output, session, inputcode, data) {
  if (is.null(data)) return(NULL)
  
  data_at_inputcode <- data %>% filter(code == inputcode) 
  prettyid_at_inputcode <- na.omit(data_at_inputcode$prettyid[1])
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
    
    data_at_inputcode %>% group_by(code, trt, prettyid) %>% 
      mutate(count_days_backwards = row_number(-as.numeric(d))) %>% 
      ungroup() %>% 
      #filter(count_days_backwards < 7) %>% 
      group_by(code, trt, prettyid) %>% 
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
    tags$strong(glue::glue("{prettyid_at_inputcode} ({data_at_inputcode$year[1]})"))
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
            plotOutput(ns("fieldcap"), height = "20vh") %>% 
              shinycssloaders::withSpinner(type = 6, color = "#158cba"),  
            tags$br(),
            htmlOutput(ns("watertext"))
          ),
          column(
            7, textOutput(ns("errsurplus")),
            plotOutput(ns("surplus"), height = "35vh")
          )
        ),
        class = "card-body"
      ) ,
      class = "card border-primary mb-3"
    ), 
    ns = ns
  )
  
}
