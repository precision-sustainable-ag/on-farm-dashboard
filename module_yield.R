yield_boxer <- function(input, output, session, inputcode, data) {

  data_at_inputcode <- data %>% 
    filter(code == inputcode, !is.na(bu_ac), !is.na(treatment)) 
  
  data_summarized <- data_at_inputcode %>% 
    group_by(treatment, N) %>% 
    summarize(
      mean_bu_ac = mean(bu_ac),
      sd_bu_ac = sd(bu_ac)
      )
  
  bare <- signif(data_summarized %>% filter(treatment == "B", N == "-N") %>% pull(mean_bu_ac), 2)
  cover <- signif(data_summarized %>% filter(treatment == "C", N == "-N") %>% pull(mean_bu_ac), 2)
  
  bareN <- signif(data_summarized %>% filter(treatment == "B", N == "+N") %>% pull(mean_bu_ac), 2)
  coverN <- signif(data_summarized %>% filter(treatment == "C", N == "+N") %>% pull(mean_bu_ac), 2)
  
  pal <- scales::hue_pal(h.start = 30, l = 60)(3)[1:2]
  
  output$showYieldBox <- reactive(length(cover))
  outputOptions(output, "showYieldBox", suspendWhenHidden = FALSE)
  
  output$summary <- renderPlot({
    if (nrow(data_at_inputcode) == 0) return(NULL)
    
    ggplot(
      data_summarized, 
      aes(treatment, mean_bu_ac, fill = treatment, alpha = N)
      ) + 
      geom_col(show.legend = F, position = position_dodge(0.85)) +
      geom_linerange(
        aes(ymin = mean_bu_ac-sd_bu_ac, ymax = mean_bu_ac+sd_bu_ac, group = N), 
        alpha = 1,
        position = position_dodge(0.85), 
        show.legend = F
      ) +
      labs(x = NULL, y = NULL) +
      scale_fill_manual(values = pal) +
      scale_alpha_manual(values = c(0.6,1)) +
      theme_classic()
  })
  
  output$yieldhead <- renderUI({
    tags$strong(inputcode)
  })
  
  output$yieldtext <- renderUI({
    if (nrow(data_at_inputcode) == 0) return(NULL)
    
    if (length(coverN) & length(bareN)) {
      Nplus <- div(
        hr(),
        "With added N fertilizer:",
        tags$li(
          span(tags$strong("Cover crop:"), style = glue::glue("color:{pal[2]};")),
          coverN, " bushels/acre"
        ),
        tags$li(
          span(tags$strong("Bare ground:"), style = glue::glue("color:{pal[1]};")),
          bareN, " bushels/acre"
        )
      )
    } else { Nplus <- div() }
    
    div(
      "Standard N fertilizer rate:",
      tags$li(
        tags$span(tags$strong("Cover crop:"), style = glue::glue("color:{pal[2]};")),
        cover, " bushels/acre"
      ),
      tags$li(
        tags$span(tags$strong("Bare ground:"), style = glue::glue("color:{pal[1]};")),
        bare, " bushels/acre"
      ),
      Nplus
    )
  })
}

# layout box: don't forget ns() inside *Output(...)
yield_boxerUI <- function(id) {
  ns <- NS(as.character(id))
  
  
  conditionalPanel(
    "output.showYieldBox",
    div(
      div(
        htmlOutput(ns("yieldhead")),
        class = "card-header"
      ),
      div(
        fluidRow(
          column(6, plotOutput(ns("summary"), height = "22vh")),
          column(6, htmlOutput(ns("yieldtext")))
        ),
        class = "card-body"
      ),
      class = "card border-primary mb-3"
    ),
    ns = ns
  )

}

yield_plotter <- function(yield_df, field_codes, lastname) {
  if (is.null(yield_df)) return(NULL)
  
  trash <- lastname
  
  df <- yield_df %>% 
    filter(N == "-N") %>% 
    select(code, state, treatment, subplot, row, bu_ac) %>% 
    spread(treatment, bu_ac) %>% 
    mutate(cover_surplus = C-B) %>% 
    filter(!is.na(cover_surplus)) %>% 
    group_by(code, state) %>% 
    summarise(
      mean_cs = mean(cover_surplus),
      sd_cs = sd(cover_surplus)/sqrt(length(cover_surplus))
      ) %>% 
    ungroup() %>% 
    mutate(flag = code %in% field_codes,
           flag = replace(flag, trash == "", F)) %>%
    filter(!is.na(state)) %>% 
    group_by(state) %>% 
    mutate(rnk = row_number(mean_cs))
  
  regions <- df %>% filter(flag) %>% pull(state) %>% unique()
  
  if (length(regions) > 0) {
    df <- df %>% filter(state %in% regions)
  }
  
  ggplot(df, aes(rnk, mean_cs, color = flag)) +
    geom_hline(yintercept = 0, size = 1, color = "grey50") +
    geom_linerange(
      aes(ymin = mean_cs-sd_cs, ymax = mean_cs+sd_cs),
      show.legend = F, 
      color = "grey65"
    ) +
    geom_point(show.legend = F, size = 3.5) +
    facet_grid(state ~ ., scales = "free_y", space = "free_y") +
    geom_text(
      data = function(d) filter(d, flag),
      aes(x = rnk, y = mean_cs+sd_cs, label = code),
      nudge_y = 5, 
      hjust = 0, 
      fontface = "bold",
      show.legend = F, 
      size = 5
    ) +
    coord_flip() +
    scale_x_continuous(expand = expand_scale(add = 2)) +
    scale_y_continuous(
      labels = scales::comma,
      sec.axis = dup_axis(name = NULL),
      expand = expand_scale(mult = 0.05, add = 2)
    ) +
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
    labs(
      x = NULL, 
      y = expression("Yield bonus with cover crop: "*frac(bu,acre)),
      title = "Your farm compared with others in your region"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = rel(1.75)),
      strip.text = element_text(size = rel(1.5)),
      axis.title.x = element_text(size = rel(1.5)),
      panel.spacing = unit(0, "lines"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      title = element_text(face = "bold")
    )
  
}
