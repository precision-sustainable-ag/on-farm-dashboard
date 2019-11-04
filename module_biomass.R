# generate fresh weight summaries ----

fresh_pseudomodule <- function(biomass_df, field_codes, lastname) {
  if (lastname == "") return(NULL)
  
  if (length(field_codes) == 0) {
    div("Select one or more fields to get a summary of your cover crop data.")
  } else {
    df <- biomass_df %>% 
      ungroup() %>% 
      filter(code %in% field_codes)  %>%
      group_by(year, code, subplot) %>%
      summarise(mean_fresh = signif(mean(fresh_wt, na.rm = T)*10/4882.428, 2)) %>% 
      arrange(year, code, subplot) %>% 
      group_by(year, code, subplot) %>% 
      mutate(
        report = as.character(glue(
          "{year}, {code}({subplot}): {tags$b(mean_fresh)} lbs/10ft{tags$sup(2)}"
        )),
        missing = as.character(glue(
          "{year}, {code}"
        ))
      )
    
    
    
    fresh_text <- 
      map(
        df %>% filter(!is.na(mean_fresh)) %>% pull(report), 
        ~tags$li(HTML(.x))
      )
    
    fresh_missing <- 
      map(
        df %>% filter(is.na(mean_fresh)) %>% pull(missing) %>% unique(), 
        ~tags$li(HTML(.x))
      )
    
    
    
    div(
      if (length(fresh_text)) {
        div(
          "We collected fresh plant tissues of your cover crop:", 
          fresh_text,
          tags$hr(),
          style = "list-style:inside;"
        )
      },
      if (length(fresh_missing)) {
        div(
          "Some measurements are not currently available:",
          fresh_missing,
          tags$hr(),
          style = "list-style:inside;"
        )
      }
    )
    
  }
}

# generate dry matter summaries ----

dry_pseudomodule <- function(biomass_df, field_codes, lastname) {
  if (lastname == "") return(NULL)
  
  
  df <- biomass_df %>% 
    ungroup() %>% 
    filter(code %in% field_codes) %>%
    group_by(year, code) %>%
    summarise(
      mean_dry = signif(mean(dry_kg_ha, na.rm = T)*0.892179, 2),
      mean_n = signif(mean(n_kg_ha, na.rm = T)*0.892179, 2)
      ) %>% 
    ungroup() %>% 
    filter(!is.na(mean_dry)) %>% 
    group_by(year, code) %>% 
    mutate(
      report_dry = as.character(glue(
        "In field {code} ({year}), your cover crop had about ",
        "{tags$b(mean_dry)} lbs/acre of dry matter"
      )),
      report_n = as.character(glue(
        ", and about {tags$b(mean_n)} lbs/acre of N."
      )),
      report_n = replace(report_n, is.na(mean_n), "."),
      report = paste0(report_dry, report_n)
    )
  
  summaries <- map(df %>% pull(report), ~tags$p(HTML(.x))) 

  div(
    h2(tags$strong("Dry matter summary")), 
    summaries, 
    tags$hr(), 
    tags$br()
  ) 

}



# plot dry matter ----
dry_plotter_module <- function(biomass_df, field_codes, lastname) {

  # forces reset on input
  trash <- lastname
  
  df <- biomass_df %>% 
    ungroup() %>% 
    mutate(
      flag = code %in% field_codes,
      flag = replace(flag, trash == "", F)
    ) %>%
    filter(!is.na(dry_kg_ha), !is.na(state)) %>% 
    group_by(state, year, code, flag) %>%
    summarise(
      mean_dry = mean(dry_kg_ha)*0.892179,
      sd_dry = sd(dry_kg_ha)*0.892179
      ) %>% 
    ungroup() %>% 
    group_by(state) %>% 
    mutate(rnk = row_number(mean_dry))
  
  regions <- df %>% filter(flag) %>% pull(state) %>% unique()
  
  if (length(regions) > 0) {
    df <- df %>% filter(state %in% regions)
  }
  
  ggplot(df, aes(rnk, mean_dry, color = flag)) +
    geom_linerange(
      aes(ymin = mean_dry-sd_dry, ymax = mean_dry+sd_dry),
      show.legend = F, 
      color = "grey65"
    ) +
    geom_point(show.legend = F, size = 3.5) +
    facet_grid(state ~ ., scales = "free_y", space = "free_y") +
    geom_text(
      data = function(d) filter(d, flag),
      aes(x = rnk, y = mean_dry+sd_dry, label = code),
      nudge_y = 200, 
      hjust = 0, 
      fontface = "bold",
      show.legend = F, 
      size = 5
    ) +
    coord_flip() +
    scale_x_continuous(expand = expand_scale(add = 2)) +
    scale_y_continuous(
      limits = c(-100,NA),
      labels = scales::comma,
      sec.axis = dup_axis(name = NULL),
      expand = expand_scale(mult = 0.05, add = 500)
      ) +
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
    labs(
      x = NULL, 
      y = expression("Dry matter: "*frac(lbs,acre)),
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
