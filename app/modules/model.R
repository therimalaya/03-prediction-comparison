model_input_panel <- function(id){
  ns <- NS(id)
  div(
    id = "model-input-panel",
    actionButton(ns('set-model'), label = "Fit Model", icon = icon('far fa-server')),
    actionButton(ns('summary-btn'), label = "Summary", icon = icon('code')),
    actionButton(ns('dataset-btn'), label = "Show Dataset", icon = icon('table'))
  )
}
model_input_ui <- function(id, error_type = "Prediction"){
  ns <- NS(id)
  bsModal(
    ns("model-input"),
    "Data",
    trigger = ns('set-model'),
    size = "large",
    div(
      class = "modal-dialog",
      textAreaInput(ns("error-formula"), label = paste(error_type, "Error Model"),
                    value = "cbind(Y1, Y2, Y3, Y4) ~ (p + relpos + eta + gamma + Method) ^ 3",
                    width = "558px", placeholder = "Write the model formula"),
      textAreaInput(ns("comp-formula"), label = "Minimum Component Model",
                    value = "cbind(Y1, Y2, Y3, Y4) ~ (p + relpos + eta + gamma + Method) ^ 3",
                    width = "558px", placeholder = "Write the model formula"),
      actionButton(ns('fit-trigger'), label = "Fit Model")
    )
  )
}
model_input_server <- function(input, output, session) {
  ns <- session$ns
  res <- reactiveValues()
  observeEvent(input[['fit-trigger']], {
    res$formula$error <- input[['error-formula']]
    res$formula$comp <- input[['comp-formula']]
    toggleModal(session, 'model-input', toggle = 'close')
  })
  return(res)
}

error_data_ui <- function(id, error_type = "Prediction"){
  ns <- NS(id)
  bsModal(
    ns("data-modal"),
    paste(error_type, "Error Data"),
    trigger = ns('dataset-btn'),
    size = "large",
    tabsetPanel(
      id = ns("error-data-panel"),
      tabPanel(
        title = "Prediction Error Data",
        DT::dataTableOutput(ns('error-data'))
      ),
      tabPanel(
        title = "Minimum Component Data",
        DT::dataTableOutput(ns('comp-data'))
      )
    )
  )
}
error_data_server <- function(input, output, session, error_data, comp_data){
  ns <- session$ns
  output[['error-data']] <- renderDataTable({
    datatable(error_data %>% mutate_at(vars(Y1:Y4), round, 2))
  })
  output[['comp-data']] <- renderDataTable({
    datatable(comp_data)
  })
  
}

model_summary_ui <- function(id, error_type = "Prediction"){
  ns <- NS(id)
  bsModal(
    ns("summary-modal"),
    "Summary",
    trigger = ns('summary-btn'),
    size = "large",
    tabsetPanel(
      id = ns("model-summary"),
      tabPanel(
        title = paste(error_type, "Error Model"),
        verbatimTextOutput(ns('error-summary'))
      ),
      tabPanel(
        title = "Minimum Component Model",
        verbatimTextOutput(ns('comp-summary'))
      )
    )
  )
}
model_summary_server <- function(input, output, session, model_formula, error_data, comp_data) {
  res <- reactiveValues()
  ## Fit Models ---
  res$fit$error <- lm(as.formula(model_formula$error), data = error_data)
  res$fit$comp <- lm(as.formula(model_formula$comp), data = comp_data)
  res$anova$error <- anova(res$fit$error) %>%
    as.data.frame() %>%
    rownames_to_column('Factors') %>%
    as_tibble()
  res$anova$comp <- anova(res$fit$comp) %>%
    as.data.frame() %>%
    rownames_to_column('Factors') %>%
    as_tibble()
  
  ## Anova Output
  output[['error-summary']] <- renderPrint({
    anova(res$fit$error)
  })
  output[['comp-summary']] <- renderPrint({
    anova(res$fit$comp)
  })
  return(res)
}

model_summary_plot_ui <- function(id) {
  ns <- NS(id)
  plotOutput(ns('summary-plot'), 
             click = ns("summary-plot-click"),
             height = "100%")
}
model_summary_plot_server <- function(input, output, session, anova_df, error_type = "Prediction"){
  ns <- session$ns
  res <- reactiveValues()
  plt <- reactive({
    aov_df <- bind_rows(list(Error = anova_df$error, 
                             Comp = anova_df$comp), 
                        .id = "Type") %>% 
      mutate(Type = factor(Type, levels = c("Error", "Comp")))
    model_labels <- c(
      Error = paste("Model:", error_type, "Error"),
      Comp = "Model: Number of Components"
    )
    aov_df %>%
      filter(!(Factors %in% c('Residuals', '(Intercept)'))) %>%
      select(Model = Type, Factors, Pillai, Fvalue = `approx F`, Pvalue = `Pr(>F)`) %>%
      mutate(Pvalue = ifelse(Pvalue < 0.05, "<0.05", ">=0.05")) %>%
      ggplot(aes(reorder(Factors, Pillai), Pillai, fill = Pvalue)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(Fvalue, 2)), family = 'mono', angle = 0, 
                hjust = "inward", size = 3) +
      facet_grid(cols = vars(Model), scales = 'free',
                 labeller = labeller(Model = model_labels)) +
      theme_grey(base_family = "mono") +
      theme(legend.position = c(0.85, 0.1),
            legend.direction = 'horizontal',
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      guides(fill = guide_legend(title.position = "top",
                                 title.hjust = 0.5)) +
      labs(x = NULL, y = "Pillai Statistic") +
      coord_flip()
  })
  output[['summary-plot']] <- renderPlot(plt(), res = 110, height = 600)
  observeEvent(input[['summary-plot-click']], {
    all_label <- ggplot2::layer_scales(plt())[['x']][['range']][['range']]
    clicked_label <- isolate(all_label[round(input[['summary-plot-click']][['y']])])
    res$panel_var <- input[['summary-plot-click']][['panelvar1']]
    res$all_label <- all_label
    res$clicked_effect[[res$panel_var]] <- clicked_label
  })
  return(res)
}

model_effect_plot <- function(id) {
  ns <- NS(id)
  div(
    id = ns("pred-effect-plot"),
    plotOutput(ns('error-model-effect')),
    plotOutput(ns('comp-model-effect'))
  )
}
model_effect_plot_server <- function(input, output, session, click_info, fit) {
  effect_plot <- function(eff_lbl, fit) {
    plt <- eff_df(eff_lbl, fit) %>%
      get_eff_plot(eff_lbl, reorder = TRUE)
    plt
  }
  eff <- click_info$clicked_effect
  panel_var <- click_info$panel_var
  if (click_info$panel_var == "Error") {
    eff <- click_info$clicked_effect
    output[['error-model-effect']] <- renderPlot({
      effect_plot(unname(eff[[panel_var]]), fit$error)
    }, res = 110, height = 400)
  }
  if (click_info$panel_var == "Comp") {
    eff <- click_info$clicked_effect
    output[['comp-model-effect']] <- renderPlot({
      effect_plot(unname(eff[[panel_var]]), fit$comp)
    }, res = 110, height = 400)
  }
}

effect_box_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns('effect_plots'))
}
effect_box_server <- function(input, output, session, fit){
  ns <- session$ns
  effect_plot <- function(eff_lbl, fit) {
    eff_df(eff_lbl, fit) %>%
      get_eff_plot(eff_lbl, reorder = TRUE)
  }
  output[['err-effect']] <- renderPlot({
    term <- input[['err-term']]
    is_valid <- !identical(deparse_term_label(term, fit$error), character(0))
    validate(
      need(is_valid, 'Invalid Effect Term')
    )
    effect_plot(term, fit$error)
  }, res = 100)
  output[['comp-effect']] <- renderPlot({
    term <- input[['comp-term']]
    is_valid <- !identical(deparse_term_label(term, fit$comp), character(0))
    validate(
      need(is_valid, 'Invalid Effect Term')
    )
    effect_plot(input[['comp-term']], fit$comp)
  }, res = 100)
  output$effect_plots <- renderUI({
    tabBox(
      id = ns("effect-box"),
      title = "Effects",
      width = 12,
      tabPanel(
        title = "Error",
        textInput(ns('err-term'), label = "", value = "Method"),
        plotOutput(ns('err-effect'))
      ),
      tabPanel(
        title = "Component",
        textInput(ns('comp-term'), label = "", value = "Method"),
        plotOutput(ns('comp-effect'))
      )
    )
  })
}