model_input <- function(id) {
  ns <- NS(id)
  div(
    id = "model-input-panel",
    actionButton(ns('set-model'), label = "Fit Model", icon = icon('far fa-server')),
    actionButton(ns('summary-btn'), label = "Summary", icon = icon('code')),
    actionButton(ns('dataset-btn'), label = "Show Dataset", icon = icon('table'))
  )
}
model_data <- function(id) {
  ns <- NS(id)
  uiOutput(ns('model-data-ui'))
}
model_summary <- function(id) {
  ns <- NS(id)
  uiOutput(ns('pred-summary-ui'))
}
model_summary_plot <- function(id) {
  ns <- NS(id)
  plotOutput(ns('pred-summary-plot'), 
             click = ns("pred-plot-click"),
             height = "100%")
}
model_effect_plot <- function(id) {
  ns <- NS(id)
  div(
    id = ns("pred-effect-plot"),
    plotOutput(ns('pred-effect-plot-pred')),
    plotOutput(ns('pred-effect-plot-comp'))
  )
}
modal_server <- function(input, output, session, pred_data, pred_comp_data) {
  ns <- session$ns
  res <- reactiveValues()
  output[['pred-data']] <- renderDataTable({
    datatable(pred_data %>% mutate_at(vars(Y1:Y4), round, 2))
  })
  output[['pred-comp-data']] <- renderDataTable({
    datatable(pred_comp_data)
  })
  output[['model-data-ui']] <- renderUI({
    bsModal(
      ns("data-modal"),
      "Data",
      trigger = ns('dataset-btn'),
      size = "large",
      tabsetPanel(
        id = ns("model-data"),
        tabPanel(
          title = "Prediction Error Data",
          dataTableOutput(ns('pred-data'))
        ),
        tabPanel(
          title = "Minimum Component Data",
          dataTableOutput(ns('pred-comp-data'))
        )
      )
    )
  })
  output[['model-summary-ui']] <- renderUI({
    bsModal(
      ns("summary-modal"),
      "Summary",
      trigger = ns('summary-btn'),
      size = "large",
      tabsetPanel(
        id = ns("model-summary"),
        tabPanel(
          title = "Prediction Error Model",
          verbatimTextOutput(ns('pred-summary'))
        ),
        tabPanel(
          title = "Minimum Component Model",
          verbatimTextOutput(ns('pred-comp-summary'))
        )
      )
    )
  })
  observeEvent(input[['fit-trigger']], {
    res$pred_mdl <- lm(as.formula(input[["pred-formula"]]), data = pred_data)
    res$comp_mdl <- lm(as.formula(input[["pred-comp-formula"]]), data = pred_comp_data)
    res$pred_aov_df <- anova(res$pred_mdl) %>%
      as.data.frame() %>%
      rownames_to_column('Factors') %>%
      as_tibble()
    res$comp_aov_df <- anova(res$comp_mdl) %>%
      as.data.frame() %>%
      rownames_to_column('Factors') %>%
      as_tibble()
    output[['pred-summary']] <- renderPrint({
      anova(res$pred_mdl)
    })
    output[['pred-comp-summary']] <- renderPrint({
      anova(res$comp_mdl)
    })
    output[['pred-summary-plot']] <- renderPlot({
      aov_df <- bind_rows(list(Pred = res$pred_aov_df, 
                               Comp = res$comp_aov_df), 
                          .id = "Type") %>% 
        mutate(Type = factor(Type, levels = c("Pred", "Comp")))
      model_labels <- c(
        Pred = "Model: Prediction Error",
        Comp = "Model: Number of Components"
      )
      res$aov_plot <- aov_df %>%
        filter(!(Factors %in% c('Residuals', '(Intercept)'))) %>%
        select(Model = Type, Factors, Pillai, Fvalue = `approx F`, Pvalue = `Pr(>F)`) %>%
        mutate(Pvalue = ifelse(Pvalue < 0.05, "<0.05", ">=0.05")) %>%
        ggplot(aes(reorder(Factors, Pillai), Pillai, fill = Pvalue)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = round(Fvalue, 2)), family = 'mono', angle = 0, 
                  hjust = "inward", size = 3) +
        facet_grid(cols = vars(Model), scales = 'free_y',
                   labeller = labeller(Model = model_labels)) +
        theme_grey(base_family = "mono") +
        theme(legend.position = c(0.85, 0.1),
              legend.direction = 'horizontal',
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        guides(fill = guide_legend(title.position = "top",
                                   title.hjust = 0.5)) +
        labs(x = NULL, y = "Pillai Statistic") +
        coord_flip()
      res$aov_plot
    }, res = 110, height = 600)
    removeModal()
  })
  observeEvent(input[['pred-plot-click']], {
    if (input$`pred-plot-click`$panelvar1 == "Pred") {
      output[['pred-effect-plot-pred']] <- renderPlot({
        all_label <- ggplot2::layer_scales(res$aov_plot)[['x']][['range']][['range']]
        clicked_label <- isolate(all_label[round(input$`pred-plot-click`$y)])
        plt <- eff_df(clicked_label, res$pred_mdl) %>% 
          get_eff_plot(clicked_label, reorder = TRUE)
        plt + ggtitle("", subtitle = "Model: Prediction Error")
      }, res = 110, height = 400)
    }
    if (input$`pred-plot-click`$panelvar1 == "Comp") {
      output[['pred-effect-plot-comp']] <- renderPlot({
        all_label <- ggplot2::layer_scales(res$aov_plot)[['x']][['range']][['range']]
        clicked_label <- isolate(all_label[round(input$`pred-plot-click`$y)])
        plt <- eff_df(clicked_label, res$comp_mdl) %>% 
          get_eff_plot(clicked_label, reorder = TRUE)
        plt + ggtitle("", subtitle = "Model: Number of Components")
      }, res = 110, height = 400)
    }
  })
  setModal <- function(failed = FALSE) {
    modalDialog(
      textAreaInput(ns("pred-formula"), label = "Prediction Error Model",
                    value = "cbind(Y1, Y2, Y3, Y4) ~ (p + relpos + eta + gamma + Method) ^ 3",
                    width = "558px", placeholder = "Write the model formula"),
      textAreaInput(ns("pred-comp-formula"), label = "Minimum Component Model",
                    value = "cbind(Y1, Y2, Y3, Y4) ~ (p + relpos + eta + gamma + Method) ^ 3",
                    width = "558px", placeholder = "Write the model formula"),
      if (failed)
        div(tags$b("Invalid name of data object", style = "color: red;")),
      footer = tagList(
        actionButton(ns('fit-trigger'), label = "Fit Model")
      )
    )
  }
  observeEvent(input[['set-model']], {
    showModal(setModal())
  })
}
