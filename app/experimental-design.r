exp_design_plot <- function(id) {
  ns <- NS(id)
  plotOutput(ns('design_plot'), click = ns("design_clicked"))
}
exp_design_table <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns('design_table'))
}
simrel_plotsUI <- function(id, which) {
  ns <- NS(id)
  plotOutput(ns(paste0('simrel_plot_', which)), height = '300px')
}
exp_design <- function(input, output, session, design){
  selected <- reactiveValues()
  design <- design %>%
    mutate(Design_idx = row_number()) %>%
    arrange(relpos, eta, p, gamma) %>%
    mutate(Design = row_number())
  output$design_plot <- renderPlot({
    selected_idx <- input[['design_table_rows_selected']]
    design %>%
      mutate(selected = case_when(
        Design %in% selected_idx ~ 'selected',
        TRUE ~ 'not-selected')) %>%
      ggplot(aes(eta, gamma, color = selected)) +
      geom_point(shape=4) +
      geom_text(aes(label = Design), nudge_x = 0.03, nudge_y = 0.1) +
      facet_grid(p ~ relpos, labeller=label_both) +
      scale_y_reverse(breaks = unique(design$gamma)) +
      scale_x_continuous(breaks = unique(design$eta)) +
      theme_minimal(base_size=16) +
      theme(text=element_text(family="mono"),
            legend.position = 'none') +
      coord_fixed(ratio=0.6) +
      ggtitle("Experimental Design") +
      scale_color_manual(values = c('black', 'red'))
  }, res = 90)
  output$design_table <- DT::renderDataTable({
    DT::datatable(design %>% select(Design, p, gamma, eta, relpos),
                  rownames = FALSE,
                  selection = list(target = 'row', mode = 'single')) %>%
      formatStyle(1:5, fontFamily = "monospace")
  })
  proxy_design_table <- DT::dataTableProxy('design_table')
  observeEvent(input$design_clicked, {
    clicked_design <- nearPoints(design, input$design_clicked)[['Design']]
    design_idx <- with(design, which(Design %in% clicked_design))
    proxy_design_table %>% selectRows(design_idx)
    selected$design <- design_idx
    selected$design_index <- design$Design_idx
  })
  observeEvent(input$design_table_row_last_clicked, {
    clicked_design <- input$design_table_row_last_clicked
    design_idx <- with(design, which(Design %in% clicked_design))
    selected$design <- design_idx
    selected$design_index <- design$Design_idx
  })
  return(selected)
}
simrel_plots <- function(input, output, session, design, selected) {
  observe({
    if (length(selected$design) > 0) {
      sobj <- design %>%
        slice(selected$design_index) %>%
        get_design(selected$design) %>%
        simulate()
      output$simrel_plot_1 <- renderPlot({
        ggsimrelplot(sobj, which = 2L)
      }, res = 85)
      output$simrel_plot_2 <- renderPlot({
        ggsimrelplot(sobj, which = 3L)
      }, res = 85)
      output$simrel_plot_3 <- renderPlot({
        ggsimrelplot(sobj, which = 3L, use_population = FALSE)
      }, res = 85)
    }
  })
}
