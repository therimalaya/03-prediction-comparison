error_ncomp <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ncomp"))
}
error_rep_input <- function(id) {
  ns <- NS(id)
  uiOutput(ns("rep"))
}
error_output <- function(id) {
  ns <- NS(id)
  uiOutput(ns('error_plot'))
}
error_server <- function(input, output, session, dgn, mthd_name) {
  observe({
    rep <- do.call(seq, as.list(input$rep))
    ncomp <- do.call(seq, as.list(input$error_ncomp))
    for (plt in names(dgn)) {
      local({
        plt_id <- plt
        output[[plt_id]] <<- renderPlot({
          pred <- err_plot(coef_error = dgn[[plt_id]][rep], 
                   ncomp = ncomp, 
                   error_type = "Prediction") +
            ylim(1, NA)
          est <- err_plot(coef_error = dgn[[plt_id]][rep], 
                   ncomp = ncomp, 
                   error_type = "Estimation") +
            ylim(0, NA)
          return(gridExtra::grid.arrange(pred, est, nrow = 2))
        }, res = 90, height = 'auto')
      })
    }
  })
  observe({
    if (length(dgn) > 0) {
      ns <- session$ns
      output$ncomp <- renderUI({
        sliderInput(
          ns("error_ncomp"), "Number of Components", value = c(0, 10),
          min = 0, max = 10, step = 1, width = '100%')
      })
      output$rep <- renderUI({
        sliderInput(ns("rep"), "Replication", min = 1, max = 50,
                    step = 1, value = c(1, 25))
      })
    }
  })
  output$error_plot <- renderUI({
    ns <- session$ns
    dgn_lst <- strsplit(names(dgn), "-")
    dgn_txt <- unique(sapply(dgn_lst, function(x) paste0("dgn-", x[[2]])))
    names(dgn_txt) <- paste0("Design", gsub("dgn", "", dgn_txt))
    mdl_txt <- unique(sapply(dgn_lst, "[[", 3))
    mdl_tab_panel_args <- list(id = "design_tabs", type = "tabs")
    dgn_tab_box_args <- list(side = 'right', width = 12, id = "design-tab-box")
    mdl_tabs <- lapply(mdl_txt, function(mdl){
      dgn_tab_box_args$title = mthd_name[mdl]
      dgn_tabs <- lapply(names(dgn_txt), function(dgn){
        plt_id <- paste(gsub("Design", "dgn", dgn), mdl, sep = "-")
        tabPanel(title = dgn, value = dgn_txt[dgn], plotOutput(ns(plt_id), height = 800))
      })
      tabPanel(title = toupper(mdl), value = mdl, 
               fluidRow(do.call(tabBox, append(dgn_tab_box_args, dgn_tabs))))
    })
    do.call(tabsetPanel, append(mdl_tab_panel_args, mdl_tabs))
  }) 
}