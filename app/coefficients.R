## ---- Coefficients ----
## Side Panel ----
method_select_input <- function(id) {
  ns <- NS(id)
  uiOutput(ns('mthd_select'))
}
coef_design_input <- function(id) {
  ns <- NS(id)
  selectInput(ns('design'), label = "Design", multiple = TRUE,
              selected = 1, choices = `names<-`(1:32, paste0('Design', 1:32)))
}
coef_design_output <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns('design_table'))
}
coef_plot_trigger <- function(id) {
  ns <- NS(id)
  actionButton(ns('req'), label = "Plot Coefficients")
}
coef_panel <- function(input, output, session, design, design_chr) {
  out <- reactiveValues()
  out$design_selected <- NULL
  method_list <- list(
    `Principal Component Regression` = "PCR",
    `Partial Least Squares 1` = "PLS1",
    `Partial Least Squares 2` = "PLS2",
    `Envelope in Predictor` = "Xenv",
    `Simulteneous Envelope` = "Senv"
  )
  output$design_table <- DT::renderDataTable({
    design_chr %>% 
      mutate(Design = row_number()) %>% 
      select(Design, p, relpos, eta, gamma) %>% 
      slice(as.numeric(input$design)) %>% 
      DT::datatable(
        class = 'compact',
        rownames = FALSE,
        selection = list(target = 'row', mode = 'single'),
        options = list(
          dom = 't',
          pageLength = 32)) %>% 
      formatStyle(1:ncol(design_chr), fontFamily='monospace')
  })
  output$mthd_select <- renderUI({
    ns <- session$ns
    selectInput(
      size = 5,
      selectize = FALSE,
      ns('method'),
      label = 'Estimator',
      multiple = TRUE,
      selected = c("PCR", "Xenv"),
      choices = method_list
    )
  })
  observeEvent(input$design_table_row_last_clicked, {
    out$design_selected <- input$design[input$design_table_row_last_clicked]
  })
  
  observe({
    out$index <-  input$design
    out$method <- input$method
    out$request <- input$req
    out$method_list <- method_list
  })
  return(out)
}

## Body Panel ----
coef_plot_output <- function(id) {
  ns <- NS(id)
  uiOutput(ns('coef_plot_output'))
}
coef_plot_ncomp <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ncomp"))
}
coef_rep_input <- function(id) {
  ns <- NS(id)
  uiOutput(ns("rep"))
}
coef_plot_server <- function(input, output, session, dgn, mthd_name) {
  observe({
    rep <- do.call(seq, as.list(input$rep))
    ncomp <- do.call(seq, as.list(input$coef_ncomp))
    for (plt in names(dgn)) {
      local({
        id <- plt
        output[[id]] <<- renderPlot({
          coef_plot(coef_error = dgn[[id]][rep], ncomp = ncomp)
        }, res = 90)
      })
    }
  })
  observe({
    if (length(dgn) > 0) {
      ns <- session$ns
      output$ncomp <- renderUI({
        sliderInput(
          ns("coef_ncomp"), "Number of Components", value = c(1, 4),
          min = 1, max = 10, step = 1, width = '100%')
      })
      output$rep <- renderUI({
        sliderInput(ns("rep"), "Replication", min = 1, max = 50,
                    step = 1, value = c(1, 10))
      })
    }
  })
  output$coef_plot_output <- renderUI({
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
        tabPanel(title = dgn, value = dgn_txt[dgn], plotOutput(ns(plt_id)))
      })
      tabPanel(title = toupper(mdl), value = mdl, 
               fluidRow(do.call(tabBox, append(dgn_tab_box_args, dgn_tabs))))
    })
    do.call(tabsetPanel, append(mdl_tab_panel_args, mdl_tabs))
  })
}
