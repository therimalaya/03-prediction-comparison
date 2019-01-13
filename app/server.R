server <- function(input, output, session) {
  ## Load design Dataset ----
  load('scripts/robj/design.rdata')
  
  ## Exploration Module -------------
  selected <- callModule(exp_design, 'design', design_chr)
  observeEvent(selected$design, {
    output$plot_clicked <- reactive({!is.null(selected$design)})
    outputOptions(output, "plot_clicked", suspendWhenHidden = FALSE)
    selected_design <- design %>%
      slice(selected$design_index) %>%
      get_design(selected$design)
    callModule(simrel_plots, 'design_plot', selected_design)
  })
  
  ## Coefficient Module ----------------
  dgn_coef <- callModule(coef_panel, 'coef', design, design_chr)
  output$coef_get_simrel_plot <- reactive({!is.null(dgn_coef$design_selected)})
  outputOptions(output, "coef_get_simrel_plot", suspendWhenHidden = FALSE)
  observeEvent(dgn_coef$design_selected, {
    selected_design <- design %>%
      get_design(as.numeric(dgn_coef$design_selected))
    callModule(simrel_plots, 'coef_simrel_plot', selected_design)
  })
  observeEvent(dgn_coef$request, {
    if (!any(is.null(dgn_coef$index), is.null(dgn_coef$method))) {
      mthd_names <- names(dgn_coef$method_list[dgn_coef$method_list %in% dgn_coef$method])
      names(mthd_names) <- tolower(dgn_coef$method)
      ce <- reactiveValues()
      fnames <- paste0('dgn-', c(outer(dgn_coef$index, tolower(dgn_coef$method), paste, sep = "-")))
      coef_err_list <- for (fname in fnames) {
        fpath <- file.path("scripts", "robj", "coef-error", paste0(fname, '.Rdata'))
        e <- environment()
        load(fpath, envir = e)
        ce[[fname]] <- out
      }
      callModule(coef_plot_server, 'coef-plot', ce, mthd_names)
    }
  })
  
  ## Error Module -----------
  err <- callModule(coef_panel, 'err', design, design_chr)
  output$err_get_simrel_plot <- reactive({!is.null(err$design_selected)})
  outputOptions(output, "err_get_simrel_plot", suspendWhenHidden = FALSE)
  observeEvent(err$design_selected, {
    selected_design <- design %>%
      get_design(as.numeric(err$design_selected))
    callModule(simrel_plots, 'err_simrel_plot', selected_design)
  })
  observeEvent(err$request, {
    if (!any(is.null(err$index), is.null(err$method))) {
      mthd_names <- names(err$method_list[err$method_list %in% err$method])
      names(mthd_names) <- tolower(err$method)
      ce <- reactiveValues()
      fnames <- paste0('dgn-', c(outer(err$index, tolower(err$method), paste, sep = "-")))
      for (fname in fnames) {
        fpath <- file.path("scripts", "robj", "coef-error", paste0(fname, '.Rdata'))
        e <- environment()
        load(fpath, envir = e)
        ce[[fname]] <- out
      }
      callModule(error_server, 'err-plot', ce, mthd_names)
    }
  })
  
  ## Prediction Model Module ----------
  callModule(error_data_server, 'pred', pred_min, pred_comp_min)
  pred_model_input <- callModule(model_input_server, 'pred')
  observeEvent(pred_model_input$formula, {
    pred_fit <- callModule(model_summary_server, 'pred', pred_model_input$formula, pred_min, pred_comp_min)
    observeEvent(pred_fit$anova, {
      callModule(effect_box_server, 'pred', pred_fit$fit)
      pred_summary_click <- callModule(model_summary_plot_server, 'pred', pred_fit$anova)
      observeEvent(pred_summary_click$clicked_effect, {
        callModule(model_effect_plot_server, 'pred', pred_summary_click, pred_fit$fit)
      })
    })
  })
  
  ## Estimation Model Module ----------
  callModule(error_data_server, 'est', est_min, est_comp_min)
  est_model_input <- callModule(model_input_server, 'est')
  observeEvent(est_model_input$formula, {
    est_fit <- callModule(model_summary_server, 'est', est_model_input$formula, est_min, est_comp_min)
    observeEvent(est_fit$anova, {
      callModule(effect_box_server, 'est', est_fit$fit)
      est_summary_click <- callModule(model_summary_plot_server, 'est', est_fit$anova)
      observeEvent(est_summary_click$panel_var, {
        callModule(model_effect_plot_server, 'est', est_summary_click, est_fit$fit)
      })
    })
  })
  
}