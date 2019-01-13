PredModelUI <- tabItem(
  tabName = "pred-menu",
  fluidPage(
    theme = shinytheme("spacelab"),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        fluidRow(
          column(12, model_input_panel('pred'))
        ),
        br(),
        fluidRow(effect_box_ui('pred'))
      ),
      mainPanel(
        div(id = "pred-modals", 
            error_data_ui('pred', error_type = "Prediction"),
            model_input_ui('pred', error_type = "Prediction"),
            model_summary_ui('pred', error_type = "Prediction")
        ),
        div(id = "pred-main-panel",
            model_summary_plot_ui('pred'),
            model_effect_plot('pred')
        )
      )
    )
  )
)