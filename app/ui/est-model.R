EstModelUI <- tabItem(
  tabName = "est-menu",
  fluidPage(
    theme = shinytheme("spacelab"),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        fluidRow(
          column(12, model_input_panel('est'))
        ),
        br(),
        fluidRow(effect_box_ui('est'))
      ),
      mainPanel(
        div(id = "est-modals", 
            error_data_ui('est', error_type = "Estimation"),
            model_input_ui('est', error_type = "Estimation"),
            model_summary_ui('est', error_type = "Estimation")
        ),
        div(id = "est-main-panel",
            model_summary_plot_ui('est'),
            model_effect_plot('est')
        )
      )
    )
  )
)