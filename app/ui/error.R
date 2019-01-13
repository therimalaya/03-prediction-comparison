ErrorUI <- tabItem(
  tabName = "error",
  fluidPage(
    theme = shinytheme("spacelab"),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        fluidRow(
          coef_design_input('err'),
          error_rep_input('err-plot'),
          coef_design_output('err'),
          method_select_input('err'),
          error_ncomp('err-plot'),
          coef_plot_trigger('err')
        ),
        br(),
        conditionalPanel(
          'output.err_get_simrel_plot',
          fluidRow(
            tabBox(
              title = "Simrel Plot",
              width = 12,
              id = "err-simrel-plot",
              tabPanel(
                title = "Plot 1",
                simrel_plotsUI('err_simrel_plot', 1, '400px')
              ),
              tabPanel(
                title = "Plot 2",
                simrel_plotsUI('err_simrel_plot', 2, '400px')
              ),
              tabPanel(
                title = "Plot 3",
                simrel_plotsUI('err_simrel_plot', 3, '400px')
              )
            )
          )
        )
      ),
      mainPanel(
        error_output('err-plot')
      )
    )
  )
)