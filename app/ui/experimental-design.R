ExperimentalDesignUI <- tabItem(
  tabName = "experimental-design",
  fluidPage(
    theme = shinythemes::shinytheme('spacelab'),
    ## Coefficients content navbar ----
    navbarPage(
      position = "static-top",
      inverse = FALSE,
      title = "Analysis of Regression Coefficients",
      ## Coefficients content plot menu ----
      tabPanel(
        title = "The Design",
        column(
          width = 6,
          box(exp_design_plot('design'), width = 12),
          box(exp_design_table('design'), width = 12)
        ),
        conditionalPanel(
          'output["plot_clicked"]',
          column(
            width = 6,
            box(simrel_plotsUI('design_plot', 1), width = 12, height = 'auto'),
            box(simrel_plotsUI('design_plot', 2), width = 12, height = 'auto'),
            box(simrel_plotsUI('design_plot', 3), width = 12, height = 'auto')
          )
        )
      ),
      ## Coefficients content About menu ----
      tabPanel(title = "About")
    )
  )
)