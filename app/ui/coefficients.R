CoefficientsUI <- tabItem(
  tabName = "coef",
  fluidPage(
    theme = shinytheme("spacelab"),
    sidebarLayout(
      ## Coefficients content plot sidebar ----
      sidebarPanel(
        width = 4,
        coef_design_input('coef'),
        coef_rep_input('coef-plot'),
        coef_design_output('coef'),
        method_select_input('coef'),
        coef_plot_ncomp('coef-plot'),
        coef_plot_trigger('coef')
      ),
      ## Coefficients content plot main panel ----
      mainPanel(
        coef_plot_output('coef-plot'),
        conditionalPanel(
          'output.coef_get_simrel_plot',
          tabsetPanel(
            id = "coef-simrel-plot",
            tabPanel(
              title = "Simrel Plot 1",
              simrel_plotsUI('coef_simrel_plot', 1, '400px')
            ),
            tabPanel(
              title = "Simrel Plot 2",
              simrel_plotsUI('coef_simrel_plot', 2, '400px')
            ),
            tabPanel(
              title = "Simrel Plot 3",
              simrel_plotsUI('coef_simrel_plot', 3, '400px')
            )
          )
        )
      )
    )
  )
)