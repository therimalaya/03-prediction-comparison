SidebarUI <- shinydashboard::dashboardSidebar(
  width = "250px",
  sidebarMenu(
    ## Experimental Design ----
    menuItem(
      "Experimental Design",
      tabName = "experimental-design",
      icon = icon('object-group')
    ),
    ## Exploration Menu ----
    menuItem(
      "Exploration",
      startExpanded = TRUE,
      tabName = "exploration",
      icon = icon("bar-chart"),
      menuSubItem("Coefficients", tabName = "coef"),
      menuSubItem("Error", tabName = "error")
    ),
    ## Modelling Menu ----
    menuItem(
      "Modelling",
      tabName = "modelling",
      icon = icon("line-chart"),
      menuSubItem("Prediction Error", tabName = "pred-menu"),
      menuSubItem("Estimation Error", tabName = "est-menu")
    )
  )
)