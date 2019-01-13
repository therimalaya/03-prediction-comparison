ui <- shinydashboard::dashboardPage(
  title = "A comparison of estimation method for multivariate linear model",
  ## Dashboard Header ----
  header = HeaderUI,
  ## Dashboard Sidebar ----
  sidebar = SidebarUI,
  
  ## Dashboard Body ----
  body = dashboardBody(
    dashboardthemes::shinyDashboardThemes("grey_light"),
    includeCSS("app/custom.css"),
    tabItems(
      ## Experimental Design sub-menu Content --------
      ExperimentalDesignUI,
      ## Coefficients sub-menu Content ----
      CoefficientsUI,
      ## Error sub-menu Content ----
      ErrorUI,
      ## Prediction Error Sub-Menu Content ----
      PredModelUI,
      ## Estimation Error Sub-Menu Content ----
      EstModelUI
    )
  )
)