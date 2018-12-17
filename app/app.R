## ---- Load Necessary Packages ----
pkgs <- c("shiny", "shinythemes", "DT", "shinydashboard", "simrel", "tidyverse", "reshape2")
for (pkg in pkgs) require(pkg, character.only = TRUE)

## ---- Source some functions ----
source('../scripts/00-function.r')
source('experimental-design.r')


## ---- Load Necessary Datasets ----
load('../scripts/robj/design.rdata')


## ---- UI Start ----
ui <- dashboardPage(
  title = "A comparison of estimation method for multivariate linear model",
  ## Dashboard Header ----
  header = dashboardHeader(
    title = "Prediction Comparison",
    titleWidth = "250px"
  ),
  ## Dashboard Sidebar ----
  sidebar = dashboardSidebar(
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
        menuSubItem("Prediction Error", tabName = "pred_error"),
        menuSubItem("Estimation Error", tabName = "est_error")
      ),
      ## Modelling Menu ----
      menuItem(
        "Modeling",
        tabName = "modeling",
        icon = icon("line-chart")
      )
    )
  ),
  
  ## Dashboard Body ----
  body = dashboardBody(
    dashboardthemes::shinyDashboardThemes("grey_light"),
    includeCSS("custom.css"),
    tabItems(
      ## Experimental Design sub-menu Content
      tabItem(
        tabName = "experimental-design",
        fluidPage(
          theme = shinytheme('spacelab'),
          column(
            width = 6,
            box(exp_design_plot('design'), width = 12),
            box(exp_design_table('design'), width = 12)
          ),
          column(
            width = 6,
            box(simrel_plotsUI('design_plot', 1), width = 12, height = 'auto'),
            box(simrel_plotsUI('design_plot', 2), width = 12, height = 'auto'),
            box(simrel_plotsUI('design_plot', 3), width = 12, height = 'auto')
          )
        )
      ),
      ## Coefficients sub-menu Content ----
      tabItem(
        tabName = "coef",
        fluidPage(
          theme = shinytheme("spacelab"),
          ## Coefficients content navbar ----
          navbarPage(
            position = "static-top",
            inverse = FALSE,
            title = "Analysis of Regression Coefficients",
            ## Coefficients content plot menu ----
            tabPanel(
              title = "Plot",
              sidebarLayout(
                ## Coefficients content plot sidebar ----
                sidebarPanel(
                  width = 3,
                  p("Sidebar panel for inputs")
                ),
                ## Coefficients content plot main panel ----
                mainPanel(
                  p("Main Panel for inputs")
                )
              )
            ),
            ## Coefficients content summary menu ----
            tabPanel(title = "Summary"),
            ## Coefficients content More menu ----
            navbarMenu(
              title = "More",
              tabPanel(title = "Table"),
              tabPanel(
                title = "About",
                fluidRow()
              )
            )
          )
        )
      ),
      ## Prediction Error sub-menu Content ----
      tabItem(tabName = "pred_error"),
      ## Estimation Error sub-menu Content ----
      tabItem(tabName = "est_error"),
      ## Modeling Menu Content ----
      tabItem(tabName = "modeling")
    )
  )
)
## ---- Server Start ----
server <- function(input, output, session) {
  selected <- callModule(exp_design, 'design', design_chr)
  output$plot_clicked <- reactive({length(selected$design)})
  callModule(simrel_plots, 'design_plot', design, selected)
}


## ---- Run the Application ----
shinyApp(ui = ui, server = server)
