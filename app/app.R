## ---- Load Necessary Packages ----
pkgs <- c("shiny", "shinythemes", "DT", "shinydashboard", "simrel", "tidyverse", "reshape2")
for (pkg in pkgs) require(pkg, character.only = TRUE)

## ---- Source some functions ----
source('../scripts/00-function.r')
source('experimental-design.R')
source('coefficients.R')
source('error.R')
# source('prediction-error.R')
# source('estimation-error.R')


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
        menuSubItem("Error", tabName = "error")
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
      ## Experimental Design sub-menu Content --------
      tabItem(
        tabName = "experimental-design",
        fluidPage(
          theme = shinytheme('spacelab'),
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
      ),
      
      ## Coefficients sub-menu Content ----
      tabItem(
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
      ),
      ## Error sub-menu Content ----
      tabItem(
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
      ),
      ## Modeling Menu Content ----
      tabItem(tabName = "modeling")
    )
  )
)
## ---- Server Start ----
server <- function(input, output, session) {
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
        fpath <- file.path("..", "scripts", "robj", "coef-error", paste0(fname, '.Rdata'))
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
        fpath <- file.path("..", "scripts", "robj", "coef-error", paste0(fname, '.Rdata'))
        e <- environment()
        load(fpath, envir = e)
        ce[[fname]] <- out
      }
      callModule(error_server, 'err-plot', ce, mthd_names)
    }
  })
}


## ---- Run the Application ----
shinyApp(ui = ui, server = server)
