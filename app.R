## ---- Load Necessary Packages ----
library(shiny)
library(shinythemes)
library(DT)
library(shinydashboard)
library(simrel)
library(tidyverse)
library(reshape2)
library(dashboardthemes)
library(gridExtra)
library(shinyBS)

## ---- Source some functions ----
source('scripts/00-function.r')
source('app/script.R')

## ---- Source Module Files ----
source('app/modules/experimental-design.R')
source('app/modules/coefficients.R')
source('app/modules/error.R')
source('app/modules/model.R')
# source('app/modules/testing.R')

## ---- Source UI Files -----
source('app/ui/sidebar.R')
source('app/ui/header.R')
source('app/ui/experimental-design.R')
source('app/ui/coefficients.R')
source('app/ui/error.R')
source('app/ui/pred-model.R')
source('app/ui/est-model.R')
# source('app/ui/testing.R')

## ---- Source UI and Server File -----
source('app/ui.R')
source('app/server.R')

## ---- Run the Application ----
shinyApp(ui = ui, server = server)

