library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(DT)
library(glue)

# source global R file
source('global.R')

# Source individual ui files
source('src/ui/sidebar_ui.R') # sidebar_ui
source('src/ui/home_ui.R') # home_ui
source('src/ui/cluster_ui.R') # cluster_ui
source('src/ui/sensitivity_ui.R') # sensitivity_ui
source('src/ui/comparison_ui.R') # comparison_ui

# Page layout
shiny::fluidPage(
    theme = shinythemes::shinytheme("cerulean"),
    shiny::titlePanel(
        shiny::fluidRow(
            shiny::column(width = 9, p("[App name]"))
        ),
        windowTitle = "[App name]"
    ),
    shiny::tags$head(
        shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        shiny::tags$style(
            shiny::HTML(".plot-box { overflow-y: scroll; height: 600px; }")
        ),
        shinyjs::useShinyjs(),  # Enable shinyjs
        # shinyjs::extendShinyjs(
        #     script = "custom.js",
        #     functions = c("myFunction")
        # )
    ),
    # sidebarLayout
    shiny::sidebarLayout(
        position = "left", 
        fluid = TRUE, 
        # load sidebar ui
        sidebar_ui, 
        # load ui for each tab in main panel
        shiny::mainPanel(shiny::tabsetPanel(
            id = "main_tabs",
            home_ui, cluster_ui, sensitivity_ui, comparison_ui
        )) 
    ), # End sidebarLayout
    shiny::hr()
) # End fluidPage

