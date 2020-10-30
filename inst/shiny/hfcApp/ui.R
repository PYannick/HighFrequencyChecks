library(shiny)
library(shinythemes)

# Define UI for data upload app ----
ui <- fluidPage(theme = shinytheme("cerulean"),
                shinyjs::useShinyjs(),
                tags$style(".glyphicon-ok {color:#00FF00}"),
                tags$style(".glyphicon-remove {color:#FF0000}"),
                title = "High Frequency Checks",
                titlePanel(title=div("High Frequency Checks", img(src="UNHCR_Logo.jpg", height="75", width="270", align = "right"))),

                mainPanel(
                  tabsetPanel(id = "tabs",
                              tabPanel("R Markdown", value = "RMD",
                                       sidebarLayout(
                                         sidebarPanel(
                                           fileInput("file1", "Choose a xlsform",
                                                     multiple = FALSE,
                                                     accept = c(".xlsx",
                                                                ".xsl")),
                                           tableOutput("contents"),
                                           width = 6),
                                         mainPanel(
                                           radioButtons("repType", "Report type:",
                                                        choices = c(Normal = "N",
                                                                    Graphical = "G"),
                                                        selected = "N"),
                                           fluidRow(
                                             column(6, selectInput('fnSelect', 'Available reports:', "", multiple=TRUE, selectize=FALSE, size=30, width='100%')),
                                             column(6, selectInput('fnSelected', 'Selected reports:', "", multiple=TRUE, selectize=FALSE, size=30, width='100%'))),
                                           tags$br(),tags$br(),
                                           textInput('fileName', 'Please set a name for the .Rmd file:', value = "", width = '100%'),
                                           tags$br(),tags$br(),
                                           actionButton('createReport', 'Create R Markdown file', width = '100%'),
                                           tags$br(),tags$br(),
                                           textOutput("text"),
                                           width = 6)
                                       )
                              ),
                              tabPanel("Help", value = "Help")
                  ), width = 12
                )
)

