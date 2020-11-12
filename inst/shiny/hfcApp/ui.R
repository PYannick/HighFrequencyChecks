
library(shiny)
library(shinythemes)

.APPonAttach()

#### APP side
ui <- fluidPage(theme = shinytheme("cerulean"),
                shinyjs::useShinyjs(),
                tags$style(".glyphicon-ok {color:#00FF00}"),
                tags$style(".glyphicon-remove {color:#FF0000}"),
                title = "High Frequency Checks",
                # titlePanel(title=div("High Frequency Checks", img(src="UNHCR_Logo.jpg", height="75", width="270", align = "right"))),
                titlePanel(title=div("High Frequency Checks", img(src="logos/UNHCR_Logo.jpg", height="75", width="270", align = "right"))),

                tabsetPanel(id = "tabs",
                            tabPanel("XLSForm", value = "XLSForm",
                                     sidebarLayout(
                                       sidebarPanel(
                                         h3("Step1"),
                                         h4("Import the needed files"),
                                         ## Inputs needed files
                                         fileInput("fileSurvey", "Choose a survey in a xlsform format",
                                                   multiple = FALSE,
                                                   accept = c(".xslx")),
                                         fileInput("fileDataset", "Choose the dataset",
                                                   multiple = FALSE,
                                                   accept = c(".xslx")),
                                         radioButtons("exportType", "Choose one:",
                                                      choiceNames = c("Dataset exported without groupname", "Dataset exported with groupname"),
                                                      choiceValues = list("nogroup", "group"),
                                                      selected = "nogroup"),
                                         h4("Needed for the traking sheet"),
                                         fileInput("samplesizeFile", "Samplesize information",
                                                   multiple = FALSE,
                                                   accept = c(".xslx")),
                                         h4("Needed for the GIS checks"),
                                         fileInput("adminBoundaries", "Boundaries shapefile",
                                                   multiple = TRUE,
                                                   accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
                                         fileInput("sampledPoints", "Sampled points shapefile",
                                                   multiple = TRUE,
                                                   accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
                                         width = 3),
                                       mainPanel(
                                         fluidRow(
                                           column(8, wellPanel(
                                             h3("Step2"),
                                             ## Table summary
                                             DT::dataTableOutput("table", width = "100%"))),
                                           column(4, wellPanel(
                                             h3("Step3"),
                                             ## Delete options
                                             checkboxGroupInput("actionOptions",
                                                                "Select actions to be done on the dataset:",
                                                                choiceNames = deleteOptions,
                                                                choiceValues = deleteOptions),
                                             h3("Step4"),
                                             selectInput('fnAvailable', 'List of reports which will be executed:', "", multiple=TRUE, selectize=FALSE, size=30, width='100%'),
                                             h3("Step5"),
                                             actionButton('createReport', 'Create R Markdown file', width = '100%')
                                           ))
                                         ),
                                         width = 9)
                                     )
                            ),
                            tabPanel("Help", value = "Help",
                                     mainPanel(
                                       h4("Help"),
                                       source("help.R")
                                     )
                            )
                )
)

