library(shiny)

# Define custom functions


# workingDirectory <- "C:/Users/yanni/Documents/GitHub/HighFrequencyChecks/inst/demo/"
# ### Trouver un moyen pour mettre ca en base de donnees dans le package
# load("configuration.Rdata")


server <- function(input, output, session) {
  getData <- reactive({
    inFile <- input$file1
    if (is.null(input$file1))
      return(NULL)
    openxlsx::read.xlsx(inFile$datapath, 1)
  })

  output$contents <- renderTable(
    getData()
  )

  shinyjs::disable('createReport')
  shinyjs::disable('fnSelected')

  observeEvent(input$file1, {
    updateSelectInput(session, 'fnSelect', choices = names(mapFunctions(getData(), "N")))
    updateSelectInput(session, 'fnSelected', choices = "")
    updateTextInput(session, 'fileName', value = paste0(input$file1$name, "_", input$repType))
    shinyjs::disable('createReport')
    output$text <- NULL
  }, ignoreInit=TRUE)

  observeEvent(input$repType, {
    updateSelectInput(session, 'fnSelect', choices = names(mapFunctions(getData(), input$repType)))
    updateSelectInput(session, 'fnSelected', choices = "")
    updateTextInput(session, 'fileName', value = paste0(input$file1$name, "_", input$repType))
    shinyjs::disable('createReport')
    output$text <- NULL
  }, ignoreInit=TRUE)

  observeEvent(input$fnSelect, {
    updateSelectInput(session, 'fnSelected', choices = input$fnSelect, selected=input$fnSelect)
    shinyjs::enable('createReport')
    output$text <- NULL
  }, ignoreInit=TRUE)

  observeEvent(input$createReport, {
    if(nchar(input$fileName)>1){
      fileName=input$fileName
    } else {
      fileName="Report"
    }
    working_directY <- HighFrequencyChecks:::RmdWrapper(variablesList=subset(getData(), !is.null(getData()$variableValue) & !is.na(getData()$variableValue) & getData()$variableValue!=""),
                                                        functionsList=mapFunctions(getData(), input$repType)[input$fnSelected],
                                                        functionsOrder=functionsConfig[,c("functionName","ord")],
                                                        functionsOutput=functionsOutputs,
                                                        reportOutput="pdf",
                                                        reportType=input$repType,
                                                        fileName=fileName)
    output$text <- renderText({ working_directY })
  }, ignoreInit=TRUE)
}
