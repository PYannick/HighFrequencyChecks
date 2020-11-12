library(magrittr)


#### Configuration part, to be put in the .RDa
# variableName <- c('ds','sampleSizeTable','adminBoundaries','sampledPoints','adminBoundariesSite','buffer','consentForValidSurvey','dateFormat',
#                   'dates','dsCoordinates','dsSite','enumeratorID','householdSize','minimumSurveyDuration','minimumSurveyDurationByIndividual',
#                   'otherPattern','questionsEnumeratorIsLazy','questionsSurveyBigValues','questionsSurveySmallValues','sampleSizeTableAvailable',
#                   'sampleSizeTableSite','sampleSizeTableTarget','startDataCollection','surveyConsent','surveyDate','uniqueID')
#
# # ,'reports'
#
# variableStatus <- rep('', length(variableName))
# variableValue <- rep('', length(variableName))
# comments <- rep('', length(variableName))
#
# variableTable <- data.frame(variableStatus=variableStatus,variableName=variableName,variableValue=variableValue,comments=comments, stringsAsFactors = F)
#
# variableModalDialog <- list(adminBoundariesSite="selectInput('variableValue', 'adminBoundariesSite', choices = colnames(session$userData$partBoundaries@data), width = '100%')",
#                             buffer="numericInput('variableValue', 'buffer', value = 10, width = '100%')",
#                             consentForValidSurvey="selectInput('variableValue', 'consentForValidSurvey', choices = session$userData$partChoices[session$userData$partChoices$list_name==session$userData$partSurvey[session$userData$partSurvey$name==session$userData$surveyConsent & !is.na(session$userData$partSurvey$name),]$type3,]$name, width = '100%')",
#                             dateFormat="textInput('variableValue', 'dateFormat', value = '%m/%d/%Y', width = '100%')",
#                             dsCoordinates=paste0("selectInput('variableLongitude', 'Longitude', choices = session$userData$partDataset[substr(session$userData$partDataset, 1, 1)=='_' | substr(session$userData$partDataset, 1, 2)=='X_'], width = '100%'),\n",
#                                                  "selectInput('variableLatitude', 'Latitude', choices = session$userData$partDataset[substr(session$userData$partDataset, 1, 1)=='_' | substr(session$userData$partDataset, 1, 2)=='X_'], width = '100%'),\n",
#                                                  "actionButton('addBtn2', 'Add', width = '100%'),\n",
#                                                  "textAreaInput('variableValue', 'Selected', value = '', width = '100%')"),
#                             dsSite="selectInput('variableValue', 'dsSite', choices = subset(session$userData$partSurvey, type2=='select_one')$name, width = '100%')",
#                             enumeratorID="selectInput('variableValue', 'enumeratorID', choices = subset(session$userData$partSurvey, type2=='integer')$name, width = '100%')",
#                             householdSize="selectInput('variableValue', 'householdSize', choices = subset(session$userData$partSurvey, type2=='integer')$name, width = '100%')",
#                             minimumSurveyDuration="numericInput('variableValue', 'minimumSurveyDuration', value = 30, width = '100%')",
#                             minimumSurveyDurationByIndividual="numericInput('variableValue', 'minimumSurveyDuration', value = 10, width = '100%')",
#                             otherPattern="textInput('variableValue', 'otherPattern', value = '_other$', width = '100%')",
#                             sampleSizeTableAvailable="selectInput('variableValue', 'sampleSizeTableAvailable', choices = session$userData$partSamplesize, width = '100%')",
#                             sampleSizeTableSite="selectInput('variableValue', 'sampleSizeTableSite', choices = session$userData$partSamplesize, width = '100%')",
#                             sampleSizeTableTarget="selectInput('variableValue', 'sampleSizeTableTarget', choices = session$userData$partSamplesize, width = '100%')",
#                             startDataCollection="dateInput('variableValue', 'startDataCollection', format = 'yyyy-mm-dd', width = '100%')",
#                             surveyConsent="selectInput('variableValue', 'surveyConsent', choices = subset(session$userData$partSurvey, type2=='select_one')$name, width = '100%')",
#                             uniqueID="selectInput('variableValue', 'uniqueID', choices = session$userData$partDataset[substr(session$userData$partDataset, 1, 1)=='_' | substr(session$userData$partDataset, 1, 2)=='X_'], width = '100%')",
#                             questionsEnumeratorIsLazy=paste0("selectInput('variableQuestions', 'Questions', choices = subset(session$userData$partSurvey, type2=='select_multiple')$name, width = '100%'),\n",
#                                                              "numericInput('variableValues', 'Value', value = 0, width = '100%'),\n",
#                                                              "actionButton('addBtn', 'Add', width = '100%'),\n",
#                                                              "textAreaInput('variableValue', 'Selected', value = '', width = '100%')"),
#                             questionsSurveyBigValues=paste0("selectInput('variableQuestions', 'Questions', choices = subset(session$userData$partSurvey, type2=='integer')$name, width = '100%'),\n",
#                                                             "numericInput('variableValues', 'Value', value = 0, width = '100%'),\n",
#                                                             "actionButton('addBtn', 'Add', width = '100%'),\n",
#                                                             "textAreaInput('variableValue', 'Selected', value = '', width = '100%')"),
#                             questionsSurveySmallValues=paste0("selectInput('variableQuestions', 'Questions', choices = subset(session$userData$partSurvey, type2=='integer')$name, width = '100%'),\n",
#                                                               "numericInput('variableValues', 'Value', value = 0, width = '100%'),\n",
#                                                               "actionButton('addBtn', 'Add', width = '100%'),\n",
#                                                               "textAreaInput('variableValue', 'Selected', value = '', width = '100%')"))
#
# deleteOptions <- c('deleteIsInterviewCompleted','deleteIsInterviewWithConsent','correctIsInterviewInTheCorrectSite','deleteIsInterviewAtTheSamplePoint',
#   'deleteIsUniqueIDMissing','deleteIsUniqueIDDuplicated','deleteIsSurveyOnMoreThanADay','deleteIsSurveyEndBeforeItStarts',
#   'deleteIsSurveyStartedBeforeTheAssessment','deleteIsSurveyMadeInTheFuture','deleteIsInterviewTooShort','deleteIsInterviewTooShortForTheHouseholdSize')
#
# save(functionsConfig, functionsGraphics, functionsOutputs, variableModalDialog, variablesDatasets, variablesNecessary, variablesOptional, variableTable, deleteOptions, file = "sysdata.rda")
#
#### Will not be necessary when included in the package ####################
# load("C:/Users/yanni/Documents/GitHub/HighFrequencyChecks/R/sysdata.rda")

# source("functions.R")

### SERVER side
updateStatus <- function(variableTable = NULL, session){
  # Is variable can be defined ?
  if(!is.null(session$userData$partSurvey)){
    variableTable[variableTable$variableName=="enumeratorID","variableStatus"] <- as.character(icon("minus", lib = "glyphicon"))
    variableTable[variableTable$variableName=="dsSite","variableStatus"] <- as.character(icon("minus", lib = "glyphicon"))
    variableTable[variableTable$variableName=="surveyConsent","variableStatus"] <- as.character(icon("minus", lib = "glyphicon"))
    variableTable[variableTable$variableName=="householdSize","variableStatus"] <- as.character(icon("minus", lib = "glyphicon"))
    variableTable[variableTable$variableName=="questionsEnumeratorIsLazy","variableStatus"] <- as.character(icon("minus", lib = "glyphicon"))
    variableTable[variableTable$variableName=="questionsSurveyBigValues","variableStatus"] <- as.character(icon("minus", lib = "glyphicon"))
    variableTable[variableTable$variableName=="questionsSurveySmallValues","variableStatus"] <- as.character(icon("minus", lib = "glyphicon"))
    variableTable[variableTable$variableName=="minimumSurveyDuration","variableStatus"] <- as.character(icon("minus", lib = "glyphicon"))
    variableTable[variableTable$variableName=="minimumSurveyDurationByIndividual","variableStatus"] <- as.character(icon("minus", lib = "glyphicon"))
    variableTable[variableTable$variableName=="otherPattern","variableStatus"] <- as.character(icon("minus", lib = "glyphicon"))
    variableTable[variableTable$variableName=="dateFormat","variableStatus"] <- as.character(icon("minus", lib = "glyphicon"))
    variableTable[variableTable$variableName=="startDataCollection","variableStatus"] <- as.character(icon("minus", lib = "glyphicon"))
  }
  if(!is.null(session$userData$partDataset)){
    variableTable[variableTable$variableName=="dsCoordinates","variableStatus"] <- as.character(icon("minus", lib = "glyphicon"))
    variableTable[variableTable$variableName=="uniqueID","variableStatus"] <- as.character(icon("minus", lib = "glyphicon"))
  }
  if(!is.null(session$userData$partSamplesize)){
    variableTable[variableTable$variableName=="sampleSizeTableAvailable","variableStatus"] <- as.character(icon("minus", lib = "glyphicon"))
    variableTable[variableTable$variableName=="sampleSizeTableSite","variableStatus"] <- as.character(icon("minus", lib = "glyphicon"))
    variableTable[variableTable$variableName=="sampleSizeTableTarget","variableStatus"] <- as.character(icon("minus", lib = "glyphicon"))
  }
  if(!is.null(session$userData$partBoundaries)){
    variableTable[variableTable$variableName=="adminBoundariesSite","variableStatus"] <- as.character(icon("minus", lib = "glyphicon"))
  }
  if(!is.null(session$userData$partSampledPoints)){
    variableTable[variableTable$variableName=="buffer","variableStatus"] <- as.character(icon("minus", lib = "glyphicon"))
  }
  if(!is.null(session$userData$surveyConsent)){
    variableTable[variableTable$variableName=="consentForValidSurvey","variableStatus"] <- as.character(icon("minus", lib = "glyphicon"))
  }
  # Is variable set?
  for(i in variableTable[,"variableName"]){
    if(!is.null(variableTable[variableTable$variableName==i,"variableValue"]) & variableTable[variableTable$variableName==i,"variableValue"]!=""){
      variableTable[variableTable$variableName==i,"variableStatus"] <- as.character(icon("ok", lib = "glyphicon"))
    }
  }
  return(variableTable)
}

server <- function(input, output, session) {

  variableTable$variableStatus <- ifelse(is.na(variableTable$variableValue) | variableTable$variableValue == "",
                                         as.character(icon("remove", lib = "glyphicon")),
                                         as.character(icon("ok", lib = "glyphicon")))

  options(DT.options = list(paging = FALSE, searching = FALSE))

  output$table <- DT::renderDataTable(DT::datatable({
    variableTable
  }, rownames = FALSE, selection = 'none', escape = FALSE) %>%
    DT::formatStyle(2, cursor = 'pointer')
  )

  ### Get files
  getSurvey <- reactive({
    inFile <- input$fileSurvey
    if (is.null(input$fileSurvey))
      return(NULL)
    worksheets <- openxlsx::getSheetNames(inFile$datapath)
    if("HFC" %in% worksheets){
      # HFC tab already exist read the information and update the table
      session$userData$partHFC <- openxlsx::read.xlsx(inFile$datapath, "HFC")
    } else {
      # nothing
      session$userData$partHFC <- NULL
    }
    session$userData$partSurvey <- openxlsx::read.xlsx(inFile$datapath, "survey")
    session$userData$partChoices <- openxlsx::read.xlsx(inFile$datapath, "choices")
    # session$userData$partSurvey <- .APPvariableGroups(session$userData$partSurvey, session$userData$partChoices)
    session$userData$partSurvey <- .APPvariableGroups(session$userData$partSurvey)[[1]]
    #print(.APPvariableGroups(session$userData$partSurvey)[[1]])
    colnames(session$userData$partSurvey) <- c("type2", "name", "type3", "fullname")
    session$userData$partSurvey[session$userData$partSurvey$type2 == "select_multiple_d","type2"] <- "select_multiple"
    # # session$userData$partSurvey$type2 <- session$userData$partSurvey$type
    # session$userData$partSurvey$type2 <- stringi::stri_split_fixed(session$userData$partSurvey$type, " ", simplify=TRUE)[,1]
    # session$userData$partSurvey$type3 <- stringi::stri_split_fixed(session$userData$partSurvey$type, " ", simplify=TRUE)[,2]
    # return(list(survey=partSurvey, choices=partChoices))
  })

  getDataset <- reactive({
    inFile <- input$fileDataset
    if (is.null(input$fileDataset))
      return(NULL)
    partDataset <- openxlsx::read.xlsx(inFile$datapath, 1)
    session$userData$partDataset <- colnames(partDataset)
    session$userData$DatasetFileName <- inFile$name
    # return(partDataset)
  })

  getSamplesize <- reactive({
    inFile <- input$samplesizeFile
    if (is.null(input$samplesizeFile))
      return(NULL)
    session$userData$partSamplesize <- openxlsx::read.xlsx(inFile$datapath, 1)
    session$userData$samplesizeFileName <- inFile$name
    # return(partSamplesize)
  })

  getBoundaries <- reactive({
    inFile <- input$adminBoundaries
    tempdirname <- dirname(inFile$datapath[1])

    for (i in 1:nrow(inFile)) {
      file.rename(
        inFile$datapath[i],
        paste0(tempdirname, "/", inFile$name[i])
      )
    }

    if (is.null(input$adminBoundaries))
      return(NULL)
    session$userData$partBoundaries <- rgdal::readOGR(paste(tempdirname,
                                                            inFile$name[grep(pattern = "*.shp$", inFile$name)],
                                                            sep = "/"))
    session$userData$boundariesFileName <- inFile$name[grep(pattern = "*.shp$", inFile$name)]
    # return(partSamplesize)
  })

  getSampledPoints <- reactive({
    inFile <- input$sampledPoints
    tempdirname <- dirname(inFile$datapath[1])

    for (i in 1:nrow(inFile)) {
      file.rename(
        inFile$datapath[i],
        paste0(tempdirname, "/", inFile$name[i])
      )
    }

    if (is.null(input$sampledPoints))
      return(NULL)
    session$userData$partSampledPoints <- rgdal::readOGR(paste(tempdirname,
                                                            inFile$name[grep(pattern = "*.shp$", inFile$name)],
                                                            sep = "/"))
    session$userData$sampledPointsFileName <- inFile$name[grep(pattern = "*.shp$", inFile$name)]
    # return(partSamplesize)
  })

  ## Inputs from the survey form
  observeEvent(input$fileSurvey, {
    getSurvey()

    if(!is.null(session$userData$partHFC)){
      partDelete <- session$userData$partHFC[data.table::`%like%`(session$userData$partHFC[,"variableName"], 'delete') | data.table::`%like%`(session$userData$partHFC[,"variableName"], 'correct'),]
      partVariableTable <- session$userData$partHFC[session$userData$partHFC$variableName %ni% partDelete$variableName,]
      variableTable[,c("variableName", "variableValue")] <<- partVariableTable[,c("variableName", "variableValue")]
      updateCheckboxGroupInput(session, "actionOptions",
                               selected = partDelete[partDelete$variableValue==TRUE, "variableName"])
    } else {
      ## Variables which could be automatically defined (should be unique in the survey)
      variableTable[variableTable$variableName=="dates","variableValue"] <<- paste0("c('", subset(session$userData$partSurvey, type2=="start")$name, "','", subset(session$userData$partSurvey, type2=="end")$name, "')")
      variableTable[variableTable$variableName=="surveyDate","variableValue"] <<- paste0("'", subset(session$userData$partSurvey, type2=="today")$name, "'")
    }

    variableTable <<- updateStatus(variableTable, session)
    output$table <- DT::renderDataTable(DT::datatable({
      variableTable
    }, rownames = FALSE, selection = 'none', escape = FALSE) %>%
      DT::formatStyle(2, cursor = 'pointer')
    )
    updateSelectInput(session, 'fnAvailable', choices = names(.APPmapFunctions(variableTable)))
  }, ignoreInit=TRUE)

  ## Inputs from thedataset
  observeEvent(input$fileDataset, {
    getDataset()

    variableTable[variableTable$variableName=="ds","variableValue"] <<- paste0("openxlsx::read.xlsx('", getwd(), "/data-raw/data/", session$userData$DatasetFileName, "')")

    variableTable <<- updateStatus(variableTable, session)
    output$table <- DT::renderDataTable(DT::datatable({
      variableTable
    }, rownames = FALSE, selection = 'none', escape = FALSE) %>%
      DT::formatStyle(2, cursor = 'pointer')
    )
    updateSelectInput(session, 'fnAvailable', choices = names(.APPmapFunctions(variableTable)))
  }, ignoreInit=TRUE)

  observeEvent(input$exportType, {
    # print(head(session$userData$partSurvey,16))
    if(input$exportType=="group"){
      ## fullname become name
      colnames(session$userData$partSurvey) <- c("type2", "shortname", "type3", "name")
    } else {
      ## name become name
      colnames(session$userData$partSurvey) <- c("type2", "name", "type3", "fullname")
    }
  }, ignoreInit=TRUE)

  ## Inputs from the Sample Size table
  observeEvent(input$samplesizeFile, {
    getSamplesize()
    session$userData$partSamplesize <- colnames(session$userData$partSamplesize)

    variableTable[variableTable$variableName=="sampleSizeTable","variableValue"] <<- paste0("openxlsx::read.xlsx('", getwd(), "/data-raw/", session$userData$samplesizeFileName, "')")

    variableTable <<- updateStatus(variableTable, session)
    output$table <- DT::renderDataTable(DT::datatable({
      variableTable
    }, rownames = FALSE, selection = 'none', escape = FALSE) %>%
      DT::formatStyle(2, cursor = 'pointer')
    )
    updateSelectInput(session, 'fnAvailable', choices = names(.APPmapFunctions(variableTable)))
  }, ignoreInit=TRUE)

  ## Inputs from the boundaries shapefile
  observeEvent(input$adminBoundaries, {
    getBoundaries()

    variableTable[variableTable$variableName=="adminBoundaries","variableValue"] <<- paste0("rgdal::readOGR('", getwd(), "/data-raw/admin/", session$userData$boundariesFileName, "')")

    variableTable <<- updateStatus(variableTable, session)
    output$table <- DT::renderDataTable(DT::datatable({
      variableTable
    }, rownames = FALSE, selection = 'none', escape = FALSE) %>%
      DT::formatStyle(2, cursor = 'pointer')
    )
    updateSelectInput(session, 'fnAvailable', choices = names(.APPmapFunctions(variableTable)))
  }, ignoreInit=TRUE)

  ## Inputs from the sampled points shapefile
  observeEvent(input$sampledPoints, {
    getSampledPoints()

    variableTable[variableTable$variableName=="sampledPoints","variableValue"] <<- paste0("rgdal::readOGR('", getwd(), "/data-raw/points/", session$userData$sampledPointsFileName, "')")

    variableTable <<- updateStatus(variableTable, session)
    output$table <- DT::renderDataTable(DT::datatable({
      variableTable
    }, rownames = FALSE, selection = 'none', escape = FALSE) %>%
      DT::formatStyle(2, cursor = 'pointer')
    )
    updateSelectInput(session, 'fnAvailable', choices = names(.APPmapFunctions(variableTable)))
  }, ignoreInit=TRUE)


  # Create modal dialog
  dataModal <- function(variableName = NULL, failed = FALSE) {
    eval(parse(text=paste0("modalDialog(\n",
      # generateModal(variableName), ",\n",
      variableModalDialog[[variableName]], ",\n",
      "footer = tagList(\n",
        "modalButton('Cancel'),\n",
        "actionButton('ok', 'OK')\n",
      ")\n",
    ")")))
  }

  observeEvent(input$addBtn, {
    updateTextAreaInput(session, "variableValue", value = ifelse(input$variableValue=="",
                                                                 paste0(input$variableQuestions, "=", input$variableValues),
                                                                 paste(input$variableValue, paste0(input$variableQuestions, "=", input$variableValues), sep = ",\n")))
  }, ignoreInit=TRUE)

  observeEvent(input$addBtn2, {
    updateTextAreaInput(session, "variableValue", value = paste0("'", input$variableLongitude, "','", input$variableLatitude, "'"))
  }, ignoreInit=TRUE)

  observeEvent(input$ok, {
    if (!is.null(input$variableValue) && nzchar(input$variableValue)) {
      if(session$userData$variableName=="surveyConsent"){
        session$userData$surveyConsent <- input$variableValue
      }
      if(session$userData$variableName=="questionsEnumeratorIsLazy" |
         session$userData$variableName=="questionsSurveyBigValues" |
         session$userData$variableName=="questionsSurveySmallValues" |
         session$userData$variableName=="dsCoordinates"){
        variableTable[variableTable$variableName==session$userData$variableName,"variableValue"] <<- paste0("c(", input$variableValue, ")")
      } else {
        if(is.numeric(input$variableValue)){
          variableTable[variableTable$variableName==session$userData$variableName,"variableValue"] <<- input$variableValue
        } else {
          variableTable[variableTable$variableName==session$userData$variableName,"variableValue"] <<- paste0("'", input$variableValue, "'")
        }
      }
      removeModal()
    }

    variableTable <<- updateStatus(variableTable, session)
    output$table <- DT::renderDataTable(DT::datatable({
      variableTable
    }, rownames = FALSE, selection = 'none', escape = FALSE) %>%
      DT::formatStyle(2, cursor = 'pointer')
    )
    updateSelectInput(session, 'fnAvailable', choices = names(.APPmapFunctions(variableTable)))
  })

  observeEvent(input$table_cell_clicked, {
    info = input$table_cell_clicked
    # do nothing if not clicked yet, or the clicked cell is not in the 2nd column, or the status is not good
    if (is.null(info$value) ||
        info$col != 1 ||
        variableTable[variableTable$variableName==info$value,"variableStatus"]=="<i class=\"glyphicon glyphicon-remove\"></i>") return()
    session$userData$variableName <- info$value

    showModal(dataModal(variableName = info$value))
  })

  observeEvent(input$createReport, {
    fileName="UnPetitNom.csv"

    variableTable <- rbind(variableTable,
                           data.frame(variableStatus="",
                                      variableName=deleteOptions,
                                      variableValue=ifelse(deleteOptions %in% input$actionOptions, TRUE, FALSE),
                                      comments="",
                                      stringsAsFactors = F))

    write.csv(variableTable[,c("variableName", "variableValue")], paste0(getwd(), "/data-raw/", fileName))

    .APPRmdWrapper(variablesList=subset(variableTable, !is.null(variableValue) & !is.na(variableValue) & variableValue!=""),
               functionsList=.APPmapFunctions(variableTable)[names(.APPmapFunctions(variableTable))],
               functionsOrder=functionsConfig[,c("functionName","ord")],
               functionsOutput=subset(functionsOutputs, outputType=="csv"),
               fileName=fileName)
  }, ignoreInit=TRUE)
}



