'%ni%' <- function(x, table) !(match(x, table, nomatch = 0) > 0)

booleanSum <- function(x){
  result <- x[1]
  if(length(x)>1){
    for(i in 2:length(x)){
      result <- result & x[i]
    }
  }
  return(result)
}

mapFunctions <- function(variablesConfig){
  functionsOutputs <- subset(functionsOutputs, !is.null(functionsOutputs$outputType) & !is.na(functionsOutputs$outputType) & functionsOutputs$outputType!="")
  functionsGraphics <- subset(functionsGraphics, !is.null(functionsGraphics$graph) & !is.na(functionsGraphics$graph) & functionsGraphics$graph!="")
  variablesConfig <- subset(variablesConfig, !is.null(variablesConfig$variableValue) & !is.na(variablesConfig$variableValue) & variablesConfig$variableValue!="")

  allFunctions <- list()
  for(i in functionsConfig$functionName){
    # Get the needed variables for the function
    variablesList <- names(functionsConfig[functionsConfig$functionName==i,])[unlist(lapply(functionsConfig[functionsConfig$functionName==i,], isTRUE))]
    variablesListNotOptional <- variablesList[mapply('%ni%', variablesList, variablesOptional)]
    # Check the variables are available in the configuration files provided
    variablesDefined <- variablesConfig[variablesConfig$variableName %in% variablesList,]
    if(!booleanSum(variablesListNotOptional %in% variablesConfig$variableName)){
      # All the necessary variables for this function are not defined
    } else if(booleanSum(variablesListNotOptional %in% variablesConfig$variableName)){
      # All the necessary variables for this function are defined
      # Remove the Necessary variables (the ones which have to be defined in any way but are not passed directly to the function)
      variableListNotNecessary <- names(variablesNecessary[variablesNecessary$functionName==i,])[unlist(lapply(variablesNecessary[variablesNecessary$functionName==i,], isTRUE))]
      if(!identical(variableListNotNecessary, character(0))){
        variablesDefined <- variablesDefined[variablesDefined$variableName %ni% variableListNotNecessary, ]
      }
      variablesDefined[variablesDefined$variableName %in% variablesDatasets$datasets, "variableValue"] <-
        variablesDefined[variablesDefined$variableName %in% variablesDatasets$datasets, "variableName"]
      # Building the function call
      functionCode <- paste0(i, "(", paste(paste0(variablesDefined$variableName, "=", variablesDefined$variableValue), collapse=", "), ")")
      # Put the function call in a list
      allFunctions[i] <- functionCode
    }
  }

  functionsList <- list()
  for(i in functionsConfig[with(functionsConfig, order(ord)), "functionName"]){
    functionsList[[i]] <- allFunctions[[i]]
  }

  return(functionsList)
}


variableGroups <- function(surveyPart, choicesPart){
  #kobo_form(formid, user = user, api = api)
  # cat("\n Your form should be placed within the `data` folder. \n \n")
  # read the survey tab of ODK from
  # mainDir <- kobo_getMainDirectory()
  # mainDir <- "C:/Users/yanni/Documents/tmpSavHFC/TestHFC"
  #
  # form_tmp <- paste(mainDir, "data", form, sep = "/", collapse = "/")

  # mainDir <- "C:/Users/yanni/Documents/tmpSavHFC/TestHFC"
  # form <- "ISCG_HC_MSNA_Questionnaire_Final_KOBO.xlsx"
  # form_tmp <- paste(mainDir, "data", form, sep = "/", collapse = "/")
  # ### First review all questions from survey sheet #################################################
  # survey <- readxl::read_excel(form_tmp, sheet = "survey")


  ### First review all questions from survey sheet #################################################
  survey <- surveyPart
  survey <- survey[,c("type", "name")]
  survey$type <- trimws(survey$type)
  survey$name <- trimws(survey$name)

  ## need to delete empty rows from the form
  survey <- as.data.frame(survey[!is.na(survey$type), ])

  ### We can now extract the id of the list name to reconstruct the full label fo rthe question
  survey$listname <- ""

  ## Extract for select_one
  survey$listname <- with(survey, ifelse(grepl("select_one", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type) ,
                                         paste0( substr(survey$type ,
                                                        (regexpr("select_one", survey$type , ignore.case = FALSE, fixed = TRUE)) + 10, 250)),
                                         survey$listname))

  survey$type <- with(survey, ifelse(grepl("select_one", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type), paste0("select_one"),
                                     survey$type))

  ## Extract for select multiple & clean type field
  survey$listname <- with(survey,  ifelse(grepl("select_multiple", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type),
                                          paste0( substr(survey$type ,
                                                         (regexpr("select_multiple", survey$type , ignore.case = FALSE, fixed = TRUE)) + 16, 250)),
                                          survey$listname ))


  survey$type <- with(survey, ifelse(grepl("select_multiple", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type), paste0("select_multiple_d"),survey$type))

  ## handle case where we have "or_other"
  survey$listname <- with(survey, ifelse(grepl("or_other", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$listname) ,
                                         paste0( substr(survey$listname , 1, (nchar(survey$listname) - 8 ))),
                                         survey$listname))

  ## Remove trailing space
  survey$listname <- trimws(survey$listname)

  ## Now creating full name in order to match with data variables name

  ### identify Repeat questions with nest levels
  survey$qrepeat <- ""
  for (i in 2:nrow(survey))
  {
    #Check based on repeat type
    if (survey[ i, c("type")] %in% c("begin repeat","begin_repeat") && survey[ i - 1, c("qrepeat")] == "")                  {survey[ i, c("qrepeat")]  <- "repeatnest1"}
    else if (survey[ i, c("type")] %in% c("begin repeat","begin_repeat") && survey[ i - 1, c("qrepeat")] == "repeatnest1")       {survey[ i, c("qrepeat")]  <-  "repeatnest2"}
    else if (!(survey[ i, c("type")] %in% c("end repeat","end_repeat"))  && survey[ i - 1, c("qrepeat")] == "repeatnest1")       {survey[ i, c("qrepeat")]  <-  "repeatnest1"}
    else if (!(survey[ i, c("type")] %in% c("end repeat","end_repeat"))  && survey[ i - 1, c("qrepeat")] == "repeatnest2")       {survey[ i, c("qrepeat")]  <-  "repeatnest2"}
    else if (survey[ i, c("type")] %in% c("end repeat","end_repeat")     && survey[ i - 1, c("qrepeat")] == "repeatnest1" )      {survey[ i, c("qrepeat")]  <-  ""}
    else if (survey[ i, c("type")] %in% c("end repeat","end_repeat")     && survey[ i - 1, c("qrepeat")] == "repeatnest2" )      {survey[ i, c("qrepeat")]  <-  "repeatnest1"}

    else   {survey[ i, c("qrepeat")]  <-  ""}
  }

  ### identify Repeat questions

  survey$qrepeatlabel <- "MainDataFrame"

  nestable <- survey[survey$type %in% c("begin_repeat","begin repeat") , c("name","qrepeat","type")]
  nestable$name <- as.character(nestable$name)
  for (i in 2:nrow(survey)){
    # Now insert the repeat label based on name
    if ( survey[ i, c("type")] == "begin repeat" )                                                {survey[ i, c("qrepeatlabel")]  <- survey[ i, c("name")]}
    else if ( survey[ i, c("type")] != "end repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest1" )   {survey[ i, c("qrepeatlabel")]  <- survey[ i - 1, c("qrepeatlabel")] }
    else if ( survey[ i, c("type")] != "end repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest2" )   {survey[ i, c("qrepeatlabel")]  <- survey[ i - 1, c("qrepeatlabel")] }

    else if ( survey[ i, c("type")] == "end repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest1")    {survey[ i, c("qrepeatlabel")]  <-  "MainDataFrame"}

    else if ( survey[ i, c("type")] == "end repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest2")    { nestabove <- as.character(survey[ i - 1, c("qrepeatlabel")])
    nestabovenum <- as.integer(which(nestable$name == nestabove ) - 1)
    survey[ i, c("qrepeatlabel")]  <-  as.character( nestable[ nestabovenum , 1] ) }

    ## Sometimes it seems that we get an underscore for type
    else if ( survey[ i, c("type")] == "begin_repeat" )                                                {survey[ i, c("qrepeatlabel")]  <- survey[ i, c("name")]}
    else if ( survey[ i, c("type")] != "end_repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest1" )   {survey[ i, c("qrepeatlabel")]  <- survey[ i - 1, c("qrepeatlabel")] }
    else if ( survey[ i, c("type")] != "end_repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest2" )   {survey[ i, c("qrepeatlabel")]  <- survey[ i - 1, c("qrepeatlabel")] }

    else if ( survey[ i, c("type")] == "end_repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest1")    {survey[ i, c("qrepeatlabel")]  <-  "MainDataFrame"}

    else if ( survey[ i, c("type")] == "end_repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest2")    { nestabove <- as.character(survey[ i - 1, c("qrepeatlabel")])
    nestabovenum <- as.integer(which(nestable$name == nestabove ) - 1)
    survey[ i, c("qrepeatlabel")]  <-  as.character( nestable[ nestabovenum , 1] ) }

    else   {survey[ i, c("qrepeatlabel")]  <-  "MainDataFrame"}
  }

  ### Get question levels in order to match the variable name
  survey$qlevel <- ""
  for (i in 2:nrow(survey)){
    if (survey[ i, c("type")] == "begin group" && survey[ i - 1, c("qlevel")] == "" )      {survey[ i, c("qlevel")]  <-  "level1"}
    else if (survey[ i, c("type")] == "begin_group" && survey[ i - 1, c("qlevel")] == "" )      {survey[ i, c("qlevel")]  <-  "level1"}

    else if (survey[ i, c("type")] == "begin group" && survey[ i - 1, c("qlevel")] == "level1") {survey[ i, c("qlevel")]  <-  "level2"}
    else if (survey[ i, c("type")] == "begin_group" && survey[ i - 1, c("qlevel")] == "level1") {survey[ i, c("qlevel")]  <-  "level2"}

    else if (survey[ i, c("type")] == "begin group" && survey[ i - 1, c("qlevel")] == "level2") {survey[ i, c("qlevel")]  <-  "level3"}
    else if (survey[ i, c("type")] == "begin_group" && survey[ i - 1, c("qlevel")] == "level2") {survey[ i, c("qlevel")]  <-  "level3"}

    else if (survey[ i, c("type")] == "begin group" && survey[ i - 1, c("qlevel")] == "level3") {survey[ i, c("qlevel")]  <-  "level4"}
    else if (survey[ i, c("type")] == "begin_group" && survey[ i - 1, c("qlevel")] == "level3") {survey[ i, c("qlevel")]  <-  "level4"}

    else if (survey[ i, c("type")] == "begin group" && survey[ i - 1, c("qlevel")] == "level4") {survey[ i, c("qlevel")]  <-  "level5"}
    else if (survey[ i, c("type")] == "begin_group" && survey[ i - 1, c("qlevel")] == "level4") {survey[ i, c("qlevel")]  <-  "level5"}

    ## Now end of group
    else if (survey[ i, c("type")] == "end group" && survey[ i - 1, c("qlevel")] == "level1") {survey[ i, c("qlevel")] <- "" }
    else if (survey[ i, c("type")] == "end_group" && survey[ i - 1, c("qlevel")] == "level1") {survey[ i, c("qlevel")] <- "" }

    else if (survey[ i, c("type")] == "end group" && survey[ i - 1, c("qlevel")] == "level2") {survey[ i, c("qlevel")]  <-  "level1"}
    else if (survey[ i, c("type")] == "end_group" && survey[ i - 1, c("qlevel")] == "level2") {survey[ i, c("qlevel")]  <-  "level1"}

    else if (survey[ i, c("type")] == "end group" && survey[ i - 1, c("qlevel")] == "level3") {survey[ i, c("qlevel")]  <-  "level2"}
    else if (survey[ i, c("type")] == "end_group" && survey[ i - 1, c("qlevel")] == "level3") {survey[ i, c("qlevel")]  <-  "level2"}

    else if (survey[ i, c("type")] == "end group" && survey[ i - 1, c("qlevel")] == "level4") {survey[ i, c("qlevel")]  <-  "level3"}
    else if (survey[ i, c("type")] == "end_group" && survey[ i - 1, c("qlevel")] == "level4") {survey[ i, c("qlevel")]  <-  "level3"}

    else if (survey[ i, c("type")] == "end group" && survey[ i - 1, c("qlevel")] == "level5") {survey[ i, c("qlevel")]  <-  "level4"}
    else if (survey[ i, c("type")] == "end_group" && survey[ i - 1, c("qlevel")] == "level5") {survey[ i, c("qlevel")]  <-  "level4"}

    else   {survey[ i, c("qlevel")]  <-  survey[ i - 1, c("qlevel")]}
  }

  ### Get question groups in order to match the variable name
  ## Concatenation ofqlevel & qrepeat & type
  survey$type2 <- survey$type
  ## We need to handle situation with both repeat & group
  ## So 12 cases to handle

  survey$qgroup <- ""
  for (i in 2:nrow(survey)){
    if (survey[ i, c("qlevel")]  %in% c("level1","level2","level3","level4","level5") &&
        survey[ i, c("qrepeat")] %in% c("", "repeatnest1", "repeatnest2") &&
        !(survey[ i, c("type")]   %in% c("begin_group","begin group","end_group","end group","begin_repeat","begin repeat","end_repeat","end repeat")) )

    {survey[ i, c("qgroup")] <- survey[ i - 1, c("qgroup")]

    } else if (survey[ i, c("qlevel")]   %in% c("level1") &&
               survey[ i, c("qrepeat")]  %in% c("", "repeatnest1", "repeatnest2") &&
               survey[ i, c("type")]     %in% c("begin_group","begin group")  )

    {survey[ i, c("qgroup")] <- survey[ i, c("name")]

    } else if (survey[ i, c("qlevel")]   %in% c("level2","level3","level4","level5") &&
               survey[ i, c("qrepeat")]  %in% c("", "repeatnest1", "repeatnest2") &&
               survey[ i, c("type")]     %in% c("begin_group","begin group") )

    {survey[ i, c("qgroup")] <- paste(survey[ i - 1, c("qgroup")], survey[ i, c("name")],sep = ".")

    } else if (survey[ i, c("qlevel")]   %in% c("level1","level2","level3","level4","level5")  &&
               survey[ i, c("qrepeat")]  %in% c("repeatnest1", "repeatnest2") &&
               survey[ i, c("type")]     %in% c("begin_repeat","begin repeat")   )

    {survey[ i, c("qgroup")] <- paste(survey[ i - 1, c("qgroup")], survey[ i, c("qrepeatlabel")], sep = ".")

    } else if (survey[ i, c("qlevel")]   %in% c("level1","level2","level3","level4","level5") &&
               survey[ i, c("qrepeat")]  %in% c("", "repeatnest1", "repeatnest2") &&
               survey[ i, c("type")]     %in% c("end_group","end group","end_repeat","end repeat") )

    {survey[ i, c("qgroup")] <- substr(survey[ i - 1, c("qgroup")] ,0, regexpr("\\.[^\\.]*$", survey[ i - 1, c("qgroup")] ) - 1)

    } else  {survey[ i, c("qgroup")]  <- ""}
  }

  survey$fullname <- ""
  ## Need to loop around the data frame in order to concatenate full name as observed in data dump
  survey[ 1, c("fullname")]  <-  survey[ 1, c("name")]
  for (i in 2:nrow(survey)){
    if (survey[ i, c("qlevel")] == "") {survey[ i, c("fullname")]  <-  survey[ i, c("name")]}
    else {survey[ i, c("fullname")]  <-  paste(survey[ i, c("qgroup")],survey[ i, c("name")],sep = ".") }
  }

  dico <- survey[,c("type", "name", "fullname", "listname")]

  ## Remove trailing space
  dico$fullname <- trimws(dico$fullname)
  dico$listname <- trimws(dico$listname)

  ## A few fix on the dico
  dico <- dico[ !is.na(dico$name), ]
  dico <- dico[ !is.na(dico$type), ]
  colnames(dico) <- c("type2", "name", "fullname", "type3")

  return(dico)
}

RmdWrapper <- function(variablesList=NULL,
                       functionsList=NULL,
                       functionsOrder=NULL,
                       functionsOutput=NULL,
                       fileName=NULL){

  working_directY <- getwd()
  vignette_directory <- paste0("/", fileName, ".Rmd")

  reportRMD  <- paste0(working_directY,vignette_directory)
  ## TO DO : CHECK IF FILE EXIST - AND REQUEST USER TO DELETE BEFORE REGENERATING - SUGGESTING TO SAVE PREVIOUS UNDER NEW NAME
  if (file.exists(reportRMD)) file.remove(reportRMD)

  ## Start Building the report ##########
  cat("---", file = reportRMD , sep = "\n", append = TRUE)
  cat("title: \"High Frequency Checks - Generated from the Shiny app\"", file = reportRMD , sep = "\n", append = TRUE)
  cat("date: \"`r format(Sys.time(), '%d %B, %Y')`\"", file = reportRMD , sep = "\n", append = TRUE)
  cat("always_allow_html: yes", file = reportRMD , sep = "\n", append = TRUE)
  cat("output:",file = reportRMD , sep = "\n", append = TRUE)
  cat("  html_document:", file = reportRMD , sep = "\n", append = TRUE)
  cat("    toc: true", file = reportRMD , sep = "\n", append = TRUE)
  cat("---", file = reportRMD , sep = "\n", append = TRUE)

  cat("\n", file = reportRMD , sep = "\n", append = TRUE)
  cat("```{r setup, include=FALSE}", file = reportRMD , sep = "\n", append = TRUE)
  cat("knitr::opts_chunk$set(echo = TRUE)", file = reportRMD , sep = "\n", append = TRUE)

  cat("library(knitr)", file = reportRMD , sep = "\n", append = TRUE)
  cat("library(gsubfn)", file = reportRMD , sep = "\n", append = TRUE)
  cat("library(dplyr)", file = reportRMD , sep = "\n", append = TRUE)
  cat("library(data.table)", file = reportRMD , sep = "\n", append = TRUE)
  cat("library(HighFrequencyChecks)", file = reportRMD , sep = "\n", append = TRUE)
  cat("options(scipen = 999)", file = reportRMD , sep = "\n", append = TRUE)
  cat("```", file = reportRMD , sep = "\n", append = TRUE)

  cat("```{r surveysDataset, eval=TRUE, echo=FALSE}", file = reportRMD , sep = "\n", append = TRUE)
  for(i in 1:length(variablesList[,1])){
    # cat(paste0(repstr[i,1], "<-", repstr[i,2]), file = reportRMD , sep = "\n", append = TRUE)
    cat(paste0(variablesList[i,"variableName"], "<-", variablesList[i,"variableValue"]), file = reportRMD , sep = "\n", append = TRUE)
  }
  cat("```", file = reportRMD , sep = "\n", append = TRUE)

  ##### A supprimer quand les gens arretterons de faire nimp
  cat("```{r, eval=TRUE, echo=FALSE}", file = reportRMD , sep = "\n", append = TRUE)
  cat("ds$union_name<-tolower(ds$union_name)", file = reportRMD , sep = "\n", append = TRUE)
  cat("sampleSizeTable$Union<-tolower(sampleSizeTable$Union)", file = reportRMD , sep = "\n", append = TRUE)
  cat("adminBoundaries$Union<-tolower(adminBoundaries$Union)", file = reportRMD , sep = "\n", append = TRUE)
  cat("```", file = reportRMD , sep = "\n", append = TRUE)
  #####

  cat("```{r runAllFunctions, eval=TRUE, echo=FALSE}", file = reportRMD , sep = "\n", append = TRUE)
  for(i in functionsOrder[with(functionsOrder, order(ord)), "functionName"][functionsOrder[with(functionsOrder, order(ord)), "functionName"] %in% names(functionsList)]){
    cat(paste0("list[var1,report", i, ",text", i, ",graph", i, "]<-", functionsList[[i]]), file = reportRMD , sep = "\n", append = TRUE)
    cat("if(!is.null(var1)){", file = reportRMD , sep = "\n", append = TRUE)
    cat("  ds<-var1\n", file = reportRMD , sep = "", append = TRUE)
    cat("}", file = reportRMD , sep = "\n", append = TRUE)
  }

  cat("reporti <- ls(all.names = T)[data.table::`%like%`(ls(all.names = T), 'reporti')]", file = reportRMD , sep = "\n", append = TRUE)
  cat("if(!identical(reporti, character(0))){", file = reportRMD , sep = "\n", append = TRUE)
  cat("list[var1,reportenumeratorErrorsDashboard,textenumeratorErrorsDashboard,graphenumeratorErrorsDashboard] <- enumeratorErrorsDashboard(enumeratorID='enumerator_id', reports=reporti)", file = reportRMD , sep = "\n", append = TRUE)
  cat("}", file = reportRMD , sep = "\n", append = TRUE)
  cat("```", file = reportRMD , sep = "\n", append = TRUE)

  cat(paste0("\n## Summary of defined parameters"), file = reportRMD , sep = "\n", append = TRUE)

  cat("```{r summaryParameters, eval=TRUE, echo=FALSE, results='asis'}", file = reportRMD , sep = "\n", append = TRUE)
  cat("if(!is.null(buffer) | !is.na(buffer) | buffer!=''){", file = reportRMD , sep = "\n", append = TRUE)
  cat("  cat(paste0('The buffer for the points to be valid is set to ***', buffer, ' meters*** from the sampled point  \n'))", file = reportRMD , sep = "\n", append = TRUE)
  cat("}", file = reportRMD , sep = "\n", append = TRUE)
  cat("if(!is.null(minimumSurveyDuration) | !is.na(minimumSurveyDuration) | minimumSurveyDuration!=''){", file = reportRMD , sep = "\n", append = TRUE)
  cat("  cat(paste0('The minimum duration for a survey to be valid is set to ***', minimumSurveyDuration, ' minutes***  \n'))", file = reportRMD , sep = "\n", append = TRUE)
  cat("}", file = reportRMD , sep = "\n", append = TRUE)
  cat("if(!is.null(minimumSurveyDurationByIndividual) | !is.na(minimumSurveyDurationByIndividual) | minimumSurveyDurationByIndividual!=''){", file = reportRMD , sep = "\n", append = TRUE)
  cat("  cat(paste0('The minimum duration taken into account the household size (duration per individual) for a survey to be valid is set to ***', minimumSurveyDurationByIndividual, ' minutes***  \n'))", file = reportRMD , sep = "\n", append = TRUE)
  cat("}", file = reportRMD , sep = "\n", append = TRUE)
  cat("df <- merge(data.frame(questionsSurveySmallValues), data.frame(questionsSurveyBigValues), by=0, all=TRUE)", file = reportRMD , sep = "\n", append = TRUE)
  cat("colnames(df) <- c('Questions', 'Lower bound', 'Upper bound')", file = reportRMD , sep = "\n", append = TRUE)
  cat("kable(df, ", file = reportRMD , sep = "\n", append = TRUE)
  cat("      caption = 'Questions with values to be checked for', ", file = reportRMD , sep = "\n", append = TRUE)
  cat("      format = 'html') %>%", file = reportRMD , sep = "\n", append = TRUE)
  cat("  kableExtra::kable_styling(full_width=T)", file = reportRMD , sep = "\n", append = TRUE)
  cat("kable(data.frame(minimumAnswers=questionsEnumeratorIsLazy), ", file = reportRMD , sep = "\n", append = TRUE)
  cat("      caption = 'Questions with an expected minimum number of answers', ", file = reportRMD , sep = "\n", append = TRUE)
  cat("      format = 'html') %>%", file = reportRMD , sep = "\n", append = TRUE)
  cat("  kableExtra::kable_styling(full_width=T)", file = reportRMD , sep = "\n", append = TRUE)
  cat("```", file = reportRMD , sep = "\n", append = TRUE)

  cat(paste0("\n## Overall duration of the assessment till now"), file = reportRMD , sep = "\n", append = TRUE)

  cat("```{r textassessmentDuration, eval=TRUE, echo=FALSE, results='asis', fig.align='center', fig.width=10, fig.height=8}", file = reportRMD , sep = "\n", append = TRUE)
  cat("if(!is.null(textassessmentDuration)){", file = reportRMD , sep = "\n", append = TRUE)
  cat("  cat(textassessmentDuration)", file = reportRMD , sep = "\n", append = TRUE)
  cat("}", file = reportRMD , sep = "\n", append = TRUE)
  cat("```", file = reportRMD , sep = "\n", append = TRUE)

  cat(paste0("\n## Reports"), file = reportRMD , sep = "\n", append = TRUE)

  cat("```{r exportResultsInCSV, eval=TRUE, echo=FALSE, results='asis'}", file = reportRMD , sep = "\n", append = TRUE)
  cat("listReports <- data.frame(Reports=character(), stringsAsFactors = FALSE)", file = reportRMD , sep = "\n", append = TRUE)
  cat("for(i in ls(all.names = T)[ls(all.names = T) %like% 'report']){", file = reportRMD , sep = "\n", append = TRUE)
  cat("  if(i=='reports'){", file = reportRMD , sep = "\n", append = TRUE)
  cat("  } else{", file = reportRMD , sep = "\n", append = TRUE)
  cat("    write.csv(get(i), paste0(i, '.csv'))", file = reportRMD , sep = "\n", append = TRUE)
  cat("    listReports <- rbind(listReports, data.frame(Reports=i), stringsAsFactors = FALSE)", file = reportRMD , sep = "\n", append = TRUE)
  cat("  }", file = reportRMD , sep = "\n", append = TRUE)
  cat("}", file = reportRMD , sep = "\n", append = TRUE)
  cat("if(length(listReports[,1]) %% 2 !=0){", file = reportRMD , sep = "\n", append = TRUE)
  cat("  listReports <- rbind(listReports, data.frame(Reports=''), stringsAsFactors = FALSE)", file = reportRMD , sep = "\n", append = TRUE)
  cat("}", file = reportRMD , sep = "\n", append = TRUE)
  cat("cat('Most of the analysis selected outputed a detailed report which could be used for further analysis or to prepare the cleaning log')", file = reportRMD , sep = "\n", append = TRUE)
  cat("kable(data.frame(listReports[1:(length(listReports[,1])/2),],", file = reportRMD , sep = "\n", append = TRUE)
  cat("                 listReports[(1+length(listReports[,1])/2):length(listReports[,1]),]), ", file = reportRMD , sep = "\n", append = TRUE)
  cat("      col.names = NULL, ", file = reportRMD , sep = "\n", append = TRUE)
  cat("      caption = 'Reports exported in .csv', ", file = reportRMD , sep = "\n", append = TRUE)
  cat("      format = 'html') %>%", file = reportRMD , sep = "\n", append = TRUE)
  cat("  kableExtra::kable_styling(full_width=T)", file = reportRMD , sep = "\n", append = TRUE)
  cat("```", file = reportRMD , sep = "\n", append = TRUE)

  cat(paste0("\n## Overview of errors which should lead to a survey deletion"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("\n### Programming Checks"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("\nThese errors are most likely linked to some issues with the phones/ tablets used for the data collection, server configuration or connectivity issues."), file = reportRMD , sep = "\n", append = TRUE)

  cat("```{r graphiProgramming, eval=TRUE, echo=FALSE, results='asis', fig.align='center', fig.width=7, fig.height=5}", file = reportRMD , sep = "\n", append = TRUE)
  cat("colors <- c('OK' = '#00cc00', 'NOK' = '#cc0000')", file = reportRMD , sep = "\n", append = TRUE)
  cat("graphsi <- c('graphisInterviewWithConsent',", file = reportRMD , sep = "\n", append = TRUE)
  cat("             'graphisUniqueIDMissing',", file = reportRMD , sep = "\n", append = TRUE)
  cat("             'graphisUniqueIDDuplicated',", file = reportRMD , sep = "\n", append = TRUE)
  cat("             'graphisSurveyEndBeforeItStarts',", file = reportRMD , sep = "\n", append = TRUE)
  cat("             'graphisSurveyStartedBeforeTheAssessment',", file = reportRMD , sep = "\n", append = TRUE)
  cat("             'graphisSurveyMadeInTheFuture')", file = reportRMD , sep = "\n", append = TRUE)
  cat("for(i in ls(all.names = T)[ls(all.names = T) %in% graphsi]){", file = reportRMD , sep = "\n", append = TRUE)
  cat("  eval(parse(text=paste0(i, ' <- ', i, ' + ggplot2::theme(plot.title=ggplot2::element_text(size=10), plot.subtitle=ggplot2::element_text(size=8, colour = \"red\")) + ggplot2::scale_fill_manual(values = colors)')))", file = reportRMD , sep = "\n", append = TRUE)
  cat("}", file = reportRMD , sep = "\n", append = TRUE)
  cat("gridExtra::grid.arrange(grobs=mget(ls(all.names = T)[ls(all.names = T) %in% graphsi]), ncol = 3)", file = reportRMD , sep = "\n", append = TRUE)
  cat("```", file = reportRMD , sep = "\n", append = TRUE)

  cat(paste0("\n### Enumerators Checks"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("\nThese errors are most likely linked to some lack of technical training of the enumerators, like proper use of a GPS, being certain the survey is ended in the tool used for the data collection before moving to the next one. Or to some bad behaviours for the surveys marked as too short."), file = reportRMD , sep = "\n", append = TRUE)

  cat("```{r graphiEnumerators, eval=TRUE, echo=FALSE, results='asis', fig.align='center', fig.width=7, fig.height=5}", file = reportRMD , sep = "\n", append = TRUE)
  cat("colors <- c('OK' = '#00cc00', 'NOK' = '#cc0000')", file = reportRMD , sep = "\n", append = TRUE)
  cat("graphsi <- c('graphisInterviewInTheCorrectSite',", file = reportRMD , sep = "\n", append = TRUE)
  cat("             'graphisInterviewAtTheSamplePoint',", file = reportRMD , sep = "\n", append = TRUE)
  cat("             'graphisSurveyOnMoreThanADay',", file = reportRMD , sep = "\n", append = TRUE)
  cat("             'graphisInterviewCompleted',", file = reportRMD , sep = "\n", append = TRUE)
  cat("             'graphisInterviewTooShort',", file = reportRMD , sep = "\n", append = TRUE)
  cat("             'graphisInterviewTooShortForTheHouseholdSize')", file = reportRMD , sep = "\n", append = TRUE)
  cat("for(i in ls(all.names = T)[ls(all.names = T) %in% graphsi]){", file = reportRMD , sep = "\n", append = TRUE)
  cat("  eval(parse(text=paste0(i, ' <- ', i, ' + ggplot2::theme(plot.title=ggplot2::element_text(size=10), plot.subtitle=ggplot2::element_text(size=8, colour = \"red\")) + ggplot2::scale_fill_manual(values = colors)')))", file = reportRMD , sep = "\n", append = TRUE)
  cat("}", file = reportRMD , sep = "\n", append = TRUE)
  cat("gridExtra::grid.arrange(grobs=mget(ls(all.names = T)[ls(all.names = T) %in% graphsi]), ncol = 3)", file = reportRMD , sep = "\n", append = TRUE)
  cat("```", file = reportRMD , sep = "\n", append = TRUE)

  cat(paste0("\n## Assessment follow-up"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("\n### Productivity"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("\nFollow-up on the daily productivity."), file = reportRMD , sep = "\n", append = TRUE)

  cat("```{r graphassessmentProductivity, eval=TRUE, echo=FALSE, results='asis', fig.align='center', fig.width=7, fig.height=4}", file = reportRMD , sep = "\n", append = TRUE)
  cat("print(graphassessmentProductivity)", file = reportRMD , sep = "\n", append = TRUE)
  cat("```", file = reportRMD , sep = "\n", append = TRUE)

  cat(paste0("\n### Daily surveys by status"), file = reportRMD , sep = "\n", append = TRUE)

  cat(paste0("\nFollow-up on the daily productivity taking into account the surveys status to get a closer look on the ones which would be usable at the end."), file = reportRMD , sep = "\n", append = TRUE)

  cat("```{r graphassessmentDailyValidSurveys, eval=TRUE, echo=FALSE, results='asis', fig.align='center', fig.width=7, fig.height=5}", file = reportRMD , sep = "\n", append = TRUE)
  cat("colors <- eval(parse(text=paste0('c(', paste0(unique(ds$survey_consent), '=\\\'', colormap::colormap(colormap=c('#fff5f0','#67000d'), nshades=length(unique(ds$survey_consent))), '\\\'', collapse = ','), ')')))", file = reportRMD , sep = "\n", append = TRUE)
  cat("colors['yes'] <- '#00cc00'", file = reportRMD , sep = "\n", append = TRUE)
  cat("print(graphassessmentDailyValidSurveys + ", file = reportRMD , sep = "\n", append = TRUE)
  cat("        ggplot2::theme(legend.position = 'bottom') + ", file = reportRMD , sep = "\n", append = TRUE)
  cat("        ggplot2::guides(fill=ggplot2::guide_legend(nrow=ceiling(length(unique(ds$survey_consent))/2), byrow=TRUE)) + ", file = reportRMD , sep = "\n", append = TRUE)
  cat("        ggplot2::scale_fill_manual(values = colors))", file = reportRMD , sep = "\n", append = TRUE)
  cat("```", file = reportRMD , sep = "\n", append = TRUE)

  cat(paste0("\n### Surveys duration outliers"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("\nThe surveys duration distribution could be usefull to revise the minimum expected duration of one survey."), file = reportRMD , sep = "\n", append = TRUE)

  cat("```{r graphassessmentDurationOutliers, eval=TRUE, echo=FALSE, results='asis', fig.align='center', fig.width=7, fig.height=1}", file = reportRMD , sep = "\n", append = TRUE)
  cat("print(graphassessmentDurationOutliers)", file = reportRMD , sep = "\n", append = TRUE)
  cat("```", file = reportRMD , sep = "\n", append = TRUE)

  cat(paste0("\n### Tracking Sheet"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("\nThe tracking sheet is a powerfull tool to monitor the progress of the assessment and to warn about the potential shortage of sampled points available in some areas."), file = reportRMD , sep = "\n", append = TRUE)

  cat("```{r graphassessmentTrackingSheet, eval=TRUE, echo=FALSE, results='asis', fig.align='center', fig.width=7, fig.height=4}", file = reportRMD , sep = "\n", append = TRUE)
  cat("print(graphassessmentTrackingSheet + ggplot2::theme(legend.position = 'bottom'))", file = reportRMD , sep = "\n", append = TRUE)
  cat("if(!is.null(textassessmentTrackingSheet)){", file = reportRMD , sep = "\n", append = TRUE)
  cat("  cat(textassessmentTrackingSheet)", file = reportRMD , sep = "\n", append = TRUE)
  cat("}", file = reportRMD , sep = "\n", append = TRUE)
  cat("```", file = reportRMD , sep = "\n", append = TRUE)

  cat(paste0("\n## Enumerators follow-up"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("\n### Productivity"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("\nBasic average number of surveys made daily by each enumerators (based on the number of days the enumerators worked)."), file = reportRMD , sep = "\n", append = TRUE)

  cat("```{r graphenumeratorProductivity, eval=TRUE, echo=FALSE, results='asis', fig.align='center', fig.width=7, fig.height=10}", file = reportRMD , sep = "\n", append = TRUE)
  cat("print(graphenumeratorProductivity)", file = reportRMD , sep = "\n", append = TRUE)
  cat("```", file = reportRMD , sep = "\n", append = TRUE)

  cat(paste0("\n### Productivity outliers"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("\nThe productivity distribution, in combination with the ***Productivity***, could be usefull to identify enumerators who are particularly performent or on the other hand not enough. Keeping in mind an enumerator who over performed could be an enumerator who is cheating. A further analysis crossed with the ***Percentage of valid surveys***, the ***Survey duration*** distribution and the time spend per question could help to identify the way the duration distribution has to be interpreted."), file = reportRMD , sep = "\n", append = TRUE)

  cat("```{r graphenumeratorProductivityOutliers, eval=TRUE, echo=FALSE, results='asis', fig.align='center', fig.width=7, fig.height=1}", file = reportRMD , sep = "\n", append = TRUE)
  cat("print(graphenumeratorProductivityOutliers)", file = reportRMD , sep = "\n", append = TRUE)
  cat("```", file = reportRMD , sep = "\n", append = TRUE)

  cat(paste0("\n### Percentage of valid surveys"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("\nWithin all the surveys made by each enumerator, what is the percentage of them which could be used for the assessment analysis."), file = reportRMD , sep = "\n", append = TRUE)

  cat("```{r graphenumeratorSurveysConsent, eval=TRUE, echo=FALSE, results='asis', fig.align='center', fig.width=7, fig.height=10}", file = reportRMD , sep = "\n", append = TRUE)
  cat("colors <- eval(parse(text=paste0('c(', paste0(unique(ds$survey_consent), '=\\\'', colormap::colormap(colormap=c('#fff5f0','#67000d'), nshades=length(unique(ds$survey_consent))), '\\\'', collapse = ','), ')')))", file = reportRMD , sep = "\n", append = TRUE)
  cat("colors['yes'] <- '#00cc00'", file = reportRMD , sep = "\n", append = TRUE)
  cat("print(graphenumeratorSurveysConsent + ", file = reportRMD , sep = "\n", append = TRUE)
  cat("        ggplot2::theme(legend.position = 'bottom') + ", file = reportRMD , sep = "\n", append = TRUE)
  cat("        ggplot2::guides(fill=ggplot2::guide_legend(nrow=ceiling(length(unique(ds$survey_consent))/2), byrow=TRUE)) + ", file = reportRMD , sep = "\n", append = TRUE)
  cat("        ggplot2::scale_fill_manual(values = colors))", file = reportRMD , sep = "\n", append = TRUE)
  cat("```", file = reportRMD , sep = "\n", append = TRUE)

  cat(paste0("\n### Surveys duration"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("\nThe survey duration distribution per enumerator could be useful to identify enumerators which are consistent (i.e. having similar duration for each of their surveys made). Be aware that a consistent survey durations could be interpreted in different ways, it could be seen as a good thing, meaning the enumerator on the overall takes similar time to ask the questions, but it could also be interpreted as a negative sign if we assume the enumerator is filling the survey by himself and monitoring his time to not have a short overall duration. A closer monitoring of the time spend per question could help to identify the way the duration distribution has to be interpreted."), file = reportRMD , sep = "\n", append = TRUE)

  cat("```{r graphenumeratorSurveysDuration, eval=TRUE, echo=FALSE, results='asis', fig.align='center', fig.width=7, fig.height=10}", file = reportRMD , sep = "\n", append = TRUE)
  cat("print(graphenumeratorSurveysDuration)", file = reportRMD , sep = "\n", append = TRUE)
  cat("```", file = reportRMD , sep = "\n", append = TRUE)

  cat(paste0("\n### Overall number of errors per type by enumerator"), file = reportRMD , sep = "\n", append = TRUE)

  cat("```{r enumeratorErrorsDashboard, eval=TRUE, echo=FALSE, results='asis', fig.align='center', fig.width=7, fig.height=30}", file = reportRMD , sep = "\n", append = TRUE)
  cat("print(graphenumeratorErrorsDashboard)", file = reportRMD , sep = "\n", append = TRUE)
  cat("```", file = reportRMD , sep = "\n", append = TRUE)
}
