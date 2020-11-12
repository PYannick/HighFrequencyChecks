#' @name kobo_dico
#' @rdname kobo_dico
#' @title  Create Data dictionnary an the xlsform
#'
#' @description  Produce a data dictionnary based on the xlsform for the project
#'
#' @param form The full filename of the form to be accessed (xls or xlsx file).
#' It is assumed that the form is stored in the data folder.
#'
#'
#' @return A "data.frame" with the full data dictionnary. To be used in the rest of the analysis.
#'
#' @author Edouard Legoupil
#'
#'
#' @examples
#' \dontrun{
#' kobo_dico(form = "form.xls")
#' }
#'
#' @export kobo_dico
#'

kobo_dico <- function(surveyPart, choicesPart) {
  #kobo_form(formid, user = user, api = api)
  # cat("\n Your form should be placed within the `data` folder. \n \n")
  # read the survey tab of ODK from
  # mainDir <- kobo_getMainDirectory()
  # mainDir <- "C:/Users/yanni/Documents/tmpSavHFC/TestHFC"
  #
  # form_tmp <- paste(mainDir, "data", form, sep = "/", collapse = "/")

  ### First review all questions from survey sheet #################################################
  survey <- surveyPart
  survey <- survey[,c("type", "name")]

  ## need to delete empty rows from the form
  survey <- as.data.frame(survey[!is.na(survey$type), ])

  ### We can now extract the id of the list name to reconstruct the full label fo rthe question
  cat(" \n Now extracting list name from questions type.\n \n")
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
  cat("\n Be careful! The current function only support 2 levels of nested repeat - for instance household / Case / Individual. \n \n")
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
  survey$type2[survey$type2 %in% c("begin_group","begin group","end_group","end group")]
  ## We need to handle situation with both repeat & group
  ## So 12 cases to handle

  cat(" \n Now rebuilding the variable full path in order to match with variable name from the exported dataset. \n
      Note that there should not be any dots in the orginal variables. \n
      Double Check as well there's no duplicate for the name column in the survey worksheet\n \n")
  survey$qgroup <- ""
  for (i in 2:nrow(survey))
  {
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
  for (i in 2:nrow(survey))
  {
    if (survey[ i, c("qlevel")] == "") {survey[ i, c("fullname")]  <-  survey[ i, c("name")]}
    else {survey[ i, c("fullname")]  <-  paste(survey[ i, c("qgroup")],survey[ i, c("name")],sep = ".") }
  }

  ## a few colummns to adjust to match questions & choices
  survey$labelchoice <- survey$labelReport #survey$label
  survey$order <- ""
  survey$weight <- ""
  survey$score <- ""
  survey$recategorise <- ""

  #### Now looking at choices --#########################################################################################################
  choices <- choicesPart
  choices <- choices[, c("list_name", "name")]
  colnames(choices) <- c("listname", "name")

  ## need to delete empty rows from the form
  choices <- as.data.frame(choices[!is.na(choices$listname), ])

  ## Remove trailing space
  choices$listname <- trimws(choices$listname)

  ## merge with related questions -
  names(survey)
  surveychoice <- survey[ ,c("type", "name", "listname", "fullname")]
  names(surveychoice)[names(surveychoice) == "name"] <- "nameq"

  choices <- plyr::join(x = choices, y = surveychoice, by = "listname", type = "left")

  choices$type <- with(choices, ifelse(grepl("select_one", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  choices$type),
                                       paste0("select_one_d"),choices$type))

  choices$type <- with(choices, ifelse(grepl("select_multiple_d", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  choices$type),
                                       paste0("select_multiple"),choices$type))

  choices$fullname <- paste0(choices$fullname, sep = ".", choices$name)

  #### Now Row bing questions & choices########################################################################################################
  choices2 <- choices[ ,c("type", "name", "fullname", "listname")]
  survey2 <- survey[,c("type", "name", "fullname", "listname")]

  survey2$formpart <- "questions"
  choices2$formpart <- "answers"
  choices2 <- subset(choices2, type=="select_multiple")

  dico <- rbind(survey2,choices2)

  ## Remove trailing space
  dico$fullname <- trimws(dico$fullname)
  dico$listname <- trimws(dico$listname)

  ## A few fix on the dico
  dico <- dico[ !is.na(dico$name), ]
  dico <- dico[ !is.na(dico$type), ]

  # utils::write.csv(dico, paste0(mainDir,"/data/dico2_",form,".csv"), row.names = FALSE, na = "")

  # f_csv(dico)
  return(dico)
}

