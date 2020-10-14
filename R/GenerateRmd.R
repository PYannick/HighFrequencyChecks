#' @name GenerateRmd
#' @rdname GenerateRmd
#' @title Create an Rmd file for the report
#' @description Create an Rmd file for the report
#'
#' @param ds dataset as a data.frame object
#' @param survey_consent name as a string of the field in the dataset where the survey consent is stored
#' @param reportingcol columns as a list of string name from the dataset you want in the result (c('col1','col2',...))
#' @param UniqueID name as a string of the field in the dataset where the unique ID is stored
#' @param dates fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
#' @param surveydate name as a string of the field in the dataset where the date of the survey is stored
#' @param start_collection date as a string of the first day of data collection ('yyyy-mm-dd')
#' @param dateformat format as a string used for the date ('\%m/\%d/\%Y')
#' @param minduration minimum acceptable survey duration as integer in minutes
#' @param sdval number of standard deviation for which the data within is considered as acceptable
#' @param HHSize name as a string of the field in the dataset where the household size is stored
#' @param delete delete action to be done as a boolean (TRUE/FALSE)
#' @param correct correction action to be done as a boolean (TRUE/FALSE)
#' @param enumeratorID name as a string of the field in the dataset where the enumerator ID is stored
#' @param enumeratorcheck specify if the report has to be displayed for each enumerator or not as a boolean (TRUE/FALSE)
#' @param otherpattern pattern as string to identify the fields containing others values ('_other$')
#' @param adm dataset containing the shapefile - Regardless the projection used for the shapefile, it is transformed to WGS84
#' @param ds_site name as a string of the field in the dataset where the site is stored
#' @param ds_coord columns as a list of string name from the dataset where the GPS coordinates are stored (c('Long','Lat'))
#' @param adm_site name as a string of the field in the shapefile where the site is stored
#' @param pts dataset containing the shapefile - Regardless the projection used for the shapefile, it is transformed to WGS84
#' @param buff value as an integer in meter to determine the buffer from a sampled point which is acceptable
#' @param questions columns as a list of string name from the dataset you want to check against (c('col1','col2',...))
#' @param value  Maximum acceptable value as integer for the checked fields
#' @param minnbanswers minimum number of answers expected per question
#' @param sf sampling frame as a data.frame object
#' @param dssite  name as a string of the field in the dataset where the site is stored
#' @param sfsite name as a string of the field in the sampling frame where the site is stored
#' @param sftarget name as a string of the field where the target number of survey is stored in the sampling frame
#' @param sfnbpts  name as a string of the field where the number of points generated is stored in the sampling frame
#' @param formul  formulas as a list of string used to compute the final number of eligible surveys and the variance from the target (C('formula1','formula2')).
#'   the values/fields available are: done and the ones generated according the survey consent values (one per value)
#' @param colorder  column names as a list of string to order the colums in the result (C('col1','col2',...)).
#'  the columns available are: site, done, final, variance and the ones generated according the survey consent values (one per value)
#'
#'
#' @return vignette file
#'
#' @author Yannick Pascaud, Edouard Legoupil
#'
#' @examples
#' \dontrun{
#' ds <- HighFrequencyChecks::sample_dataset
#'
#' survey_consent <- "survey_consent"
#' reportingcol <- c("enumerator_id","X_uuid")
#' UniqueID <- "X_uuid"
#'
#' ## Setting dates
#' dates <- c("survey_start","end_survey")
#' surveydate <- "survey_date"
#' start_collection <- "2018-11-11"
#' dateformat <- "%m/%d/%Y"
#' minduration <- 30
#' sdval <- 2
#' HHSize <-"consent_received.respondent_info.hh_size"
#'
#'
#' ## Setting action on dataset
#' delete <- FALSE
#' correct <- FALSE
#'
#' ## Setting enumerator
#' enumeratorID <- "enumerator_id"
#' enumeratorcheck <- FALSE
#' otherpattern <- "_other$"
#'
#' ## Setting linked geodata
#' adm <- HighFrequencyChecks::admin
#' ds_site <- "union_name"
#' ds_coord <- c("X_gps_reading_longitude","X_gps_reading_latitude")
#' adm_site <- "Union"
#' pts <- HighFrequencyChecks::SamplePts
#' buff <- 10
#'
#' ## setting questions to check
#' questions <-c("consent_received.food_security.spend_food",
#'   "consent_received.food_security.spend_medication",
#'   "consent_received.food_security.spend_education",
#'   "consent_received.food_security.spend_fix_shelter",
#'   "consent_received.food_security.spend_clothing",
#'   "consent_received.food_security.spend_hygiene",
#'   "consent_received.food_security.spend_fuel",
#'   "consent_received.food_security.spend_hh_items",
#'   "consent_received.food_security.spend_transport",
#'   "consent_received.food_security.spend_communication",
#'   "consent_received.food_security.spend_tobacco",
#'   "consent_received.food_security.spend_rent",
#'   "consent_received.food_security.spend_debts",
#'   "consent_received.food_security.spend_other")
#' value <- 25000
#' minnbanswers <- 3
#'
#'
#' ## setting sampling plan
#' sf <- HighFrequencyChecks::SampleSize
#' dssite <- "union_name"
#' sfsite <- "Union"
#' survey_consent <- "survey_consent"
#' sftarget <- "SS"
#' sfnbpts <- "TotPts"
#' formul <- c("done-no-not_eligible-delete",
#'"done-no-not_eligible-deleted-SS")
#' colorder <- c("site","SS","Provisio","done","not_eligible",
#'  "no","deleted","yes","final","variance")
#'
#' GenerateRmd(ds,
#'   survey_consent,
#'   reportingcol,
#'   UniqueID,
#'   dates,
#'   surveydate,
#'   start_collection,
#'   dateformat,
#'   minduration,
#'   sdval,
#'   HHSize,
#'   delete,
#'   correct,
#'   enumeratorID,
#'   enumeratorcheck,
#'   otherpattern,
#'   adm,
#'   ds_site,
#'   ds_coord,
#'   adm_site,
#'   pts,
#'   buff,
#'   questions,
#'   value,
#'   minnbanswers,
#'   sf, dssite,
#'   sfsite,
#'   sftarget,
#'   sfnbpts,
#'   formul,
#'   colorder)
#'}
#' @export GenerateRmd
#'
GenerateRmd <- function(usexlsform=FALSE,
  form=NULL,
  ds=NULL,
  survey_consent=NULL,
  reportingcol=NULL,
  UniqueID=NULL,
  dates=NULL,
  surveydate=NULL,
  start_collection=NULL,
  dateformat=NULL,
  minduration=NULL,
  sdval=NULL,
  HHSize=NULL,
  delete=FALSE,
  correct=FALSE,
  enumeratorID=NULL,
  enumeratorcheck=FALSE,
  otherpattern=NULL,
  adm=NULL,
  ds_site=NULL,
  ds_coord=NULL,
  adm_site=NULL,
  pts=NULL,
  buff=NULL,
  questions=NULL,
  value=NULL,
  minnbanswers=NULL,
  sf=NULL,
  dssite=NULL,
  sfsite=NULL,
  sftarget=NULL,
  sfnbpts=NULL,
  formul=NULL,
  colorder=NULL)
{

  cat("Let's summarise what the high Frequency check you just set up:\n")

  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
stop("We can not go further, please provide the name of the dataset you want to check")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
stop("We can not go further, you need to provide the columns you want to display
 to check the results. \n For instance, include at minima the
 enumerator id column if you want to check by enumerator)")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
cat("You have not provided the reference of the fields where the
survey consent is stored. This part of the check will be skipped")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
cat("You have not provided the reference of the fields where the survey
start and end date is stored.\n
 For instance (c('start_date','end_date')).
 As a result, all the check related to interview duration will be skipped ")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
cat("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete) | !is.logical(delete)){
cat("Please provide the delete action to be done (TRUE/FALSE)")
  }
  if(is.null(adm) | !isS4(adm) | nrow(adm)==0){
cat("Please provide the spatial dataset of the boundaries shapefile")
  }
  if(is.null(ds_site) | !is.character(ds_site)){
cat("Please provide the field where the site to check against is stored")
  }
  if(is.null(ds_coord) | !is.character(ds_coord) | length(ds_coord)!=2){
cat("Please provide the fields where the coordinates are stored (c('Long','Lat'))")
  }
  if(is.null(adm_site) | !is.character(ds_site)){
cat("Please provide the field where the site in the shapefile is stored")
  }
  if(delete){
ds[,survey_consent][is.na(ds[,dates[2]])]<-"deleted"
  }
  if(is.null(correct) | !is.logical(correct)){
cat("Please provide the correction action to be done (TRUE/FALSE)")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
cat("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }

  if(is.null(UniqueID) | !is.character(UniqueID)){
cat("Please provide the field where the survey unique ID is stored")
  }
  if(is.null(start_collection) | !is.character(start_collection)){
cat("Please provide the date when the data collection began ('yyyy-mm-dd')")
  }

  if(is.null(pts) | !isS4(pts) | nrow(pts)==0){
cat("Please provide the spatial dataset of the sample points shapefile")
  }
  if(is.null(buff) | !is.numeric(buff)){
cat("Please provide the buffer in meters")
  }
  if(is.null(enumeratorcheck) | !is.logical(enumeratorcheck)){
cat("Please provide the enumerator check action to be done (TRUE/FALSE)")
  }
  if(isTRUE(enumeratorcheck) & (is.null(enumeratorID) | !is.character(enumeratorID))){
cat("Please provide the field where the enumerator ID is stored")
  }
  if(is.null(otherpattern) | !is.character(otherpattern)){
cat("Please provide the pattern for other fields ('_other$')")
  }
  if(is.null(sdval) | !is.numeric(sdval)){
cat("Please provide the number of standard deviations you want to check for")
  }
  if(is.null(questions) | !is.character(questions)){
cat("Please provide the fields you want to check for (c('field1','field2',...))")
  }
  if(is.null(value) | !is.numeric(value)){
cat("Please provide the maximum value for which you want to check")
  }
  if(is.null(minduration) | !is.numeric(minduration)){
cat("Please provide the minimum survey time to check against")
  }
  if(is.null(HHSize) | !is.character(HHSize)){
cat("Please provide the field where the HH size is stored")
  }
  if(is.null(sdval) | !is.numeric(sdval)){
cat("Please provide the number of standard deviations you want to check for")
  }

  cat("##############\n")
  cat("We now start building your report\n")
  cat("##############\n")
  wdir <- getwd()

  #file <- "form.xlsx"
  #repstr <- openxlsx::read.xlsx(paste0(wdir,file), 1)

  reportRMD  <- paste0(wdir,"/vignettes/HighFrequencyCheckReport.Rmd")

  ## TO DO : CHECK IF FILE EXIST - AND REQUEST USER TO DELETE BEFORE REGENERATING - SUGGESTING TO SAVE PREVIOUS UNDER NEW NAME
  if (file.exists(reportRMD)) file.remove(reportRMD)

  ## Start Building the report ##########

  cat("---", file = reportRMD , sep = "\n")
  cat("title: \"High Frequency Checks template\"", file = reportRMD , sep = "\n", append = TRUE)
  cat("author: \"Your Name\"", file = reportRMD , sep = "\n", append = TRUE)
  cat("date: \"`r format(Sys.time(), '%d %B, %Y')`\"", file = reportRMD , sep = "\n", append = TRUE)
  cat("always_allow_html: yes", file = reportRMD , sep = "\n", append = TRUE)
  cat("output:",file = reportRMD , sep = "\n", append = TRUE)
  cat("  pdf_document:", file = reportRMD , sep = "\n", append = TRUE)
  cat("toc: true", file = reportRMD , sep = "\n", append = TRUE)
  cat("toc_depth: 3", file = reportRMD , sep = "\n", append = TRUE)
  cat("  html_document: default", file = reportRMD , sep = "\n", append = TRUE)
  cat("geometry: margin=0.5in", file = reportRMD , sep = "\n", append = TRUE)
  cat("---", file = reportRMD , sep = "\n", append = TRUE)

  cat("\n", file = reportRMD , sep = "\n", append = TRUE)
  cat("```{r setup, include=FALSE}", file = reportRMD , sep = "\n", append = TRUE)
  cat("knitr::opts_chunk$set(echo = FALSE)", file = reportRMD , sep = "\n", append = TRUE)

  cat("library(knitr)", file = reportRMD , sep = "\n", append = TRUE)
  cat("library(gsubfn)", file = reportRMD , sep = "\n", append = TRUE)
  cat("library(dplyr)", file = reportRMD , sep = "\n", append = TRUE)
  cat("library(data.table)", file = reportRMD , sep = "\n", append = TRUE)
  cat("library(HighFrequencyChecks)", file = reportRMD , sep = "\n", append = TRUE)
  cat("```", file = reportRMD , sep = "\n", append = TRUE)

  cat(paste0("Supervising the quality of data collection is not straightforward. Survey questionnaires are often quite long and have systematic control is worth automatizing."), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("`HighFrequencyCheck` can be used to detect programming errors, surveyor errors, data fabrication, poorly understood questions, and other issues. The results of these checks can also be useful in improving your survey, identifying enumerator effects, and assessing the reliability of your outcome measures. It allows teams to catch survey issues and collection mistakes in time to correct them and ensure high-quality data"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("The `HighFrequencyCheck` package is a translation in in R of the [Stata package](https://github.com/PovertyAction/high-frequency-checks) based on [best practices from Innovations for Poverty Action](https://www.povertyactionlab.org/resource/data-quality-checks).  High Frequency Checks are also recommend by the [World Bank](https://dimewiki.worldbank.org/wiki/High_Frequency_Checks). It can be installed from github with `devtools::install_github(\"unhcr/HighFrequencyChecks\")`."), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("The package brings a series of convenience functions to monitor data quality during the data collection when running a survey with [KoboToolbox](http://kobo.unhcr.org) (or any `xlsform` compatible platform). "), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("Those are the basis of a  feedback mechanism with enumerators and can be performed periodically during the data collection process to check for possible errors and provide meaningful inputs to the enumerators. All these functions do not have to be ran at the same period of time. They are provided there to help data supervisor to build reports:"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("* A `wrapper` function is included to generate directly an final data quality assessment Rmd Report"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("* A `ShinyApp` Interface is also included to provide a live monitoring dashboard to be run locally."), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("## Introduction: Measuring data collection quality "), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("Data collection quality monitoring includes 4 different dimensions"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" 1. Correct set-up of data collection devices"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0(" 2. Data collected according the sampling plan"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" 3. Enumerator rigorous work standards "), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0(" 4. Enumerator productivity "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("Ideally the data collection monitoring dashboard should be known to all enumerators so that they are aware of the data quality metrics that will be used to assess the quality of their work and potentially some incentive can be offered for the enumerators performing on the quality metrics (_It is good to recall that each records in household survey cost between 15 to 50 USD_). Some of those indicators can support some remedial supervision interventions, such as calling individually the enumerator and point some specific issues that were detected."), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("It is important to prepare high frequency checks (code and instructions), as part of the data quality assurance plan, before before starting with field data collection."), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("Below are the required configuration and an illustration of those indicators based on a demo dataset included in the package."), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("## Process configuration"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("### loading the required R packages"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("```{r message=FALSE, warning=FALSE, include=TRUE, echo = FALSE}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("library(knitr)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("library(gsubfn)"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("library(dplyr)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("library(data.table)"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("library(DT)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("library(ggplot2)"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("# library(plotly)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("library(HighFrequencyChecks)"), file = reportRMD , sep = "\n", append = TRUE)
#  cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
#cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("### Loading survey dataset (microdata)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
#  cat(paste0("In a production environment, it is possible to connect this a live API (kobotoolbox, ODK , etc.)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
#  cat(paste0("```{r, include=TRUE}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("ds <- ", ds), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("# correction for uppercase/lowercase in the site name "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("ds$union_name <- tolower(ds$union_name)"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("### Loading sampling plan"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("The sampling plan is defined through the sampling strategy. It includes for each enumerator the sufficient details for the enumerator to reach out to respondent satisfying the sampling target definition (i.e. name, location, phone number)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("```{r, include=TRUE}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("SampleSize <- HighFrequencyChecks::SampleSize"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("# correction for uppercase/lowercase in the site name "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("SampleSize$Union <- tolower(SampleSize$Union)"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# name as a string of the field where the number of points"), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("# generated is stored in the sampling frame"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("sf_nbpts=\"TotPts\""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("# name as a string of the field in the dataset where the site is stored"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("sf_site=\"Union\""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
  cat(paste0("# name as a string of the field in the sampling frame where the site is stored"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("sf_target=\"SS\""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("Sampling targets shall also be defined"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r, include=TRUE}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# formulas as a list of string used to compute the final number "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# of eligible surveys  and the variance from the target"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# (C('formula1','formula2'))."), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# the values/fields available are: done and the ones generated "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# according the survey consent values (one per value)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("formul = c(\"done-no-not_eligible-deleted\","), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" \"done-no-not_eligible-deleted-SS\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# column names as a list of string to order the colums in the result"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# the columns available are: site, done, final, variance and "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# the ones generated according the survey consent values (one per value)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("colorder = c(\"site\","), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" \"SS\","), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" \"TotPts\","), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" \"done\","), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" \"not_eligible\","), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" \"no\","), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" \"deleted\","), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" \"yes\","), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" \"final\","), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" \"variance\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("### Loading geodata for the surveyed area"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("Often sampling strategy includes a geographic coverage."), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r message=FALSE, warning=FALSE, include=TRUE}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#This can be overview through either:"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# a defined polygon, aka area or admin unit"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("adm <- HighFrequencyChecks::admin"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# Unique key with the survey dataset is changed to all small cap for further join"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("adm$Union <- tolower(adm$Union)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# OR a sampling point, around which enumerators are supposed to randomly interview persons."), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("pts <- HighFrequencyChecks::SamplePts"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("### Specific variables to be controlled"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("When it comes to back checks, variables can be divided into the following different categories:  "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" * Type 1 variables are based on straightforward questions where there is __very little possibility of error__. For example, age and education. If there is an error in these variables, it means there is a serious problem with enumerator, or with the questions."), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" * Type 2 variables are based on questions where a __risk of error is possible__. For example, questions based on sensitive topics, or questions that involve calculations or classification with possibility of 'or other'. If there is an error in these variables, there might be a need to provide further training for enumerators."), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" * Type 3 variables are based on questions about the survey instrument, and errors in this case provide feedback which can help improve the survey instrument itself. This is often the case for questions including __or other, please specify__ question type."), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("Below are initialized variables to perform data quality control for this specific dataset"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#### Geodata"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r, include=TRUE}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# Name of variables for geographic coordinates as recorded by data collection device GPS"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("ds_coord <- c(\"X_gps_reading_longitude\",\"X_gps_reading_latitude\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# Name of location (__matching external geodata__)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("df_site <- \"union_name\""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#Name of unique key in polygon file"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("admin_site <- \"Union\""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#### Variable of interest for data quality check"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r, include=TRUE}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# Variable recording initial consent"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("survey_consent <- \"survey_consent\""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# Variable recording multiples screening questions"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("questions <- c(\"consent_received.shelter_nfi.non_food_items[.]\","), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" \"consent_received.food_security.main_income[.]\","), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" \"consent_received.child_protection.boy_risk[.]\","), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" \"consent_received.child_protection.girl_risk[.]\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# Variable to be checked"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("reportingcol <- c(\"enumerator_id\",\"X_uuid\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("####  specifying Metadata variables"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r, include=TRUE}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# unique ID for each record"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("UniqueID <- \"X_uuid\""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# dates"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("dates <- c(\"survey_start\",\"end_survey\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("## Official date for start of data collection"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("start_collection <- \"11/11/2018\""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("surveydate <- \"survey_date\""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# Variable used to record enumerator identifiers"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("enumeratorID <- \"enumerator_id\""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#### Quality target"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r, include=TRUE}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# What is the minimum survey duration in minutes (when using all skip logic)?"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("minduration <- 10"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# minans answers per specific questions"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("minans <- 3"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# Standard value"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("sdvalue <- 2"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#Size of the buffer in meters around the assigned data collection location points"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("buff <- 10"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("### Server specific config"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r, include=TRUE}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("otherpattern <- \"_other$\""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("dateformat <- \"%m/%d/%Y\""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("delete <- FALSE"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("correct <- TRUE"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("## Corrective actions"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("### Correct set-up of data collection devices and encoding of the forms"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("These checks are designed to ensure that responses are consistent for a particular survey instrument, and that the responses fall within a particular range. For example, checks to ensure that all variables are standardized, and there are no outliers in the data. Share a daily log of such errors, and check if it is an issue with enumerators, in which case there might be a need to re-train enumerators."), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("Missing data: Are some questions skipped more than others? Are there questions that no respondents answered? This may indicate a programming error."), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" *  Categorical variables: Are respondents selecting the given categories or are many respondents selecting “None of the above”, or “other”? If conducting a survey, you may want to add categories or modify your existing ones."), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" *   Too many similar responses: Is there a question where all respondents answer in the same way?"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#### Respondent ID"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("Respondent IDs: Are there duplicates of your unique identifiers? If so, does the reason why make sense? (e.g., one circumstance in which there may be duplicates of unique IDs is when surveyors have to end and restart an interview.) Are there blank or invalid IDs? This might be a sign your surveyors are not interviewing the correct respondent. "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r eval=TRUE, echo = FALSE, results='asis'}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("list_unique_id <- isUniqueIDDuplicated(ds, UniqueID, survey_consent, reportingcol, delete)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("ds <- list_unique_id[[1]]"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("if(nrow(list_unique_id[[2]]) > 0){ "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("DT::datatable(list_unique_id[[2]], "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" caption = \"Detected records with errors: Duplicate respondent ID\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("} else {"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  cat(\">__No errors__: All records have a unique repondent ID\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r eval=TRUE, echo = FALSE, results='asis'}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("list_missing_id <- isUniqueIDMissing(ds, UniqueID, survey_consent, reportingcol, delete)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("ds <- list_missing_id[[1]] "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("if(nrow(list_missing_id[[2]] ) > 0){ "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("DT::datatable(list_missing_id[[2]], "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" caption = \"Detected records with errors: Missing respondent ID\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("} else {"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  cat(\">__No errors__: All records have an ID\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#### Configuration of dates on device"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" * Checking record for which interview that do not end on the same day as they started"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r eval=TRUE, echo = FALSE, results='asis'}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("list_date_mistake <- isSurveyOnMoreThanADay(ds, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  survey_consent, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  dates, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  reportingcol, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  delete)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("ds <- list_date_mistake[[1]]"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("if(nrow(list_date_mistake[[2]]) > 0){ "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("DT::datatable(list_date_mistake[[2]], "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" caption = \"Detected records with errors\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("} else {"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  cat(\">__No errors__: All interviews ended on the same day as they started\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" * Checking record for which interview ended before they start"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r eval=TRUE, echo = FALSE, results='asis'}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("list_date_mistake2 <- isSurveyEndBeforeItStarts(ds, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("survey_consent, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("dates, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("reportingcol, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("delete)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#ds <- list_date_mistake2[[1]]"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("if(nrow(list_date_mistake2[[2]]) > 0){ "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("DT::datatable(list_date_mistake2[[2]], "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" caption = \"Detected records with errors: interviews ended before they start\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("} else {"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  cat(\">__No errors__: All interviews ended before they start\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" * Checking record for which interview tagged in the future"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r eval=TRUE, echo = FALSE, results='asis'}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("list_date_mistake3 <- isSurveyMadeInTheFuture(ds, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("survey_consent, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("dates, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("reportingcol, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("delete)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#ds <- list_date_mistake3[[1]]"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("if(nrow(list_date_mistake3[[2]]) > 0){ "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("DT::datatable(list_date_mistake3[[2]], "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" caption = \"Detected records with errors - date are not in the future\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("} else {"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  cat(\">__No errors__: records date are not in the future\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("### Data collected according the plan"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#### Interviews made before the first day of data collection"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r eval=TRUE, echo = FALSE, results='asis'}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("list_date_mistake4 <- isSurveyStartedBeforeTheAssessment(ds, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("dates,"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("survey_consent,  "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("start_collection, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("reportingcol, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("delete)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("ds <- list_date_mistake4[[1]]"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("if(nrow(list_date_mistake4[[2]]) > 0){ "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("DT::datatable(list_date_mistake4[[2]], "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" caption = \"Detected records with errors - records occured after the official beginning of data collection\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("} else {"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  cat(paste0(\">__No errors__: all records occured after the official beginning of data collection on \", start_collection))"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#### Recorded site name for each interview matches the name of the location"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r eval=TRUE, echo = FALSE, message=FALSE, warning=FALSE, results='asis'}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("list_site <- isInterviewInTheCorrectSite(adm,"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" ds, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" df_site, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" ds_coord, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" admin_site, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" survey_consent, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" reportingcol, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" correct)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("ds <- list_site[[1]]"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("if(nrow(list_site[[2]]) > 0){ "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("DT::datatable(list_site[[2]], "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" caption = \"Detected records with errors - location name not matching with GPS\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("} else {"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  cat(\">__No errors__: all records location name not matching with GPS\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#### Recorded locations for each interview within a **`r buff`** meters buffer from a sample point"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r eval=TRUE, echo = FALSE, message=FALSE, warning=FALSE, results='asis'}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("list_sitept <- isInterviewAtTheSamplePoint(ds, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("pts, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("ds_coord, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("buff, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("survey_consent, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("reportingcol, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("delete)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("ds <- list_sitept[[1]]"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("if(nrow(list_sitept[[2]]) > 0){ "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("DT::datatable(list_sitept[[2]], "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" caption = \"Detected records with errors - recorded location too far from sampling points\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("} else {"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  cat(\">__No errors__: all records location in accpetable distance from sampling points\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#### Enumerators who made a survey below **`r minduration`** minutes"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r eval=TRUE, echo = FALSE, results='asis'}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("list_duration_Xmin <- isInterviewTooShort(ds, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" survey_consent, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" dates, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" reportingcol, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" minduration, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" TRUE)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("ds <- list_duration_Xmin[[1]]"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("if(nrow(list_duration_Xmin[[2]]) > 0){ "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("DT::datatable(list_duration_Xmin[[2]], "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" caption = paste0(\"Detected records with errors - Interviews duration shorter than \", minduration))"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("} else {"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  cat(paste0(\">__No errors__: No interviews duration shorter than \", minduration))"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#### Tracking sheet per site"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("Test for target number: since surveys are submitted in daily waves, keep track of the numbers of surveys submitted and the target number of surveys needed for an area to be completed."), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r eval=TRUE, echo = FALSE, message=FALSE, warning=FALSE, results='asis'}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("trackingSheet <- assessmentTrackingSheet(ds,"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  SampleSize,"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  df_site,"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  sf_site,"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  survey_consent,"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  sf_target,"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  sf_nbpts,"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  formul,"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  colorder)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("DT::datatable(trackingSheet[[2]], "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" caption = \"trackingSheet\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("## Pro-active actions"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("### Enumerators rigorous work standards"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("These are designed to check if data shared by any particular enumerator is significantly different from the data shared by other enumerators. Some parameters to check enumerator performance include percentage of \"Don't know\" responses, or average interview duration. In the first case, there might be a need to re-draft the questions, while in the second case, there might be a need to re-train enumerators."), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#### Responses with outliers"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("Outliers: Are some respondents reporting values drastically higher or lower than the average response? Do these variables need to be top or bottom coded? Many outlier checks can be directly programmed into the survey, either to flag responses or bar responses that are outside the acceptable range."), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#### Check the duration of consent and other modules by the enumerator"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#### Durations of Interviews"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("Beware that Interviews with potential errors on the dates are not marked for deletion which can lead to weird duration"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r eval=TRUE, echo = FALSE, results='asis'}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("list_dur <- assessmentDuration(ds, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("dates)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("# ds <- dts"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("cat(\"> The total time of data collection is \", list_dur[[1]], \" minutes and the average time per survey is \", list_dur[[2]], \" minutes\")"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#### Enumerators who pick up less than **`r minans`** answers per specific questions"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r eval=TRUE, echo = FALSE, results='asis'}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("reportlog_less_X_answers <- enumeratorIsLazy(ds, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("   enumeratorID, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("   questions, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("   minans)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("if(nrow(as.data.frame(reportlog_less_X_answers[[2]])) > 0 ){ "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("DT::datatable(reportlog_less_X_answers[[2]], "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" caption = paste0(\"Detected records with errors - Enumerators who pick up less than \",minans, \" answers per specific questions\"))"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("} "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#### Check percentage of “don’t know” and refusal responses by the enumerator. "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#### Number of other distinct values (for the questions with a possibility of other)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r eval=TRUE, echo = FALSE, message=FALSE, warning=FALSE, results='asis'}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("reportlog_others_values <- surveyOtherValues(ds, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("   otherpattern, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("   enumeratorID, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("   TRUE)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("if(nrow(reportlog_others_values[[2]]) > 0){ "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("DT::datatable(reportlog_others_values[[2]], "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" caption = paste0(\"Detected of other distinct values\"))"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("} "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("### Enumerator productivity"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#### How many completed interview per day?"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r eval=TRUE, echo = FALSE, results='asis'}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("reportlog_productivity <- assessmentProductivity(ds, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" surveydate, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" dateformat, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" survey_consent)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("if(nrow(reportlog_productivity[[2]]) > 0){ "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("DT::datatable(reportlog_productivity[[2]], "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" caption = paste0(\"Completed interview per day\"))"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("} "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r eval=FALSE, echo = FALSE, results='asis'}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("assessmentProductivityGraphical(ds, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" surveydate, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" dateformat, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" survey_consent)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#### How many attempted interview per day and obtained consent?"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r eval=TRUE, echo = FALSE, message=FALSE, warning=FALSE, results='asis'}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("reportlog_nb_status <- assessmentDailyValidSurveys(ds, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  surveydate, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  dateformat, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  survey_consent)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("if(nrow(reportlog_nb_status[[2]]) > 0){ "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("DT::datatable(reportlog_nb_status[[2]], "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" caption = paste0(\"attempted interview per day and obtained consent\"))"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("} "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("if(!is.null(reportlog_nb_status[4])){"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("  print(reportlog_nb_status[4])"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#### Percentage of survey per consent status by enumerator"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r eval=TRUE, echo = FALSE, message=FALSE, warning=FALSE, results='asis'}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("reportlog_refusal <- enumeratorSurveysConsent(ds, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("   survey_consent, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("   enumeratorID)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("if(nrow(reportlog_refusal[[2]]) > 0){ "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("DT::datatable(reportlog_refusal[[2]], "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" caption = paste0(\"Percentage of survey per consent status by enumerator\"))"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("} "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#### Average interview duration by enumerator"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r eval=TRUE, echo = FALSE, results='asis'}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("reportlog_duration <- enumeratorSurveysDuration(ds, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("dates, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("enumeratorID)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("if(nrow(reportlog_others_values[[2]]) > 0){ "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("DT::datatable(reportlog_duration[[2]], "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" caption = paste0(\"Average interview duration by enumerator\"))"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("} "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#### Number of surveys per day by enumerator"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r eval=TRUE, echo = FALSE, message=FALSE, warning=FALSE, results='asis'}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("reportlog_nb_survey <- enumeratorProductivity(ds, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" surveydate, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" enumeratorID)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("if(nrow(reportlog_nb_survey[[2]]) > 0){ "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("DT::datatable(reportlog_nb_survey[[2]], "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" caption = paste0(\"Number of surveys per day by enumerator\"))"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("} "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("#### Enumerators with productivity significantly different from the average (low or high)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```{r eval=TRUE, echo = FALSE, results='asis'}"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(""), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("reportlog_productivity <- enumeratorProductivityOutliers(ds, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("enumeratorID, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("surveydate, "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("sdvalue)"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("if(nrow(reportlog_productivity[[2]]) > 0){ "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("DT::datatable(reportlog_productivity[[2]], "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0(" caption = paste0(\"Enumerators with productivity significantly different from the average\"))"), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("} "), file = reportRMD , sep = "\n", append = TRUE)
cat(paste0("```"), file = reportRMD , sep = "\n", append = TRUE)



}


