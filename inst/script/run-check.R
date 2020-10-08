library(HighFrequencyChecks)
library(readr)
library(magrittr)

## pull live data feed from your kobo project
# devtools::install_github("UNHCR-WEB/hcrdata")
library(hcrdata)
ds <-
  hcrdata::hcrfetch(
    src = "kobo",
    dataset = "High Frequency Survey - Remote or in-person interviews",
    file = "data.json") %>%
  jsonlite::fromJSON() %>%
  purrr::pluck("results") %>%
  #tibble::as_tibble() %>%
  purrr::set_names(~stringr::str_replace_all(., "(\\/)", "."))

ds <- as.data.frame(ds)
names(ds)
str(ds)

### Setting date ###

dates <- c("start","end")
ds$start <- readr::parse_datetime(as.character(ds$start))
str(ds$start)
ds$end <- readr::parse_datetime(as.character(ds$end))
str(ds$end)

strptime(ds[,dates[1]],"%Y-%m-%dT%R")

strptime(ds[,dates[1]],"%Y-%m-%dT%R")

SurveyLength = as.double.difftime((strptime(ds[,dates[2]],"%Y-%m-%dT%R") -
                                     strptime(ds[,dates[1]],"%Y-%m-%dT%R")),
                                  units = "secs") / 60

SurveyLength = as.double.difftime(( readr::parse_datetime(as.character(ds[,dates[2]])) -
                                      readr::parse_datetime(as.character(ds[,dates[1]]))),
                                  units = "secs") / 60

surveydte <- "today"
ds$today <- parse_date(as.character(ds$today))
str(ds$today)


## Official date for start of data collection
startdate <- "11/09/2020"

minduration <- 10
#Standard value
sdvalue <- 2

# formulas as a list of string used to compute the final number
# of eligible surveys  and the variance from the target
# (C('formula1','formula2')).
# the values/fields available are: done and the ones generated
# according the survey consent values (one per value)
formul = c("done-no-not_eligible-deleted",
           "done-no-not_eligible-deleted-SS")


## consent #####

consent <- "screen.Consent"

#Variable recording multiples screening questions

questions <- c( "ConsentInterview2"  ,
                "screen.YesDisplaced"    ,
                "screen.YesAtrisk" ,
                "screen.DisplacedNo" ,
                "screen.YesOver15" ,
                "screen.Over15No",
                "screen.Consent"  )

## Variable of interest ####
reportingcol <- c("username","_uuid")



enumid <- "username"
uuid <- "_uuid"


#minans answers per specific questions
minans <- 3

delete <- FALSE

### Starting check #####

list <- chk5b_duration_Xmin(ds, survey_consent, dates,  reportingcol, minduration, delete)
sample_dataset <- list[[1]]

if(nrow(list[[2]])>0){
  DT::datatable(list[[2]],
                caption = paste0("Detected records with errors - Interviews duration shorter than ", mindur))
} else {
  cat(paste0(">__No errors__: No interviews duration shorter than ", mindur))
}

list <- chk2b_unique_id(ds, UniqueID, survey_consent, reportingcol, delete)
ds <- dts_unique_id

if(nrow(err_unique_id)>0){
  DT::datatable(err_unique_id,
                caption = "Detected records with errors: Duplicate respondent ID")
} else {
  cat(">__No errors__: All records have a unique repondent ID")
}


## Checking date ###########
list <- chk3a_date_mistake(ds,
                             consent,
                             dates,
                             reportcol,
                             FALSE)
data <- list[[1]]

if(nrow(list[[2]])>0){
  DT::datatable(err_date_mistake,
                caption = "Detected records with errors")
} else {

  cat(">__No errors__: All interviews ended on the same day as they started")
}


list[dts_date_mistake2,err_date_mistake2] <- chk3b_date_mistake(ds,
                               consent,
                               dates,
                               reportcol,
                               FALSE)
#data <- dts_date_mistake2

if(nrow(err_date_mistake2)>0){
  DT::datatable(err_date_mistake2,
                caption = "Detected records with errors: interviews ended before they start")
} else {
  cat(">__No errors__: All interviews ended before they start")
}


list[dts_date_mistake3,err_date_mistake3] <- chk3d_date_mistake(ds,
                               consent,
                               dates,
                               reportcol,
                               FALSE)
#data <- dts_date_mistake3

if(nrow(err_date_mistake3)>0){
  DT::datatable(err_date_mistake3,
                caption = "Detected records with errors - date are not in the future")
} else {
  cat(">__No errors__: records date are not in the future")
}

## How many completed interview per day? ####
reportlog_productivity <- chk7ai_productivity(ds,
                                              surveydte,
                                              dteformat,
                                              consent)
if(nrow(reportlog_productivity)>0){
  DT::datatable(reportlog_productivity,
                caption = paste0("Completed interview per day"))
}

chk7aii_productivity_hist(ds,
                          surveydte,
                          dteformat,
                          consent)


#### How many attempted interview per day and obtained consent?  ####
reportlog_nb_status <- chk7bi_nb_status(ds,
                                        surveydte,
                                        dteformat,
                                        consent)
if(nrow(reportlog_nb_status)>0){
  DT::datatable(reportlog_nb_status,
                caption = paste0("attempted interview per day and obtained consent"))
}
