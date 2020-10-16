library(HighFrequencyChecks)
library(readr)
library(magrittr)

## pull live data feed from your kobo project
# devtools::install_github("UNHCR-WEB/hcrdata")
library(hcrdata)
#hcrdata:::hcrbrowse()

### Getting data #####

data <-
  hcrdata::hcrfetch(
    src = "kobo",
    dataset = "High Frequency Survey - Remote or in-person interviews",
    file = "data.json") %>%
  jsonlite::fromJSON() %>%
  purrr::pluck("results") %>%
  #tibble::as_tibble() %>%
  purrr::set_names(~stringr::str_replace_all(., "(\\/)", "."))

ds <- as.data.frame(data)
# ds <- read.csv("data-raw/MainDataFrame.csv", sep=";")

#names(ds)
#str(ds)

### Setting date #####

dates <- c("start","end")
ds$start <- readr::parse_datetime(as.character(ds$start))
str(ds$start)
ds$end <- readr::parse_datetime(as.character(ds$end))
str(ds$end)

surveydate <- "today"
ds$today  <- readr::parse_date(as.character(ds$today))
#head(ds$today)
dateformat <- "%Y-%m-%d"

start_collection <- "2020-10-05"
minduration <- 30
sdval <- 2

## Related actions...
delete_completed <- TRUE
delete_dateMoreThanADay <- TRUE
delete_dateBeforeItStarts <- TRUE
delete_dateBeforeTheAssessment <- TRUE
delete_InTheFuture <- TRUE
delete_InterviewTooShort <- TRUE

## Setting if relevant HH size - influence on survey duration
HHSize <-"monit.progres_groupsize"
delete_InterviewTooShortHH <- TRUE

## Setting Enumerator & unique ID #####
ds$X_uuid <- ds$"_uuid"
UniqueID <- "X_uuid"
## Related actions
delete_missing_id <- TRUE
delete_unique_id <- TRUE

### Setting Enumerator variable ####
#levels(as.factor(ds$username))
enumeratorID <- "username"
## Related action
enumeratorcheck <- TRUE

## Setting consent #####
survey_consent <- "ConsentScore"
## Related action
delete_withconsent <- TRUE



## Check on categorical variables ####
questions_cat <-c("consent_received.food_security.spend_food",
                  "consent_received.food_security.spend_other")
minnbanswers <- 3

## Pattern for or other Dollar to indicate end of variable
otherpattern <- "_other$"



## Check on numeric variables ####
questions_num <-c("consent_received.food_security.spend_food",
       "consent_received.food_security.spend_other")
## Max numeric value
value <- 25000


## Setting sampling plan ####
# SampleSize <- HighFrequencyChecks::SampleSize
# dssite <- "union_name"
# sfsite <- "Union"
# sftarget <- "SS"
# sfnbpts <- "TotPts"


## Setting linked geodata ####
# adm <- HighFrequencyChecks::admin
# ds_site <- "union_name"
# ds_coord <- c("X_gps_reading_longitude","X_gps_reading_latitude")
# adm_site <- "Union"
# pts <- HighFrequencyChecks::SamplePts
# buff <- 10

## related cleaning actions
delete_sitept <- TRUE
correct_site <- TRUE



### Setting Report Content  #####
## Variable displayed in the tables
reportingcol <- c("username","X_uuid")
#View(ds[, c("username","X_uuid")])

## based on the modalities in the consent variable
## done defined independently
# no -- specific to form - modality for consent
# not eligibel idem
# delete independently - define based on delete action

## the following is a formula with minus (or potential add) - the elements to
# be decided are both fixed

levels(as.factor(ds$ConsentScore))
formul <- c("done-no-deleted",
               "done-no-deleted-SS")

## This

## site <- name of the var site in the form
## SS <- integer name of sftarget
## "provision" <- integer - sfnbpts - number of person available for interview per sampling unit

colorder <- c("site","SS","Provision",
              "done",#"not_eligible",
                 "no",  ##modality in consent..
              "deleted",
              "yes",##modality in consent..
              "final",
              "variance")


## Run Wrapper functions #####

GenerateRmd(ds,
              survey_consent,
              reportingcol,
              UniqueID,
              dates,
              surveydate,
              start_collection,
              dateformat,
              minduration,
              sdval,
              HHSize,
              delete,
              correct_site,
              enumeratorID,
              enumeratorcheck,
              otherpattern,
              adm,
              ds_site,
              ds_coord,
              adm_site,
              pts,
              buff,
              questions,
              value,
              minnbanswers,
              sf, dssite,
              sfsite,
              sftarget,
              sfnbpts,
              formul,
              colorder)

## Run all functions one by one #####

list_iscompleted <- isInterviewCompleted(ds,
                                         survey_consent,
                                         dates,
                                         reportingcol,
                                         delete_completed)
head(list_iscompleted[[2]],10)

list_withconsent <- isInterviewWithConsent(ds,
                                           survey_consent,
                                           reportingcol,
                                           delete_withconsent)
head(list_withconsent[[2]],10)

list_site <- isInterviewInTheCorrectSite(ds,
                                         adm,
                                         ds_site,
                                         ds_coord,
                                         adm_site,
                                         survey_consent,
                                         reportingcol,
                                         correct_site)
head(list_site[[2]], 10)

list_sitept  <- isInterviewAtTheSamplePoint( ds, pts,
                                             ds_coord, buff,
                                             survey_consent,
                                             reportingcol,
                                             delete_sitept)
head(list_sitept[[2]], 10)

list_missing_id <- isUniqueIDMissing(ds, UniqueID,
                                     survey_consent,
                                     reportingcol,
                                     delete_missing_id)
head(list_missing_id[[2]], 10)

list_unique_id <- isUniqueIDDuplicated(ds, UniqueID,
                                       survey_consent,
                                       reportingcol,
                                       delete_unique_id)
head(list_unique_id[[2]], 10)


list_dateMoreThanADay <- isSurveyOnMoreThanADay(ds,
                                            survey_consent,
                                            dates,
                                            reportingcol,
                                            delete_dateMoreThanADay)
head(list_dateMoreThanADay[[2]], 10)

list_dateBeforeItStarts <- isSurveyEndBeforeItStarts(ds,
                                                survey_consent,
                                                dates,
                                                reportingcol,
                                                delete_dateBeforeItStarts)
head(list_dateBeforeItStarts[[2]], 10)

list_dateBeforeTheAssessment <- isSurveyStartedBeforeTheAssessment(ds,
                                                         dates,
                                                         survey_consent,
                                                         start_collection,
                                                         reportingcol,
                                                         delete_dateBeforeTheAssessment)
head(list_dateBeforeTheAssessment[[2]], 10)

list_dateInTheFuture <- isSurveyMadeInTheFuture(ds,
                                                survey_consent,
                                              dates,
                                              reportingcol,
                                              delete_InTheFuture)
head(list_dateInTheFuture[[2]], 10)

log <- surveyMissingValues(ds, enumeratorID, enumeratorcheck)
head(log[[2]],10)

log <- surveyDistinctValues(ds, enumeratorID, enumeratorcheck)
head(log[[2]],10)

log <- surveyOtherValues(ds,
                         otherpattern,
                         enumeratorID,
                         enumeratorcheck)
head(log[[2]],10)

log <- surveyOutliers(ds, sdval, reportingcol, enumeratorID, enumeratorcheck)
head(log[[2]],10)

log <- surveyBigValues(ds, questions, value,reportingcol, enumeratorID, enumeratorcheck)
head(log[[2]],10)

assessmentDuration(ds, dates)

list_InterviewTooShort <- isInterviewTooShort(ds,
                                              survey_consent,
                                              dates,
                                              reportingcol,
                                              minduration,
                                              delete_InterviewTooShort)
head(list_InterviewTooShort[[2]], 10)

list_InterviewTooShortHH <- isInterviewTooShortForTheHouseholdSize(ds,
                                                             survey_consent,
                                                             dates, HHSize,
                                                             reportingcol,
                                                             minduration,
                                                             delete_InterviewTooShortHH)
head(list_InterviewTooShort[[2]], 10)

log <- assessmentDurationOutliers(ds, dates, sdval, reportingcol)
head(log[[2]],10)

## recheck my merge
log <- enumeratorSurveysConsent(ds, survey_consent, enumeratorID)
head(log[[2]],10)

log <- enumeratorSurveysDuration(ds, dates, enumeratorID)
head(log[[2]],10)

log <- enumeratorProductivity(ds, surveydate, enumeratorID)
head(log[[2]],10)

log <- enumeratorProductivityOutliers(ds, enumeratorID, surveydate, sdval)
head(log[[2]],10)

#log <- enumeratorIsLazy(ds, enumeratorID, questions_cat, minnbanswers)
head(log[[2]],10)

log <- assessmentProductivity(ds,
                              surveydate,
                              dateformat,
                              survey_consent)
head(log[[2]],10)

log <- assessmentDailyValidSurveys(ds,
                                   surveydate,
                                   dateformat,
                                   survey_consent )
head(log[[2]],10)
log[[4]]

log <- assessmentTrackingSheet(ds,
                               SampleSize,
                               dssite,
                               sfsite,
                               survey_consent,
                               sftarget,
                               sfnbpts,
                               formul,
                               colorder)
head(log[[2]],10)
