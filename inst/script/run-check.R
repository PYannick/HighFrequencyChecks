library(HighFrequencyChecks)
library(readr)
library(magrittr)

## pull live data feed from your kobo project
# devtools::install_github("UNHCR-WEB/hcrdata")
library(hcrdata)
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



# formulas as a list of string used to compute the final number
# of eligible surveys  and the variance from the target
# (C('formula1','formula2')).
# the values/fields available are: done and the ones generated
# according the survey consent values (one per value)
formul = c("done-no-not_eligible-deleted",
           "done-no-not_eligible-deleted-SS")


## consent #####



survey_consent <- "ConsentScore"


ds$uuid <- ds$"_uuid"
UniqueID <- "uuid"


## Variable of interest ####
reportingcol <- c("username","uuid")
View(ds[, c("username","uuid")])

levels(as.factor(ds$username))
enumeratorID <- "username"
#'
## Setting dates
surveydate <- "today"
ds$today
dateformat <- "%Y/%m/%d"
start_collection <- "2018-11-11"
minduration <- 30
sdval <- 2
#HHSize <-"consent_received.respondent_info.hh_size"
#'
#'
## Setting action on dataset
delete <- FALSE
correct <- FALSE
#'
## Setting enumerator
enumeratorcheck <- FALSE
otherpattern <- "_other$"  ## Dollar to indicate end of variable
#'
## Setting linked geodata
# adm <- HighFrequencyChecks::admin
# ds_site <- "union_name"
# ds_coord <- c("X_gps_reading_longitude","X_gps_reading_latitude")
# adm_site <- "Union"
# pts <- HighFrequencyChecks::SamplePts
# buff <- 10
#'
## setting questions to check
 questions <-c("consent_received.food_security.spend_food",
#       "consent_received.food_security.spend_medication",
#       "consent_received.food_security.spend_education",
#       "consent_received.food_security.spend_fix_shelter",
#       "consent_received.food_security.spend_clothing",
#       "consent_received.food_security.spend_hygiene",
#       "consent_received.food_security.spend_fuel",
#       "consent_received.food_security.spend_hh_items",
#       "consent_received.food_security.spend_transport",
#       "consent_received.food_security.spend_communication",
#       "consent_received.food_security.spend_tobacco",
#       "consent_received.food_security.spend_rent",
#       "consent_received.food_security.spend_debts",
       "consent_received.food_security.spend_other")
 value <- 25000


 minnbanswers <- 3
#'
#'
## setting sampling plan
# sf <- HighFrequencyChecks::SampleSize
# dssite <- "union_name"
# sfsite <- "Union"
# sftarget <- "SS"
# sfnbpts <- "TotPts"

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
#'
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
              correct,
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


list_iscompleted <- isInterviewCompleted(ds,
                                         survey_consent,
                                         dates,
                                         reportingcol,
                                         delete)
head(list_iscompleted[[2]],10)

list_withconsent <- isInterviewWithConsent(ds,
                                           survey_consent,
                                           reportingcol,
                                           delete)
head(list_withconsent[[2]],10)

list_site <- isInterviewInTheCorrectSite(ds,
                                         adm,
                                         ds_site,
                                         ds_coord,
                                         adm_site,
                                         survey_consent,
                                         reportingcol,
                                         correct)
head(list_site[[2]], 10)

list_sitept  <- isInterviewAtTheSamplePoint( ds, pts, ds_coord, buff, survey_consent, reportingcol, delete)
head(list_sitept[[2]], 10)

list_missing_id <- isUniqueIDMissing(ds, UniqueID, survey_consent, reportingcol, delete)
head(list_missing_id[[2]], 10)

list_unique_id <- isUniqueIDDuplicated(ds, UniqueID, survey_consent, reportingcol, delete)
head(list_unique_id[[2]], 10)


list_date_mistake <- isSurveyOnMoreThanADay(ds, survey_consent,dates, reportingcol, delete)
head(list_date_mistake[[2]], 10)

list_date_mistake2 <- isSurveyEndBeforeItStarts(ds, survey_consent,dates, reportingcol, delete)
head(list_date_mistake2[[2]], 10)

list_date_mistake3 <- isSurveyStartedBeforeTheAssessment(ds, dates, survey_consent,start_collection, reportingcol, delete)
head(list_date_mistake3[[2]], 10)

list_date_mistake4 <- isSurveyMadeInTheFuture(ds, survey_consent,
                                              dates, reportingcol, delete)
head(list_date_mistake4[[2]], 10)

log <- surveyMissingValues(ds, enumeratorID, enumeratorcheck)
head(log[[2]],10)

log <- surveyDistinctValues(ds, enumeratorID, enumeratorcheck)
head(log[[2]],10)

log <- surveyOtherValues(ds, otherpattern, enumeratorID, enumeratorcheck)
head(log[[2]],10)

log <- surveyOutliers(ds, sdval, reportingcol, enumeratorID, enumeratorcheck)
head(log[[2]],10)

log <- surveyBigValues(ds, questions, value,reportingcol, enumeratorID, enumeratorcheck)
head(log[[2]],10)

assessmentDuration(ds, dates)

list <- isInterviewTooShort(ds, survey_consent, dates,  reportingcol, minduration, delete)
head(list[[2]], 10)

list_duration_Xmin <- isInterviewTooShortForTheHouseholdSize(ds, survey_consent, dates, HHSize, reportingcol, minduration, delete)
head(list_duration_Xmin[[2]], 10)

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

#log <- enumeratorIsLazy(ds, enumeratorID, questions, minnbanswers)
head(log[[2]],10)

log <- assessmentProductivity(ds, surveydate, dateformat, survey_consent)
head(log[[2]],10)

log <- assessmentDailyValidSurveys(ds, surveydate, dateformat, survey_consent )
head(log[[2]],10)
log[[4]]

log <- assessmentTrackingSheet(ds,
                               sf,
                               dssite,
                               sfsite,
                               survey_consent,
                               sftarget,
                               sfnbpts,
                               formul,
                               colorder)
head(log[[2]],10)
