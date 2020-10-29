ds <- HighFrequencyChecks::sample_dataset

survey_consent <- "survey_consent"
reportingcol <- c("enumerator_id","X_uuid")
UniqueID <- "X_uuid"

## Setting dates
dates <- c("survey_start","end_survey")
surveydate <- "survey_date"
start_collection <- "2018-11-11"
dateformat <- "%m/%d/%Y"
minduration <- 30
sdval <- 2
HHSize <-"consent_received.respondent_info.hh_size"


## Setting action on dataset
delete <- FALSE
correct <- FALSE

## Setting enumerator
enumeratorID <- "enumerator_id"
enumeratorcheck <- FALSE
otherpattern <- "_other$"

## Setting linked geodata
adm <- HighFrequencyChecks::admin
ds_site <- "union_name"
ds_coord <- c("X_gps_reading_longitude","X_gps_reading_latitude")
adm_site <- "Union"
pts <- HighFrequencyChecks::SamplePts
buff <- 10

## setting questions to check
questions <-c("consent_received.food_security.spend_food",
      "consent_received.food_security.spend_medication",
      "consent_received.food_security.spend_education",
      "consent_received.food_security.spend_fix_shelter",
      "consent_received.food_security.spend_clothing",
      "consent_received.food_security.spend_hygiene",
      "consent_received.food_security.spend_fuel",
      "consent_received.food_security.spend_hh_items",
      "consent_received.food_security.spend_transport",
      "consent_received.food_security.spend_communication",
      "consent_received.food_security.spend_tobacco",
      "consent_received.food_security.spend_rent",
      "consent_received.food_security.spend_debts",
      "consent_received.food_security.spend_other")
value <- 25000
minnbanswers <- 3


## setting sampling plan
sf <- HighFrequencyChecks::SampleSize
dssite <- "union_name"
sfsite <- "Union"
survey_consent <- "survey_consent"
sftarget <- "SS"
sfnbpts <- "TotPts"
formul <- c("done-no-not_eligible-delete",
               "done-no-not_eligible-deleted-SS")
colorder <- c("site","SS","Provisio","done","not_eligible",
                 "no","deleted","yes","final","variance")


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


log <- enumeratorSurveysConsent(ds, survey_consent, enumeratorID)
head(log[[2]],10)

log <- enumeratorSurveysDuration(ds, dates, enumeratorID)
head(log[[2]],10)

log <- enumeratorProductivity(ds, surveydate, enumeratorID)
head(log[[2]],10)

log <- enumeratorProductivityOutliers(ds, enumeratorID, surveydate, sdval)
head(log[[2]],10)

log <- enumeratorIsLazy(ds, enumeratorID, questions, minnbanswers)
head(log[[2]],10)

log <- assessmentProductivity(ds, surveydate, dateformat, survey_consent)
head(log[[2]],10)

assessmentProductivityGraphical(ds, surveydate, dateformat, survey_consent)

log <- assessmentDailyValidSurveys(ds, surveydate, dateformat, survey_consent )
head(log[[2]],10)

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
