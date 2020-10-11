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

ReportWrapper(ds,
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
