#' @name chk3a_date_mistake
#' @rdname chk3a_date_mistake
#' @title Surveys that do not end on the same day as they started
#' @description This function check that all interviews in the dataset start and end the same day.
#' There is an option to automatically mark for deletion the surveys which have different starting and ending dates.
#'
#' @param ds dataset as a data.frame object
#' @param dates fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
#' @param survey_consent name as a string of the field in the dataset where the survey consent is stored
#' @param reportingcol columns as a list of string name from the dataset you want in the result (c('col1','col2',...))
#' @param delete delete action to be done as a boolean (TRUE/FALSE)
#'
#' @return  ds same dataset as the inputed one but with survey marked for deletion if errors are found and delete=TRUE
#' @return  errors  list of the errors found
#'
#' @author Yannick Pascaud
#'
#' @examples
#' {
#' ds <- HighFrequencyChecks::sample_dataset
#' survey_consent <- "survey_consent"
#' dates <- c("survey_start","end_survey")
#' reportingcol <- c("enumerator_id","X_uuid")
#' delete <- FALSE
#'
#'
#' list_date_mistake <- chk3a_date_mistake(ds, survey_consent,dates, reportingcol, delete)
#' head(list_date_mistake[[2]], 10)
#'}
#' @export chk3a_date_mistake

chk3a_date_mistake <- function(ds=NULL,
                               survey_consent=NULL,
                               dates=NULL,
                               reportingcol=NULL,
                               delete=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete) | !is.logical(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(delete){
    ds[,survey_consent][stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[1]])),"uuuu-MM-dd")!= stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[2]])),"uuuu-MM-dd")]<-"deleted"
  }


  errors <- subset(ds, stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[1]])),"uuuu-MM-dd") != stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[2]])),"uuuu-MM-dd")) %>%
    select(reportingcol, survey_start=dates[1], survey_end=dates[2])
  return(list(ds,errors,NULL,NULL))
}

#' @name chk3b_date_mistake
#' @rdname chk3b_date_mistake
#' @title Surveys where end date/time is before the start date/time
#' @description This function check that all interviews in the dataset start before they end.
#' There is an option to automatically mark for deletion the surveys which have an ending date/time before the starting ones.
#'
#' @param ds dataset as a data.frame object
#' @param dates fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
#' @param survey_consent name as a string of the field in the dataset where the survey consent is stored
#' @param reportingcol columns as a list of string name from the dataset you want in the result (c('col1','col2',...))
#' @param delete delete action to be done as a boolean (TRUE/FALSE)
#'
#' @return  ds same dataset as the inputed one but with survey marked for deletion if errors are found and delete=TRUE
#' @return  errors  list of the errors found
#'
#' @author Yannick Pascaud
#'
#' @examples
#' {
#' ds <- HighFrequencyChecks::sample_dataset
#' survey_consent <- "survey_consent"
#' dates <- c("survey_start","end_survey")
#' reportingcol <- c("enumerator_id","X_uuid")
#' delete <- FALSE
#'
#'
#' list_date_mistake2 <- chk3b_date_mistake(ds, survey_consent,dates, reportingcol, delete)
#' head(list_date_mistake2[[2]], 10)
#'}
#' @export chk3b_date_mistake

chk3b_date_mistake <- function(ds=NULL,
                               survey_consent=NULL,
                               dates=NULL,
                               reportingcol=NULL,
                               delete=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete) | !is.logical(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(delete){
    ds[,survey_consent][readr::parse_datetime(as.character(ds[,dates[1]])) > readr::parse_datetime(as.character(ds[,dates[2]]))]<-"deleted"
  }



  errors <- subset(ds, readr::parse_datetime(as.character(ds[,dates[1]])) > readr::parse_datetime(as.character(ds[,dates[2]]))) %>%
              select(reportingcol, survey_start=dates[1], survey_end=dates[2])
  return(list(ds,errors,NULL,NULL))

}


#' @name chk3c_date_mistake
#' @rdname chk3c_date_mistake
#' @title Surveys that show start date earlier than first day of data collection
#' @description This function check that all interviews in the dataset start after the actual first day of data collection.
#' There is an option to automatically mark for deletion the surveys which have started before the first day of data collection.
#'
#' @param ds dataset as a data.frame object
#' @param dates fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
#' @param survey_consent name as a string of the field in the dataset where the survey consent is stored
#' @param start_collection date as a string of the first day of data collection ('yyyy-mm-dd')
#' @param reportingcol columns as a list of string name from the dataset you want in the result (c('col1','col2',...))
#' @param delete delete action to be done as a boolean (TRUE/FALSE)
#'
#' @return  ds same dataset as the inputed one but with survey marked for deletion if errors are found and delete=TRUE
#' @return  errors  list of the errors found
#'
#' @author Yannick Pascaud
#'
#' @examples
#' {
#' ds <- HighFrequencyChecks::sample_dataset
#' dates <- c("survey_start","end_survey")
#' survey_consent <- "survey_consent"
#' start_collection <- "2018-11-11"
#' reportingcol <- c("enumerator_id","X_uuid")
#' delete <- FALSE
#'
#'
#' list_date_mistake3 <- chk3c_date_mistake(ds, dates, survey_consent,start_collection, reportingcol, delete)
#' head(list_date_mistake3[[2]], 10)
#'}
#' @export chk3c_date_mistake


chk3c_date_mistake <- function(ds = NULL,
                               dates = NULL,
                               survey_consent = NULL,
                               start_collection = NULL,
                               reportingcol = NULL,
                               delete = NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(start_collection) | !is.character(start_collection)){
    stop("Please provide the date when the data collection began ('yyyy-mm-dd')")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete) | !is.logical(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(delete){
    ds[,survey_consent][start_collection > stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[1]])),"uuuu-MM-dd")]<-"deleted"
  }


  errors <- subset(ds,start_collection > stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[1]])),"uuuu-MM-dd")) %>%
    select(reportingcol, survey_start=dates[1])
  return(list(ds,errors,NULL,NULL))

}


#' @name chk3d_date_mistake
#' @rdname chk3d_date_mistake
#' @title Surveys that have start date/time after system date
#' @description This function check that all interviews in the dataset do not start after the current date.
#' There is an option to automatically mark for deletion the surveys which have a start date in the future.
#'
#' @param ds dataset as a data.frame object
#' @param dates fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
#' @param survey_consent name as a string of the field in the dataset where the survey consent is stored
#' @param reportingcol columns as a list of string name from the dataset you want in the result (c('col1','col2',...))
#' @param delete delete action to be done as a boolean (TRUE/FALSE)
#'
#' @return  ds same dataset as the inputed one but with survey marked for deletion if errors are found and delete=TRUE
#' @return  errors  list of the errors found
#'
#' @author Yannick Pascaud
#'
#' @examples
#' {
#' ds <- HighFrequencyChecks::sample_dataset
#' dates <- c("survey_start","end_survey")
#' survey_consent <- "survey_consent"
#' reportingcol <- c("enumerator_id","X_uuid")
#' delete <- FALSE
#'
#'
#' list_date_mistake4 <- chk3d_date_mistake(ds, uuid, survey_consent,reportingcol, delete)
#' head(list_date_mistake4[[2]], 10)
#'}
#' @export chk3d_date_mistake


chk3d_date_mistake <- function(ds=NULL,
                               survey_consent=NULL,
                               dates=NULL,
                               reportingcol=NULL,
                               delete=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete) | !is.logical(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(delete){
    ds[,survey_consent][Sys.Date() < stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[1]])),"uuuu-MM-dd")]<-"deleted"
  }

  # TO BE BE CHANGED WITH DYNAMIC COLUMS

  errors <- subset(ds,Sys.Date() < stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[1]])),"uuuu-MM-dd")) %>%
    select(reportingcol, survey_start=dates[1])
  return(list(ds,errors,NULL,NULL))

}
