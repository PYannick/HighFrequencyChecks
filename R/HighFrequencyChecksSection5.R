#' @name chk5a_duration
#' @rdname chk5a_duration
#' @title Compute the average and total time for the surveys
#' @description This function compute the average and total time for the surveys
#' Warning: If there are uncorrected mistakes in the survey dates, it can lead to have the length of the survey in seconds and this check will not performed well
#'
#' @param ds dataset as a data.frame object
#' @param dates fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
#'
#' @return avg  average time per survey
#' @return tot total time
#'
#' @author Yannick Pascaud
#'
#' @examples
#' \dontrun{
#' df <- HighFrequencyChecks::sample_dataset
#' dt <- c("survey_start","end_survey")
#'
#' chk5a_duration(df, dt)
#'}
#' @export chk5a_duration

chk5a_duration <- function(ds=NULL, dates=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }

  surveytime <- as.double.difftime((strptime(ds[,dates[2]],"%Y-%m-%dT%R") - strptime(ds[,dates[1]],"%Y-%m-%dT%R")), units = "secs")/60
  avg <- round(mean(surveytime), digits = 2)
  tot <- round(sum(surveytime), digits = 2)
  return(list(avg,tot))
}

#' @name chk5b_duration_Xmin
#' @rdname chk5b_duration_Xmin
#' @title Check that the duration of each interview is more than a threshold
#' @description This function check that the duration of each interview is more than a specified threshold.
#' There is an option to automatically mark for deletion the surveys which are under the threshold.
#' Warning: If there are uncorrected mistakes in the survey dates, it can lead to have the length of the survey in seconds and this check will not performed well
#'
#' @param ds dataset as a data.frame object
#' @param dates fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
#' @param minduration minimum acceptable survey duration as integer in minutes
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
#' \dontrun{
#' df <- HighFrequencyChecks::sample_dataset
#' sc <- "survey_consent"
#' dt <- c("survey_start","end_survey")
#' rc <- c("enumerator_id","X_uuid")
#' md <- 30
#' dl <- FALSE
#'
#' list[dts,error] <- chk5b_duration_Xmin(df, sc, dt, rc, md, dl)
#' head(error,10)
#'}
#' @export chk5b_duration_Xmin

chk5b_duration_Xmin <- function(ds=NULL, survey_consent=NULL, dates=NULL, reportingcol=NULL, minduration=30, delete=NULL){
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
  if(is.null(minduration) | !is.numeric(minduration)){
    stop("Please provide the minimum survey time to check against")
  }
  if(is.null(delete) | !is.logical(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  tmp<-data.frame(ds[reportingcol], SurveyLength=as.double.difftime((strptime(ds[,dates[2]],"%Y-%m-%dT%R") - strptime(ds[,dates[1]],"%Y-%m-%dT%R")), units = "secs")/60)

  if(delete){
    ds[,survey_consent][tmp$SurveyLength<minduration]<-"deleted"
  }
  errors <- subset(tmp, SurveyLength<minduration)
  return(list(ds,errors))
}


#' @name chk5c_duration_Xmin_HHSize
#' @rdname chk5c_duration_Xmin_HHSize
#' @title Check that the duration relative to the household size of each interview is more than a threshold
#' @description This function check that the duration relative to the household size of each interview is more than a specified threshold.
#' There is an option to automatically mark for deletion the surveys which are under the threshold.
#' Warning: If there are uncorrected mistakes in the survey dates, it can lead to have the length of the survey in seconds and this check will not performed well
#'
#' @param ds dataset as a data.frame object
#' @param dates fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
#' @param minduration minimum acceptable survey duration as integer in minutes
#' @param HHSize name as a string of the field in the dataset where the household size is stored
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
#' \dontrun{
#' df <- HighFrequencyChecks::sample_dataset
#' sc <- "survey_consent"
#' hs <-"consent_received.respondent_info.hh_size"
#' dt <- c("survey_start","end_survey")
#' rc <- c("enumerator_id","X_uuid")
#' md <- 30
#' dl <- FALSE
#'
#' list[dts,error] <- chk5c_duration_Xmin_HHSize(df, sc, dt, hs, rc, md, dl)
#' head(error,10)
#'}
#' @export chk5c_duration_Xmin_HHSize

chk5c_duration_Xmin_HHSize <- function(ds=NULL, survey_consent=NULL, dates=NULL, HHSize=NULL, reportingcol=NULL, minduration=10, delete=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(HHSize) | !is.character(HHSize)){
    stop("Please provide the field where the HH size is stored")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(minduration) | !is.numeric(minduration)){
    stop("Please provide the minimum survey time to check against")
  }
  if(is.null(delete) | !is.logical(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  tmp<-data.frame(ds[reportingcol], HHSize=ds[,HHSize], SurveyLength=as.double.difftime((strptime(ds[,dates[2]],"%Y-%m-%dT%R") - strptime(ds[,dates[1]],"%Y-%m-%dT%R")), units = "secs")/60)

  if(delete){
    ds[,survey_consent][(tmp$SurveyLength/tmp$HHSize)<minduration]<-"deleted"
  }
  errors <- subset(tmp, (SurveyLength/HHSize)<minduration)
  return(list(ds,errors))
}

#' @name chk5d_duration_outliers
#' @rdname chk5d_duration_outliers
#' @title Report the outlier durations for the surveys
#' @description This function report the outlier durations for the surveys
#'
#' @param ds dataset as a data.frame object
#' @param dates fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
#' @param sdval number of standard deviation for which the duration within is considered as acceptable
#' @param reportingcol columns as a list of string name from the dataset you want in the result (c('col1','col2',...))
#'
#' @return logf  the report
#'
#' @author Yannick Pascaud
#'
#' @examples
#' \dontrun{
#' df <- HighFrequencyChecks::sample_dataset
#' dt <- c("survey_start","end_survey")
#'
#' log <- chk5d_duration_outliers(df, dt)
#' head(log,10)
#'}
#' @export chk5d_duration_outliers

chk5d_duration_outliers <- function(ds=NULL, dates=NULL, sdval=NULL, reportingcol=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(sdval) | !is.numeric(sdval)){
    stop("Please provide the number of standard deviations you want to check for")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }

  surveytime <- data.frame(duration=as.double.difftime((strptime(ds[,dates[2]],"%Y-%m-%dT%R") - strptime(ds[,dates[1]],"%Y-%m-%dT%R")), units = "secs")/60)
  duration_outliers <- data.frame(outliers::scores(surveytime, type = "z"))
  tmp <- data.frame(ds[,reportingcol],surveytime,duration_outliers)
  colnames(tmp)[length(tmp)] <- "Zscore"
  logf <- subset(tmp, abs(Zscore)>sdval)
  return(logf)
}
