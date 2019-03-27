
# chk3a_date_mistake: Surveys that do not end on the same day as they started
# chk3b_date_mistake: Surveys where end date/time is before the start date/time
# chk3c_date_mistake: Surveys that show start date earlier than first day of data collection
# chk3d_date_mistake: Surveys that have start date/time after system date


chk3a_date_mistake <- function(ds=NULL, survey_consent=NULL, dates=NULL, reportingcol=NULL, delete=NULL){
  if(is.null(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates)){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(delete){
    ds[,survey_consent][stri_datetime_format(strptime(ds[,dates[1]], "%Y-%m-%dT%H:%M:%OS"),"uuuu-MM-dd")!=stri_datetime_format(strptime(ds[,dates[2]], "%Y-%m-%dT%H:%M:%OS"),"uuuu-MM-dd")]<-"deleted"
  }

  errors <- subset(ds,stri_datetime_format(strptime(ds[,dates[1]], "%Y-%m-%dT%H:%M:%OS"),"uuuu-MM-dd")!=stri_datetime_format(strptime(ds[,dates[2]], "%Y-%m-%dT%H:%M:%OS"),"uuuu-MM-dd")) %>%
    select(reportingcol, survey_start=dates[1], survey_end=dates[2])
  return(list(ds,errors))
}

chk3b_date_mistake <- function(ds=NULL, survey_consent=NULL, dates=NULL, reportingcol=NULL, delete=NULL){
  if(is.null(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates)){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(delete){
    ds[,survey_consent][strptime(ds[,dates[1]], "%Y-%m-%dT%H:%M:%OS")>strptime(ds[,dates[2]], "%Y-%m-%dT%H:%M:%OS")]<-"deleted"
  }

  errors <- subset(ds,strptime(ds[,dates[1]], "%Y-%m-%dT%H:%M:%OS")>strptime(ds[,dates[2]], "%Y-%m-%dT%H:%M:%OS")) %>%
    select(reportingcol, survey_start=dates[1], survey_end=dates[2])
  return(list(ds,errors))
}

chk3c_date_mistake <- function(ds=NULL, survey_consent=NULL, dates=NULL, start_collection=NULL, reportingcol=NULL, delete=NULL){
  if(is.null(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates)){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(start_collection)){
    stop("Please provide the date when the data collection began ('yyyy-mm-dd')")
  }
  if(is.null(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(delete){
    ds[,survey_consent][start_collection>stri_datetime_format(strptime(ds[,dates[1]], "%Y-%m-%dT%H:%M:%OS"),"uuuu-MM-dd")]<-"deleted"
  }

  errors <- subset(ds,start_collection>stri_datetime_format(strptime(ds[,dates[1]], "%Y-%m-%dT%H:%M:%OS"),"uuuu-MM-dd")) %>%
    select(reportingcol, survey_start=dates[1])
  return(list(ds,errors))
}

chk3d_date_mistake <- function(ds=NULL, survey_consent=NULL, dates=NULL, reportingcol=NULL, delete=NULL){
  if(is.null(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates)){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(delete){
    ds[,survey_consent][Sys.Date()<stri_datetime_format(strptime(ds[,dates[1]], "%Y-%m-%dT%H:%M:%OS"),"uuuu-MM-dd")]<-"deleted"
  }

  # TO BE BE CHANGED WITH DYNAMIC COLUMS
  errors <- subset(ds,Sys.Date()<stri_datetime_format(strptime(ds[,dates[1]], "%Y-%m-%dT%H:%M:%OS"),"uuuu-MM-dd")) %>%
    select(reportingcol, survey_start=dates[1])
  return(list(ds,errors))
}

