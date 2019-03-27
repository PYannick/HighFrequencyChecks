
# chk5a_duration: Calculating duration
# chk5b_duration_Xmin: Survey less than Xmin
# chk5c_duration_Xmin_HHSize: Survey less than Xmin taking into account the HH size
# chk5d_duration_outliers: Survey durations outliers


chk5a_duration <- function(ds=NULL, dates=NULL){
  if(is.null(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(dates)){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }

  surveytime <- as.double.difftime((strptime(ds[,dates[2]],"%Y-%m-%dT%R") - strptime(ds[,dates[1]],"%Y-%m-%dT%R")), units = "secs")/60
  avg <- round(mean(surveytime), digits = 2)
  tot <- round(sum(surveytime), digits = 2)
  return(list(avg,tot))
}

chk5b_duration_Xmin <- function(ds=NULL, survey_consent=NULL, dates=NULL, reportingcol=NULL, minduration=30, delete=NULL){
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
  if(is.null(minduration)){
    stop("Please provide the minimum survey time to check against")
  }
  if(is.null(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  tmp<-data.frame(ds[reportingcol], SurveyLength=as.double.difftime((strptime(ds[,dates[2]],"%Y-%m-%dT%R") - strptime(ds[,dates[1]],"%Y-%m-%dT%R")), units = "secs")/60)

  if(delete){
    ds[,survey_consent][tmp$SurveyLength<minduration]<-"deleted"
  }
  errors <- subset(tmp, SurveyLength<minduration)
  return(list(ds,errors))
}

chk5c_duration_Xmin_HHSize <- function(ds=NULL, survey_consent=NULL, dates=NULL, HHSize=NULL, reportingcol=NULL, minduration=10, delete=NULL){
  if(is.null(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates)){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(HHSize)){
    stop("Please provide the field where the HH size is stored")
  }
  if(is.null(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(minduration)){
    stop("Please provide the minimum survey time to check against")
  }
  if(is.null(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  tmp<-data.frame(ds[reportingcol], HHSize=ds[,HHSize], SurveyLength=as.double.difftime((strptime(ds[,dates[2]],"%Y-%m-%dT%R") - strptime(ds[,dates[1]],"%Y-%m-%dT%R")), units = "secs")/60)

  if(delete){
    ds[,survey_consent][(tmp$SurveyLength/tmp$HHSize)<minduration]<-"deleted"
  }
  errors <- subset(tmp, (SurveyLength/HHSize)<minduration)
  return(list(ds,errors))
}

chk5d_duration_outliers <- function(ds=NULL, dates=NULL, sdval=NULL, reportingcol=NULL){
  if(is.null(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(dates)){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(sdval)){
    stop("Please provide the number of standard deviations you want to check for")
  }
  if(is.null(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }

  surveytime <- data.frame(duration=as.double.difftime((strptime(ds[,dates[2]],"%Y-%m-%dT%R") - strptime(ds[,dates[1]],"%Y-%m-%dT%R")), units = "secs")/60)
  duration_outliers <- data.frame(scores(surveytime, type = "z"))
  tmp<-data.frame(ds[,reportingcol],surveytime,duration_outliers)
  colnames(tmp)[length(tmp)]<-"Zscore"
  logf<-subset(tmp, abs(Zscore)>sdval)
  return(logf)
}


