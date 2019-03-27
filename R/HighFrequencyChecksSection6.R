
# chk6a_refusal: Check the percentage of survey refusals by enumerator
# chk6b_duration: Check the average interview duration by enumerator
# chk6c_nb_survey: Check the number of surveys per day by enumerator
# chk6f_productivity: Surveyors with very low or high productivity
# chk6g_question_less_X_answers: Enumerators who pick up less than X answers per question


chk6a_refusal <- function(ds=NULL, survey_consent=NULL, enumeratorID=NULL){
  if(is.null(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(enumeratorID)){
    stop("Please provide the field where the enumerator ID is stored")
  }

  tmp<-(ds %>% group_by(enumeratorID=ds[,enumeratorID]) %>% count(survey_consent) %>% mutate(pct=round(100*n/sum(n), digits=2)))[-3]
  logf<-dcast(tmp,enumeratorID ~ survey_consent, value.var="pct")
  logf[is.na(logf)]<-0
  return(logf)
}

chk6b_duration <- function(ds=NULL, dates=NULL, enumeratorID=NULL){
  if(is.null(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(dates)){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(enumeratorID)){
    stop("Please provide the field where the enumerator ID is stored")
  }

  ds$surveytime <- as.double.difftime((strptime(ds[,dates[2]],"%Y-%m-%dT%R") - strptime(ds[,dates[1]],"%Y-%m-%dT%R")), units = "secs")/60
  overall_avg_duration <- round(mean(ds$surveytime), digits=2)
  logf<-ds %>% group_by(enumeratorID=ds[,enumeratorID]) %>% summarize(duration_mean = round(mean(surveytime), digits=2),
                                                   overall_avg_duration,
                                                   perc_diff_avg = round(((duration_mean - overall_avg_duration) / overall_avg_duration) * 100, digits=2))
  return(logf)
}

chk6c_nb_survey <- function(ds=NULL, surveydate=NULL, enumeratorID=NULL){
  if(is.null(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveydate)){
    stop("Please provide the field where the survey date is stored")
  }
  if(is.null(enumeratorID)){
    stop("Please provide the field where the enumerator ID is stored")
  }

  logf<-ds %>%
    group_by_(enumeratorID) %>%
    summarize_(days_worked = interp(~length(unique(var)), var = as.name(surveydate)), total_surveys_done = ~n()) %>%
    mutate(daily_average = round(total_surveys_done/days_worked, digits = 2))
  return(logf)
}

chk6f_productivity <- function(ds=NULL, enumeratorID=NULL, surveydate=NULL, sdval=NULL){
  if(is.null(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(enumeratorID)){
    stop("Please provide the field where the enumerator ID is stored")
  }
  if(is.null(surveydate)){
    stop("Please provide the field where the survey date is stored")
  }
  if(is.null(sdval)){
    stop("Please provide the number of standard deviations you want to check for")
  }

  tmp<-ds %>%
    group_by_(enumeratorID) %>%
    summarize_(days_worked = interp(~length(unique(var)), var = as.name(surveydate)), total_surveys_done = ~n()) %>%
    mutate(daily_average = total_surveys_done/days_worked)
  survey_outliers<-scores(tmp$daily_average, type = "z")
  tmp<-data.frame(tmp,survey_outliers)
  logf<-subset(tmp, abs(survey_outliers)>sdval)
  return(logf)
}

chk6g_question_less_X_answers <- function(ds=NULL, enumeratorID=NULL, questions=NULL, minnbanswers=NULL){
  if(is.null(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(enumeratorID)){
    stop("Please provide the field where the enumerator ID is stored")
  }
  if(is.null(questions)){
    stop("Please provide the fields you want to check for (c('field1[.]','field2[.]',...))")
  }
  if(is.null(minnbanswers)){
    stop("Please provide the minimum number of expected answers")
  }

  tmp<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("enumeratorID","NbErr","field"))
  for(i in questions){
    tmp<-rbind(tmp,data.frame(data.frame(ds, nb=rowSums(ds[,colnames(ds) %like% i], na.rm=TRUE)) %>%
                       group_by(enumeratorID=ds[,enumeratorID]) %>%
                       summarize(NbErr=sum(nb<minnbanswers)), field=stri_replace_all_fixed(i, "[.]", "")))
  }
  logf<-tmp
  return(logf)
}





