
# chk7ai_productivity: Summary of daily average productivity
# chk7aii_productivity_hist: Overall productivity histogram
# chk7bi_nb_status: Daily number of survey with per consent status
# chk7bii_tracking: Overall tracking sheet


chk7ai_productivity <- function(ds=NULL, surveydate=NULL, dateformat=NULL, survey_consent=NULL){
  if(is.null(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveydate)){
    stop("Please provide the field where the survey date is stored")
  }
  if(is.null(dateformat)){
    stop("Please provide the format used for the date ('%m/%d/%Y')")
  }
  if(is.null(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }

  tmp<-ds %>%
      group_by_(surveydate=surveydate) %>%
      summarize(NbSurvey=n())
  tmp$surveydate<-as.Date(tmp$surveydate, dateformat)
  logf<-tmp[with(tmp, order(surveydate)), ]
  return(logf)
}

chk7aii_productivity_hist <- function(ds=NULL, surveydate=NULL, dateformat=NULL, survey_consent=NULL){
  if(is.null(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveydate)){
    stop("Please provide the field where the survey date is stored")
  }
  if(is.null(dateformat)){
    stop("Please provide the format used for the date ('%m/%d/%Y')")
  }
  if(is.null(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }

  tmp<-ds %>% group_by_(surveydate=surveydate) %>% count_(survey_consent)
  colnames(tmp)[2]<-"survey_consent"
  tmp$surveydate<-as.Date(tmp$surveydate, dateformat)
  tmp<-tmp[with(tmp, order(surveydate)), ]
  tmp<-dcast(tmp,surveydate ~ survey_consent, value.var="n")
  tmp[is.na(tmp)]<-0

  graph<-plot_ly(type = 'bar', width = 1800, height = 900)
  for(i in 2:length(tmp)){
    graph<-add_trace(graph, x=tmp[,1], y = tmp[,i], name = colnames(tmp)[i])
  }
  graph<-layout(graph, yaxis = list(title = 'Nb Survey'), barmode = 'stack')

  return(graph)
}

chk7bi_nb_status <- function(ds=NULL, surveydate=NULL, dateformat=NULL, survey_consent=NULL){
  if(is.null(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveydate)){
    stop("Please provide the field where the survey date is stored")
  }
  if(is.null(dateformat)){
    stop("Please provide the format used for the date ('%m/%d/%Y')")
  }
  if(is.null(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }

  tmp<-ds %>% group_by_(surveydate=surveydate) %>% count_(survey_consent)
  colnames(tmp)[2]<-"survey_consent"
  tmp$surveydate<-as.Date(tmp$surveydate, dateformat)
  tmp<-tmp[with(tmp, order(surveydate)), ]
  logf<-dcast(tmp,surveydate ~ survey_consent, value.var="n")
  logf[is.na(logf)]<-0
  return(logf)
}

chk7bii_tracking <- function(ds=NULL, sf=NULL, dssite=NULL, sfsite=NULL, survcons=NULL, sftarget=NULL, sfnbpts=NULL, formul=NULL, colorder=NULL){
  if(is.null(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(sf)){
    stop("Please provide the sampling frame")
  }
  if(is.null(dssite)){
    stop("Please provide the field where the site is stored in the dataset")
  }
  if(is.null(sfsite)){
    stop("Please provide the field where the site is stored in the sampling frame")
  }
  if(is.null(survcons)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(sftarget)){
    stop("Please provide the field where the target number of survey is stored in the sampling frame")
  }
  if(is.null(sfnbpts)){
    stop("Please provide the field where the number of points generated is stored in the sampling frame")
  }
  if(is.null(formul)){
    stop("Please provide the formulas used to compute the final number of eligible surveys and the variance from the target (C('formula1','formula2'))")
  }
  if(is.null(colorder)){
    stop("Please provide the order the colums have to be displayed in the result (C('col1','col2',...))")
  }

  df1<-data.frame(sf[,sfsite], sf[,sftarget], sf[,sfnbpts])
  colnames(df1)<-c("site",sftarget,sfnbpts)
  ## df2<-data.frame(ds, stringsAsFactors = FALSE) %>% group_by_(dssite) %>% count_(survcons) %>% mutate(done=sum(n))
  ## colnames(df2)[2]<-"site"
  #df2<-ds[,c(dssite,survcons)]
  #colnames(df2)<-c("site","consent")
  #df2$site<-as.character(df2$site)
  #df2$consent<-as.character(df2$consent)
  ## dssite<-lazyeval::lazy(dssite)
  ## survcons<-lazyeval::lazy(survcons)
  df2<-ds %>% group_by_(site=dssite, consent=survcons) %>% summarize(n=n()) %>% mutate(done=sum(n))
  ##df2<-ds %>% group_by(.dots=list(site,consent)) # %>% summarize_(n=n()) %>% mutate(done=sum(n))

  #df2<-ds %>% group_by_(site=dssite) %>% count_(survcons) %>% mutate(done=sum(n))
  df2<-dcast(df2,site + done ~ consent, value.var="n")
  df<-merge(df1,df2, by.x=c("site"), by.y=c("site"), all.x=TRUE)
  df[is.na(df)]<-0

  formul[1]<-paste0("df[,'", stri_replace_all_fixed(formul[1],c("+","-","/","*"),c("'] + df[,'","'] - df[,'","'] / df[,'","'] * df[,'"), vectorize_all=FALSE), "']")
  formul[2]<-paste0("df[,'", stri_replace_all_fixed(formul[2],c("+","-","/","*"),c("'] + df[,'","'] - df[,'","'] / df[,'","'] * df[,'"), vectorize_all=FALSE), "']")
  df$final<-eval(parse(text=formul[1]))
  df$variance<-eval(parse(text=formul[2]))

  logf<-df[colorder]
  return(logf)
}



