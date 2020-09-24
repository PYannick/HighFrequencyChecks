#' @name chk7ai_productivity
#' @rdname chk7ai_productivity
#' @title Summary of daily average productivity
#' @description This function display the number of surveys conducted per day.
#'
#' @param ds dataset as a data.frame object
#' @param surveydate name as a string of the field in the dataset where the date of the survey is stored
#' @param dateformat format as a string used for the date ('\%m/\%d/\%Y')
#' @param survey_consent name as a string of the field in the dataset where the survey consent is stored
#'
#' @return logf  the report
#'
#' @author Yannick Pascaud
#'
#' @examples
#' \dontrun{
#' df <- sample_dataset
#' sdte <- "survey_date"
#' dtf <- "%m/%d/%Y"
#' sc <- "survey_consent"
#'
#' chk7ai_productivity(df, sdte, dtf, sc)
#'}
#' @export chk7ai_productivity

chk7ai_productivity <- function(ds=NULL, surveydate=NULL, dateformat=NULL, survey_consent=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveydate) | !is.character(surveydate)){
    stop("Please provide the field where the survey date is stored")
  }
  if(is.null(dateformat) | !is.character(dateformat)){
    stop("Please provide the format used for the date ('%m/%d/%Y')")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }

  tmp<-ds %>%
      group_by_(surveydate=surveydate) %>%
      summarize(NbSurvey=n())
  tmp$surveydate<-as.Date(tmp$surveydate, dateformat)
  logf<-tmp[with(tmp, order(surveydate)), ]
  return(logf)
}

#' @name chk7aii_productivity_hist
#' @rdname chk7aii_productivity_hist
#' @title Overall productivity histogram
#' @description This function create an histogram showing the overall productivity per consent status per day.
#'
#' @param ds dataset as a data.frame object
#' @param surveydate name as a string of the field in the dataset where the date of the survey is stored
#' @param dateformat format as a string used for the date ('\%m/\%d/\%Y')
#' @param survey_consent name as a string of the field in the dataset where the survey consent is stored
#'
#' @return graph  graphic as a plot.ly object
#'
#' @author Yannick Pascaud
#'
#' @examples
#' \dontrun{
#' df <- sample_dataset
#' sdte <- \"survey_date\"
#' dtf <- \"\%m/\%d/\%Y\"
#' sc <- \"survey_consent\"
#'
#' chk7aii_productivity_hist(df, sdte, dtf, sc)
#'}
#' @export chk7aii_productivity_hist

chk7aii_productivity_hist <- function(ds=NULL, surveydate=NULL, dateformat=NULL, survey_consent=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveydate) | !is.character(surveydate)){
    stop("Please provide the field where the survey date is stored")
  }
  if(is.null(dateformat) | !is.character(dateformat)){
    stop("Please provide the format used for the date ('%m/%d/%Y')")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }

  tmp <- ds %>% group_by_(surveydate=surveydate) %>% count_(survey_consent)
  colnames(tmp)[2] <- "survey_consent"
  tmp$surveydate <- as.Date(tmp$surveydate, dateformat)
  tmp <- tmp[with(tmp, order(surveydate)), ]
  tmp <- reshape2::dcast(tmp,surveydate ~ survey_consent, value.var="n")
  tmp[is.na(tmp)] <- 0

  graph <- plotly::plot_ly(type = 'bar', width = 1800, height = 900)
  for(i in 2:length(tmp)){
    graph<-add_trace(graph, x=tmp[,1], y = tmp[,i], name = colnames(tmp)[i])
  }
  graph <- plotly::layout(graph,
                  yaxis = list(title = "Nb Survey"),
                  barmode = "stack")

  return(graph)
}


#' @name chk7bi_nb_status
#' @rdname chk7bi_nb_status
#' @title Daily number of survey per consent status
#' @description This function display the number of surveys conducted per day per constent status.
#'
#' @param ds dataset as a data.frame object
#' @param surveydate name as a string of the field in the dataset where the date of the survey is stored
#' @param dateformat format as a string used for the date ('\%m/\%d/\%Y')
#' @param survey_consent name as a string of the field in the dataset where the survey consent is stored
#'
#' @return logf  the report
#'
#' @author Yannick Pascaud
#'
#' @examples
#' \dontrun{
#' df <- sample_dataset
#' sdte <- \"survey_date\"
#' dtf <- \"\%m/\%d/\%Y\"
#' sc <- \"survey_consent\"
#'
#' chk7bi_nb_status(df, sdte, dtf, sc)
#'}
#'
#' @export chk7bi_nb_status

chk7bi_nb_status <- function(ds=NULL, surveydate=NULL, dateformat=NULL, survey_consent=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveydate) | !is.character(surveydate)){
    stop("Please provide the field where the survey date is stored")
  }
  if(is.null(dateformat) | !is.character(dateformat)){
    stop("Please provide the format used for the date ('%m/%d/%Y')")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }

  tmp <- ds %>% group_by_(surveydate=surveydate) %>% count_(survey_consent)
  colnames(tmp)[2] <- "survey_consent"
  tmp$surveydate <- as.Date(tmp$surveydate, dateformat)
  tmp <- tmp[with(tmp, order(surveydate)), ]
  logf <- reshape2::dcast(tmp,surveydate ~ survey_consent, value.var="n")
  logf[is.na(logf)] <- 0
  return(logf)
}

#' @name chk7bii_tracking
#' @rdname chk7bii_tracking
#' @title Overall tracking sheet
#' @description This function display the overall tracking sheet.
#'
#' @param ds dataset as a data.frame object
#' @param sf sampling frame as a data.frame object
#' @param dssite  name as a string of the field in the dataset where the site is stored
#' @param sfsite name as a string of the field in the sampling frame where the site is stored
#' @param survcons name as a string of the field in the dataset where the survey consent is stored
#' @param sftarget name as a string of the field where the target number of survey is stored in the sampling frame
#' @param sfnbpts  name as a string of the field where the number of points generated is stored in the sampling frame
#' @param formul  formulas as a list of string used to compute the final number of eligible surveys and the variance from the target (C('formula1','formula2')).
#'   the values/fields available are: done and the ones generated according the survey consent values (one per value)
#' @param colorder  column names as a list of string to order the colums in the result (C('col1','col2',...)).
#'  the columns available are: site, done, final, variance and the ones generated according the survey consent values (one per value)
#'
#' @return logf  the report
#'
#' @author Yannick Pascaud
#'
#' @examples
#' \dontrun{
#' df <- sample_dataset
#' df <- sample_dataset
#' sf <- SampleSize
#' dssite <- \"union_name\"
#' sfsite <- \"Union\"
#' sc <- \"survey_consent\"
#' sftarget <- \"SS\"
#' sfnbpts <- \"TotPts\"
#' #formul <- c(\"done-no-not_eligible-delete\",
#' #               \"done-no-not_eligible-deleted-SS\")
#' #colorder <- c(\"site\",\"SS\",\"Provisio\",\"done\",\"not_eligible\",
#' #                 \"no\",\"deleted\",\"yes\",\"final\",\"variance\")
#'
#' chk7bii_tracking(df, sf, dssite, sfsite, sc, sftarget,
#'                   sfnbpts, formul, colorder)
#'}
#' @export chk7bii_tracking
#'

chk7bii_tracking <- function(ds=NULL, sf=NULL, dssite=NULL, sfsite=NULL, survcons=NULL, sftarget=NULL, sfnbpts=NULL, formul=NULL, colorder=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(sf) | nrow(sf)==0 | !is.data.frame(sf)){
    stop("Please provide the sampling frame")
  }
  if(is.null(dssite) | !is.character(dssite)){
    stop("Please provide the field where the site is stored in the dataset")
  }
  if(is.null(sfsite) | !is.character(sfsite)){
    stop("Please provide the field where the site is stored in the sampling frame")
  }
  if(is.null(survcons) | !is.character(survcons)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(sftarget) | !is.character(sftarget)){
    stop("Please provide the field where the target number of survey is stored in the sampling frame")
  }
  if(is.null(sfnbpts) | !is.character(sfnbpts)){
    stop("Please provide the field where the number of points generated is stored in the sampling frame")
  }
  if(is.null(formul) | !is.character(formul) | length(formul)!=2){
    stop("Please provide the formulas used to compute the final number of eligible surveys and the variance from the target (C('formula1','formula2'))")
  }
  if(is.null(colorder) | !is.character(colorder)){
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
  df2 <-reshape2::dcast(df2,site + done ~ consent, value.var="n")
  df <- merge(df1,df2, by.x=c("site"), by.y=c("site"), all.x=TRUE)
  df[is.na(df)] <- 0

  formul[1] <- paste0("df[,'", stringi::stri_replace_all_fixed(formul[1],c("+","-","/","*"),c("'] + df[,'","'] - df[,'","'] / df[,'","'] * df[,'"), vectorize_all=FALSE), "']")
  formul[2] <- paste0("df[,'", stringi::stri_replace_all_fixed(formul[2],c("+","-","/","*"),c("'] + df[,'","'] - df[,'","'] / df[,'","'] * df[,'"), vectorize_all=FALSE), "']")
  df$final <- eval(parse(text=formul[1]))
  df$variance <- eval(parse(text=formul[2]))

  logf <- df[colorder]
  return(logf)
}
