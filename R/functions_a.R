#' @name assessmentDuration
#' @rdname assessmentDuration
#' @title Compute the average and total time for the surveys
#' @description This function compute the average and total time for the surveys
#' Warning: If there are uncorrected mistakes in the survey dates, it can lead to have the length of the survey in seconds and this check will not performed well
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param dates name of the fields where the information about the start and end date of the survey is stored: list of string (c('start_date','end_date'))
#'
#' @return dst      same dataset as the inputed one but with survey marked for deletion if errors are found and delete=TRUE (or NULL)
#' @return ret_log  list of the errors found (or NULL)
#' @return var      a list of value (or NULL)
#' @return graph    graphical representation of the results (or NULL)
#'
#' @author Yannick Pascaud
#'
#' @examples
#' {
#' ds <- HighFrequencyChecks::sample_dataset
#' dates <- c("survey_start","end_survey")
#'
#' list[dst,ret_log,var,graph] <- assessmentDuration(ds=ds,
#'                                                   dates=dates)
#' print(var)
#'}
#' @export assessmentDuration
assessmentDuration <- function(ds=NULL,
                               dates=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }

  surveytime <- as.double.difftime((readr::parse_datetime(as.character(ds[,dates[2]])) -
                                    readr::parse_datetime(as.character(ds[,dates[1]]))),
                                    units = "secs") / 60
  # surveytime <- as.double.difftime((strptime(ds[,dates[2]],"%Y-%m-%dT%R") - strptime(ds[,dates[1]],"%Y-%m-%dT%R")), units = "secs")/60

  # avg <- round(mean(surveytime), digits = 2)
  # tot <- round(sum(surveytime), digits = 2)
  msg<-paste0("The total time of data collection is ", round(sum(surveytime), digits = 2),
         " minutes and the average time per survey is ", round(mean(surveytime), digits = 2), " minutes")
  return(list(NULL,NULL,msg,NULL))
}

#' @name assessmentDurationOutliers
#' @rdname assessmentDurationOutliers
#' @title Report the outlier durations for the surveys
#' @description This function report the outlier durations for the surveys
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param dates name of the fields where the information about the start and end date of the survey is stored: list of string (c('start_date','end_date'))
#' @param sdval (Optional, by default set to 2) number of standard deviation for which the data within is considered as acceptable: integer
#' @param uniqueID name of the field where the survey unique ID is stored: string
#' @param enumeratorID name of the field where the enumerator ID is stored: string
#' @param reportingColumns (Optional, by default it is built from the enumeratorID and the UniqueID) name of the columns from the dataset you want in the result: list of string (c('col1','col2',...))
#'
#' @return dst      same dataset as the inputed one but with survey marked for deletion if errors are found and delete=TRUE (or NULL)
#' @return ret_log  list of the errors found (or NULL)
#' @return var      a list of value (or NULL)
#' @return graph    graphical representation of the results (or NULL)
#'
#' @author Yannick Pascaud
#'
#' @examples
#' {
#' ds <- HighFrequencyChecks::sample_dataset
#' dates <- c("survey_start","end_survey")
#' uniqueID <- "X_uuid"
#' enumeratorID <- "enumerator_id"
#' reportingColumns <- c(enumeratorID, uniqueID)
#' sdval<-2
#'
#' list[dst,ret_log,var,graph] <- assessmentDurationOutliers(ds=ds,
#'                                                           dates=dates,
#'                                                           sdval=sdval,
#'                                                           reportingColumns=reportingColumns)
#' head(ret_log,10)
#' print(graph)
#'}
#' @export assessmentDurationOutliers
assessmentDurationOutliers <- function(ds=NULL,
                                       dates=NULL,
                                       sdval=2,
                                       reportingColumns=c(enumeratorID, uniqueID)){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(sdval) | !is.numeric(sdval)){
    stop("Please provide the number of standard deviations you want to check for")
  }
  if(is.null(reportingColumns) | !is.character(reportingColumns)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }

  surveytime <- data.frame(duration= as.double.difftime((readr::parse_datetime(as.character(ds[,dates[2]])) -
                                                         readr::parse_datetime(as.character(ds[,dates[1]]))),
                                                         units = "secs") / 60)
  # surveytime <- data.frame(duration=as.double.difftime((strptime(ds[,dates[2]],"%Y-%m-%dT%R") - strptime(ds[,dates[1]],"%Y-%m-%dT%R")), units = "secs")/60)
  duration_outliers <- data.frame(outliers::scores(surveytime, type = "z"))
  tmp <- data.frame(ds[,reportingColumns],surveytime,duration_outliers)
  colnames(tmp)[length(tmp)] <- "Zscore"
  logf <- subset(tmp, abs(Zscore)>sdval)
  graph <- ggplot2::ggplot(surveytime) + ggplot2::geom_boxplot(ggplot2::aes(duration), outlier.colour = "red") +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.y=element_blank(),
                   axis.line.y=element_blank(),
                   axis.ticks.y=element_blank(),
                   panel.grid.major.y=element_blank(),
                   panel.grid.minor.y=element_blank())
  return(list(NULL,logf,NULL,graph))
}

#' @name assessmentProductivity
#' @rdname assessmentProductivity
#' @title Summary of daily average productivity
#' @description This function display the number of surveys conducted per day.
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param surveyDate name of the field in the dataset where the date of the survey is stored: string
#' @param dateFormat format used for the date: string ('\%m/\%d/\%Y')
#' @param surveyConsent name of the field in the dataset where the survey consent is stored: string
#'
#' @return dst      same dataset as the inputed one but with survey marked for deletion if errors are found and delete=TRUE (or NULL)
#' @return ret_log  list of the errors found (or NULL)
#' @return var      a list of value (or NULL)
#' @return graph    graphical representation of the results (or NULL)
#'
#' @author Yannick Pascaud
#'
#' @examples
#' {
#' ds <- HighFrequencyChecks::sample_dataset
#' surveyDate <- "survey_date"
#' dateFormat <- "%m/%d/%Y"
#' surveyConsent <- "survey_consent"
#'
#' list[dst,ret_log,var,graph] <- assessmentProductivity(ds=ds,
#'                                                       surveyDate=surveyDate,
#'                                                       dateFormat=dateFormat,
#'                                                       surveyConsent=surveyConsent)
#' head(ret_log,10)
#' print(graph)
#'}
#' @export assessmentProductivity
assessmentProductivity <- function(ds=NULL,
                                   surveyDate=NULL,
                                   dateFormat=NULL,
                                   surveyConsent=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveyDate) | !is.character(surveyDate)){
    stop("Please provide the field where the survey date is stored")
  }
  if(is.null(dateFormat) | !is.character(dateFormat)){
    stop("Please provide the format used for the date ('%m/%d/%Y')")
  }
  if(is.null(surveyConsent) | !is.character(surveyConsent)){
    stop("Please provide the field where the survey consent is stored")
  }

  tmp<-ds %>%
    # group_by(surveydate=surveydate) %>%
    group_by(surveydate=.data[[ surveyDate ]]) %>%
    summarize(NbSurvey=n())
  tmp$surveydate<-as.Date(tmp$surveydate, dateFormat)
  logf<-tmp[with(tmp, order(surveydate)), ]
  graph <- ggplot2::ggplot(tmp) +
    ggplot2::geom_col(ggplot2::aes(x=surveydate, y=NbSurvey)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x=ggplot2::element_blank(), panel.grid.minor.x=ggplot2::element_blank()) +
    ggplot2::labs(x = "Dates", y="Number of surveys")
  return(list(NULL,logf,NULL,graph))
}

#' @name assessmentDailyValidSurveys
#' @rdname assessmentDailyValidSurveys
#' @title Daily number of survey per consent status
#' @description This function display the number of surveys conducted per day per constent status.
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param surveyDate name of the field in the dataset where the date of the survey is stored: string
#' @param dateFormat format used for the date: string ('\%m/\%d/\%Y')
#' @param surveyConsent name of the field in the dataset where the survey consent is stored: string
#'
#' @return dst      same dataset as the inputed one but with survey marked for deletion if errors are found and delete=TRUE (or NULL)
#' @return ret_log  list of the errors found (or NULL)
#' @return var      a list of value (or NULL)
#' @return graph    graphical representation of the results (or NULL)
#'
#' @author Yannick Pascaud
#'
#' @examples
#' {
#' ds <- HighFrequencyChecks::sample_dataset
#' surveyDate <- "survey_date"
#' dateFormat <- "%m/%d/%Y"
#' surveyConsent <- "survey_consent"
#'
#' list[dst,ret_log,var,graph] <- assessmentDailyValidSurveys(ds=ds,
#'                                                            surveyDate=surveyDate,
#'                                                            dateFormat=dateFormat,
#'                                                            surveyConsent=surveyConsent)
#' head(ret_log,10)
#' print(graph)
#'}
#' @export assessmentDailyValidSurveys
assessmentDailyValidSurveys <- function(ds=NULL,
                                        surveyDate=NULL,
                                        dateFormat=NULL,
                                        surveyConsent=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveyDate) | !is.character(surveyDate)){
    stop("Please provide the field where the survey date is stored")
  }
  if(is.null(dateFormat) | !is.character(dateFormat)){
    stop("Please provide the format used for the date ('%m/%d/%Y')")
  }
  if(is.null(surveyConsent) | !is.character(surveyConsent)){
    stop("Please provide the field where the survey consent is stored")
  }

  tmp <- ds %>% group_by(surveydate=.data[[surveyDate]]) %>% count(.data[[surveyConsent]])
  colnames(tmp)[2] <- "surveyConsent"
  tmp$surveydate <- as.Date(tmp$surveydate, dateFormat)
  tmp <- tmp[with(tmp, order(surveydate)), ]
  logf <- reshape2::dcast(tmp,surveydate ~ surveyConsent, value.var="n")
  logf[is.na(logf)] <- 0
  graph <- ggplot2::ggplot(tmp) +
    ggplot2::geom_col(ggplot2::aes(x=surveydate, y=n, fill=surveyConsent)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x=ggplot2::element_blank(), panel.grid.minor.x=ggplot2::element_blank()) +
    ggplot2::labs(x = "Dates", y="Number of surveys", fill="Consent status")
  return(list(NULL,logf,NULL,graph))
}

#' @name assessmentTrackingSheet
#' @rdname assessmentTrackingSheet
#' @title Overall tracking sheet
#' @description This function display the overall tracking sheet.
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param dsSite name of the field in the dataset where the site is stored: string
#' @param sampleSizeTable dataset containing the sampling frame: data.frame
#' @param sampleSizeTableSite name of the field in the sampling frame where the site is stored: string
#' @param sampleSizeTableTarget name of the field where the target number of survey is stored in the sampling frame: string
#' @param sampleSizeTableAvailable name of the field where the number of points generated is stored in the sampling frame: string
#' @param surveyConsent name of the field in the dataset where the survey consent is stored: string
#' @param consentForValidSurvey value defined in the kobo form to acknowledge the surveyed person gave his consent: string
#'
#' @return dst      same dataset as the inputed one but with survey marked for deletion if errors are found and delete=TRUE (or NULL)
#' @return ret_log  list of the errors found (or NULL)
#' @return var      a list of value (or NULL)
#' @return graph    graphical representation of the results (or NULL)
#'
#' @author Yannick Pascaud
#'
#' @examples
#' {
#' ds <- HighFrequencyChecks::sample_dataset
#' dsSite <- "union_name"
#' sampleSizeTable <- HighFrequencyChecks::SampleSize
#' sampleSizeTableSite <- "Union"
#' sampleSizeTableTarget <- "SS"
#' sampleSizeTableAvailable <- "TotPts"                # Usually the Target + a buffer
#' surveyConsent <- "survey_consent"
#' consentForValidSurvey <- "yes"                      # consent value for yes
#'
#' list[dst,ret_log,var,graph] <- assessmentTrackingSheet(ds=ds,
#'                                                  dsSite=dsSite,
#'                                                  sampleSizeTable=sampleSizeTable,
#'                                                  sampleSizeTableSite=sampleSizeTableSite,
#'                                                  sampleSizeTableTarget=sampleSizeTableTarget,
#'                                                  sampleSizeTableAvailable=sampleSizeTableAvailable,
#'                                                  surveyConsent=surveyConsent,
#'                                                  consentForValidSurvey=consentForValidSurvey)
#' head(ret_log,10)
#' print(graph)
#'}
#' @export assessmentTrackingSheet
assessmentTrackingSheet <- function(ds=NULL,
                                    dsSite=NULL,
                                    sampleSizeTable=NULL,
                                    sampleSizeTableSite=NULL,
                                    sampleSizeTableTarget=NULL,
                                    sampleSizeTableAvailable=NULL,
                                    surveyConsent=NULL,
                                    consentForValidSurvey=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(sampleSizeTable) | nrow(sampleSizeTable)==0 | !is.data.frame(sampleSizeTable)){
    stop("Please provide the sampling frame")
  }
  if(is.null(dsSite) | !is.character(dsSite)){
    stop("Please provide the field where the site is stored in the dataset")
  }
  if(is.null(sampleSizeTableSite) | !is.character(sampleSizeTableSite)){
    stop("Please provide the field where the site is stored in the sampling frame")
  }
  if(is.null(surveyConsent) | !is.character(surveyConsent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(sampleSizeTableTarget) | !is.character(sampleSizeTableTarget)){
    stop("Please provide the field where the target number of survey is stored in the sampling frame")
  }
  if(is.null(sampleSizeTableAvailable) | !is.character(sampleSizeTableAvailable)){
    stop("Please provide the field where the number of points generated is stored in the sampling frame")
  }
  if((is.null(consentForValidSurvey) | !is.character(consentForValidSurvey))){
    stop("Please provide either the value for in the consent for the valid surveys")
  }

  df1<-data.frame(sampleSizeTable[,sampleSizeTableSite], sampleSizeTable[,sampleSizeTableTarget], sampleSizeTable[,sampleSizeTableAvailable], stringsAsFactors = FALSE)
  colnames(df1)<-c("site",sampleSizeTableTarget,sampleSizeTableAvailable)
  ds[,surveyConsent][ds$survey_consent!=consentForValidSurvey]<-"no"
  df2 <- data.frame(ds %>%
         group_by(site=.data[[ dsSite ]],
                  consent=.data[[ surveyConsent ]]) %>%
         summarize(n=n()) %>%
         mutate(done=sum(n)), stringsAsFactors = FALSE)

  tmp <- merge(df1[c("site",sampleSizeTableTarget,sampleSizeTableAvailable)],
               df2[df2$consent==consentForValidSurvey,c('site', "n", "done")],
               by=c("site"))
  tmp$toDo <- tmp[,sampleSizeTableTarget]-tmp$n
  tmp$RemainingPoints <- tmp[,sampleSizeTableAvailable]-tmp$done
  colnames(tmp) <- c("Site", "Target", "PointsAvailable", "ValidSurveys", "TotalSurveyDone", "ToDo", "RemainingPoints")

  logf <- tmp

  alertText <- "There is not enough points available to reach the sample size for:  \n"
  alertTextToBeDisplayed <- FALSE
  for(i in 1:length(tmp[,1])){
    if(((tmp[i,]$RemainingPoints - tmp[i,]$ToDo) < 0)){
      alertText <- paste(alertText, tmp[i,1], sep = "  \n")
      alertTextToBeDisplayed <- TRUE
    }
  }
  if(!alertTextToBeDisplayed){
    alertText <- NULL
  }

  tmp <- reshape2::melt(tmp[,c("Site", "ValidSurveys", "TotalSurveyDone", "ToDo", "RemainingPoints")], id="Site")
  tmp$grph[tmp$variable=="ValidSurveys" | tmp$variable=="ToDo"]<-"Graph1"
  tmp$grph[tmp$variable=="TotalSurveyDone" | tmp$variable=="RemainingPoints"]<-"Graph2"
  tmp <- tmp %>%
    mutate(
      gap = case_when(
        variable == "ValidSurveys" ~ 0.2,
        variable == "TotalSurveyDone" ~ 0.6,
        variable == "ToDo" ~ 0.2,
        variable == "RemainingPoints" ~ 0.6
      )
    )
  cpt=0
  for(i in unique(tmp$Site)){
    tmp$x[tmp$Site==i] <- cpt + tmp[tmp$Site==i,]$gap
    cpt=cpt+1
  }

  graph <- ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = x, y = value, fill = variable), data=subset(tmp, grph=="Graph2"), orientation = "x", width = 0.4,
             position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_col(ggplot2::aes(x = x, y = value, fill = variable), data=subset(tmp, grph=="Graph1"), orientation = "x", width = 0.4,
             position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_x_continuous(
      breaks = tmp$x[tmp$variable == "ToDo"] + 0.2,
      labels = tmp$Site[tmp$variable == "ToDo"]) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.y=ggplot2::element_blank()) +
    ggplot2::labs(x = "Sites", y="Number of surveys", fill="Categories") +
    ggplot2::coord_flip()

  return(list(NULL,logf,alertText,graph))
}
