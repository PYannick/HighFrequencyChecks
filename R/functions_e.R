#' @name enumeratorSurveysConsent
#' @rdname enumeratorSurveysConsent
#' @title Check the percentage of survey refusals by enumerator
#' @description This function display the percentage of survey refusal per enumerator.
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param surveyConsent name of the field in the dataset where the survey consent is stored: string
#' @param enumeratorID name of the field where the enumerator ID is stored: string
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
#' surveyConsent <- "survey_consent"
#' enumeratorID <- "enumerator_id"
#'
#' list[dst,ret_log,var,graph] <- enumeratorSurveysConsent(ds,
#'                                                         surveyConsent,
#'                                                         enumeratorID)
#' head(ret_log,10)
#' print(graph)
#'}
#' @export enumeratorSurveysConsent
enumeratorSurveysConsent <- function(ds=NULL,
                                     surveyConsent=NULL,
                                     enumeratorID=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveyConsent) | !is.character(surveyConsent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(enumeratorID) | !is.character(enumeratorID)){
    stop("Please provide the field where the enumerator ID is stored")
  }

  tmp<-(ds %>% group_by(enumeratorID=ds[,enumeratorID]) %>%
          count(.data[[ surveyConsent ]]) %>%
          mutate(pct=round(100*n/sum(n), digits=2)))[-3]
  colnames(tmp)[2] <- "surveyConsent"
  logf <- reshape2::dcast(tmp,enumeratorID ~ surveyConsent, value.var = "pct")
  logf[is.na(logf)] <- 0
  graph <- ggplot2::ggplot(tmp) + ggplot2::geom_col(ggplot2::aes(x=as.character(enumeratorID), y=pct, fill=surveyConsent)) + ggplot2::coord_flip()
  return(list(NULL,logf,NULL,graph))
}

#' @name enumeratorSurveysDuration
#' @rdname enumeratorSurveysDuration
#' @title Check the average interview duration by enumerator
#' @description This function display the average interview duration per enumerator.
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param dates name of the fields where the information about the start and end date of the survey is stored: list of string (c('start_date','end_date'))
#' @param enumeratorID name of the field where the enumerator ID is stored: string
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
#' enumeratorID <- "enumerator_id"
#'
#' list[dst,ret_log,var,graph] <- enumeratorSurveysDuration(ds,
#'                                                          dates,
#'                                                          enumeratorID)
#' head(ret_log,10)
#' print(graph)
#'}
#' @export enumeratorSurveysDuration
enumeratorSurveysDuration <- function(ds=NULL,
                                      dates=NULL,
                                      enumeratorID=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(enumeratorID) | !is.character(enumeratorID)){
    stop("Please provide the field where the enumerator ID is stored")
  }

  ds$surveytime <- as.double.difftime((readr::parse_datetime(as.character(ds[,dates[2]])) -
                                       readr::parse_datetime(as.character(ds[,dates[1]]))),
                                       units = "secs") / 60
  #ds$surveytime <- as.double.difftime((strptime(ds[,dates[2]],"%Y-%m-%dT%R") - strptime(ds[,dates[1]],"%Y-%m-%dT%R")), units = "secs")/60
  overall_avg_duration <- round(mean(ds$surveytime), digits=2)
  logf <- ds %>%
          group_by(enumeratorID=ds[,enumeratorID]) %>%
          summarize(duration_mean = round(mean(surveytime), digits=2),
                    overall_avg_duration,
                    perc_diff_avg = round(((duration_mean - overall_avg_duration) / overall_avg_duration) * 100, digits=2))

  graph <- eval(parse(text=paste0("ggplot2::ggplot(ds) + ggplot2::geom_boxplot(ggplot2::aes(surveytime, as.character(", enumeratorID, ")), outlier.colour = 'red') + ggplot2::theme_light()")))
  return(list(NULL,logf,NULL,graph))
}

#' @name enumeratorProductivity
#' @rdname enumeratorProductivity
#' @title Check the number of surveys by enumerator
#' @description This function display the total number of survey made and the average per day per enumerator.
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param surveyDate name of the field in the dataset where the date of the survey is stored: string
#' @param enumeratorID name of the field where the enumerator ID is stored: string
#'
#' @return dst      same dataset as the inputed one but with survey marked for deletion if errors are found and delete=TRUE (or NULL)
#' @return ret_log  list of the errors found (or NULL)
#' @return var      a list of value (or NULL)
#' @return graph    graphical representation of the results (or NULL)
#'
#' @author Yannick Pascaud
#'
#' @examples
#'  {
#' ds <- HighFrequencyChecks::sample_dataset
#' surveyDate <- "survey_date"
#' enumeratorID <- "enumerator_id"
#'
#' list[dst,ret_log,var,graph] <- enumeratorProductivity(ds,
#'                                                       surveyDate,
#'                                                       enumeratorID)
#' head(ret_log,10)
#' print(graph)
#'}
#' @export enumeratorProductivity
enumeratorProductivity <- function(ds=NULL,
                                   surveyDate=NULL,
                                   enumeratorID=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveyDate) | !is.character(surveyDate)){
    stop("Please provide the field where the survey date is stored")
  }
  if(is.null(enumeratorID) | !is.character(enumeratorID)){
    stop("Please provide the field where the enumerator ID is stored")
  }

  logf <- ds %>%
    group_by(.data[[ enumeratorID ]]) %>%
    summarize(days_worked = length(unique(.data[[ surveyDate ]])),
              total_surveys_done = n()) %>%
    mutate(daily_average = round(total_surveys_done / days_worked, digits = 2))
  graph <- eval(parse(text=paste0("ggplot2::ggplot(logf) + ggplot2::geom_col(ggplot2::aes(x=as.character(", enumeratorID, "), y=daily_average)) + ggplot2::coord_flip()")))
  return(list(NULL,logf,NULL,graph))
}

#' @name enumeratorProductivityOutliers
#' @rdname enumeratorProductivityOutliers
#' @title Check the surveyors with very low or high productivity
#' @description This function display the surveyors with very low or high productivity.
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param surveyDate name of the field in the dataset where the date of the survey is stored: string
#' @param enumeratorID name of the field where the enumerator ID is stored: string
#' @param sdval (Optional, by default set to 2) number of standard deviation for which the data within is considered as acceptable: integer
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
#' enumeratorID <- "enumerator_id"
#' surveyDate <- "survey_date"
#'
#' list[dst,ret_log,var,graph] <- enumeratorProductivityOutliers(ds,
#'                                                               enumeratorID,
#'                                                               surveyDate)
#' head(ret_log,10)
#' print(graph)
#'}
#' @export enumeratorProductivityOutliers
enumeratorProductivityOutliers <- function(ds=NULL,
                                           enumeratorID=NULL,
                                           surveyDate=NULL,
                                           sdval=2){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(enumeratorID) | !is.character(enumeratorID)){
    stop("Please provide the field where the enumerator ID is stored")
  }
  if(is.null(surveyDate) | !is.character(surveyDate)){
    stop("Please provide the field where the survey date is stored")
  }
  if(is.null(sdval) | !is.numeric(sdval)){
    stop("Please provide the number of standard deviations you want to check for")
  }

  tmp <- ds %>%
    group_by(.data[[ enumeratorID ]]) %>%
    summarize(days_worked = length(unique(.data[[ surveyDate ]])),
              total_surveys_done = n()) %>%
    mutate(daily_average = total_surveys_done / days_worked)

  survey_outliers <- outliers::scores(tmp$daily_average, type = "z")
  tmp <- data.frame(tmp,survey_outliers)
  logf <- subset(tmp, abs(survey_outliers) > sdval)
  graph <- ggplot2::ggplot(tmp) + ggplot2::geom_boxplot(ggplot2::aes(daily_average), outlier.colour = "red") +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.y=element_blank(),
                   axis.line.y=element_blank(),
                   axis.ticks.y=element_blank(),
                   panel.grid.major.y=element_blank(),
                   panel.grid.minor.y=element_blank())
  return(list(NULL,logf,NULL,graph))
}

#' @name enumeratorIsLazy
#' @rdname enumeratorIsLazy
#' @title Check the enumerators who pick up less than X answers per specific question
#' @description This function display the surveyors who picked up less than a specified amount of answers per specific question.
#' This can be useful for select_multiple questions where respondent shall give at least 3 options for instance.
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param enumeratorID name of the field where the enumerator ID is stored: string
#' @param questionsEnumeratorIsLazy columns name from the dataset and value you want to check against (c(col1=value1,col2=value2,...)): named list of integer
#'  the column name is the main part of the name generated by kobo (eg: for the question 'main_income', kobo will generate one TRUE/FALSE
#'  column per possible answer as 'main_income.work', 'main_income.remittance'..., only the main part 'main_income' has to be specified here)
#'
#' @return dst      same dataset as the inputed one but with survey marked for deletion if errors are found and delete=TRUE (or NULL)
#' @return ret_log  list of the errors found (or NULL)
#' @return var      a list of value (or NULL)
#' @return graph    graphical representation of the results (or NULL)
#'
#' @author Yannick Pascaud
#'
#' @examples
#' \dontrun{
#' ds <- HighFrequencyChecks::sample_dataset
#' enumeratorID <- "enumerator_id"
#' questionsEnumeratorIsLazy <- c(consent_received.shelter_nfi.non_food_items=3,
#'                                consent_received.food_security.main_income=3,
#'                                consent_received.child_protection.boy_risk=3,
#'                                consent_received.child_protection.girl_risk=3)
#'
#' list[dst,ret_log,var,graph] <- enumeratorIsLazy(ds,
#'                                                 enumeratorID,
#'                                                 questionsEnumeratorIsLazy)
#' head(ret_log,10)
#'}
#' @export enumeratorIsLazy
enumeratorIsLazy <- function(ds=NULL,
                             enumeratorID=NULL,
                             questionsEnumeratorIsLazy=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(enumeratorID) | !is.character(enumeratorID)){
    stop("Please provide the field where the enumerator ID is stored")
  }
  if(is.null(questionsEnumeratorIsLazy)){
    stop("Please provide the fields you want to check for (c('field1[.]','field2[.]',...))")
  }

  tmp <- stats::setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("enumeratorID","NbErr","field"))
  for(i in length(questionsEnumeratorIsLazy)){
    tmp <- rbind(tmp, data.frame(data.frame(ds, nb = rowSums(ds[,colnames(ds) %like% paste0(names(questionsEnumeratorIsLazy[i]), "[.]")], na.rm=TRUE) ) %>%
                                   group_by(enumeratorID = ds[,enumeratorID]) %>%
                                   summarize(NbErr = sum(nb < questionsEnumeratorIsLazy[i])), field = names(questionsEnumeratorIsLazy[i])))
  }
  logf<-tmp
  return(list(NULL,logf,NULL,NULL))
}
