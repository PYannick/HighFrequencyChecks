
# chk6a_refusal: Check the percentage of survey refusals by enumerator
# chk6b_duration: Check the average interview duration by enumerator
# chk6c_nb_survey: Check the number of surveys per day by enumerator
# chk6f_productivity: Surveyors with very low or high productivity
# chk6g_question_less_X_answers: Enumerators who pick up less than X answers per question

#' @name chk6a_refusal
#' @rdname chk6a_refusal
#' @title Check the percentage of survey refusals by enumerator
#' @description This function display the percentage of survey refusal per enumerator.
#'
#' @param ds dataset as a data.frame object
#' @param survey_consent name as a string of the field in the dataset where the survey consent is stored
#' @param enumeratorID name as a string of the field in the dataset where the enumerator ID is stored
#'
#' @return logf  the report
#'
#' @author Yannick Pascaud
#'
#' @examples
#' \dontrun{
#' df <- sample_dataset
#' sc <- "survey_consent"
#' eid <- "enumerator_id"
#'
#' chk6a_refusal(df, sc, eid)
#'}
#' @export chk6a_refusal


chk6a_refusal <- function(ds=NULL, survey_consent=NULL, enumeratorID=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(enumeratorID) | !is.character(enumeratorID)){
    stop("Please provide the field where the enumerator ID is stored")
  }

  tmp<-(ds %>% group_by(enumeratorID=ds[,enumeratorID]) %>% count_(survey_consent) %>% mutate(pct=round(100*n/sum(n), digits=2)))[-3]
  colnames(tmp)[2] <- "survey_consent"
  logf <- data.table::dcast(tmp,enumeratorID ~ survey_consent, value.var = "pct")
  logf[is.na(logf)] <- 0
  return(logf)
}

#' @name chk6b_duration
#' @rdname chk6b_duration
#' @title Check the average interview duration by enumerator
#' @description This function display the average interview duration per enumerator.
#'
#' @param ds dataset as a data.frame object
#' @param dates fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
#' @param enumeratorID name as a string of the field in the dataset where the enumerator ID is stored
#'
#'
#' @return logf  the report
#'
#' @author Yannick Pascaud
#'
#' @examples
#' \dontrun{
#' df <- sample_dataset
#' dt <- c("survey_start","end_survey")
#' eid <- "enumerator_id"
#'
#' chk6b_duration(df, dt, eid)
#'}
#' @export chk6b_duration

chk6b_duration <- function(ds=NULL, dates=NULL, enumeratorID=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(enumeratorID) | !is.character(enumeratorID)){
    stop("Please provide the field where the enumerator ID is stored")
  }

  ds$surveytime <- as.double.difftime((strptime(ds[,dates[2]],"%Y-%m-%dT%R") - strptime(ds[,dates[1]],"%Y-%m-%dT%R")), units = "secs")/60
  overall_avg_duration <- round(mean(ds$surveytime), digits=2)
  logf<-ds %>% group_by(enumeratorID=ds[,enumeratorID]) %>% summarize(duration_mean = round(mean(surveytime), digits=2),
                                                   overall_avg_duration,
                                                   perc_diff_avg = round(((duration_mean - overall_avg_duration) / overall_avg_duration) * 100, digits=2))
  return(logf)
}

#' @name chk6c_nb_survey
#' @rdname chk6c_nb_survey
#' @title Check the number of surveys by enumerator
#' @description This function display the total number of survey made and the average per day per enumerator.
#'
#' @param ds dataset as a data.frame object
#' @param surveydate name as a string of the field in the dataset where the date of the survey is stored
#' @param enumeratorID name as a string of the field in the dataset where the enumerator ID is stored
#'
#' @return logf  the report
#'
#' @author Yannick Pascaud
#'
#' @examples
#' \dontrun{
#' df <- sample_dataset
#' sdte <- "survey_date"
#' eid <- "enumerator_id"
#'
#' chk6c_nb_survey(df, sdte, eid)
#'}
#' @export chk6c_nb_survey

chk6c_nb_survey <- function(ds=NULL, surveydate=NULL, enumeratorID=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveydate) | !is.character(surveydate)){
    stop("Please provide the field where the survey date is stored")
  }
  if(is.null(enumeratorID) | !is.character(enumeratorID)){
    stop("Please provide the field where the enumerator ID is stored")
  }

  logf <- ds %>%
          group_by_(enumeratorID) %>%
          summarize_(days_worked = lazyeval::interp(~length(unique(var)),
                                          var = as.name(surveydate)),
                     total_surveys_done = ~n()) %>%
          mutate(daily_average = round(total_surveys_done / days_worked, digits = 2))
  return(logf)
}

#' @name chk6f_productivity
#' @rdname chk6f_productivity
#' @title Check the surveyors with very low or high productivity
#' @description This function display the surveyors with very low or high productivity.
#'
#' @param ds dataset as a data.frame object
#' @param surveydate name as a string of the field in the dataset where the date of the survey is stored
#' @param enumeratorID name as a string of the field in the dataset where the enumerator ID is stored
#' @param sdval number of standard deviation for which the duration within is considered as acceptable
#'
#' @return logf  the report
#'
#' @author Yannick Pascaud
#'
#' @examples
#' \dontrun{
#' df <- sample_dataset
#' eid <- "enumerator_id"
#' sdte <- "survey_date"
#' sdv<-2
#'
#' chk6f_productivity(df, eid, sdte, sdv)
#'}
#' @export chk6f_productivity

chk6f_productivity <- function(ds=NULL, enumeratorID=NULL, surveydate=NULL, sdval=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(enumeratorID) | !is.character(enumeratorID)){
    stop("Please provide the field where the enumerator ID is stored")
  }
  if(is.null(surveydate) | !is.character(surveydate)){
    stop("Please provide the field where the survey date is stored")
  }
  if(is.null(sdval) | !is.numeric(sdval)){
    stop("Please provide the number of standard deviations you want to check for")
  }

  tmp <- ds %>%
         group_by_(enumeratorID) %>%
         summarize_(days_worked = lazyeval::interp(~length(unique(var)),
                                         var = as.name(surveydate)),
                    total_surveys_done = ~n()) %>%
         mutate(daily_average = total_surveys_done / days_worked)

  survey_outliers <- outliers::scores(tmp$daily_average, type = "z")
  tmp <- data.frame(tmp,survey_outliers)
  logf <- subset(tmp, abs(survey_outliers)>sdval)

  return(logf)
}


#' @name chk6g_question_less_X_answers
#' @rdname chk6g_question_less_X_answers
#' @title Check the enumerators who pick up less than X answers per specific question
#' @description This function display the surveyors who picked up less than a specified amount of answers per specific question.
#' This can be useful for select_multiple questions where respondent shall give at least 3 options for instance.
#'
#' @param ds dataset as a data.frame object
#' @param enumeratorID name as a string of the field in the dataset where the enumerator ID is stored
#' @param questions columns as a list of string name from the dataset you want to check against (c('col1','col2',...))
#' @param minnbanswers minimum number of answers expected per question
#'
#' @return logf  the report
#'
#' @author Yannick Pascaud
#'
#' @examples
#' \dontrun{
#' df <- sample_dataset
#' eid <- "enumerator_id"
#' qu <- c("consent_received.shelter_nfi.non_food_items[.]",
#'       "consent_received.food_security.main_income[.]",
#'       "consent_received.child_protection.boy_risk[.]",
#'       "consent_received.child_protection.girl_risk[.]")
#' mna <- 3
#'
#' chk6g_question_less_X_answers(df, eid, qu, mna)
#'}
#' @export chk6g_question_less_X_answers

chk6g_question_less_X_answers <- function(ds=NULL, enumeratorID=NULL, questions=NULL, minnbanswers=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(enumeratorID) | !is.character(enumeratorID)){
    stop("Please provide the field where the enumerator ID is stored")
  }
  if(is.null(questions) | !is.character(questions)){
    stop("Please provide the fields you want to check for (c('field1[.]','field2[.]',...))")
  }
  if(is.null(minnbanswers) | !is.numeric(minnbanswers)){
    stop("Please provide the minimum number of expected answers")
  }

  tmp <- stats::setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("enumeratorID","NbErr","field"))
  for(i in questions){
    tmp <- rbind(tmp, data.frame(data.frame(ds, nb = rowSums(ds[,colnames(ds) %like% i], na.rm=TRUE) ) %>%
                       group_by(enumeratorID = ds[,enumeratorID]) %>%
                       summarize(NbErr = sum(nb < minnbanswers)), field = stringi::stri_replace_all_fixed(i, "[.]", "")))
  }
  logf<-tmp
  return(logf)
}





