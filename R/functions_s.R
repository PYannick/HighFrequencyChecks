#' @name surveyMissingValues
#' @rdname surveyMissingValues
#' @title Report the percentage of missing values (NA) per fields
#' @description This function provide a report showing the percentage of missing values (NA) for each fields.
#' This report can be global (all the surveys) or displayed for each enumerator ID
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param enumeratorID name of the field where the enumerator ID is stored: string
#' @param enumeratorCheck (Optional, by default set to FALSE) specify if the report has to be displayed for each enumerator or not: boolean (TRUE/FALSE)
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
#'
#' list[dst,ret_log,var,graph] <- surveyMissingValues(ds=ds,
#'                                                    enumeratorID=enumeratorID,
#'                                                    enumeratorCheck=FALSE)
#' head(ret_log,10)
#'}
#' @export surveyMissingValues
surveyMissingValues <- function(ds=NULL,
                                enumeratorID=NULL,
                                enumeratorCheck=FALSE){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(enumeratorCheck) | !is.logical(enumeratorCheck)){
    stop("Please provide the enumeratorcheck action to be done (TRUE/FALSE)")
  }
  if(isTRUE(enumeratorCheck) & (is.null(enumeratorID) | !is.character(enumeratorID))){
    stop("Please provide the field where the enumerator ID is stored")
  }

  if(!enumeratorCheck){
    logf<-data.frame(variables=colnames(ds), pct = colMeans(is.na(ds[]) | ds[]=="") * 100)
  } else {
    logf  <-ds %>%
      group_by(ds[,enumeratorID]) %>%
      summarise_all(funs(100*mean(is.na(.) | .=="")))
  }
  return(list(NULL,logf,NULL,NULL))
}

#' @name surveyDistinctValues
#' @rdname surveyDistinctValues
#' @title Number of distinct values (not missing) per fields
#' @description This function provide a report showing the number of distinct values for each fields.
#' This report can be global (all the surveys) or displayed for each enumerator ID
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param enumeratorID name of the field where the enumerator ID is stored: string
#' @param enumeratorCheck (Optional, by default set to FALSE) specify if the report has to be displayed for each enumerator or not: boolean (TRUE/FALSE)
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
#'
#' list[dst,ret_log,var,graph] <- surveyDistinctValues(ds=ds,
#'                                                     enumeratorID=enumeratorID,
#'                                                     enumeratorCheck=FALSE)
#' head(ret_log,10)
#'}
#' @export surveyDistinctValues
surveyDistinctValues <- function(ds=NULL,
                                 enumeratorID=NULL,
                                 enumeratorCheck=FALSE){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(enumeratorCheck) | !is.logical(enumeratorCheck)){
    stop("Please provide the enumeratorcheck action to be done (TRUE/FALSE)")
  }
  if(isTRUE(enumeratorCheck) & (is.null(enumeratorID) | !is.character(enumeratorID))){
    stop("Please provide the field where the enumerator ID is stored")
  }

  n_distinct_no_na <- function(x) n_distinct(x[!is.na(x)])

  if(!enumeratorCheck){
    logf<-data.frame(variables=colnames(ds), nb=sapply(ds, n_distinct_no_na))
  } else {
    logf<-ds %>%
      group_by(ds[,enumeratorID]) %>%
      # summarise_all(funs(n_distinct_no_na(.)))
      summarise_all(~ n_distinct_no_na(.))
  }
  return(list(NULL,t(logf),NULL,NULL))
}

#' @name surveyOtherValues
#' @rdname surveyOtherValues
#' @title List of other distinct values (not missing) per fields other with count
#' @description This function provide a report showing all distinct other values and the number of occurrences for each fields "other".
#' This report can be global (all the surveys) or displayed for each enumerator ID
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param otherPattern patternto identify the fields containing others values (eg: '_other$'): string
#' @param enumeratorID name of the field where the enumerator ID is stored: string
#' @param enumeratorCheck (Optional, by default set to FALSE) specify if the report has to be displayed for each enumerator or not: boolean (TRUE/FALSE)
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
#' otherPattern <- "_other$"
#' enumeratorID <- "enumerator_id"
#'
#' list[dst,ret_log,var,graph] <- surveyOtherValues(ds=ds,
#'                                                  otherPattern=otherPattern,
#'                                                  enumeratorID=enumeratorID,
#'                                                  enumeratorCheck=FALSE)
#' head(ret_log,10)
#'}
#' @export surveyOtherValues
surveyOtherValues <- function(ds=NULL,
                              otherPattern=NULL,
                              enumeratorID=NULL,
                              enumeratorCheck=FALSE){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(otherPattern) | !is.character(otherPattern)){
    stop("Please provide the pattern for other fields ('_other$')")
  }
  if(is.null(enumeratorCheck) | !is.logical(enumeratorCheck)){
    stop("Please provide the enumeratorcheck action to be done (TRUE/FALSE)")
  }
  if(isTRUE(enumeratorCheck) & (is.null(enumeratorID) | !is.character(enumeratorID))){
    stop("Please provide the field where the enumerator ID is stored")
  }

  if(!enumeratorCheck){
    tmp <- data.frame(ds[,colnames(ds[,colnames(ds) %like% otherPattern])], stringsAsFactors = FALSE)
    tmp <- data.frame(utils::stack(tmp[1:ncol(tmp)]))
    logf <- subset(tmp, values!="") %>% group_by(field=ind, values) %>% summarize(nb=n())
  } else {
    tmp <- data.frame(ds[,c(enumeratorID,colnames(ds[,colnames(ds) %like% otherPattern]))], stringsAsFactors = FALSE)
    tmp <- data.frame(tmp[1], utils::stack(tmp[2:ncol(tmp)]))
    logf <- subset(tmp, values!="") %>% group_by(field=.data[[ colnames(tmp[3]) ]], .data[[ enumeratorID ]], .data[[ colnames(tmp[2]) ]]) %>% summarize(nb=n())
  }
  return(list(NULL,logf,NULL,NULL))
}

#' @name surveyOutliers
#' @rdname surveyOutliers
#' @title Report the outlier values for all numerical field
#' @description This function provide a report showing all outlier values for each numerical fields.
#' The function will try to automatically determine the type of distribution (between Normal and Log-Normal)
#' based on the difference between mean and median between untransformed normalized and log transformed normalized distribution.
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param enumeratorID name of the field where the enumerator ID is stored: string
#' @param sdval (Optional, by default set to 2) number of standard deviation for which the data within is considered as acceptable: integer
#' @param uniqueID name of the field where the survey unique ID is stored: string
#' @param reportingColumns (Optional, by default it is built from the enumeratorID and the UniqueID) name of the columns from the dataset you want in the result: list of string (c('col1','col2',...))
#' @param enumeratorCheck (Optional, by default set to FALSE) specify if the report has to be displayed for each enumerator or not: boolean (TRUE/FALSE)
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
#' uniqueID <- "X_uuid"
#' reportingColumns <- c(enumeratorID, uniqueID)
#' sdval<-2
#'
#' list[dst,ret_log,var,graph] <- surveyOutliers(ds=ds,
#'                                               enumeratorID=enumeratorID,
#'                                               sdval=sdval,
#'                                               reportingColumns=reportingColumns,
#'                                               enumeratorCheck=FALSE)
#' head(ret_log,10)
#'}
#' @export surveyOutliers
surveyOutliers <- function(ds=NULL,
                           enumeratorID=NULL,
                           sdval=2,
                           reportingColumns=c(enumeratorID, uniqueID),
                           enumeratorCheck=FALSE){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(sdval) | !is.numeric(sdval)){
    stop("Please provide the number of standard deviations you want to check for")
  }
  if(is.null(reportingColumns) | !is.character(reportingColumns)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(enumeratorCheck) | !is.logical(enumeratorCheck)){
    stop("Please provide the enumeratorcheck action to be done (TRUE/FALSE)")
  }
  if(isTRUE(enumeratorCheck) & (is.null(enumeratorID) | !is.character(enumeratorID))){
    stop("Please provide the field where the enumerator ID is stored")
  }

  signedlog10 <- function(x){
    ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
  }

  normalized <- function(x, bound=c(0,1)){
    if(diff(range(x, na.rm = TRUE))){
      (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE)) * diff(bound) + bound[1L]
    } else {
      rep_len(0,length(x))
    }
  }

  z_score <- function(x){
    pop_sd <- stats::sd(x, na.rm = TRUE)*sqrt((length( stats::na.omit(x))-1)/(length( stats::na.omit(x))))
    pop_mean <- mean(x, na.rm = TRUE)
    inz<-function(x){
      return((x - pop_mean) / pop_sd)
    }
    return(inz(x))
  }

  norm_or_lognorm <- function(x){
    norm<-normalized(x)
    lognorm<-normalized(signedlog10(x))
    if(abs(mean(norm, na.rm = TRUE)- stats::median(norm, na.rm = TRUE)) > abs(mean(lognorm, na.rm = TRUE)- stats::median(lognorm, na.rm = TRUE))){
      # more like log normal
      return(list(lognorm,"LogNormal"))
    } else {
      # more like normal
      return(list(norm,"Normal"))
    }
  }
  '%ni%' <- Negate('%in%')

  tmp<-ds[,colnames(ds[colMeans(is.na(ds))<1])]
  tmp<-sapply(tmp[sapply(tmp, is.numeric)], norm_or_lognorm)
  distribution_type<-data.frame(t(data.frame(tmp[2,])), stringsAsFactors = FALSE)
  colnames(distribution_type)<-"DistributionType"
  distribution_type$ind<-rownames(distribution_type)

  scores_outliers <- data.frame(sapply(tmp[1,], z_score), stringsAsFactors = FALSE)
  scores_outliers[,reportingColumns]<-ds[,reportingColumns]

  scores_outliers <- data.frame(scores_outliers[reportingColumns], utils::stack(scores_outliers[(names(scores_outliers) %ni% reportingColumns)]), stringsAsFactors = FALSE)
  scores_outliers <- subset(scores_outliers, abs(values) >= sdval)
  scores_outliers$ind <- as.character(scores_outliers$ind)
  logf <- left_join(scores_outliers,distribution_type,by=c("ind"="ind"))
  return(list(NULL,logf,NULL,NULL))
}

#' @name surveyBigValues
#' @rdname surveyBigValues
#' @title Report the values greater than a specified value per specified fields
#' @description This function provide a report showing all values which are greater than a certain threshold for a specified list of fields.
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param questionsSurveyBigValues columns name from the dataset and value you want to check against (c(col1=value1,col2=value2,...)): named list of integer
#' @param uniqueID name of the field where the survey unique ID is stored: string
#' @param enumeratorID name of the field where the enumerator ID is stored: string
#' @param reportingColumns (Optional, by default it is built from the enumeratorID and the UniqueID) name of the columns from the dataset you want in the result: list of string (c('col1','col2',...))
#' @param enumeratorCheck (Optional, by default set to FALSE) specify if the report has to be displayed for each enumerator or not: boolean (TRUE/FALSE)
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
#' questionsSurveyBigValues <-c(consent_received.food_security.spend_food=25000,
#'                              consent_received.food_security.spend_medication=25000,
#'                              consent_received.food_security.spend_education=25000,
#'                              consent_received.food_security.spend_fix_shelter=25000,
#'                              consent_received.food_security.spend_clothing=25000,
#'                              consent_received.food_security.spend_hygiene=25000,
#'                              consent_received.food_security.spend_fuel=25000,
#'                              consent_received.food_security.spend_hh_items=25000,
#'                              consent_received.food_security.spend_transport=25000,
#'                              consent_received.food_security.spend_communication=25000,
#'                              consent_received.food_security.spend_tobacco=25000,
#'                              consent_received.food_security.spend_rent=25000,
#'                              consent_received.food_security.spend_debts=25000,
#'                              consent_received.food_security.spend_other=25000)
#' enumeratorID <- "enumerator_id"
#' uniqueID <- "X_uuid"
#' reportingColumns <- c(enumeratorID, uniqueID)
#'
#' list[dst,ret_log,var,graph] <- surveyBigValues(ds=ds,
#'                                                questionsSurveyBigValues=questionsSurveyBigValues,
#'                                                enumeratorID=enumeratorID,
#'                                                reportingColumns=reportingColumns,
#'                                                enumeratorCheck=FALSE)
#' head(ret_log,10)
#'}
#' @export surveyBigValues
surveyBigValues <- function(ds=NULL,
                            questionsSurveyBigValues=NULL,
                            enumeratorID=NULL,
                            reportingColumns=c(enumeratorID, uniqueID),
                            enumeratorCheck=FALSE){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(questionsSurveyBigValues)){
    stop("Please provide the fields you want to check for (c('field1','field2',...))")
  }
  if(is.null(reportingColumns) | !is.character(reportingColumns)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(enumeratorCheck) | !is.logical(enumeratorCheck)){
    stop("Please provide the enumeratorcheck action to be done (TRUE/FALSE)")
  }
  if(isTRUE(enumeratorCheck) & (is.null(enumeratorID) | !is.character(enumeratorID))){
    stop("Please provide the field where the enumerator ID is stored")
  }

  tmp <- data.frame(ds[reportingColumns], utils::stack(ds[names(questionsSurveyBigValues)]), stringsAsFactors = FALSE)
  # logf <- subset(tmp, values>=value)
  logf <- data.frame()
  for(i in 1:length(questionsSurveyBigValues)){
    logf <- rbind(logf, subset(tmp[tmp$ind==names(questionsSurveyBigValues[i]),], values>=questionsSurveyBigValues[i]))
  }
  return(list(NULL,logf,NULL,NULL))
}

#' @name surveySmallValues
#' @rdname surveySmallValues
#' @title Report the values lower than a specified value per specified fields
#' @description This function provide a report showing all values which are lower than a certain threshold for a specified list of fields.
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param questionsSurveySmallValues columns name from the dataset and value you want to check against (c(col1=value1,col2=value2,...)): named list of integer
#' @param uniqueID name of the field where the survey unique ID is stored: string
#' @param enumeratorID name of the field where the enumerator ID is stored: string
#' @param reportingColumns (Optional, by default it is built from the enumeratorID and the UniqueID) name of the columns from the dataset you want in the result: list of string (c('col1','col2',...))
#' @param enumeratorCheck (Optional, by default set to FALSE) specify if the report has to be displayed for each enumerator or not: boolean (TRUE/FALSE)
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
#' questionsSurveySmallValues <-c(consent_received.food_security.spend_food=25000,
#'                              consent_received.food_security.spend_medication=25000,
#'                              consent_received.food_security.spend_education=25000,
#'                              consent_received.food_security.spend_fix_shelter=25000,
#'                              consent_received.food_security.spend_clothing=25000,
#'                              consent_received.food_security.spend_hygiene=25000,
#'                              consent_received.food_security.spend_fuel=25000,
#'                              consent_received.food_security.spend_hh_items=25000,
#'                              consent_received.food_security.spend_transport=25000,
#'                              consent_received.food_security.spend_communication=25000,
#'                              consent_received.food_security.spend_tobacco=25000,
#'                              consent_received.food_security.spend_rent=25000,
#'                              consent_received.food_security.spend_debts=25000,
#'                              consent_received.food_security.spend_other=25000)
#' enumeratorID <- "enumerator_id"
#' uniqueID <- "X_uuid"
#' reportingColumns <- c(enumeratorID, uniqueID)
#'
#' list[dst,ret_log,var,graph] <- surveySmallValues(ds=ds,
#'                                        questionsSurveySmallValues=questionsSurveySmallValues,
#'                                        enumeratorID=enumeratorID,
#'                                        reportingColumns=reportingColumns,
#'                                        enumeratorCheck=FALSE)
#' head(ret_log,10)
#'}
#' @export surveySmallValues
surveySmallValues <- function(ds=NULL,
                              questionsSurveySmallValues=NULL,
                              enumeratorID=NULL,
                              reportingColumns=c(enumeratorID, uniqueID),
                              enumeratorCheck=FALSE){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(questionsSurveySmallValues)){
    stop("Please provide the fields you want to check for (c('field1','field2',...))")
  }
  if(is.null(reportingColumns) | !is.character(reportingColumns)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(enumeratorCheck) | !is.logical(enumeratorCheck)){
    stop("Please provide the enumeratorcheck action to be done (TRUE/FALSE)")
  }
  if(isTRUE(enumeratorCheck) & (is.null(enumeratorID) | !is.character(enumeratorID))){
    stop("Please provide the field where the enumerator ID is stored")
  }

  tmp <- data.frame(ds[reportingColumns], utils::stack(ds[names(questionsSurveySmallValues)]), stringsAsFactors = FALSE)
  # logf <- subset(tmp, values>=value)
  logf <- data.frame()
  for(i in 1:length(questionsSurveySmallValues)){
    logf <- rbind(logf, subset(tmp[tmp$ind==names(questionsSurveySmallValues[i]),], values<=questionsSurveySmallValues[i]))
  }
  return(list(NULL,logf,NULL,NULL))
}
