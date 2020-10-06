#' @name chk4aiii_missing_pct
#' @rdname chk4aiii_missing_pct
#' @title Report the percentage of missing values (NA) per fields
#' @description This function provide a report showing the percentage of missing values (NA) for each fields.
#' This report can be global (all the surveys) or displayed for each enumerator ID
#'
#' @param ds dataset as a data.frame object
#' @param enumeratorID name as a string of the field in the dataset where the enumerator ID is stored
#' @param enumeratorcheck specify if the report has to be displayed for each enumerator or not as a boolean (TRUE/FALSE)
#'
#' @return logf  the report
#'
#' @author Yannick Pascaud
#'
#' @examples
#' \dontrun{
#' df <- HighFrequencyChecks::sample_dataset
#' enumeratorID <- "enumerator_id"
#' enumeratorcheck <- FALSE
#'
#' log <- chk4aiii_missing_pct(df, enumeratorID, enumeratorcheck)
#' head(log,10)
#'}
#' @export chk4aiii_missing_pct

chk4aiii_missing_pct <- function(ds=NULL, enumeratorID=NULL, enumeratorcheck=FALSE){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(enumeratorcheck) | !is.logical(enumeratorcheck)){
    stop("Please provide the enumeratorcheck action to be done (TRUE/FALSE)")
  }
  if(isTRUE(enumeratorcheck) & (is.null(enumeratorID) | !is.character(enumeratorID))){
    stop("Please provide the field where the enumerator ID is stored")
  }

  if(!enumeratorcheck){
    logf<-data.frame(variables=colnames(ds), pct = colMeans(is.na(ds[]) | ds[]=="") * 100)
  } else {
    logf  <-ds %>%
            group_by(ds[,enumeratorID]) %>%
            summarise_all(funs(100*mean(is.na(.) | .=="")))
  }
  return(logf)
}


#' @name chk4bii_distinct_values
#' @rdname chk4bii_distinct_values
#' @title Number of distinct values (not missing) per fields
#' @description This function provide a report showing the number of distinct values for each fields.
#' This report can be global (all the surveys) or displayed for each enumerator ID
#'
#' @param ds dataset as a data.frame object
#' @param enumeratorID name as a string of the field in the dataset where the enumerator ID is stored
#' @param enumeratorcheck specify if the report has to be displayed for each enumerator or not as a boolean (TRUE/FALSE)
#'
#' @return logf  the report
#'
#' @author Yannick Pascaud
#'
#' @examples
#' \dontrun{
#' df <- HighFrequencyChecks::sample_dataset
#' enumeratorID <- "enumerator_id"
#' enumeratorcheck <- FALSE
#'
#' log <- chk4bii_distinct_values(df, enumeratorID, enumeratorcheck)
#' head(log,10)
#'}
#' @export chk4bii_distinct_values


chk4bii_distinct_values <- function(ds=NULL, enumeratorID=NULL, enumeratorcheck=FALSE){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(enumeratorcheck) | !is.logical(enumeratorcheck)){
    stop("Please provide the enumeratorcheck action to be done (TRUE/FALSE)")
  }
  if(isTRUE(enumeratorcheck) & (is.null(enumeratorID) | !is.character(enumeratorID))){
    stop("Please provide the field where the enumerator ID is stored")
  }

  n_distinct_no_na <- function(x) n_distinct(x[!is.na(x)])

  if(!enumeratorcheck){
    logf<-data.frame(variables=colnames(ds), nb=sapply(ds, n_distinct_no_na))
  } else {
    logf<-ds %>%
      group_by(ds[,enumeratorID]) %>%
      summarise_all(funs(n_distinct_no_na(.)))
  }
  return(t(logf))
}

#' @name chk4biv_others_values
#' @rdname chk4biv_others_values
#' @title List of other distinct values (not missing) per fields other with count
#' @description This function provide a report showing all distinct other values and the number of occurrences for each fields "other".
#' This report can be global (all the surveys) or displayed for each enumerator ID
#'
#' @param ds dataset as a data.frame object
#' @param otherpattern pattern as string to identify the fields containing others values ('_other$')
#' @param enumeratorID name as a string of the field in the dataset where the enumerator ID is stored
#' @param enumeratorcheck specify if the report has to be displayed for each enumerator or not as a boolean (TRUE/FALSE)
#'
#' @return logf  the report
#'
#' @author Yannick Pascaud
#'
#' @examples
#' \dontrun{
#' df <- HighFrequencyChecks::sample_dataset
#' otherpattern <- "_other$"
#' enumeratorID <- "enumerator_id"
#' enumeratorcheck <- FALSE
#'
#' log <- chk4biv_others_values(df, otherpattern, enumeratorID, enumeratorcheck)
#' head(log,10)
#'}
#' @export chk4biv_others_values


chk4biv_others_values <- function(ds=NULL, otherpattern=NULL, enumeratorID=NULL, enumeratorcheck=FALSE){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(otherpattern) | !is.character(otherpattern)){
    stop("Please provide the pattern for other fields ('_other$')")
  }
  if(is.null(enumeratorcheck) | !is.logical(enumeratorcheck)){
    stop("Please provide the enumeratorcheck action to be done (TRUE/FALSE)")
  }
  if(isTRUE(enumeratorcheck) & (is.null(enumeratorID) | !is.character(enumeratorID))){
    stop("Please provide the field where the enumerator ID is stored")
  }

  if(!enumeratorcheck){
    tmp <- ds[,colnames(ds[,colnames(ds) %like% otherpattern])]
    tmp <- data.frame(utils::stack(tmp[1:ncol(tmp)]))
    logf <- subset(tmp, values!="") %>% group_by(field=ind, values) %>% summarize(nb=n())
  } else {
    tmp <- ds[,c(enumeratorID,colnames(ds[,colnames(ds) %like% otherpattern]))]
    tmp <- data.frame(tmp[1], utils::stack(tmp[2:ncol(tmp)]))
    logf <- subset(tmp, values!="") %>% group_by_(field=colnames(tmp[3]), enumeratorID, colnames(tmp[2])) %>% summarize(nb=n())
  }
  return(logf)
}

#' @name chk4d_outliers
#' @rdname chk4d_outliers
#' @title Report the outlier values for all numerical field
#' @description This function provide a report showing all outlier values for each numerical fields.
#' The function will try to automatically determine the type of distribution (between Normal and Log-Normal)
#' based on the difference between mean and median between untransformed normalized and log transformed normalized distribution.
#'
#' @param ds dataset as a data.frame object
#' @param sdval number of standard deviation for which the data within is considered as acceptable
#' @param reportingcol columns as a list of string name from the dataset you want in the result (c('col1','col2',...))
#' @param enumeratorID name as a string of the field in the dataset where the enumerator ID is stored
#' @param enumeratorcheck specify if the report has to be displayed for each enumerator or not as a boolean (TRUE/FALSE)
#'
#' @return logf  the report
#'
#' @author Yannick Pascaud
#'
#' @examples
#' \dontrun{
#' df <- HighFrequencyChecks::sample_dataset
#' sdval <- 2
#' reportingcol <- c("enumerator_id","X_uuid")
#' enumeratorID <- "enumerator_id"
#' enumeratorcheck <- FALSE
#'
#' log <- chk4d_outliers(df, sdval, reportingcol, enumeratorID, enumeratorcheck)
#' head(log,10)
#'}
#' @export chk4d_outliers

chk4d_outliers <- function(ds=NULL, sdval=NULL, reportingcol=NULL, enumeratorID=NULL, enumeratorcheck=FALSE){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(sdval) | !is.numeric(sdval)){
    stop("Please provide the number of standard deviations you want to check for")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(enumeratorcheck) | !is.logical(enumeratorcheck)){
    stop("Please provide the enumeratorcheck action to be done (TRUE/FALSE)")
  }
  if(isTRUE(enumeratorcheck) & (is.null(enumeratorID) | !is.character(enumeratorID))){
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
  # scores_na <- function(x){
  #   scores(x[!is.na(x) & x > -1], type = "z")
  # }
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
  scores_outliers[,reportingcol]<-ds[,reportingcol]

  scores_outliers <- data.frame(scores_outliers[reportingcol], utils::stack(scores_outliers[(names(scores_outliers) %ni% reportingcol)]), stringsAsFactors = FALSE)
  scores_outliers <- subset(scores_outliers, abs(values) >= sdval)
  scores_outliers$ind <- as.character(scores_outliers$ind)
  logf <- left_join(scores_outliers,distribution_type,by=c("ind"="ind"))
  return(logf)
}



#' @name chk4e_values_greater_X
#' @rdname chk4e_values_greater_X
#' @title Report the values greater than a specified value per specified fields
#' @description This function provide a report showing all values which are greater than a certain threshold for a specified list of fields.
#'
#' @param ds dataset as a data.frame object
#' @param questions columns as a list of string name from the dataset you want to check against (c('col1','col2',...))
#' @param value   maximum acceptable value as integer for the checked fields
#' @param reportingcol columns as a list of string name from the dataset you want in the result (c('col1','col2',...))
#' @param enumeratorID name as a string of the field in the dataset where the enumerator ID is stored
#' @param enumeratorcheck specify if the report has to be displayed for each enumerator or not as a boolean (TRUE/FALSE)
#'
#' @return logf  the report
#'
#' @author Yannick Pascaud
#'
#' @examples
#' \dontrun{
#' df <- HighFrequencyChecks::sample_dataset
#' qu <-c("consent_received.food_security.spend_food",
#'       "consent_received.food_security.spend_medication",
#'       "consent_received.food_security.spend_education",
#'       "consent_received.food_security.spend_fix_shelter",
#'       "consent_received.food_security.spend_clothing",
#'       "consent_received.food_security.spend_hygiene",
#'       "consent_received.food_security.spend_fuel",
#'       "consent_received.food_security.spend_hh_items",
#'       "consent_received.food_security.spend_transport",
#'       "consent_received.food_security.spend_communication",
#'       "consent_received.food_security.spend_tobacco",
#'       "consent_received.food_security.spend_rent",
#'       "consent_received.food_security.spend_debts",
#'       "consent_received.food_security.spend_other")
#' v <- 25000
#' rc <- c("enumerator_id","X_uuid")
#' eid <- "enumerator_id"
#' ec <- FALSE
#'
#' log <- chk4e_values_greater_X(df, qu, v, eid, ec)
#' head(log,10)
#'}
#' @export chk4e_values_greater_X
#'
chk4e_values_greater_X <- function(ds=NULL, questions=NULL, value=NULL, reportingcol=NULL, enumeratorID=NULL, enumeratorcheck=FALSE){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(questions) | !is.character(questions)){
    stop("Please provide the fields you want to check for (c('field1','field2',...))")
  }
  if(is.null(value) | !is.numeric(value)){
    stop("Please provide the maximum value for which you want to check")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(enumeratorcheck) | !is.logical(enumeratorcheck)){
    stop("Please provide the enumeratorcheck action to be done (TRUE/FALSE)")
  }
  if(isTRUE(enumeratorcheck) & (is.null(enumeratorID) | !is.character(enumeratorID))){
    stop("Please provide the field where the enumerator ID is stored")
  }

  tmp <- data.frame(ds[reportingcol], utils::stack(ds[questions]), stringsAsFactors = FALSE)
  logf <- subset(tmp, values>=value)
  return(logf)
}
