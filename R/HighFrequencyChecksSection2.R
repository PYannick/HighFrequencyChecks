#' @name chk2a_missing_id
#' @rdname chk2a_missing_id
#' @title Missing unique ID
#' @description This function check that all interviews in the dataset have an ID.
#'   There is an option to automatically mark for deletion the surveys which have not an ID.
#'
#' @param ds dataset as a data.frame object
#' @param UniqueID name as a string of the field in the dataset where the unique ID is stored
#' @param survey_consent name as a string of the field in the dataset where the survey consent is stored
#' @param reportingcol columns as a list of string name from the dataset you want in the result (c('col1','col2',...))
#' @param delete delete action to be done as a boolean (TRUE/FALSE)
#'
#' @return  ds same dataset as the inputed one but with survey marked for deletion if errors are found and delete=TRUE
#' @return  errors  list of the errors found
#'
#' @author Yannick Pascaud
#'
#' @examples
#' {
#' ds <- HighFrequencyChecks::sample_dataset
#' UniqueID <- "X_uuid"
#' survey_consent <- "survey_consent"
#' reportingcol <- c("enumerator_id","X_uuid")
#' delete <- FALSE
#'
#'
#' list_missing_id <- chk2a_missing_id(ds, UniqueID, survey_consent, reportingcol, delete)
#' head(list_missing_id[[2]], 10)
#'}
#' @export chk2a_missing_id


chk2a_missing_id <- function(ds=NULL,
                             UniqueID=NULL,
                             survey_consent=NULL,
                             reportingcol=NULL,
                             delete=NULL)
  {
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(UniqueID) | !is.character(UniqueID)){
    stop("Please provide the field where the survey unique ID is stored")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete) | !is.logical(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(delete){
    ds[,survey_consent][is.na(ds[,UniqueID])] <- "deleted"
  }

  # TO BE BE CHANGED WITH DYNAMIC COLUMS
   errors <- subset(ds,is.na(ds[,UniqueID]) | ds[,UniqueID]=="") %>%
   #  dplyr::select(reportingcol, survey_consent=survey_consent)
     dplyr::select(all_of(reportingcol), survey_consent=survey_consent)
  #errors <- ds[ which(is.na(ds[,UniqueID]) | ds[,UniqueID]=="") , c(reportingcol, survey_consent)]
  return(list(ds,errors))
}


#' @name chk2b_unique_id
#' @rdname chk2b_unique_id
#' @title Duplicates in unique ID
#' @description This function check that all interviews in the dataset have an ID which is unique.
#' There is an option to automatically mark for deletion the surveys which have a duplicated unique ID.
#'
#' @param ds dataset as a data.frame object
#' @param UniqueID name as a string of the field in the dataset where the unique ID is stored
#' @param survey_consent name as a string of the field in the dataset where the survey consent is stored
#' @param reportingcol columns as a list of string name from the dataset you want in the result (c('col1','col2',...))
#' @param delete delete action to be done as a boolean (TRUE/FALSE)
#'
#' @return  ds same dataset as the inputed one but with survey marked for deletion if errors are found and delete=TRUE
#' @return  errors  list of the errors found
#'
#' @author Yannick Pascaud
#'
#' @examples
#' {
#' ds <- HighFrequencyChecks::sample_dataset
#' UniqueID <- "X_uuid"
#' survey_consent <- "survey_consent"
#' reportingcol <- c("enumerator_id","X_uuid")
#' delete <- FALSE
#'
#'
#' list_unique_id <- chk2b_unique_id(ds, UniqueID, survey_consent, reportingcol, delete)
#' head(list_unique_id[[2]], 10)
#'}
#' @export chk2b_unique_id

chk2b_unique_id <- function(ds=NULL,
                            UniqueID=NULL,
                            survey_consent=NULL,
                            reportingcol=NULL,
                            delete=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(UniqueID) | !is.character(UniqueID)){
    stop("Please provide the field where the survey unique ID is stored")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete) | !is.logical(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(delete){
    ds[,survey_consent][duplicated(ds[,UniqueID])] <- "deleted"
  }

  # TO BE BE CHANGED WITH DYNAMIC COLUMS
  #errors <- subset(ds,duplicated(ds[,UniqueID])) %>% select(reportingcol, survey_consent=survey_consent)
  errors <- ds[ which(duplicated(ds[,UniqueID])) , c(reportingcol, survey_consent)]
  return(list(ds,errors))
}
