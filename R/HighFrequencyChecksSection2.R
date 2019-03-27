
# chk2a_missing_id: Missing unique ID
# chk2b_unique_id: Duplicates in unique ID


chk2a_missing_id <- function(ds=NULL, UniqueID=NULL, survey_consent=NULL, reportingcol=NULL, delete=NULL){
  if(is.null(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(UniqueID)){
    stop("Please provide the field where the survey unique ID is stored")
  }
  if(is.null(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(delete){
    ds[,survey_consent][is.na(ds[,UniqueID])]<-"deleted"
  }

  # TO BE BE CHANGED WITH DYNAMIC COLUMS
  errors <- subset(ds,is.na(ds[,UniqueID]) | ds[,UniqueID]=="") %>% select(reportingcol, survey_consent=survey_consent)
  return(list(ds,errors))
}

chk2b_unique_id <- function(ds=NULL, UniqueID=NULL, survey_consent=NULL, reportingcol=NULL, delete=NULL){
  if(is.null(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(UniqueID)){
    stop("Please provide the field where the survey unique ID is stored")
  }
  if(is.null(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(delete){
    ds[,survey_consent][duplicated(ds[,UniqueID])]<-"deleted"
  }

  # TO BE BE CHANGED WITH DYNAMIC COLUMS
  errors <- subset(ds,duplicated(ds[,UniqueID])) %>% select(reportingcol, survey_consent=survey_consent)
  return(list(ds,errors))
}







