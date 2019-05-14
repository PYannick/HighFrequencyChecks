
# chk4aiii_missing_pct: Missing values percentage per variables
# chk4bii_distinct_values: Number of distinct values (not missing) per fields
# chk4biv_others_values: List of other distinct values (not missing) per fields other with count
# chk4c_distribution: Distribution of specific coded values ----------------Not implemented yet-----------------
# chk4d_outliers: Outliers
# chk4e_values_greater_X: Check values greater than X


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
    logf<-data.frame(variables=colnames(ds), pct=colMeans(is.na(ds[]) | ds[]=="") * 100)
  } else {
    logf<-ds %>%
      group_by(ds[,enumeratorID]) %>%
      summarise_all(funs(100*mean(is.na(.) | .=="")))
  }
  return(logf)
}

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
    tmp <- data.frame(stack(tmp[1:ncol(tmp)]))
    logf <- subset(tmp, values!="") %>% group_by(field=ind, values) %>% summarize(nb=n())
  } else {
    tmp <- ds[,c(enumeratorID,colnames(ds[,colnames(ds) %like% otherpattern]))]
    tmp <- data.frame(tmp[1], stack(tmp[2:ncol(tmp)]))
    logf <- subset(tmp, values!="") %>% group_by_(field=colnames(tmp[3]), enumeratorID, colnames(tmp[2])) %>% summarize(nb=n())
  }
  return(logf)
}

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
    pop_sd <- sd(x, na.rm = TRUE)*sqrt((length(na.omit(x))-1)/(length(na.omit(x))))
    pop_mean <- mean(x, na.rm = TRUE)
    inz<-function(x){
      return((x - pop_mean) / pop_sd)
    }
    return(inz(x))
  }
  norm_or_lognorm <- function(x){
    norm<-normalized(x)
    lognorm<-normalized(signedlog10(x))
    if(abs(mean(norm, na.rm = TRUE)-median(norm, na.rm = TRUE))>abs(mean(lognorm, na.rm = TRUE)-median(lognorm, na.rm = TRUE))){
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

  scores_outliers <- data.frame(scores_outliers[reportingcol], stack(scores_outliers[(names(scores_outliers) %ni% reportingcol)]), stringsAsFactors = FALSE)
  scores_outliers <- subset(scores_outliers, abs(values)>=sdval)
  scores_outliers$ind <- as.character(scores_outliers$ind)
  logf<-left_join(scores_outliers,distribution_type,by=c("ind"="ind"))
  return(logf)
}

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

  tmp <- data.frame(ds[reportingcol], stack(ds[questions]), stringsAsFactors = FALSE)
  logf <- subset(tmp, values>=value)
  return(logf)
}
