#' @name isInterviewCompleted
#' @rdname isInterviewCompleted
#' @title Check that all interviews were completed
#' @description This function check that all interviews in the dataset are completed,
#'   meaning all the interviews have an end date and time.
#'   There is an option to automatically mark for deletion the surveys which have not an end date.
#'
#' @param ds dataset as a data.frame object
#' @param survey_consent name as a string of the field in the dataset where the survey consent is stored
#' @param dates fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
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
#' survey_consent <- "survey_consent"
#' dates <- c("survey_start","end_survey")
#' reportingcol <- c("enumerator_id","X_uuid")
#' delete <- FALSE
#'
#' list_iscompled <- isInterviewCompleted(ds, survey_consent, dt, reportingcol, delete)
#' head(list_iscompled[[2]],10)
#'}
#'
#' @export isInterviewCompleted

isInterviewCompleted <- function(ds=NULL,
                                      survey_consent=NULL,
                                      dates=NULL,
                                      reportingcol=NULL,
                                      delete=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete) | !is.logical(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(delete){
    ds[,survey_consent][is.na(ds[,dates[2]])]<-"deleted"
  }

  errors <- subset(ds,is.na(ds[,dates[2]])) %>% select(reportingcol, survey_end=dates[2])
  return(list(ds,errors,NULL,NULL))
}

#' @name isInterviewWithConsent
#' @rdname isInterviewWithConsent
#' @title Check that all surveys have consent
#' @description This function check that all interviews in the dataset have information about the consent
#'  of the people surveyed, meaning all the field where this information is stored is not empty.
#'  There is an option to automatically mark for deletion the surveys which have not consent information.
#' @param ds dataset as a data.frame object
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
#' survey_consent <- "survey_consent"
#' reportingcol <- c("enumerator_id","X_uuid")
#' delete <- TRUE
#'
#' list_withconsent <- isInterviewWithConsent(ds, survey_consent, reportingcol, delete)
#' head(list_withconsent[[2]]),10)
#'}
#' @export isInterviewWithConsent

isInterviewWithConsent <- function(ds=NULL,
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
  if(is.null(reportingcol) | !is.character(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete) | !is.logical(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(delete){
    ds[,survey_consent][is.na(ds[,survey_consent])] <- "deleted"
  }


  errors <- subset(ds,is.na(survey_consent)) %>% select(reportingcol, survey_consent=survey_consent)
  return(list(ds,errors,NULL,NULL))
}

#' @name isInterviewInTheCorrectSite
#' @rdname isInterviewInTheCorrectSite
#' @title GIS check surveys for site
#' @description This function check that all interviews in the dataset were made in the correct site.
#' It is based on a GIS shapefile providing the boundaries of each site with their names.
#' The function is based on the GPS data filled in the survey to determine their location.
#' There is an option to automatically correct the site in the surveys whith the correct location.
#'
#' @param ds dataset as a data.frame object
#' @param adm dataset containing the shapefile - Regardless the projection used for the shapefile, it is transformed to WGS84
#' @param ds_site name as a string of the field in the dataset where the site is stored
#' @param ds_coord columns as a list of string name from the dataset where the GPS coordinates are stored (c('Long','Lat'))
#' @param adm_site name as a string of the field in the shapefile where the site is stored
#' @param survey_consent name as a string of the field in the dataset where the survey consent is stored
#' @param reportingcol columns as a list of string name from the dataset you want in the result (c('col1','col2',...))
#' @param correct correction action to be done as a boolean (TRUE/FALSE)
#'
#' @return  ds same dataset as the inputed one but with survey marked for deletion if errors are found and delete=TRUE
#' @return  errors  list of the errors found
#'
#' @author Yannick Pascaud
#'
#' @examples
#' {
#'   admin <- HighFrequencyChecks::admin
#'   df <- HighFrequencyChecks::sample_dataset
#'   df_site <- "union_name"
#'   df_coord <- c("X_gps_reading_longitude","X_gps_reading_latitude")
#'   admin_site <- "Union"
#'   sc <- "survey_consent"
#'   reportingcol <- c("enumerator_id","X_uuid")
#'   correct <- FALSE
#'
#'   list_site <- isInterviewInTheCorrectSite(admin, df, df_site, df_coord, admin_site, sc, reportingcol, correct)
#'   head(list_site[[2]], 10)
#'}
#' @export isInterviewInTheCorrectSite
#'

isInterviewInTheCorrectSite <- function(adm=NULL,
                            ds=NULL,
                            ds_site=NULL,
                            ds_coord=NULL,
                            adm_site=NULL,
                            survey_consent=NULL,
                            reportingcol=NULL,
                            correct=NULL){
  if(is.null(adm) | !isS4(adm) | nrow(adm)==0){
    stop("Please provide the spatial dataset of the boundaries shapefile")
  }
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(ds_site) | !is.character(ds_site)){
    stop("Please provide the field where the site to check against is stored")
  }
  if(is.null(ds_coord) | !is.character(ds_coord) | length(ds_coord)!=2){
    stop("Please provide the fields where the coordinates are stored (c('Long','Lat'))")
  }
  if(is.null(adm_site) | !is.character(ds_site)){
    stop("Please provide the field where the site in the shapefile is stored")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(correct) | !is.logical(correct)){
    stop("Please provide the correction action to be done (TRUE/FALSE)")
  }

  if(sp::is.projected(adm)){
    adm <- sp::spTransform(adm, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  }

  dfsp <- ds
  sp::coordinates(dfsp) <- dfsp[,c(ds_coord[1],ds_coord[2])]
  sp::proj4string(dfsp) <- sp::proj4string(adm)
  dfsp_over_adm <- sp::over(dfsp,adm)
  fm <- data.frame(ds,dfsp_over_adm, stringsAsFactors = FALSE)
  fm[,adm_site][is.na(fm[,adm_site])] <- ""

  # Should not be kept if the site in the kobo form is the same than the one in the shapefile
  #fm[,adm_site]<-tolower(as.character(gsub(" ", "_", fm[,adm_site])))
  #fm[,ds_site]<-tolower(as.character(gsub(" ", "_", fm[,ds_site])))

  fm$check <- ifelse(fm[,ds_site] != fm[,adm_site],"NOk","Ok")
  if(correct){
    ds[,ds_site][fm$check=="NOk"] <- fm[,adm_site][fm$check=="NOk"]
  }


  errors <- subset(fm,check=="NOk") %>% select(reportingcol, SiteRec=ds_site, SiteReal=adm_site)
  return(list(ds,errors,NULL,NULL))
}

#' @name isInterviewAtTheSamplePoint
#' @rdname isInterviewAtTheSamplePoint
#' @title GIS check surveys if fall without Xm radius from a sampled point
#' @description This function check that all interviews in the dataset were made within a distance from a sampled point.
#' It is based on a GIS shapefile providing the sample points for the assessment.
#' The function is based on the GPS data filled in the survey to determine their location.
#' There is an option to automatically mark for deletion the surveys which are to far away from a sampled point.
#'
#' One internal function "make_GeodesicBuffer" used to create the buffers is created by Valentin
#' https://stackoverflow.com/users/5193830/valentin
#'
#' @param ds dataset as a data.frame object
#' @param pts dataset containing the shapefile - Regardless the projection used for the shapefile, it is transformed to WGS84
#' @param ds_coord columns as a list of string name from the dataset where the GPS coordinates are stored (c('Long','Lat'))
#' @param buff value as an integer in meter to determine the buffer from a sampled point which is acceptable
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
#'  {
#' ds <- HighFrequencyChecks::sample_dataset
#' pts <- HighFrequencyChecks::SamplePts
#' ds_coord <- c("X_gps_reading_longitude","X_gps_reading_latitude")
#' buff <- 10
#' survey_consent <- "survey_consent"
#' reportingcol <- c("enumerator_id","X_uuid")
#' delete <- FALSE
#'
#'
#' list_sitept  <- isInterviewAtTheSamplePoint( ds, pts, ds_coord, buff, survey_consent, reportingcol, delete)
#' head(list_sitept[[2]], 10)
#'}
#' @export isInterviewAtTheSamplePoint
#'
isInterviewAtTheSamplePoint <- function(ds=NULL,
                           pts=NULL,
                           ds_coord=NULL,
                           buff=10,
                           survey_consent=NULL,
                           reportingcol=NULL,
                           delete=NULL){
  if(is.null(pts) | !isS4(pts) | nrow(pts)==0){
    stop("Please provide the spatial dataset of the sample points shapefile")
  }
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(ds_coord) | !is.character(ds_coord) | length(ds_coord)!=2){
    stop("Please provide the fields where the coordinates are stored (c('Long','Lat'))")
  }
  if(is.null(buff) | !is.numeric(buff)){
    stop("Please provide the buffer in meters")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete) | !is.logical(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  # function made by Valentin: https://stackoverflow.com/users/5193830/valentin
  make_GeodesicBuffer <- function(pts, width) {
    # A) Construct buffers as points at given distance and bearing
    dg <- seq(from = 0, to = 360, by = 5)
    # Construct equidistant points defining circle shapes (the "buffer points")
    buff.XY <- geosphere::destPoint(p = pts,
                                    b = rep(dg, each = length(pts)),
                                    d = width)
    # B) Make SpatialPolygons
    # Group (split) "buffer points" by id
    buff.XY <- as.data.frame(buff.XY)
    id  <- rep(1:dim(pts)[1], times = length(dg))
    lst <- split(buff.XY, id)
    # Make Spatial Polygons out of the list of coordinates
    poly   <- lapply(lst, sp::Polygon, hole = FALSE)
    polys  <- lapply(list(poly), sp::Polygons, ID = NA)
    spolys <- sp::SpatialPolygons(Srl = polys,
                                  proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
    # Disaggregate (split in unique polygons)
    spolys <- sp::disaggregate(spolys)
    return(spolys)
  }

  #buffer<-readOGR(GIS_folder, pts, stringsAsFactors = FALSE)
  if(sp::is.projected(pts)){
    pts <- sp::spTransform(pts, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  }
  #buffer<-gBuffer(buffer, width=buff, byid=TRUE)
  buffer <- make_GeodesicBuffer(as.matrix(data.frame(lon=pts$coords.x1,lat=pts$coords.x2)),buff)

  dfsp <-ds
  sp::coordinates(dfsp) <- dfsp[,c(ds_coord[1],ds_coord[2])]
  sp::proj4string(dfsp) <- sp::proj4string(buffer)
  dfsp_over_buffer <- sp::over(dfsp,buffer)
  fm <- data.frame(ds,dfsp_over_buffer, stringsAsFactors = FALSE)

  fm$Outside <- ifelse(is.na(fm$dfsp_over_buffer),"NOk","Ok")
  if(delete){
    ds[,survey_consent][fm$Outside=="NOk"]<-"deleted"
  }


  errors <- subset(fm, Outside=="NOk") %>% select(reportingcol, Outside=Outside)
  return(list(ds,errors,NULL,NULL))
}


#' @name isUniqueIDMissing
#' @rdname isUniqueIDMissing
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
#' list_missing_id <- isUniqueIDMissing(ds, UniqueID, survey_consent, reportingcol, delete)
#' head(list_missing_id[[2]], 10)
#'}
#' @export isUniqueIDMissing


isUniqueIDMissing <- function(ds=NULL,
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
    dplyr::select(reportingcol, survey_consent=survey_consent)
  return(list(ds,errors,NULL,NULL))
}


#' @name isUniqueIDDuplicated
#' @rdname isUniqueIDDuplicated
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
#' list_unique_id <- isUniqueIDDuplicated(ds, UniqueID, survey_consent, reportingcol, delete)
#' head(list_unique_id[[2]], 10)
#'}
#' @export isUniqueIDDuplicated

isUniqueIDDuplicated <- function(ds=NULL,
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

  errors <- subset(ds,duplicated(ds[,UniqueID])) %>%
    dplyr::select(reportingcol, survey_consent=survey_consent)
  return(list(ds,errors,NULL,NULL))
}

#' @name isSurveyOnMoreThanADay
#' @rdname isSurveyOnMoreThanADay
#' @title Surveys that do not end on the same day as they started
#' @description This function check that all interviews in the dataset start and end the same day.
#' There is an option to automatically mark for deletion the surveys which have different starting and ending dates.
#'
#' @param ds dataset as a data.frame object
#' @param dates fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
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
#' survey_consent <- "survey_consent"
#' dates <- c("survey_start","end_survey")
#' reportingcol <- c("enumerator_id","X_uuid")
#' delete <- FALSE
#'
#'
#' list_date_mistake <- isSurveyOnMoreThanADay(ds, survey_consent,dates, reportingcol, delete)
#' head(list_date_mistake[[2]], 10)
#'}
#' @export isSurveyOnMoreThanADay

isSurveyOnMoreThanADay <- function(ds=NULL,
                               survey_consent=NULL,
                               dates=NULL,
                               reportingcol=NULL,
                               delete=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete) | !is.logical(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(delete){
    ds[,survey_consent][stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[1]])),"uuuu-MM-dd")!= stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[2]])),"uuuu-MM-dd")]<-"deleted"
  }


  errors <- subset(ds, stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[1]])),"uuuu-MM-dd") != stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[2]])),"uuuu-MM-dd")) %>%
    select(reportingcol, survey_start=dates[1], survey_end=dates[2])
  return(list(ds,errors,NULL,NULL))
}

#' @name isSurveyEndBeforeItStarts
#' @rdname isSurveyEndBeforeItStarts
#' @title Surveys where end date/time is before the start date/time
#' @description This function check that all interviews in the dataset start before they end.
#' There is an option to automatically mark for deletion the surveys which have an ending date/time before the starting ones.
#'
#' @param ds dataset as a data.frame object
#' @param dates fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
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
#' survey_consent <- "survey_consent"
#' dates <- c("survey_start","end_survey")
#' reportingcol <- c("enumerator_id","X_uuid")
#' delete <- FALSE
#'
#'
#' list_date_mistake2 <- isSurveyEndBeforeItStarts(ds, survey_consent,dates, reportingcol, delete)
#' head(list_date_mistake2[[2]], 10)
#'}
#' @export isSurveyEndBeforeItStarts

isSurveyEndBeforeItStarts <- function(ds=NULL,
                               survey_consent=NULL,
                               dates=NULL,
                               reportingcol=NULL,
                               delete=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete) | !is.logical(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(delete){
    ds[,survey_consent][readr::parse_datetime(as.character(ds[,dates[1]])) > readr::parse_datetime(as.character(ds[,dates[2]]))]<-"deleted"
  }



  errors <- subset(ds, readr::parse_datetime(as.character(ds[,dates[1]])) > readr::parse_datetime(as.character(ds[,dates[2]]))) %>%
    select(reportingcol, survey_start=dates[1], survey_end=dates[2])
  return(list(ds,errors,NULL,NULL))

}


#' @name isSurveyStartedBeforeTheAssessment
#' @rdname isSurveyStartedBeforeTheAssessment
#' @title Surveys that show start date earlier than first day of data collection
#' @description This function check that all interviews in the dataset start after the actual first day of data collection.
#' There is an option to automatically mark for deletion the surveys which have started before the first day of data collection.
#'
#' @param ds dataset as a data.frame object
#' @param dates fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
#' @param survey_consent name as a string of the field in the dataset where the survey consent is stored
#' @param start_collection date as a string of the first day of data collection ('yyyy-mm-dd')
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
#' dates <- c("survey_start","end_survey")
#' survey_consent <- "survey_consent"
#' start_collection <- "2018-11-11"
#' reportingcol <- c("enumerator_id","X_uuid")
#' delete <- FALSE
#'
#'
#' list_date_mistake3 <- isSurveyStartedBeforeTheAssessment(ds, dates, survey_consent,start_collection, reportingcol, delete)
#' head(list_date_mistake3[[2]], 10)
#'}
#' @export isSurveyStartedBeforeTheAssessment


isSurveyStartedBeforeTheAssessment <- function(ds = NULL,
                               dates = NULL,
                               survey_consent = NULL,
                               start_collection = NULL,
                               reportingcol = NULL,
                               delete = NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(start_collection) | !is.character(start_collection)){
    stop("Please provide the date when the data collection began ('yyyy-mm-dd')")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete) | !is.logical(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(delete){
    ds[,survey_consent][start_collection > stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[1]])),"uuuu-MM-dd")]<-"deleted"
  }


  errors <- subset(ds,start_collection > stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[1]])),"uuuu-MM-dd")) %>%
    select(reportingcol, survey_start=dates[1])
  return(list(ds,errors,NULL,NULL))

}


#' @name isSurveyMadeInTheFuture
#' @rdname isSurveyMadeInTheFuture
#' @title Surveys that have start date/time after system date
#' @description This function check that all interviews in the dataset do not start after the current date.
#' There is an option to automatically mark for deletion the surveys which have a start date in the future.
#'
#' @param ds dataset as a data.frame object
#' @param dates fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
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
#' dates <- c("survey_start","end_survey")
#' survey_consent <- "survey_consent"
#' reportingcol <- c("enumerator_id","X_uuid")
#' delete <- FALSE
#'
#'
#' list_date_mistake4 <- isSurveyMadeInTheFuture(ds, uuid, survey_consent,reportingcol, delete)
#' head(list_date_mistake4[[2]], 10)
#'}
#' @export isSurveyMadeInTheFuture


isSurveyMadeInTheFuture <- function(ds=NULL,
                               survey_consent=NULL,
                               dates=NULL,
                               reportingcol=NULL,
                               delete=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete) | !is.logical(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(delete){
    ds[,survey_consent][Sys.Date() < stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[1]])),"uuuu-MM-dd")]<-"deleted"
  }

  # TO BE BE CHANGED WITH DYNAMIC COLUMS

  errors <- subset(ds,Sys.Date() < stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[1]])),"uuuu-MM-dd")) %>%
    select(reportingcol, survey_start=dates[1])
  return(list(ds,errors,NULL,NULL))

}

#' @name surveyMissingValues
#' @rdname surveyMissingValues
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
#' log <- surveyMissingValues(df, enumeratorID, enumeratorcheck)
#' head(log,10)
#'}
#' @export surveyMissingValues

surveyMissingValues <- function(ds=NULL, enumeratorID=NULL, enumeratorcheck=FALSE){
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
  return(list(NULL,logf,NULL,NULL))
}


#' @name surveyDistinctValues
#' @rdname surveyDistinctValues
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
#' log <- surveyDistinctValues(df, enumeratorID, enumeratorcheck)
#' head(log,10)
#'}
#' @export surveyDistinctValues


surveyDistinctValues <- function(ds=NULL, enumeratorID=NULL, enumeratorcheck=FALSE){
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
  return(list(NULL,t(logf),NULL,NULL))
}

#' @name surveyOtherValues
#' @rdname surveyOtherValues
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
#' log <- surveyOtherValues(df, otherpattern, enumeratorID, enumeratorcheck)
#' head(log,10)
#'}
#' @export surveyOtherValues


surveyOtherValues <- function(ds=NULL, otherpattern=NULL, enumeratorID=NULL, enumeratorcheck=FALSE){
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
    tmp <- data.frame(ds[,colnames(ds[,colnames(ds) %like% otherpattern])], stringsAsFactors = FALSE)
    tmp <- data.frame(utils::stack(tmp[1:ncol(tmp)]))
    logf <- subset(tmp, values!="") %>% group_by(field=ind, values) %>% summarize(nb=n())
  } else {
    tmp <- data.frame(ds[,c(enumeratorID,colnames(ds[,colnames(ds) %like% otherpattern]))], stringsAsFactors = FALSE)
    tmp <- data.frame(tmp[1], utils::stack(tmp[2:ncol(tmp)]))
    logf <- subset(tmp, values!="") %>% group_by_(field=colnames(tmp[3]), enumeratorID, colnames(tmp[2])) %>% summarize(nb=n())
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
#' log <- surveyOutliers(df, sdval, reportingcol, enumeratorID, enumeratorcheck)
#' head(log,10)
#'}
#' @export surveyOutliers

surveyOutliers <- function(ds=NULL,
                           sdval=NULL,
                           reportingcol=NULL,
                           enumeratorID=NULL,
                           enumeratorcheck=FALSE){
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
  return(list(NULL,logf,NULL,NULL))
}



#' @name surveyBigValues
#' @rdname surveyBigValues
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
#' log <- surveyBigValues(df, qu, v, eid, ec)
#' head(log,10)
#'}
#' @export surveyBigValues
#'
surveyBigValues <- function(ds=NULL, questions=NULL, value=NULL, reportingcol=NULL, enumeratorID=NULL, enumeratorcheck=FALSE){
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
  return(list(NULL,logf,NULL,NULL))
}


#' @name assessmentDuration
#' @rdname assessmentDuration
#' @title Compute the average and total time for the surveys
#' @description This function compute the average and total time for the surveys
#' Warning: If there are uncorrected mistakes in the survey dates, it can lead to have the length of the survey in seconds and this check will not performed well
#'
#' @param ds dataset as a data.frame object
#' @param dates fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
#'
#' @return avg  average time per survey
#' @return tot total time
#'
#' @author Yannick Pascaud
#'
#' @examples
#' {
#' ds <- HighFrequencyChecks::sample_dataset
#' dates <- c("survey_start","end_survey")
#'
#' assessmentDuration(ds, dates)
#'}
#' @export assessmentDuration

assessmentDuration <- function(ds=NULL, dates=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }

  surveytime <- as.double.difftime(( readr::parse_datetime(as.character(ds[,dates[2]])) -
                                       readr::parse_datetime(as.character(ds[,dates[1]]))),
                                   units = "secs") / 60

  avg <- round(mean(surveytime), digits = 2)
  tot <- round(sum(surveytime), digits = 2)
  return(list(NULL,NULL,list(avg=avg,tot=tot),NULL))
}

#' @name isInterviewTooShort
#' @rdname isInterviewTooShort
#' @title Check that the duration of each interview is more than a threshold
#' @description This function check that the duration of each interview is more than a specified threshold.
#' There is an option to automatically mark for deletion the surveys which are under the threshold.
#' Warning: If there are uncorrected mistakes in the survey dates, it can lead to have the length of the survey in seconds and this check will not performed well
#'
#' @param ds dataset as a data.frame object
#' @param dates fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
#' @param minduration minimum acceptable survey duration as integer in minutes
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
#' survey_consent <- "survey_consent"
#' dates <- c("survey_start","end_survey")
#' reportingcol <- c("enumerator_id","X_uuid")
#' minduration <- 30
#' delete <- FALSE
#'
#' list <- isInterviewTooShort(ds, survey_consent, dates,  reportingcol, minduration, delete)
#' head(list[[2]], 10)
#'}
#' @export isInterviewTooShort

isInterviewTooShort <- function(ds=NULL,
                                survey_consent=NULL,
                                dates=NULL,
                                reportingcol=NULL,
                                minduration=30,
                                delete=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(minduration) | !is.numeric(minduration)){
    stop("Please provide the minimum survey time to check against")
  }
  if(is.null(delete) | !is.logical(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  tmp <- data.frame(ds[reportingcol],
                    SurveyLength = as.double.difftime(( readr::parse_datetime(as.character(ds[,dates[2]])) -
                                                          readr::parse_datetime(as.character(ds[,dates[1]]))),
                                                      units = "secs") / 60)

  if(delete){
    ds[,survey_consent][tmp$SurveyLength<minduration] <- "deleted"
  }
  errors <- subset(tmp, SurveyLength<minduration)
  return(list(ds,errors,NULL,NULL))
}


#' @name isInterviewTooShortForTheHouseholdSize
#' @rdname isInterviewTooShortForTheHouseholdSize
#' @title Check that the duration relative to the household size of each interview is more than a threshold
#' @description This function check that the duration relative to the household size of each interview is more than a specified threshold.
#' There is an option to automatically mark for deletion the surveys which are under the threshold.
#' Warning: If there are uncorrected mistakes in the survey dates, it can lead to have the length of the survey in seconds and this check will not performed well
#'
#' @param ds dataset as a data.frame object
#' @param dates fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
#' @param minduration minimum acceptable survey duration as integer in minutes
#' @param HHSize name as a string of the field in the dataset where the household size is stored
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
#' survey_consent <- "survey_consent"
#' HHSize <-"consent_received.respondent_info.hh_size"
#' dates <- c("survey_start","end_survey")
#' reportingcol <- c("enumerator_id","X_uuid")
#' minduration <- 30
#' delete <- FALSE
#'
#' list_duration_Xmin <- isInterviewTooShortForTheHouseholdSize(ds, survey_consent, dates, HHSize, reportingcol, minduration, delete)
#' head(list_duration_Xmin[[2]], 10)
#'}
#' @export isInterviewTooShortForTheHouseholdSize

isInterviewTooShortForTheHouseholdSize <- function(ds=NULL,
                                       survey_consent=NULL,
                                       dates=NULL,
                                       HHSize=NULL,
                                       reportingcol=NULL,
                                       minduration=10,
                                       delete=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(HHSize) | !is.character(HHSize)){
    stop("Please provide the field where the HH size is stored")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(minduration) | !is.numeric(minduration)){
    stop("Please provide the minimum survey time to check against")
  }
  if(is.null(delete) | !is.logical(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  tmp<-data.frame(ds[reportingcol], HHSize=ds[,HHSize],
                  SurveyLength=as.double.difftime(( readr::parse_datetime(as.character(ds[,dates[2]])) -
                                                      readr::parse_datetime(as.character(ds[,dates[1]]))),
                                                  units = "secs") / 60)

  if(delete){
    ds[,survey_consent][(tmp$SurveyLength/tmp$HHSize)<minduration]<-"deleted"
  }
  errors <- subset(tmp, (SurveyLength/HHSize)<minduration)
  return(list(ds,errors,NULL,NULL))
}

#' @name assessmentDurationOutliers
#' @rdname assessmentDurationOutliers
#' @title Report the outlier durations for the surveys
#' @description This function report the outlier durations for the surveys
#'
#' @param ds dataset as a data.frame object
#' @param dates fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
#' @param sdval number of standard deviation for which the duration within is considered as acceptable
#' @param reportingcol columns as a list of string name from the dataset you want in the result (c('col1','col2',...))
#'
#' @return logf  the report
#'
#' @author Yannick Pascaud
#'
#' @examples
#' {
#' ds <- HighFrequencyChecks::sample_dataset
#' dates <- c("survey_start","end_survey")
#' sdval <- 5
#' reportingcol <- c("enumerator_id","X_uuid")
#'
#' log <- assessmentDurationOutliers(ds, dates, sdval, reportingcol)
#' head(log,10)
#'}
#' @export assessmentDurationOutliers

assessmentDurationOutliers <- function(ds=NULL,
                                    dates=NULL,
                                    sdval=NULL,
                                    reportingcol=NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(sdval) | !is.numeric(sdval)){
    stop("Please provide the number of standard deviations you want to check for")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }

  surveytime <- data.frame(duration= as.double.difftime(( readr::parse_datetime(as.character(ds[,dates[2]])) -
                                                            readr::parse_datetime(as.character(ds[,dates[1]]))),
                                                        units = "secs") / 60)
  duration_outliers <- data.frame(outliers::scores(surveytime, type = "z"))
  tmp <- data.frame(ds[,reportingcol],surveytime,duration_outliers)
  colnames(tmp)[length(tmp)] <- "Zscore"
  logf <- subset(tmp, abs(Zscore)>sdval)
  return(list(NULL,logf,NULL,NULL))
}


#' @name enumeratorSurveysConsent
#' @rdname enumeratorSurveysConsent
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
#' ds <- HighFrequencyChecks::sample_dataset
#' sc <- "survey_consent"
#' enumeratorID <- "enumerator_id"
#'
#' log <- enumeratorSurveysConsent(ds, sc, enumeratorID)
#' head(log,10)
#'}
#' @export enumeratorSurveysConsent


enumeratorSurveysConsent <- function(ds=NULL,
                          survey_consent=NULL,
                          enumeratorID=NULL){
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
  return(list(NULL,logf,NULL,NULL))
}

#' @name enumeratorSurveysDuration
#' @rdname enumeratorSurveysDuration
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
#' ds <- HighFrequencyChecks::sample_dataset
#' dt <- c("survey_start","end_survey")
#' enumeratorID <- "enumerator_id"
#'
#' log <- enumeratorSurveysDuration(ds, dt, enumeratorID)
#' head(log,10)
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

  ds$surveytime <- as.double.difftime((strptime(ds[,dates[2]],"%Y-%m-%dT%R") - strptime(ds[,dates[1]],"%Y-%m-%dT%R")), units = "secs")/60
  overall_avg_duration <- round(mean(ds$surveytime), digits=2)
  logf<-ds %>% group_by(enumeratorID=ds[,enumeratorID]) %>% summarize(duration_mean = round(mean(surveytime), digits=2),
                                                                      overall_avg_duration,
                                                                      perc_diff_avg = round(((duration_mean - overall_avg_duration) / overall_avg_duration) * 100, digits=2))
  return(list(NULL,logf,NULL,NULL))
}

#' @name enumeratorProductivity
#' @rdname enumeratorProductivity
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
#' ds <- HighFrequencyChecks::sample_dataset
#' surveydate <- "survey_date"
#' enumeratorID <- "enumerator_id"
#'
#' log <- enumeratorProductivity(ds, surveydate, enumeratorID)
#' head(log,10)
#'}
#' @export enumeratorProductivity

enumeratorProductivity <- function(ds=NULL,
                            surveydate=NULL,
                            enumeratorID=NULL){
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
  return(list(NULL,logf,NULL,NULL))
}

#' @name enumeratorProductivityOutliers
#' @rdname enumeratorProductivityOutliers
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
#' ds <- HighFrequencyChecks::sample_dataset
#' enumeratorID <- "enumerator_id"
#' surveydate <- "survey_date"
#' sdval<-2
#'
#' log <- enumeratorProductivityOutliers(ds, enumeratorID, surveydate, sdval)
#' head(log,10)
#'}
#' @export enumeratorProductivityOutliers

enumeratorProductivityOutliers <- function(ds=NULL,
                               enumeratorID=NULL,
                               surveydate=NULL,
                               sdval=NULL){
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
  logf <- subset(tmp, abs(survey_outliers) > sdval)

  return(list(NULL,logf,NULL,NULL))
}


#' @name enumeratorIsLazy
#' @rdname enumeratorIsLazy
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
#' ds <- HighFrequencyChecks::sample_dataset
#' enumeratorID <- "enumerator_id"
#' questions <- c("consent_received.shelter_nfi.non_food_items[.]",
#'       "consent_received.food_security.main_income[.]",
#'       "consent_received.child_protection.boy_risk[.]",
#'       "consent_received.child_protection.girl_risk[.]")
#' mna <- 3
#'
#' log <- enumeratorIsLazy(ds, enumeratorID, questions, mna)
#' head(log,10)
#'}
#' @export enumeratorIsLazy

enumeratorIsLazy <- function(ds=NULL,
                                          enumeratorID=NULL,
                                          questions=NULL,
                                          minnbanswers=NULL){
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
  return(list(NULL,logf,NULL,NULL))
}


#' @name assessmentProductivity
#' @rdname assessmentProductivity
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
#' df <- HighFrequencyChecks::sample_dataset
#' sdte <- "survey_date"
#' dtf <- "%m/%d/%Y"
#' sc <- "survey_consent"
#'
#' log <- assessmentProductivity(df, sdte, dtf, sc)
#' head(log,10)
#'}
#' @export assessmentProductivity

assessmentProductivity <- function(ds=NULL, surveydate=NULL, dateformat=NULL, survey_consent=NULL){
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
    group_by(surveydate=surveydate) %>%
    summarize(NbSurvey=n())
  tmp$surveydate<-as.Date(tmp$surveydate, dateformat)
  logf<-tmp[with(tmp, order(surveydate)), ]
  return(list(NULL,logf,NULL,NULL))
}

#' @name assessmentProductivityGraphical
#' @rdname assessmentProductivityGraphical
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
#' {
#' ds <- HighFrequencyChecks::sample_dataset
#' surveydate <- "survey_date"
#' dateformat <- "%m/%d/%Y"
#' survey_consent <- "survey_consent"
#'
#' assessmentProductivityGraphical(ds, surveydate, dateformat, survey_consent)
#'}
#' @export assessmentProductivityGraphical

assessmentProductivityGraphical <- function(ds = NULL,
                                      surveydate = NULL,
                                      dateformat = NULL,
                                      survey_consent = NULL){
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

  tmp <- ds %>%
    group_by_(surveydate=surveydate) %>%
    count(survey_consent)

  colnames(tmp)[2] <- "survey_consent"
  tmp$surveydate <- as.Date(tmp$surveydate, dateformat)
  tmp <- tmp[with(tmp, order(surveydate)), ]
  tmp <- reshape2::dcast(tmp, surveydate ~ survey_consent, value.var="n")
  tmp[is.na(tmp)] <- 0

  graph <- plotly::plot_ly(type = 'bar', width = 800, height = 600)
  for(i in 2:length(tmp)){
    graph<-add_trace(graph, x=tmp[,1], y = tmp[,i], name = colnames(tmp)[i])
  }
  graph <- plotly::layout(graph,
                          yaxis = list(title = "Nb Survey"),
                          barmode = "stack")

  return(list(NULL,NULL,NULL,graph))
}


#' @name assessmentDailyValidSurveys
#' @rdname assessmentDailyValidSurveys
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
#' {
#' ds <- HighFrequencyChecks::sample_dataset
#' surveydate <- "survey_date"
#' dateformat <- "%m/%d/%Y"
#' survey_consent <- "survey_consent"
#'
#' log <- assessmentDailyValidSurveys(ds, surveydate, dateformat, survey_consent )
#' head(log,10)
#'}
#'
#' @export assessmentDailyValidSurveys

assessmentDailyValidSurveys <- function(ds=NULL,
                             surveydate=NULL,
                             dateformat=NULL,
                             survey_consent=NULL){
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

  tmp <- ds %>% group_by_(surveydate=surveydate) %>% count(survey_consent)
  colnames(tmp)[2] <- "survey_consent"
  tmp$surveydate <- as.Date(tmp$surveydate, dateformat)
  tmp <- tmp[with(tmp, order(surveydate)), ]
  logf <- reshape2::dcast(tmp,surveydate ~ survey_consent, value.var="n")
  logf[is.na(logf)] <- 0
  return(list(NULL,logf,NULL,NULL))
}

#' @name assessmentTrackingSheet
#' @rdname assessmentTrackingSheet
#' @title Overall tracking sheet
#' @description This function display the overall tracking sheet.
#'
#' @param ds dataset as a data.frame object
#' @param sf sampling frame as a data.frame object
#' @param dssite  name as a string of the field in the dataset where the site is stored
#' @param sfsite name as a string of the field in the sampling frame where the site is stored
#' @param survey_consent name as a string of the field in the dataset where the survey consent is stored
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
#' ds <- HighFrequencyChecks::sample_dataset
#' sf <- HighFrequencyChecks::SampleSize
#' dssite <- "union_name"
#' sfsite <- "Union"
#' survey_consent <- "survey_consent"
#' sftarget <- "SS"
#' sfnbpts <- "TotPts"
#' formul <- c("done-no-not_eligible-delete",
#'                "done-no-not_eligible-deleted-SS")
#' colorder <- c("site","SS","Provisio","done","not_eligible",
#'                  "no","deleted","yes","final","variance")
#'
#' log <- assessmentTrackingSheet(ds,
#'                         sf,
#'                         dssite,
#'                         sfsite,
#'                         survey_consent,
#'                         sftarget,
#'                         sfnbpts,
#'                         formul,
#'                         colorder)
#' head(log,10)
#'
#'}
#' @export assessmentTrackingSheet
#'

assessmentTrackingSheet <- function(ds=NULL,
                             sf=NULL,
                             dssite=NULL,
                             sfsite=NULL,
                             survey_consent=NULL,
                             sftarget=NULL,
                             sfnbpts=NULL,
                             formul=NULL,
                             colorder=NULL){
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
  if(is.null(survey_consent) | !is.character(survey_consent)){
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
  ## df2<-data.frame(ds, stringsAsFactors = FALSE) %>% group_by(dssite) %>% count(survey_consent) %>% mutate(done=sum(n))
  ## colnames(df2)[2]<-"site"
  #df2<-ds[,c(dssite,survey_consent)]
  #colnames(df2)<-c("site","consent")
  #df2$site<-as.character(df2$site)
  #df2$consent<-as.character(df2$consent)
  ## dssite<-lazyeval::lazy(dssite)
  ## survey_consent<-lazyeval::lazy(survey_consent)
  df2 <- ds %>% group_by_(site=dssite, consent=survey_consent) %>% summarize(n=n()) %>% mutate(done=sum(n))
  ##df2<-ds %>% group_by(.dots=list(site,consent)) # %>% summarize_(n=n()) %>% mutate(done=sum(n))

  #df2<-ds %>% group_by_(site=dssite) %>% count_(survey_consent) %>% mutate(done=sum(n))
  df2 <-reshape2::dcast(df2,site + done ~ consent, value.var="n")
  df <- merge(df1,df2, by.x=c("site"), by.y=c("site"), all.x=TRUE)
  df[is.na(df)] <- 0

  formul[1] <- paste0("df[,'",
                      stringi::stri_replace_all_fixed(formul[1],
                                                      c("+","-","/","*"),
                                                      c("'] + df[,'","'] - df[,'","'] / df[,'","'] * df[,'"),
                                                      vectorize_all=FALSE),
                      "']")
  df$final <- eval(parse(text=formul[1]))

  formul[2] <- paste0("df[,'",
                      stringi::stri_replace_all_fixed(formul[2],
                                                      c("+","-","/","*"),
                                                      c("'] + df[,'","'] - df[,'","'] / df[,'","'] * df[,'"),
                                                      vectorize_all=FALSE),
                      "']")
  df$variance <- eval(parse(text=formul[2]))

  logf <- df[colorder]
  return(list(NULL,logf,NULL,NULL))
}




