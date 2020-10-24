#' @name isInterviewCompleted
#' @rdname isInterviewCompleted
#' @title Check that all interviews were completed
#' @description This function check that all interviews in the dataset are completed,
#'   meaning all the interviews have an end date and time.
#'   There is an option to automatically mark for deletion the surveys which have not an end date.
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param surveyConsent name of the field in the dataset where the survey consent is stored: string
#' @param dates name of the fields where the information about the start and end date of the survey is stored: list of string (c('start_date','end_date'))
#' @param uniqueID name of the field where the survey unique ID is stored: string
#' @param enumeratorID name of the field where the enumerator ID is stored: string
#' @param reportingColumns (Optional, by default it is built from the enumeratorID and the UniqueID) name of the columns from the dataset you want in the result: list of string (c('col1','col2',...))
#' @param deleteIsInterviewCompleted (Optional, by default set as FALSE) if TRUE, the survey in error will be marked as 'deletedIsInterviewCompleted': boolean (TRUE/FALSE)
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
#' dates <- c("survey_start","end_survey")
#' uniqueID <- "X_uuid"
#' enumeratorID <- "enumerator_id"
#'
#' list[dst,ret_log,var,graph] <- isInterviewCompleted(ds=ds,
#'                                                     surveyConsent=surveyConsent,
#'                                                     dates=dates)
#' head(ret_log,10)
#' print(graph)
#'}
#'
#' @export isInterviewCompleted
isInterviewCompleted <- function(ds=NULL,
                                 surveyConsent=NULL,
                                 dates=NULL,
                                 reportingColumns=c(enumeratorID, uniqueID),
                                 deleteIsInterviewCompleted=FALSE){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveyConsent) | !is.character(surveyConsent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(reportingColumns) | !is.character(reportingColumns)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(deleteIsInterviewCompleted) | !is.logical(deleteIsInterviewCompleted)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(deleteIsInterviewCompleted){
    ds[,surveyConsent][is.na(ds[,dates[2]])]<-"deletedIsInterviewCompleted"
  }

  errors <- subset(ds,is.na(ds[,dates[2]])) %>% select(all_of(reportingColumns), survey_end=dates[2])
  # graph <- piechart(data.frame(check=is.na(ds[,dates[2]])), "isInterviewCompleted")
  graph <- piechart(data.frame(categories=c("OK", "NOK"),
                               Nb=c(length(ds[,1])-length(errors[,1]),
                                    length(errors[,1]))),
                    "isInterviewCompleted")
  return(list(ds,errors,NULL,graph))
}

#' @name isInterviewWithConsent
#' @rdname isInterviewWithConsent
#' @title Check that all surveys have consent
#' @description This function check that all interviews in the dataset have information about the consent
#'  of the people surveyed, meaning all the field where this information is stored is not empty.
#'  There is an option to automatically mark for deletion the surveys which have not consent information.
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param surveyConsent name of the field in the dataset where the survey consent is stored: string
#' @param uniqueID name of the field where the survey unique ID is stored: string
#' @param enumeratorID name of the field where the enumerator ID is stored: string
#' @param reportingColumns (Optional, by default it is built from the enumeratorID and the UniqueID) name of the columns from the dataset you want in the result: list of string (c('col1','col2',...))
#' @param deleteIsInterviewWithConsent (Optional, by default set as FALSE) if TRUE, the survey in error will be marked as 'deletedIsInterviewWithConsent': boolean (TRUE/FALSE)
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
#' uniqueID <- "X_uuid"
#' enumeratorID <- "enumerator_id"
#'
#' list[dst,ret_log,var,graph] <- isInterviewWithConsent(ds,
#'                                                       surveyConsent)
#' head(ret_log,10)
#' print(graph)
#'}
#' @export isInterviewWithConsent
isInterviewWithConsent <- function(ds=NULL,
                                   surveyConsent=NULL,
                                   reportingColumns=c(enumeratorID, uniqueID),
                                   deleteIsInterviewWithConsent=FALSE){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveyConsent) | !is.character(surveyConsent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(reportingColumns) | !is.character(reportingColumns)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(deleteIsInterviewWithConsent) | !is.logical(deleteIsInterviewWithConsent)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(deleteIsInterviewWithConsent){
    ds[,surveyConsent][is.na(ds[,surveyConsent])] <- "deletedIsInterviewWithConsent "
  }

  errors <- subset(ds,is.na(surveyConsent)) %>% select(all_of(reportingColumns), survey_consent=all_of(surveyConsent))
  # graph <- piechart(data.frame(check=is.na(ds[,surveyConsent])), "isInterviewWithConsent")
  graph <- piechart(data.frame(categories=c("OK", "NOK"),
                               Nb=c(length(ds[,1])-length(errors[,1]),
                                    length(errors[,1]))),
                    "isInterviewWithConsent")
  return(list(ds,errors,NULL,graph))
}

#' @name isInterviewInTheCorrectSite
#' @rdname isInterviewInTheCorrectSite
#' @title GIS check surveys for site
#' @description This function check that all interviews in the dataset were made in the correct site.
#' It is based on a GIS shapefile providing the boundaries of each site with their names.
#' The function is based on the GPS data filled in the survey to determine their location.
#' There is an option to automatically correct the site in the surveys whith the correct location.
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param dsSite name of the field in the dataset where the site is stored: string
#' @param dsCoordinates name of the fields from the dataset where the information about the GPS coordinates are stored: list of string (c('Long','Lat'))
#' @param adminBoundaries dataset containing the shapefile of the site boundaries - Regardless the projection used for the shapefile, it is transformed to WGS84
#' @param adminBoundariesSite name of the field in the shapefile where the site is stored: string
#' @param surveyConsent name of the field in the dataset where the survey consent is stored: string
#' @param uniqueID name of the field where the survey unique ID is stored: string
#' @param enumeratorID name of the field where the enumerator ID is stored: string
#' @param reportingColumns (Optional, by default it is built from the enumeratorID and the UniqueID) name of the columns from the dataset you want in the result: list of string (c('col1','col2',...))
#' @param correctIsInterviewInTheCorrectSite (Optional, by default set as FALSE) if TRUE, the site in the survey which is wrong will be replaced by the real one: boolean (TRUE/FALSE)
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
#'   ds <- HighFrequencyChecks::sample_dataset
#'   dsSite <- "union_name"
#'   dsCoordinates <- c("X_gps_reading_longitude","X_gps_reading_latitude")
#'   adminBoundaries <- HighFrequencyChecks::admin
#'   adminBoundariesSite <- "Union"
#'   surveyConsent <- "survey_consent"
#'   uniqueID <- "X_uuid"
#'   enumeratorID <- "enumerator_id"
#'
#'   list[dst,ret_log,var,graph] <- isInterviewInTheCorrectSite(ds,
#'                                                              dsSite,
#'                                                              dsCoordinates,
#'                                                              adminBoundaries,
#'                                                              adminBoundariesSite,
#'                                                              surveyConsent)
#'   head(ret_log, 10)
#'}
#' @export isInterviewInTheCorrectSite
#'
isInterviewInTheCorrectSite <- function(ds=NULL,
                                        dsSite=NULL,
                                        dsCoordinates=NULL,
                                        adminBoundaries=NULL,
                                        adminBoundariesSite=NULL,
                                        surveyConsent=NULL,
                                        reportingColumns=c(enumeratorID, uniqueID),
                                        correctIsInterviewInTheCorrectSite=FALSE){
  if(is.null(adminBoundaries) | !isS4(adminBoundaries) | nrow(adminBoundaries)==0){
    stop("Please provide the spatial dataset of the boundaries shapefile")
  }
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(dsSite) | !is.character(dsSite)){
    stop("Please provide the field where the site to check against is stored")
  }
  if(is.null(dsCoordinates) | !is.character(dsCoordinates) | length(dsCoordinates)!=2){
    stop("Please provide the fields where the coordinates are stored (c('Long','Lat'))")
  }
  if(is.null(adminBoundariesSite) | !is.character(dsSite)){
    stop("Please provide the field where the site in the shapefile is stored")
  }
  if(is.null(surveyConsent) | !is.character(surveyConsent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(reportingColumns) | !is.character(reportingColumns)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(correctIsInterviewInTheCorrectSite) | !is.logical(correctIsInterviewInTheCorrectSite)){
    stop("Please provide the correction action to be done (TRUE/FALSE)")
  }

  if(sp::is.projected(adminBoundaries)){
    adm <- sp::spTransform(adminBoundaries, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  }

  dfsp <- ds
  sp::coordinates(dfsp) <- dfsp[,c(dsCoordinates[1],dsCoordinates[2])]
  sp::proj4string(dfsp) <- sp::proj4string(adminBoundaries)
  dfsp_over_adm <- sp::over(dfsp,adminBoundaries)
  fm <- data.frame(ds,dfsp_over_adm, stringsAsFactors = FALSE)
  fm[,adminBoundariesSite][is.na(fm[,adminBoundariesSite])] <- ""

  fm$check <- ifelse(fm[,dsSite] != fm[,adminBoundariesSite],"NOk","Ok")
  if(correctIsInterviewInTheCorrectSite){
    ds[,dsSite][fm$check=="NOk"] <- fm[,adminBoundariesSite][fm$check=="NOk"]
  }

  errors <- subset(fm,check=="NOk") %>% select(all_of(reportingColumns), SiteRec=all_of(dsSite), SiteReal=all_of(adminBoundariesSite))
  graph <- piechart(data.frame(categories=c("OK", "NOK"),
                               Nb=c(length(ds[,1])-length(errors[,1]),
                                    length(errors[,1]))),
                    "isInterviewInTheCorrectSite")
  return(list(ds,errors,NULL,graph))
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
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param dsCoordinates name of the fields from the dataset where the information about the GPS coordinates are stored: list of string (c('Long','Lat'))
#' @param sampledPoints dataset containing the shapefile of the households sampled - Regardless the projection used for the shapefile, it is transformed to WGS84
#' @param buffer value in meter to determine the buffer from a sampled point which is acceptable: integer
#' @param surveyConsent name of the field in the dataset where the survey consent is stored: string
#' @param uniqueID name of the field where the survey unique ID is stored: string
#' @param enumeratorID name of the field where the enumerator ID is stored: string
#' @param reportingColumns (Optional, by default it is built from the enumeratorID and the UniqueID) name of the columns from the dataset you want in the result: list of string (c('col1','col2',...))
#' @param deleteIsInterviewAtTheSamplePoint (Optional, by default set as FALSE) if TRUE, the survey in error will be marked as 'deletedIsInterviewAtTheSamplePoint': boolean (TRUE/FALSE)
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
#' sampledPoints <- HighFrequencyChecks::SamplePts
#' dsCoordinates <- c("X_gps_reading_longitude","X_gps_reading_latitude")
#' buffer <- 10
#' surveyConsent <- "survey_consent"
#' uniqueID <- "X_uuid"
#' enumeratorID <- "enumerator_id"
#'
#' list[dst,ret_log,var,graph] <- isInterviewAtTheSamplePoint(ds,
#'                                                            dsCoordinates,
#'                                                            sampledPoints,
#'                                                            buffer,
#'                                                            surveyConsent)
#' head(ret_log, 10)
#' }
#' @export isInterviewAtTheSamplePoint
isInterviewAtTheSamplePoint <- function(ds=NULL,
                                        dsCoordinates=NULL,
                                        sampledPoints=NULL,
                                        buffer=10,
                                        surveyConsent=NULL,
                                        reportingColumns=c(enumeratorID, uniqueID),
                                        deleteIsInterviewAtTheSamplePoint=FALSE){
  if(is.null(sampledPoints) | !isS4(sampledPoints) | nrow(sampledPoints)==0){
    stop("Please provide the spatial dataset of the sample points shapefile")
  }
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(dsCoordinates) | !is.character(dsCoordinates) | length(dsCoordinates)!=2){
    stop("Please provide the fields where the coordinates are stored (c('Long','Lat'))")
  }
  if(is.null(buffer) | !is.numeric(buffer)){
    stop("Please provide the buffer in meters")
  }
  if(is.null(surveyConsent) | !is.character(surveyConsent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(reportingColumns) | !is.character(reportingColumns)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(deleteIsInterviewAtTheSamplePoint) | !is.logical(deleteIsInterviewAtTheSamplePoint)){
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

  if(sp::is.projected(sampledPoints)){
    sampledPoints <- sp::spTransform(sampledPoints, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  }
  bufferSHP <- make_GeodesicBuffer(as.matrix(data.frame(lon=sampledPoints$coords.x1,lat=sampledPoints$coords.x2)),buffer)

  dfsp <-ds
  sp::coordinates(dfsp) <- dfsp[,c(dsCoordinates[1],dsCoordinates[2])]
  sp::proj4string(dfsp) <- sp::proj4string(bufferSHP)
  dfsp_over_buffer <- sp::over(dfsp,bufferSHP)
  fm <- data.frame(ds,dfsp_over_buffer, stringsAsFactors = FALSE)

  fm$Outside <- ifelse(is.na(fm$dfsp_over_buffer),"NOk","Ok")
  if(deleteIsInterviewAtTheSamplePoint){
    ds[,surveyConsent][fm$Outside=="NOk"]<-"deletedIsInterviewAtTheSamplePoint"
  }

  errors <- subset(fm, Outside=="NOk") %>% select(all_of(reportingColumns), Outside=Outside)
  graph <- piechart(data.frame(categories=c("OK", "NOK"),
                               Nb=c(length(ds[,1])-length(errors[,1]),
                                    length(errors[,1]))),
                    "isInterviewAtTheSamplePoint")
  return(list(ds,errors,NULL,graph))
}

#' @name isUniqueIDMissing
#' @rdname isUniqueIDMissing
#' @title Missing unique ID
#' @description This function check that all interviews in the dataset have an ID.
#'   There is an option to automatically mark for deletion the surveys which have not an ID.
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param uniqueID name of the field where the survey unique ID is stored: string
#' @param surveyConsent name of the field in the dataset where the survey consent is stored: string
#' @param enumeratorID name of the field where the enumerator ID is stored: string
#' @param reportingColumns (Optional, by default it is built from the enumeratorID and the UniqueID) name of the columns from the dataset you want in the result: list of string (c('col1','col2',...))
#' @param deleteIsUniqueIDMissing (Optional, by default set as FALSE) if TRUE, the survey in error will be marked as 'deletedIsUniqueIDMissing': boolean (TRUE/FALSE)
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
#' uniqueID <- "X_uuid"
#' surveyConsent <- "survey_consent"
#' enumeratorID <- "enumerator_id"
#'
#' list[dst,ret_log,var,graph] <- isUniqueIDMissing(ds,
#'                                                  uniqueID,
#'                                                  surveyConsent)
#' head(ret_log, 10)
#'}
#' @export isUniqueIDMissing
isUniqueIDMissing <- function(ds=NULL,
                              uniqueID=NULL,
                              surveyConsent=NULL,
                              reportingColumns=c(enumeratorID, uniqueID),
                              deleteIsUniqueIDMissing=FALSE)
{
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveyConsent) | !is.character(surveyConsent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(uniqueID) | !is.character(uniqueID)){
    stop("Please provide the field where the survey unique ID is stored")
  }
  if(is.null(reportingColumns) | !is.character(reportingColumns)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(deleteIsUniqueIDMissing) | !is.logical(deleteIsUniqueIDMissing)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(deleteIsUniqueIDMissing){
    ds[,surveyConsent][is.na(ds[,uniqueID])] <- "deletedIsUniqueIDMissing"
  }

  # TO BE BE CHANGED WITH DYNAMIC COLUMS

  errors <- subset(ds,is.na(ds[,uniqueID]) | ds[,uniqueID]=="") %>%
    dplyr::select(all_of(reportingColumns), survey_consent=surveyConsent)
  graph <- piechart(data.frame(categories=c("OK", "NOK"),
                               Nb=c(length(ds[,1])-length(errors[,1]),
                                    length(errors[,1]))),
                    "isUniqueIDMissing")
  return(list(ds,errors,NULL,graph))
}

#' @name isUniqueIDDuplicated
#' @rdname isUniqueIDDuplicated
#' @title Duplicates in unique ID
#' @description This function check that all interviews in the dataset have an ID which is unique.
#' There is an option to automatically mark for deletion the surveys which have a duplicated unique ID.
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param uniqueID name of the field where the survey unique ID is stored: string
#' @param surveyConsent name of the field in the dataset where the survey consent is stored: string
#' @param enumeratorID name of the field where the enumerator ID is stored: string
#' @param reportingColumns (Optional, by default it is built from the enumeratorID and the UniqueID) name of the columns from the dataset you want in the result: list of string (c('col1','col2',...))
#' @param deleteIsUniqueIDDuplicated (Optional, by default set as FALSE) if TRUE, the survey in error will be marked as 'deletedIsUniqueIDDuplicated': boolean (TRUE/FALSE)
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
#' uniqueID <- "X_uuid"
#' surveyConsent <- "survey_consent"
#' enumeratorID <- "enumerator_id"
#'
#' list[dst,ret_log,var,graph] <- isUniqueIDDuplicated(ds,
#'                                                     uniqueID,
#'                                                     surveyConsent)
#' head(ret_log, 10)
#'}
#' @export isUniqueIDDuplicated
isUniqueIDDuplicated <- function(ds=NULL,
                                 uniqueID=NULL,
                                 surveyConsent=NULL,
                                 reportingColumns=c(enumeratorID, uniqueID),
                                 deleteIsUniqueIDDuplicated=FALSE){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveyConsent) | !is.character(surveyConsent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(uniqueID) | !is.character(uniqueID)){
    stop("Please provide the field where the survey unique ID is stored")
  }
  if(is.null(reportingColumns) | !is.character(reportingColumns)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(deleteIsUniqueIDDuplicated) | !is.logical(deleteIsUniqueIDDuplicated)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(deleteIsUniqueIDDuplicated){
    ds[,surveyConsent][duplicated(ds[,uniqueID])] <- "deletedIsUniqueIDDuplicated"
  }

  # TO BE BE CHANGED WITH DYNAMIC COLUMS

  errors <- subset(ds,duplicated(ds[,uniqueID])) %>%
    dplyr::select(all_of(reportingColumns), survey_consent=surveyConsent)
  graph <- piechart(data.frame(categories=c("OK", "NOK"),
                               Nb=c(length(ds[,1])-length(errors[,1]),
                                    length(errors[,1]))),
                    "isUniqueIDDuplicated")
  return(list(ds,errors,NULL,graph))
}

#' @name isSurveyOnMoreThanADay
#' @rdname isSurveyOnMoreThanADay
#' @title Surveys that do not end on the same day as they started
#' @description This function check that all interviews in the dataset start and end the same day.
#' There is an option to automatically mark for deletion the surveys which have different starting and ending dates.
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param surveyConsent name of the field in the dataset where the survey consent is stored: string
#' @param dates name of the fields where the information about the start and end date of the survey is stored: list of string (c('start_date','end_date'))
#' @param uniqueID name of the field where the survey unique ID is stored: string
#' @param enumeratorID name of the field where the enumerator ID is stored: string
#' @param reportingColumns (Optional, by default it is built from the enumeratorID and the UniqueID) name of the columns from the dataset you want in the result: list of string (c('col1','col2',...))
#' @param deleteIsSurveyOnMoreThanADay (Optional, by default set as FALSE) if TRUE, the survey in error will be marked as 'deletedIsSurveyOnMoreThanADay': boolean (TRUE/FALSE)
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
#' dates <- c("survey_start","end_survey")
#' uniqueID <- "X_uuid"
#' enumeratorID <- "enumerator_id"
#' deleteIsSurveyOnMoreThanADay <- FALSE
#'
#' list[dst,ret_log,var,graph] <- isSurveyOnMoreThanADay(ds,
#'                                                       surveyConsent,
#'                                                       dates)
#' head(ret_log, 10)
#'}
#' @export isSurveyOnMoreThanADay
isSurveyOnMoreThanADay <- function(ds=NULL,
                                   surveyConsent=NULL,
                                   dates=NULL,
                                   reportingColumns=c(enumeratorID, uniqueID),
                                   deleteIsSurveyOnMoreThanADay=FALSE){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveyConsent) | !is.character(surveyConsent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(reportingColumns) | !is.character(reportingColumns)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(deleteIsSurveyOnMoreThanADay) | !is.logical(deleteIsSurveyOnMoreThanADay)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(deleteIsSurveyOnMoreThanADay){
    # ds[,survey_consent][stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[1]])),"uuuu-MM-dd")!= stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[2]])),"uuuu-MM-dd")]<-"deleted"
    ds[,surveyConsent][stringi::stri_datetime_format(strptime(ds[,dates[1]], "%Y-%m-%dT%H:%M:%OS"),"uuuu-MM-dd")!=stringi::stri_datetime_format(strptime(ds[,dates[2]], "%Y-%m-%dT%H:%M:%OS"),"uuuu-MM-dd")]<-"deletedIsSurveyOnMoreThanADay"
  }

  # errors <- subset(ds, stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[1]])),"uuuu-MM-dd") != stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[2]])),"uuuu-MM-dd")) %>%
  #   select(reportingcol, survey_start=dates[1], survey_end=dates[2])
  errors <- subset(ds,stringi::stri_datetime_format(strptime(ds[,dates[1]], "%Y-%m-%dT%H:%M:%OS"),"uuuu-MM-dd")!=stringi::stri_datetime_format(strptime(ds[,dates[2]], "%Y-%m-%dT%H:%M:%OS"),"uuuu-MM-dd")) %>%
    select(all_of(reportingColumns), survey_start=dates[1], survey_end=dates[2])
  graph <- piechart(data.frame(categories=c("OK", "NOK"),
                               Nb=c(length(ds[,1])-length(errors[,1]),
                                    length(errors[,1]))),
                    "isSurveyOnMoreThanADay")
  return(list(ds,errors,NULL,graph))
}

#' @name isSurveyEndBeforeItStarts
#' @rdname isSurveyEndBeforeItStarts
#' @title Surveys where end date/time is before the start date/time
#' @description This function check that all interviews in the dataset start before they end.
#' There is an option to automatically mark for deletion the surveys which have an ending date/time before the starting ones.
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param surveyConsent name of the field in the dataset where the survey consent is stored: string
#' @param dates name of the fields where the information about the start and end date of the survey is stored: list of string (c('start_date','end_date'))
#' @param uniqueID name of the field where the survey unique ID is stored: string
#' @param enumeratorID name of the field where the enumerator ID is stored: string
#' @param reportingColumns (Optional, by default it is built from the enumeratorID and the UniqueID) name of the columns from the dataset you want in the result: list of string (c('col1','col2',...))
#' @param deleteIsSurveyEndBeforeItStarts (Optional, by default set as FALSE) if TRUE, the survey in error will be marked as 'deletedIsSurveyEndBeforeItStarts': boolean (TRUE/FALSE)
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
#' dates <- c("survey_start","end_survey")
#' uniqueID <- "X_uuid"
#' enumeratorID <- "enumerator_id"
#'
#' list[dst,ret_log,var,graph] <- isSurveyEndBeforeItStarts(ds,
#'                                                          surveyConsent,
#'                                                          dates)
#' head(ret_log, 10)
#'}
#' @export isSurveyEndBeforeItStarts
isSurveyEndBeforeItStarts <- function(ds=NULL,
                                      surveyConsent=NULL,
                                      dates=NULL,
                                      reportingColumns=c(enumeratorID, uniqueID),
                                      deleteIsSurveyEndBeforeItStarts=FALSE){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveyConsent) | !is.character(surveyConsent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(reportingColumns) | !is.character(reportingColumns)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(deleteIsSurveyEndBeforeItStarts) | !is.logical(deleteIsSurveyEndBeforeItStarts)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(deleteIsSurveyEndBeforeItStarts){
    # ds[,survey_consent][readr::parse_datetime(as.character(ds[,dates[1]])) > readr::parse_datetime(as.character(ds[,dates[2]]))]<-"deleted"
    ds[,surveyConsent][strptime(ds[,dates[1]], "%Y-%m-%dT%H:%M:%OS")>strptime(ds[,dates[2]], "%Y-%m-%dT%H:%M:%OS")]<-"deletedIsSurveyEndBeforeItStarts"
  }

  # errors <- subset(ds, readr::parse_datetime(as.character(ds[,dates[1]])) > readr::parse_datetime(as.character(ds[,dates[2]]))) %>%
  #   select(reportingcol, survey_start=dates[1], survey_end=dates[2])
  errors <- subset(ds,strptime(ds[,dates[1]], "%Y-%m-%dT%H:%M:%OS")>strptime(ds[,dates[2]], "%Y-%m-%dT%H:%M:%OS")) %>%
    select(all_of(reportingColumns), survey_start=dates[1], survey_end=dates[2])
  graph <- piechart(data.frame(categories=c("OK", "NOK"),
                               Nb=c(length(ds[,1])-length(errors[,1]),
                                    length(errors[,1]))),
                    "isSurveyEndBeforeItStarts")
  return(list(ds,errors,NULL,graph))
}

#' @name isSurveyStartedBeforeTheAssessment
#' @rdname isSurveyStartedBeforeTheAssessment
#' @title Surveys that show start date earlier than first day of data collection
#' @description This function check that all interviews in the dataset start after the actual first day of data collection.
#' There is an option to automatically mark for deletion the surveys which have started before the first day of data collection.
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param dates name of the fields where the information about the start and end date of the survey is stored: list of string (c('start_date','end_date'))
#' @param surveyConsent name of the field in the dataset where the survey consent is stored: string
#' @param startDataCollection date of the first day of the data collection: string ('yyyy-mm-dd')
#' @param uniqueID name of the field where the survey unique ID is stored: string
#' @param enumeratorID name of the field where the enumerator ID is stored: string
#' @param reportingColumns (Optional, by default it is built from the enumeratorID and the UniqueID) name of the columns from the dataset you want in the result: list of string (c('col1','col2',...))
#' @param deleteIsSurveyStartedBeforeTheAssessment (Optional, by default set as FALSE) if TRUE, the survey in error will be marked as 'deletedIsSurveyStartedBeforeTheAssessment': boolean (TRUE/FALSE)
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
#' surveyConsent <- "survey_consent"
#' startDataCollection <- "2018-11-11"
#' uniqueID <- "X_uuid"
#' enumeratorID <- "enumerator_id"
#' deleteIsSurveyStartedBeforeTheAssessment <- FALSE
#'
#' list[dst,ret_log,var,graph] <- isSurveyStartedBeforeTheAssessment(ds,
#'                                                                   dates,
#'                                                                   surveyConsent,
#'                                                                   startDataCollection)
#' head(ret_log, 10)
#'}
#' @export isSurveyStartedBeforeTheAssessment
isSurveyStartedBeforeTheAssessment <- function(ds = NULL,
                                               dates = NULL,
                                               surveyConsent = NULL,
                                               startDataCollection = NULL,
                                               reportingColumns=c(enumeratorID, uniqueID),
                                               deleteIsSurveyStartedBeforeTheAssessment = FALSE){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveyConsent) | !is.character(surveyConsent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(startDataCollection) | !is.character(startDataCollection)){
    stop("Please provide the date when the data collection began ('yyyy-mm-dd')")
  }
  if(is.null(reportingColumns) | !is.character(reportingColumns)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(deleteIsSurveyStartedBeforeTheAssessment) | !is.logical(deleteIsSurveyStartedBeforeTheAssessment)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(deleteIsSurveyStartedBeforeTheAssessment){
    # ds[,survey_consent][start_collection > stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[1]])),"uuuu-MM-dd")]<-"deleted"
    ds[,surveyConsent][startDataCollection>stringi::stri_datetime_format(strptime(ds[,dates[1]], "%Y-%m-%dT%H:%M:%OS"),"uuuu-MM-dd")]<-"deletedIsSurveyStartedBeforeTheAssessment"
  }

  errors <- subset(ds,startDataCollection > stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[1]])),"uuuu-MM-dd")) %>%
    select(reportingColumns, survey_start=dates[1])
  graph <- piechart(data.frame(categories=c("OK", "NOK"),
                               Nb=c(length(ds[,1])-length(errors[,1]),
                                    length(errors[,1]))),
                    "isSurveyStartedBeforeTheAssessment")
  return(list(ds,errors,NULL,graph))
}

#' @name isSurveyMadeInTheFuture
#' @rdname isSurveyMadeInTheFuture
#' @title Surveys that have start date/time after system date
#' @description This function check that all interviews in the dataset do not start after the current date.
#' There is an option to automatically mark for deletion the surveys which have a start date in the future.
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param surveyConsent name of the field in the dataset where the survey consent is stored: string
#' @param dates name of the fields where the information about the start and end date of the survey is stored: list of string (c('start_date','end_date'))
#' @param uniqueID name of the field where the survey unique ID is stored: string
#' @param enumeratorID name of the field where the enumerator ID is stored: string
#' @param reportingColumns (Optional, by default it is built from the enumeratorID and the UniqueID) name of the columns from the dataset you want in the result: list of string (c('col1','col2',...))
#' @param deleteIsSurveyMadeInTheFuture (Optional, by default set as FALSE) if TRUE, the survey in error will be marked as 'deletedIsSurveyMadeInTheFuture': boolean (TRUE/FALSE)
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
#' surveyConsent <- "survey_consent"
#' uniqueID <- "X_uuid"
#' enumeratorID <- "enumerator_id"
#' deleteIsSurveyMadeInTheFuture  <- FALSE
#'
#' list[dst,ret_log,var,graph] <- isSurveyMadeInTheFuture(ds,
#'                                                        surveyConsent,
#'                                                        dates)
#' head(ret_log, 10)
#'}
#' @export isSurveyMadeInTheFuture
isSurveyMadeInTheFuture <- function(ds=NULL,
                                    surveyConsent=NULL,
                                    dates=NULL,
                                    reportingColumns=c(enumeratorID, uniqueID),
                                    deleteIsSurveyMadeInTheFuture =FALSE){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveyConsent) | !is.character(surveyConsent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(reportingColumns) | !is.character(reportingColumns)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(deleteIsSurveyMadeInTheFuture) | !is.logical(deleteIsSurveyMadeInTheFuture)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(deleteIsSurveyMadeInTheFuture){
    # ds[,survey_consent][Sys.Date() < stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[1]])),"uuuu-MM-dd")]<-"deleted"
    ds[,surveyConsent][Sys.Date() < stringi::stri_datetime_format(strptime(ds[,dates[1]], "%Y-%m-%dT%H:%M:%OS"),"uuuu-MM-dd")]<-"deleted"
  }

  # TO BE BE CHANGED WITH DYNAMIC COLUMS

  # errors <- subset(ds,Sys.Date() < stringi::stri_datetime_format(readr::parse_datetime(as.character(ds[,dates[1]])),"uuuu-MM-dd")) %>%
  #   select(reportingcol, survey_start=dates[1])
  errors <- subset(ds,Sys.Date() < stringi::stri_datetime_format(strptime(ds[,dates[1]], "%Y-%m-%dT%H:%M:%OS"),"uuuu-MM-dd")) %>%
    select(all_of(reportingColumns), survey_start=dates[1])
  graph <- piechart(data.frame(categories=c("OK", "NOK"),
                               Nb=c(length(ds[,1])-length(errors[,1]),
                                    length(errors[,1]))),
                    "isSurveyMadeInTheFuture")
  return(list(ds,errors,NULL,graph))
}

#' @name isInterviewTooShort
#' @rdname isInterviewTooShort
#' @title Check that the duration of each interview is more than a threshold
#' @description This function check that the duration of each interview is more than a specified threshold.
#' There is an option to automatically mark for deletion the surveys which are under the threshold.
#' Warning: If there are uncorrected mistakes in the survey dates, it can lead to have the length of the survey in seconds and this check will not performed well
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param surveyConsent name of the field in the dataset where the survey consent is stored: string
#' @param dates name of the fields where the information about the start and end date of the survey is stored: list of string (c('start_date','end_date'))
#' @param minimumSurveyDuration minimum acceptable survey duration in minutes: integer
#' @param uniqueID name of the field where the survey unique ID is stored: string
#' @param enumeratorID name of the field where the enumerator ID is stored: string
#' @param reportingColumns (Optional, by default it is built from the enumeratorID and the UniqueID) name of the columns from the dataset you want in the result: list of string (c('col1','col2',...))
#' @param deleteIsInterviewTooShort (Optional, by default set as FALSE) if TRUE, the survey in error will be marked as 'deletedIsInterviewTooShort': boolean (TRUE/FALSE)
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
#' dates <- c("survey_start","end_survey")
#' uniqueID <- "X_uuid"
#' enumeratorID <- "enumerator_id"
#' minimumSurveyDuration <- 30
#'
#' list[dst,ret_log,var,graph] <- isInterviewTooShort(ds,
#'                                                    surveyConsent,
#'                                                    dates,
#'                                                    minimumSurveyDuration)
#' head(ret_log, 10)
#' print(graph)
#'}
#' @export isInterviewTooShort
isInterviewTooShort <- function(ds=NULL,
                                surveyConsent=NULL,
                                dates=NULL,
                                minimumSurveyDuration=30,
                                reportingColumns=c(enumeratorID, uniqueID),
                                deleteIsInterviewTooShort=FALSE){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveyConsent) | !is.character(surveyConsent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(reportingColumns) | !is.character(reportingColumns)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(minimumSurveyDuration) | !is.numeric(minimumSurveyDuration)){
    stop("Please provide the minimum survey time to check against")
  }
  if(is.null(deleteIsInterviewTooShort) | !is.logical(deleteIsInterviewTooShort)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  tmp <- data.frame(ds[reportingColumns],
                    SurveyLength = as.double.difftime((readr::parse_datetime(as.character(ds[,dates[2]])) -
                                                       readr::parse_datetime(as.character(ds[,dates[1]]))),
                                                       units = "secs") / 60)
  # tmp<-data.frame(ds[reportingColumns], SurveyLength=as.double.difftime((strptime(ds[,dates[2]],"%Y-%m-%dT%R") - strptime(ds[,dates[1]],"%Y-%m-%dT%R")), units = "secs")/60)

  if(deleteIsInterviewTooShort){
    ds[,surveyConsent][tmp$SurveyLength<minimumSurveyDuration] <- "deletedIsInterviewTooShort"
  }
  errors <- subset(tmp, SurveyLength<minimumSurveyDuration)
  # graph <- piechart(data.frame(check=tmp$SurveyLength<minimumSurveyDuration), "isInterviewTooShort")
  # graph <- piechart(data.frame(categories=c("OK", "NOK", "NA"),
  #                              Nb=c(sum((tmp$SurveyLength<minimumSurveyDuration)==FALSE, na.rm = TRUE),
  #                                   sum((tmp$SurveyLength<minimumSurveyDuration)==TRUE, na.rm=TRUE),
  #                                   sum(is.na((tmp$SurveyLength<minimumSurveyDuration))))),
  #                   "isInterviewTooShort")
  graph <- piechart(data.frame(categories=c("OK", "NOK"),
                               Nb=c(length(ds[,1])-length(errors[,1]),
                                    length(errors[,1]))),
                    "isInterviewTooShort")
  return(list(ds,errors,NULL,graph))
}

#' @name isInterviewTooShortForTheHouseholdSize
#' @rdname isInterviewTooShortForTheHouseholdSize
#' @title Check that the duration relative to the household size of each interview is more than a threshold
#' @description This function check that the duration relative to the household size of each interview is more than a specified threshold.
#' There is an option to automatically mark for deletion the surveys which are under the threshold.
#' Warning: If there are uncorrected mistakes in the survey dates, it can lead to have the length of the survey in seconds and this check will not performed well
#'
#' @param ds dataset containing the survey (from kobo): data.frame
#' @param surveyConsent name of the field in the dataset where the survey consent is stored: string
#' @param dates name of the fields where the information about the start and end date of the survey is stored: list of string (c('start_date','end_date'))
#' @param householdSize name of the field in the dataset where the household size is stored: string
#' @param minimumSurveyDurationByIndividual minimum acceptable survey duration for one individual in minutes: integer
#' @param uniqueID name of the field where the survey unique ID is stored: string
#' @param enumeratorID name of the field where the enumerator ID is stored: string
#' @param reportingColumns (Optional, by default it is built from the enumeratorID and the UniqueID) name of the columns from the dataset you want in the result: list of string (c('col1','col2',...))
#' @param deleteIsInterviewTooShortForTheHouseholdSize (Optional, by default set as FALSE) if TRUE, the survey in error will be marked as 'deletedIsInterviewTooShortForTheHouseholdSize': boolean (TRUE/FALSE)
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
#' dates <- c("survey_start","end_survey")
#' householdSize <-"consent_received.respondent_info.hh_size"
#' uniqueID <- "X_uuid"
#' enumeratorID <- "enumerator_id"
#' minimumSurveyDurationByIndividual <- 10
#' deleteIsInterviewTooShortForTheHouseholdSize <- FALSE
#'
#' list[dst,ret_log,var,graph] <- isInterviewTooShortForTheHouseholdSize(ds,
#'                                                  surveyConsent,
#'                                                  dates,
#'                                                  householdSize,
#'                                                  minimumSurveyDurationByIndividual)
#' head(ret_log, 10)
#' print(graph)
#'}
#' @export isInterviewTooShortForTheHouseholdSize
isInterviewTooShortForTheHouseholdSize <- function(ds=NULL,
                                                   surveyConsent=NULL,
                                                   dates=NULL,
                                                   householdSize=NULL,
                                                   minimumSurveyDurationByIndividual=10,
                                                   reportingColumns=c(enumeratorID, uniqueID),
                                                   deleteIsInterviewTooShortForTheHouseholdSize=FALSE){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveyConsent) | !is.character(surveyConsent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(householdSize) | !is.character(householdSize)){
    stop("Please provide the field where the HH size is stored")
  }
  if(is.null(reportingColumns) | !is.character(reportingColumns)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(minimumSurveyDurationByIndividual) | !is.numeric(minimumSurveyDurationByIndividual)){
    stop("Please provide the minimum survey time to check against")
  }
  if(is.null(deleteIsInterviewTooShortForTheHouseholdSize) | !is.logical(deleteIsInterviewTooShortForTheHouseholdSize)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  tmp<-data.frame(ds[reportingColumns], HHSize=ds[,householdSize],
                  SurveyLength=as.double.difftime((readr::parse_datetime(as.character(ds[,dates[2]])) -
                                                   readr::parse_datetime(as.character(ds[,dates[1]]))),
                                                   units = "secs") / 60)
  # tmp<-data.frame(ds[reportingColumns], HHSize=ds[,HouseholdSize], SurveyLength=as.double.difftime((strptime(ds[,dates[2]],"%Y-%m-%dT%R") - strptime(ds[,dates[1]],"%Y-%m-%dT%R")), units = "secs")/60)

  if(deleteIsInterviewTooShortForTheHouseholdSize){
    ds[,surveyConsent][(tmp$SurveyLength/tmp$HHSize)<minimumSurveyDurationByIndividual] <- "deletedIsInterviewTooShortForTheHouseholdSize"
  }
  errors <- subset(tmp, (SurveyLength/HHSize)<minimumSurveyDurationByIndividual)
  # graph <- piechart(data.frame(categories=c("OK", "NOK", "NA"),
  #                              Nb=c(sum(((tmp$SurveyLength/tmp$HHSize)<minimumSurveyDurationByIndividual)==FALSE, na.rm = TRUE),
  #                                   sum(((tmp$SurveyLength/tmp$HHSize)<minimumSurveyDurationByIndividual)==TRUE, na.rm=TRUE),
  #                                   sum(is.na((tmp$SurveyLength/tmp$HHSize)<minimumSurveyDurationByIndividual)))),
  #                   "isInterviewTooShortForTheHouseholdSize")
  graph <- piechart(data.frame(categories=c("OK", "NOK"),
                               Nb=c(length(ds[,1])-length(errors[,1]),
                                    length(errors[,1]))),
                    "isInterviewTooShortForTheHouseholdSize")
  return(list(ds,errors,NULL,graph))
}
