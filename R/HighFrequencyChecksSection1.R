#' @name chk1a_interview_completed
#' @rdname chk1a_interview_completed
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
#' head(chk1a_interview_completed(ds, survey_consent, dt, reportingcol, delete),10)
#'}
#'
#' @export chk1a_interview_completed

chk1a_interview_completed <- function(ds=NULL,
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
  return(list(ds,errors))
}

#' @name chk1b_survey_consent
#' @rdname chk1b_survey_consent
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
#' df <- HighFrequencyChecks::sample_dataset
#' sc <- "survey_consent"
#' reportingcol <- c("enumerator_id","X_uuid")
#' delete <- TRUE
#'
#' head(chk1b_survey_consent(df, sc, dt, reportingcol, delete),10)
#'}
#' @export chk1b_survey_consent

chk1b_survey_consent <- function(ds=NULL,
                                 survey_consent=NULL,
                                 reportingcol=NULL,
                                 delete=NULL){
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

  errors <- subset(ds, is.na(survey_consent)) %>%
    select(reportingcol, survey_consent = survey_consent)
  return(list(ds,errors))
}

#' @name chk1di_GIS_site
#' @rdname chk1di_GIS_site
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
#'   list_site <- chk1di_GIS_site(admin, df, df_site, df_coord, admin_site, sc, reportingcol, correct)
#'   head(list_site[[2]], 10)
#'}
#' @export chk1di_GIS_site
#'

chk1di_GIS_site <- function(adm=NULL,
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

  # errors <- subset(fm,check=="NOk") %>% select(reportingcol,
  #                                              SiteRec = ds_site,
  #                                              SiteReal = adm_site)
  errors <- fm[ which(fm$check=="NOk"), c(reportingcol,ds_site, adm_site)]
  return(list(ds,errors))
}

#' @name chk1dii_GIS_Xm
#' @rdname chk1dii_GIS_Xm
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
#' df <- HighFrequencyChecks::sample_dataset
#' pts <- HighFrequencyChecks::SamplePts
#' df_coord <- c("X_gps_reading_longitude","X_gps_reading_latitude")
#' bu <- 10
#' sc <- "survey_consent"
#' reportingcol <- c("enumerator_id","X_uuid")
#' delete <- FALSE
#'
#'
#' list_sitept  <- chk1dii_GIS_Xm( df, pts, df_coord, bu, sc, reportingcol, delete)
#' head(list_sitept[[2]], 10)
#'}
#' @export chk1dii_GIS_Xm
#'
chk1dii_GIS_Xm <- function(ds=NULL,
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
    # A) Construct buffers as points at given distance and bearing ---------------
    dg <- seq(from = 0, to = 360, by = 5)
    # Construct equidistant points defining circle shapes (the "buffer points")
    buff.XY <- geosphere::destPoint(p = pts,
                                    b = rep(dg, each = length(pts)),
                                    d = width)
    # B) Make SpatialPolygons -------------------------------------------------
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

  #errors <- subset(fm,check=="NOk") %>% select(reportingcol, Outside=check)
  errors <- fm[ which(fm$check=="NOk"), c(reportingcol, "Outside")]

  return(list(ds,errors))
}



