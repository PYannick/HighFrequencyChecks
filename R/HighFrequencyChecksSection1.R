
# chk1a_interview_completed: Check that all interviews were completed
# chk1b_survey_consent: Check that all surveys have consent
# chk1di_GIS_site: GIS check surveys for site
# chk1dii_GIS_Xm: GIS check surveys if fall without Xm radius from a sampled point

chk1a_interview_completed <- function(ds=NULL, survey_consent=NULL, dates=NULL, reportingcol=NULL, delete=NULL){
  if(is.null(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(dates)){
    stop("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }
  if(is.null(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(delete){
    ds[,survey_consent][is.na(ds[,dates[2]])]<-"deleted"
  }

  errors <- subset(ds,is.na(ds[,dates[2]])) %>% select(reportingcol, survey_end=dates[2])
  return(list(ds,errors))
}

chk1b_survey_consent <- function(ds=NULL, survey_consent=NULL, reportingcol=NULL, delete=NULL){
  if(is.null(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(delete){
    ds[,survey_consent][is.na(ds[,survey_consent])]<-"deleted"
  }

  errors <- subset(ds,is.na(survey_consent)) %>% select(reportingcol, survey_consent=survey_consent)
  return(list(ds,errors))
}

chk1di_GIS_site <- function(adm=NULL, ds=NULL, ds_site=NULL, ds_coord=NULL, adm_site=NULL, survey_consent=NULL, reportingcol=NULL, correct=NULL){
  if(is.null(adm)){
    stop("Please provide the spatial dataset of the boundaries shapefile")
  }
  if(is.null(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(ds_site)){
    stop("Please provide the field where the site to check against is stored")
  }
  if(is.null(ds_coord)){
    stop("Please provide the fields where the coordinates are stored (c('Long','Lat'))")
  }
  if(is.null(adm_site)){
    stop("Please provide the field where the site in the shapefile is stored")
  }
  if(is.null(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(correct)){
    stop("Please provide the correction action to be done (TRUE/FALSE)")
  }

  if(is.projected(adm)){
    adm<-spTransform(adm, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  }

  dfsp<-ds
  coordinates(dfsp)<-dfsp[,c(ds_coord[1],ds_coord[2])]
  proj4string(dfsp)<-proj4string(adm)
  dfsp_over_adm<-over(dfsp,adm)
  fm<-data.frame(ds,dfsp_over_adm, stringsAsFactors = FALSE)
  fm[,admin_site][is.na(fm[,admin_site])]<-""

  # Should not be kept if the site in the kobo form is the same than the one in the shapefile
  #fm[,adm_site]<-tolower(as.character(gsub(" ", "_", fm[,adm_site])))
  #fm[,ds_site]<-tolower(as.character(gsub(" ", "_", fm[,ds_site])))

  fm$check<-ifelse(fm[,ds_site]!=fm[,adm_site],"NOk","Ok")
  if(correct){
    ds[,ds_site][fm$check=="NOk"]<-fm[,adm_site][fm$check=="NOk"]
  }

  errors <- subset(fm,check=="NOk") %>% select(reportingcol, SiteRec=ds_site, SiteReal=adm_site)
  return(list(ds,errors))
}

chk1dii_GIS_Xm <- function(pts=NULL, ds=NULL, ds_coord=NULL, buff=10, survey_consent=NULL, reportingcol=NULL, delete=NULL){
  if(is.null(pts)){
    stop("Please provide the spatial dataset of the sample points shapefile")
  }
  if(is.null(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(ds_coord)){
    stop("Please provide the fields where the coordinates are stored (c('Long','Lat'))")
  }
  if(is.null(buff)){
    stop("Please provide the buffer in meters")
  }
  if(is.null(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }
  if(is.null(reportingcol)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  # function made by Valentin: https://stackoverflow.com/users/5193830/valentin
  make_GeodesicBuffer <- function(pts, width) {
    # A) Construct buffers as points at given distance and bearing ---------------
    dg <- seq(from = 0, to = 360, by = 5)
    # Construct equidistant points defining circle shapes (the "buffer points")
    buff.XY <- destPoint(p = pts,
                         b = rep(dg, each = length(pts)),
                         d = width)
    # B) Make SpatialPolygons -------------------------------------------------
    # Group (split) "buffer points" by id
    buff.XY <- as.data.frame(buff.XY)
    id  <- rep(1:dim(pts)[1], times = length(dg))
    lst <- split(buff.XY, id)
    # Make SpatialPolygons out of the list of coordinates
    poly   <- lapply(lst, Polygon, hole = FALSE)
    polys  <- lapply(list(poly), Polygons, ID = NA)
    spolys <- SpatialPolygons(Srl = polys,
                                  proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
    # Disaggregate (split in unique polygons)
    spolys <- disaggregate(spolys)
    return(spolys)
  }

  #buffer<-readOGR(GIS_folder, pts, stringsAsFactors = FALSE)
  if(is.projected(pts)){
    pts<-spTransform(pts, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  }
  #buffer<-gBuffer(buffer, width=buff, byid=TRUE)
  buffer<-make_GeodesicBuffer(as.matrix(data.frame(lon=pts$coords.x1,lat=pts$coords.x2)),buff)

  dfsp<-ds
  coordinates(dfsp)<-dfsp[,c(ds_coord[1],ds_coord[2])]
  proj4string(dfsp)<-proj4string(buffer)
  dfsp_over_buffer<-over(dfsp,buffer)
  fm<-data.frame(ds,dfsp_over_buffer, stringsAsFactors = FALSE)

  fm$check<-ifelse(is.na(fm$dfsp_over_buffer),"NOk","Ok")
  if(delete){
    ds[,survey_consent][fm$check=="NOk"]<-"deleted"
  }

  errors <- subset(fm,check=="NOk") %>% select(reportingcol, Outside=check)
  return(list(ds,errors))
}



