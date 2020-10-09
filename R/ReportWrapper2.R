#' @name ReportWrapper1
#' @rdname ReportWrapper1
#' @title Create an Rmd file for the report
#' @description Create an Rmd file for the report
#'
#' @param ds dataset as a data.frame object
#' @param survey_consent name as a string of the field in the dataset where the survey consent is stored
#' @param reportingcol columns as a list of string name from the dataset you want in the result (c('col1','col2',...))
#' @param UniqueID name as a string of the field in the dataset where the unique ID is stored
#' @param dates fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
#' @param surveydate name as a string of the field in the dataset where the date of the survey is stored
#' @param start_collection date as a string of the first day of data collection ('yyyy-mm-dd')
#' @param dateformat format as a string used for the date ('\%m/\%d/\%Y')
#' @param minduration minimum acceptable survey duration as integer in minutes
#' @param sdval number of standard deviation for which the data within is considered as acceptable
#' @param HHSize name as a string of the field in the dataset where the household size is stored
#' @param delete delete action to be done as a boolean (TRUE/FALSE)
#' @param correct correction action to be done as a boolean (TRUE/FALSE)
#' @param enumeratorID name as a string of the field in the dataset where the enumerator ID is stored
#' @param enumeratorcheck specify if the report has to be displayed for each enumerator or not as a boolean (TRUE/FALSE)
#' @param otherpattern pattern as string to identify the fields containing others values ('_other$')
#' @param adm dataset containing the shapefile - Regardless the projection used for the shapefile, it is transformed to WGS84
#' @param ds_site name as a string of the field in the dataset where the site is stored
#' @param ds_coord columns as a list of string name from the dataset where the GPS coordinates are stored (c('Long','Lat'))
#' @param adm_site name as a string of the field in the shapefile where the site is stored
#' @param pts dataset containing the shapefile - Regardless the projection used for the shapefile, it is transformed to WGS84
#' @param buff value as an integer in meter to determine the buffer from a sampled point which is acceptable
#' @param questions columns as a list of string name from the dataset you want to check against (c('col1','col2',...))
#' @param value  Maximum acceptable value as integer for the checked fields
#' @param minnbanswers minimum number of answers expected per question
#' @param sf sampling frame as a data.frame object
#' @param dssite  name as a string of the field in the dataset where the site is stored
#' @param sfsite name as a string of the field in the sampling frame where the site is stored
#' @param sftarget name as a string of the field where the target number of survey is stored in the sampling frame
#' @param sfnbpts  name as a string of the field where the number of points generated is stored in the sampling frame
#' @param formul  formulas as a list of string used to compute the final number of eligible surveys and the variance from the target (C('formula1','formula2')).
#'   the values/fields available are: done and the ones generated according the survey consent values (one per value)
#' @param colorder  column names as a list of string to order the colums in the result (C('col1','col2',...)).
#'  the columns available are: site, done, final, variance and the ones generated according the survey consent values (one per value)
#'
#'
#' @return vignette file
#'
#' @author Yannick Pascaud, Edouard Legoupil
#'
#' @examples
#' \dontrun{
#' ds <- HighFrequencyChecks::sample_dataset
#'
#' survey_consent <- "survey_consent"
#' reportingcol <- c("enumerator_id","X_uuid")
#' UniqueID <- "X_uuid"
#'
#' ## Setting dates
#' dates <- c("survey_start","end_survey")
#' surveydate <- "survey_date"
#' start_collection <- "2018-11-11"
#' dateformat <- "%m/%d/%Y"
#' minduration <- 30
#' sdval <- 2
#' HHSize <-"consent_received.respondent_info.hh_size"
#'
#'
#' ## Setting action on dataset
#' delete <- FALSE
#' correct <- FALSE
#'
#' ## Setting enumerator
#' enumeratorID <- "enumerator_id"
#' enumeratorcheck <- FALSE
#' otherpattern <- "_other$"
#'
#' ## Setting linked geodata
#' adm <- HighFrequencyChecks::admin
#' ds_site <- "union_name"
#' ds_coord <- c("X_gps_reading_longitude","X_gps_reading_latitude")
#' adm_site <- "Union"
#' pts <- HighFrequencyChecks::SamplePts
#' buff <- 10
#'
#' ## setting questions to check
#' questions <-c("consent_received.food_security.spend_food",
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
#' value <- 25000
#' minnbanswers <- 3
#'
#'
#' ## setting sampling plan
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
#' ReportWrapper(ds,
#'               survey_consent,
#'               reportingcol,
#'               UniqueID,
#'               dates,
#'               surveydate,
#'               start_collection,
#'               dateformat,
#'               minduration,
#'               sdval,
#'               HHSize,
#'               delete,
#'               correct,
#'               enumeratorID,
#'               enumeratorcheck,
#'               otherpattern,
#'               adm,
#'               ds_site,
#'               ds_coord,
#'               adm_site,
#'               pts,
#'               buff,
#'               questions,
#'               value,
#'               minnbanswers,
#'               sf, dssite,
#'               sfsite,
#'               sftarget,
#'               sfnbpts,
#'               formul,
#'               colorder)
#'}
#' @export ReportWrapper1
#'
ReportWrapper1 <- function(usexlsform=FALSE,
                          form=NULL,
                          ds=NULL,
                          survey_consent=NULL,
                          reportingcol=NULL,
                          UniqueID=NULL,
                          dates=NULL,
                          surveydate=NULL,
                          start_collection=NULL,
                          dateformat=NULL,
                          minduration=NULL,
                          sdval=NULL,
                          HHSize=NULL,
                          delete=FALSE,
                          correct=FALSE,
                          enumeratorID=NULL,
                          enumeratorcheck=FALSE,
                          otherpattern=NULL,
                          adm=NULL,
                          ds_site=NULL,
                          ds_coord=NULL,
                          adm_site=NULL,
                          pts=NULL,
                          buff=NULL,
                          questions=NULL,
                          value=NULL,
                          minnbanswers=NULL,
                          sf=NULL,
                          dssite=NULL,
                          sfsite=NULL,
                          sftarget=NULL,
                          sfnbpts=NULL,
                          formul=NULL,
                          colorder=NULL)
{

  cat("Let's summarise what the high Frequency check you just set up:\n")

  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("We can not go further, please provide the name of the dataset you want to check")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
    stop("We can not go further, you need to provide the columns you want to display
         to check the results. \n For instance, include at minima the
         enumerator id column if you want to check by enumerator)")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    cat("You have not provided the reference of the fields where the
        survey consent is stored. This part of the check will be skipped")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    cat("You have not provided the reference of the fields where the survey
    start and end date is stored.\n
         For instance (c('start_date','end_date')).
         As a result, all the check related to interview duration will be skipped ")
  }
  if(is.null(reportingcol) | !is.character(reportingcol)){
    cat("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(delete) | !is.logical(delete)){
    cat("Please provide the delete action to be done (TRUE/FALSE)")
  }
  if(is.null(adm) | !isS4(adm) | nrow(adm)==0){
    cat("Please provide the spatial dataset of the boundaries shapefile")
  }
  if(is.null(ds_site) | !is.character(ds_site)){
    cat("Please provide the field where the site to check against is stored")
  }
  if(is.null(ds_coord) | !is.character(ds_coord) | length(ds_coord)!=2){
    cat("Please provide the fields where the coordinates are stored (c('Long','Lat'))")
  }
  if(is.null(adm_site) | !is.character(ds_site)){
    cat("Please provide the field where the site in the shapefile is stored")
  }
  if(delete){
    ds[,survey_consent][is.na(ds[,dates[2]])]<-"deleted"
  }
  if(is.null(correct) | !is.logical(correct)){
    cat("Please provide the correction action to be done (TRUE/FALSE)")
  }
  if(is.null(dates) | !is.character(dates) | length(dates)!=2){
    cat("Please provide the fields where the survey start and end date is stored (c('start_date','end_date'))")
  }

  if(is.null(UniqueID) | !is.character(UniqueID)){
    cat("Please provide the field where the survey unique ID is stored")
  }
  if(is.null(start_collection) | !is.character(start_collection)){
    cat("Please provide the date when the data collection began ('yyyy-mm-dd')")
  }

  if(is.null(pts) | !isS4(pts) | nrow(pts)==0){
    cat("Please provide the spatial dataset of the sample points shapefile")
  }
  if(is.null(buff) | !is.numeric(buff)){
    cat("Please provide the buffer in meters")
  }
  if(is.null(enumeratorcheck) | !is.logical(enumeratorcheck)){
    cat("Please provide the enumerator check action to be done (TRUE/FALSE)")
  }
  if(isTRUE(enumeratorcheck) & (is.null(enumeratorID) | !is.character(enumeratorID))){
    cat("Please provide the field where the enumerator ID is stored")
  }
  if(is.null(otherpattern) | !is.character(otherpattern)){
    cat("Please provide the pattern for other fields ('_other$')")
  }
  if(is.null(sdval) | !is.numeric(sdval)){
    cat("Please provide the number of standard deviations you want to check for")
  }
  if(is.null(questions) | !is.character(questions)){
    cat("Please provide the fields you want to check for (c('field1','field2',...))")
  }
  if(is.null(value) | !is.numeric(value)){
    cat("Please provide the maximum value for which you want to check")
  }
  if(is.null(minduration) | !is.numeric(minduration)){
    cat("Please provide the minimum survey time to check against")
  }
  if(is.null(HHSize) | !is.character(HHSize)){
    cat("Please provide the field where the HH size is stored")
  }
  if(is.null(sdval) | !is.numeric(sdval)){
    cat("Please provide the number of standard deviations you want to check for")
  }

  cat("##############\n")
  cat("We now start building your report\n")
  cat("##############\n")
  wdir <- getwd()

  #file <- "form.xlsx"
  #repstr <- openxlsx::read.xlsx(paste0(wdir,file), 1)

  reportRMD  <- paste0(wdir,"vignettes/HighFrequencyCheckReport.Rmd")

  ## TO DO : CHECK IF FILE EXIST - AND REQUEST USER TO DELETE BEFORE REGENERATING - SUGGESTING TO SAVE PREVIOUS UNDER NEW NAME
  if (file.exists(reportRMD)) file.remove(reportRMD)

  ## Start Building the report ##########

  cat("---", file = reportRMD , sep = "\n", append = TRUE)
  cat("title: \"High Frequency Checks template\"", file = reportRMD , sep = "\n", append = TRUE)
  cat("author: \"Your Name\"", file = reportRMD , sep = "\n", append = TRUE)
  cat("date: \"`r format(Sys.time(), '%d %B, %Y')`\"", file = reportRMD , sep = "\n", append = TRUE)
  cat("always_allow_html: yes", file = reportRMD , sep = "\n", append = TRUE)
  cat("output:",file = reportRMD , sep = "\n", append = TRUE)
  cat("  pdf_document:", file = reportRMD , sep = "\n", append = TRUE)
  cat("    toc: true", file = reportRMD , sep = "\n", append = TRUE)
  cat("    toc_depth: 3", file = reportRMD , sep = "\n", append = TRUE)
  cat("  html_document: default", file = reportRMD , sep = "\n", append = TRUE)
  cat("geometry: margin=0.5in", file = reportRMD , sep = "\n", append = TRUE)
  cat("---", file = reportRMD , sep = "\n", append = TRUE)

  cat("\n", file = reportRMD , sep = "\n", append = TRUE)
  cat("```{r setup, include=FALSE}", file = reportRMD , sep = "\n", append = TRUE)
  cat("knitr::opts_chunk$set(echo = TRUE)", file = reportRMD , sep = "\n", append = TRUE)

  cat("library(knitr)", file = reportRMD , sep = "\n", append = TRUE)
  cat("library(gsubfn)", file = reportRMD , sep = "\n", append = TRUE)
  cat("library(dplyr)", file = reportRMD , sep = "\n", append = TRUE)
  cat("library(data.table)", file = reportRMD , sep = "\n", append = TRUE)
  cat("library(HighFrequencyChecks)", file = reportRMD , sep = "\n", append = TRUE)
  cat("```", file = reportRMD , sep = "\n", append = TRUE)


  for(i in 1:length(repstr[,1])){
    if(repstr[i,1]=="begin_ds_struct"){
      cat("```{r surveysDataset, eval=TRUE, echo=FALSE}", file = reportRMD , sep = "\n", append = TRUE)
      cat("dset<-list()", file = reportRMD , sep = "\n", append = TRUE)
      ds_struct=sple=gis=init_var=free_code=report=FALSE
      ds_struct<-TRUE
    } else   if(repstr[i,1]=="begin_sample"){
      cat("```{r sampleSizeDataset, eval=TRUE, echo=FALSE}", file = reportRMD , sep = "\n", append = TRUE)
      ds_struct=sple=gis=init_var=free_code=report=FALSE
      sple<-TRUE
    } else   if(repstr[i,1]=="begin_shp"){
      cat("```{r shapefiles, eval=TRUE, echo=FALSE}", file = reportRMD , sep = "\n", append = TRUE)
      ds_struct=sple=gis=init_var=free_code=report=FALSE
      gis<-TRUE
    } else if(repstr[i,1]=="begin_init_var"){
      cat("```{r initializeVariables, eval=TRUE, echo=FALSE}", file = reportRMD , sep = "\n", append = TRUE)
      ds_struct=sple=gis=init_var=free_code=report=FALSE
      init_var<-TRUE
    } else if(repstr[i,1]=="free_code") {
      ds_struct=sple=gis=init_var=free_code=report=FALSE
      free_code<-TRUE
    } else if(repstr[i,1]=="begin_report") {
      ds_struct=sple=gis=init_var=free_code=report=FALSE
      report<-TRUE
    } else if(repstr[i,1]=="end_ds_struct"|
              repstr[i,1]=="end_sample" |
              repstr[i,1]=="end_shp" |
              repstr[i,1]=="end_init_var") {
      cat("```", file = reportRMD , sep = "\n", append = TRUE)
      ds_struct=sple=gis=init_var=free_code=report=FALSE
    } else if(repstr[i,1]=="end_report") {
      ds_struct=sple=gis=init_var=free_code=report=FALSE
    }

    if(ds_struct){
      if(repstr[i,1]=="begin_ds_struct"){
        # ignore
      } else {
        # cat("```{r eval=TRUE, echo=FALSE}", file = reportRMD , sep = "\n", append = TRUE)
        cat(paste0("dset[[\"", repstr[i,1], "\"]]<-", repstr[i,2]), file = reportRMD , sep = "\n", append = TRUE)
        # cat("```", file = reportRMD , sep = "\n", append = TRUE)
      }
    }
    if(init_var | sple | gis){
      if(repstr[i,1]=="begin_init_var"){
        # ignore
      } else {
        cat(paste0(repstr[i,1], "<-", repstr[i,2]), file = reportRMD , sep = "\n", append = TRUE)
      }
    }

    if(free_code){
      cat("```{r eval=TRUE, echo=FALSE}", file = reportRMD , sep = "\n", append = TRUE)
      cat(repstr[i,2], file = reportRMD , sep = "\n", append = TRUE)
      cat("```", file = reportRMD , sep = "\n", append = TRUE)
    }
    if(report){
      if(repstr[i,1]=="title1"){
        cat(paste0("\n## ", repstr[i,2]), file = reportRMD , sep = "\n", append = TRUE)
      } else if(repstr[i,1]=="title2"){
        cat(paste0("\n### ", repstr[i,2]), file = reportRMD , sep = "\n", append = TRUE)
      } else if(repstr[i,1]=="title3"){
        cat(paste0("\n#### ", repstr[i,2]), file = reportRMD , sep = "\n", append = TRUE)
      } else if(repstr[i,1]=="text"){
        cat(paste0("\n", repstr[i,2]), file = reportRMD , sep = "\n", append = TRUE)
      } else if(repstr[i,1]=="function"){
        cat("```{r eval=TRUE, echo=FALSE, results='asis'}", file = reportRMD , sep = "\n", append = TRUE)

        # cat(paste0("list[var1,var2]<-", repstr[i,2], "(", repstr[i,3], ")"), file = reportRMD , sep = "\n", append = TRUE)
        cat(paste0("tmp<-(stringi::stri_replace_all_fixed(\"", repstr[i,3], "\",
                                             names(dset)[stringi::stri_detect_fixed(\"", repstr[i,3], "\", names(dset))],
                                             paste0(\"dset[['\", names(dset)[stringi::stri_detect_fixed(\"", repstr[i,3], "\", names(dset))], \"']]\"),
                                             vectorize_all=FALSE))"), file = reportRMD , sep = "\n", append = TRUE)
        cat(paste0("list[var1,var2,var3,var4]<-eval(parse(text=paste0(\"", repstr[i,2], "(\", tmp, \")\")))"), file = reportRMD , sep = "\n", append = TRUE)
        ##

        cat("if(!is.null(var1)){", file = reportRMD , sep = "\n", append = TRUE)
        # cat("  Header<-var1", file = reportRMD , sep = "\n", append = TRUE)
        cat("  dset[[names(dset)[stringi::stri_detect_fixed(\"", repstr[i,3], "\", names(dset))]]]<-var1\n", file = reportRMD , sep = "", append = TRUE)
        ##
        cat("}", file = reportRMD , sep = "\n", append = TRUE)

        if(!is.na(repstr[i,4]) & stringi::stri_detect_fixed(repstr[i,4], "output=csv")){
          cat("write.csv(var2, paste0(\"", repstr[i,2], "\", \".csv\"))\n", file = reportRMD , sep = "", append = TRUE)
          cat("```", file = reportRMD , sep = "\n", append = TRUE)
          cat("Please see the generated csv file: ", repstr[i,2], ".csv\n", file = reportRMD , sep = "", append = TRUE)

        } else {
          cat("if(!is.null(var2)){", file = reportRMD , sep = "\n", append = TRUE)
          cat("  if(nrow(var2)>0){", file = reportRMD , sep = "\n", append = TRUE)
          cat("    kable(var2)", file = reportRMD , sep = "\n", append = TRUE)
          cat("  } else {", file = reportRMD , sep = "\n", append = TRUE)
          cat("    cat(\"\nno errors\")", file = reportRMD , sep = "\n", append = TRUE)
          cat("  }", file = reportRMD , sep = "\n", append = TRUE)
          cat("}", file = reportRMD , sep = "\n", append = TRUE)
          cat("```", file = reportRMD , sep = "\n", append = TRUE)
        }

      } else {
        # ignore
      }

      # cat(paste0(repstr[i,1], "<-", repstr[i,2]), file = reportRMD , sep = "\n", append = TRUE)
    }


  }

}


