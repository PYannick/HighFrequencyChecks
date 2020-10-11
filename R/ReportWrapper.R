#' @name ReportWrapper
#' @rdname ReportWrapper
#' @title Create an Rmd file for the report
#' @description Create an Rmd file for the report
#'
#' @param working_directY the current working directory
#' @param xlsform_name the name of the xlsform used to build the report
#' @param xlsform_location the location of the xlsform from the working directory
#' @param vignette_directory the directory where to generate the .Rmd from the working directory
#'
#' @return vignette file
#'
#' @author Yannick Pascaud
#'
#' @examples
#' \dontrun{
#' working_directY <- getwd()
#' xlsform_name <- "form.xlsx"
#' xlsform_location <- "/inst/demo/"
#' vignette_directory <- "/vignettes/HFC_WR.Rmd"
#'
#' ReportWrapper(working_directY,
#'               xlsform_name,
#'               xlsform_location,
#'               vignette_directory)
#'}
#' @export ReportWrapper
#'

ReportWrapper <- function(working_directY=NULL,
                           xlsform_name=NULL,
                           xlsform_location=NULL,
                           vignette_directory=NULL){

# working_directY <- getwd()
# xlsform_name <- "form.xlsx"
# xlsform_location <- "/inst/demo/"
# vignette_directory <- "/vignettes/HFC_WR.Rmd"

repstr <- openxlsx::read.xlsx(paste0(working_directY, xlsform_location, xlsform_name), 1)
#
reportRMD  <- paste0(working_directY,vignette_directory)
## TO DO : CHECK IF FILE EXIST - AND REQUEST USER TO DELETE BEFORE REGENERATING - SUGGESTING TO SAVE PREVIOUS UNDER NEW NAME
if (file.exists(reportRMD)) file.remove(reportRMD)

## Start Building the report ##########

cat("---", file = reportRMD , sep = "\n", append = TRUE)
cat("title: \"High Frequency Checks template\"", file = reportRMD , sep = "\n", append = TRUE)
cat("author: \"Yannick Pascaud\"", file = reportRMD , sep = "\n", append = TRUE)
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
}

