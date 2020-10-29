
RmdWrapper <- function(variablesList=NULL,
                       functionsList=NULL,
                       functionsOrder=NULL,
                       functionsOutput=NULL,
                       reportOutput=NULL,
                       reportType=NULL,
                       fileName=NULL){

  working_directY <- getwd()
  vignette_directory <- paste0("/", fileName, ".Rmd")

  reportRMD  <- paste0(working_directY,vignette_directory)
  ## TO DO : CHECK IF FILE EXIST - AND REQUEST USER TO DELETE BEFORE REGENERATING - SUGGESTING TO SAVE PREVIOUS UNDER NEW NAME
  if (file.exists(reportRMD)) file.remove(reportRMD)

  ## Start Building the report ##########

  if(reportOutput=="pdf"){
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
  } else if(reportOutput=="html"){
    cat("---", file = reportRMD , sep = "\n", append = TRUE)
    cat("title: \"High Frequency Checks template\"", file = reportRMD , sep = "\n", append = TRUE)
    cat("author: \"Yannick Pascaud\"", file = reportRMD , sep = "\n", append = TRUE)
    cat("date: \"`r format(Sys.time(), '%d %B, %Y')`\"", file = reportRMD , sep = "\n", append = TRUE)
    cat("always_allow_html: yes", file = reportRMD , sep = "\n", append = TRUE)
    cat("output:",file = reportRMD , sep = "\n", append = TRUE)
    cat("  unhcRstyle::unhcr_html:", file = reportRMD , sep = "\n", append = TRUE)
    cat("  toc: true", file = reportRMD , sep = "\n", append = TRUE)
    cat("---", file = reportRMD , sep = "\n", append = TRUE)
  }

  cat("\n", file = reportRMD , sep = "\n", append = TRUE)
  cat("```{r setup, include=FALSE}", file = reportRMD , sep = "\n", append = TRUE)
  cat("knitr::opts_chunk$set(echo = TRUE)", file = reportRMD , sep = "\n", append = TRUE)

  cat("library(knitr)", file = reportRMD , sep = "\n", append = TRUE)
  cat("library(gsubfn)", file = reportRMD , sep = "\n", append = TRUE)
  cat("library(dplyr)", file = reportRMD , sep = "\n", append = TRUE)
  cat("library(data.table)", file = reportRMD , sep = "\n", append = TRUE)
  cat("library(HighFrequencyChecks)", file = reportRMD , sep = "\n", append = TRUE)
  cat("options(scipen = 999)", file = reportRMD , sep = "\n", append = TRUE)
  cat("```", file = reportRMD , sep = "\n", append = TRUE)

  cat("```{r surveysDataset, eval=TRUE, echo=FALSE}", file = reportRMD , sep = "\n", append = TRUE)
  for(i in 1:length(variablesList[,1])){
    # cat(paste0(repstr[i,1], "<-", repstr[i,2]), file = reportRMD , sep = "\n", append = TRUE)
    cat(paste0(variablesList[i,"variableName"], "<-", variablesList[i,"variableValue"]), file = reportRMD , sep = "\n", append = TRUE)
  }
  cat("```", file = reportRMD , sep = "\n", append = TRUE)

  ##### A supprimer quand les gens arretterons de faire nimp
  cat("```{r, eval=TRUE, echo=FALSE}", file = reportRMD , sep = "\n", append = TRUE)
  cat("ds$union_name<-tolower(ds$union_name)", file = reportRMD , sep = "\n", append = TRUE)
  cat("sampleSizeTable$Union<-tolower(sampleSizeTable$Union)", file = reportRMD , sep = "\n", append = TRUE)
  cat("adminBoundaries$Union<-tolower(adminBoundaries$Union)", file = reportRMD , sep = "\n", append = TRUE)
  cat("```", file = reportRMD , sep = "\n", append = TRUE)
  #####

  for(i in functionsOrder[with(functionsOrder, order(ord)), "functionName"][functionsOrder[with(functionsOrder, order(ord)), "functionName"] %in% names(functionsList)]){
    cat(paste0("\n## ", i), file = reportRMD , sep = "\n", append = TRUE)
    cat("```{r eval=TRUE, echo=FALSE, results='asis', fig.align='center'}", file = reportRMD , sep = "\n", append = TRUE)
    cat(paste0("list[var1,report", i, ",var3,var4]<-", functionsList[[i]]), file = reportRMD , sep = "\n", append = TRUE)

    cat("if(!is.null(var1)){", file = reportRMD , sep = "\n", append = TRUE)
    cat("  ds<-var1\n", file = reportRMD , sep = "", append = TRUE)
    cat("}", file = reportRMD , sep = "\n", append = TRUE)

    cat("if(!is.null(var3)){", file = reportRMD , sep = "\n", append = TRUE)
    cat(  "cat(var3)", file = reportRMD , sep = "\n", append = TRUE)
    cat("}", file = reportRMD , sep = "\n", append = TRUE)

    if(i %in% functionsOutput[,1]){
      cat(paste0("write.csv(report", i, ", paste0(\"", i, "\", \".csv\"))\n"), file = reportRMD , sep = "", append = TRUE)
      cat(paste0("cat(\"Please see the generated csv file:", i, ".csv\")"), file = reportRMD , sep = "\n", append = TRUE)
    } else if(reportType=="N"){
      cat(paste0("if(!is.null(report", i, ")){"), file = reportRMD , sep = "\n", append = TRUE)
      cat(paste0("  if(nrow(report", i, ")>0){"), file = reportRMD , sep = "\n", append = TRUE)
      cat(paste0("    kable(report", i, ")"), file = reportRMD , sep = "\n", append = TRUE)
      cat("  } else {", file = reportRMD , sep = "\n", append = TRUE)
      cat("    cat(\"\nno errors\")", file = reportRMD , sep = "\n", append = TRUE)
      cat("  }", file = reportRMD , sep = "\n", append = TRUE)
      cat("}", file = reportRMD , sep = "\n", append = TRUE)
    } else if(reportType=="G"){
      cat("if(!is.null(var4)){", file = reportRMD , sep = "\n", append = TRUE)
      cat("  print(var4)\n", file = reportRMD , sep = "", append = TRUE)
      cat("}", file = reportRMD , sep = "\n", append = TRUE)
    }

    cat("```", file = reportRMD , sep = "\n", append = TRUE)
  }
  return(working_directY)
}


