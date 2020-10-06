
wdir <- "C:/Users/yanni/Documents/GitHub/HighFrequencyChecks/"
file <- "form.xlsx"

repstr <- openxlsx::read.xlsx(paste0(wdir,file), 1)



reportRMD  <- paste0(wdir,"HFC.Rmd")
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
cat("library(HighFrequencyChecks)", file = reportRMD , sep = "\n", append = TRUE)
cat("```", file = reportRMD , sep = "\n", append = TRUE)


for(i in 1:length(repstr[,1])){
  if(repstr[i,1]=="begin_init_var"){
    cat("```{r eval=TRUE, echo=FALSE}", file = reportRMD , sep = "\n", append = TRUE)
    init_var<-TRUE
    free_code<-FALSE
    report<-FALSE
  } else if(repstr[i,1]=="end_init_var") {
    cat("```", file = reportRMD , sep = "\n", append = TRUE)
    init_var<-FALSE
    free_code<-FALSE
    report<-FALSE
  } else if(repstr[i,1]=="free_code") {
    init_var<-FALSE
    free_code<-TRUE
    report<-FALSE
  } else if(repstr[i,1]=="begin_report") {
    init_var<-FALSE
    free_code<-FALSE
    report<-TRUE
  } else if(repstr[i,1]=="end_report") {
    init_var<-FALSE
    free_code<-FALSE
    report<-FALSE
  }
  
  if(init_var){
    cat(paste0(repstr[i,1], "<-", repstr[i,2]), file = reportRMD , sep = "\n", append = TRUE)
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
      cat(paste0("list[var1,var2]<-", repstr[i,2], "(", repstr[i,3], ")"), file = reportRMD , sep = "\n", append = TRUE)
      
      cat("if(!is.null(var1)){", file = reportRMD , sep = "\n", append = TRUE)
      cat("  Header<-var1", file = reportRMD , sep = "\n", append = TRUE)
      cat("}", file = reportRMD , sep = "\n", append = TRUE)
      
      if(!is.na(repstr[i,4]) & stringi::stri_detect_fixed(repstr[i,4], "output=csv")){
        cat("write.csv(var2, paste0(", repstr[i,2], ", \".csv\"))\n", file = reportRMD , sep = "", append = TRUE)
        cat("```", file = reportRMD , sep = "\n", append = TRUE)
        cat("Please see the generated csv file: ", repstr[i,2], ".csv\n", file = reportRMD , sep = "", append = TRUE)
        
      } else {
        cat("if(nrow(var2)>0 | !is.null(var2)){", file = reportRMD , sep = "\n", append = TRUE)
        cat("  kable(var2)", file = reportRMD , sep = "\n", append = TRUE)
        cat("} else {", file = reportRMD , sep = "\n", append = TRUE)
        cat("  cat(\"\nno errors\")", file = reportRMD , sep = "\n", append = TRUE)
        cat("}", file = reportRMD , sep = "\n", append = TRUE)
        cat("```", file = reportRMD , sep = "\n", append = TRUE)
      }
      
    } else {
      # ignore
    }
    
    # cat(paste0(repstr[i,1], "<-", repstr[i,2]), file = reportRMD , sep = "\n", append = TRUE)
  }

  
}


