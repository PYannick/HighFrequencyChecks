#' @name hfc_projectinit
#' @rdname hfc_projectinit
#' @title  Analysis package project initiation
#'
#' @description  Create analysis package project structure
#'
#'
#' @return A structure of directory and scripts in order to set up quickly a project.
#'
#' @export hfc_projectinit
#'
#' @author Edouard Legoupil, Elliott Messeiller
#' @examples
#' \dontrun{
#' hfc_projectinit()
#' }


hfc_projectinit <- function() {

    mainDir <- getwd()

    cat("Let's create various standard folders and copy some analysis script\n")
    destfile = paste0(mainDir,"/DESCRIPTION")
    if (!file.exists(destfile)) {
      fileConn <- file(destfile)
      writeLines(c("Package: analysispackage",
                   "Type: Package",
                   "Title: My Analysis package",
                   "Version: 0.1.0",
                   "Author: me",
                   "Maintainer: me <me@unhcr.org>",
                   "Description: ",
                   "Encoding: UTF-8",
                   "RoxygenNote: 7.1.1"), fileConn)
      close(fileConn)
    }

    destfile = paste0(mainDir,"/.Rbuildignore")
    if (!file.exists(destfile)) {
      fileConn <- file(destfile)
      writeLines(c("usethis::use_data_raw()"), fileConn)
      close(fileConn)
    }

    ## doc folder creation ####
    subDir <- "docs"
    if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
      cat("docs exists in mainDir and is a directory.\n")
    } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
      cat("doc directory exists in your project directory but is a file.\n")
      # you will probably want to handle this separately
    } else {
      cat("docs directory does not exist in your project directory - creating now!\n ")
      dir.create(file.path(mainDir, subDir))
    }

    if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
      # By this point, the directory either existed or has been successfully created
      setwd(file.path(mainDir, subDir))
    } else {
      cat("")
      # Handle this error as appropriate
    }
    destfile = paste0(mainDir,"/docs/README.md")
    if (!file.exists(destfile)) {
      fileConn <- file(destfile)
      writeLines(c("### This folder is where your user name, password and config are stored"), fileConn)
      close(fileConn)
    }

    ## man folder creation ####
    subDir <- "man"
    if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
      cat("man exists in mainDir and is a directory.\n")
    } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
      cat("man directory exists in your project directory but is a file.\n")
      # you will probably want to handle this separately
    } else {
      cat("man directory does not exist in your project directory - creating now!\n ")
      dir.create(file.path(mainDir, subDir))
    }

    if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
      # By this point, the directory either existed or has been successfully created
      setwd(file.path(mainDir, subDir))
    } else {
      cat("")
      # Handle this error as appropriate
    }
    destfile = paste0(mainDir,"/man/README.md")
    if (!file.exists(destfile)) {
      fileConn <- file(destfile)
      writeLines(c("### This folder is where the project documentation will be build"), fileConn)
      close(fileConn)
    }


    ## R folder creation ####
    subDir <- "R"
    if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
      cat("Code exists in mainDir and is a directory.\n")
    } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
      cat("Code directory exists in your project directory but is a file.\n")
      # you will probably want to handle this separately
    } else {
      cat("Code directory does not exist in your project directory - creating now!\n ")
      dir.create(file.path(mainDir, subDir))
    }

    if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
      # By this point, the directory either existed or has been successfully created
      setwd(file.path(mainDir, subDir))
    } else {
      cat("")
      # Handle this error as appropriate
    }
    destfile = paste0(mainDir,"/R/README.md")
    if (!file.exists(destfile)) {
      fileConn <- file(destfile)
      writeLines(c("### This folder is where analysis scripts are saved"), fileConn)
      close(fileConn)
    }

    ## Need to test if we have multiple .libPaths()

    path <- as.data.frame(.libPaths())
    if (nrow(path) == 1) {path_correct <- as.character(path[1,1])
    } else {cat("You have multiple library path! \n")
      if (dir.exists(file.path(path[1,1],"/HighFrequencyChecks"))) {
        path_correct <- as.character(path[1,1])
      } else {path_correct <- as.character(path[2,1])}
    }

    # subsubDir <- "script"
    # if (file.exists(paste(mainDir, subDir,"/",subsubDir,"/", sep = "/", collapse = "/"))) {
    #   cat("script exists in subDir and is a directory.\n")
    # } else if (file.exists(paste(mainDir, subDir, subsubDir, sep = "/", collapse = "/"))) {
    #   cat("script directory exists in your project directory.\n")
    #   # you will probably want to handle this separately
    # } else {
    #   cat("script directory does not exist in your project directory - creating now!\n ")
    #   dir.create(file.path(mainDir, subDir,subsubDir))
    # }

    destfile = paste0(mainDir,"/R/run-check.R")
    if (!file.exists(destfile)) {
      file.copy(paste(path_correct,"/HighFrequencyChecks/script/run-check.R",sep = ""), destfile)
    }else{
      file.remove(destfile)
      file.copy(paste(path_correct,"/HighFrequencyChecks/script/run-check.R",sep = ""), destfile)
    }



    ## vignettes folder creation ####
    subDir <- "vignettes"
    if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
      cat("Code exists in mainDir and is a directory.\n")
    } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
      cat("Code directory exists in your project directory but is a file.\n")
      # you will probably want to handle this separately
    } else {
      cat("Code directory does not exist in your project directory - creating now!\n ")
      dir.create(file.path(mainDir, subDir))
    }

    if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
      # By this point, the directory either existed or has been successfully created
      setwd(file.path(mainDir, subDir))
    } else {
      cat("")
      # Handle this error as appropriate
    }
    destfile = paste0(mainDir,"/vignettes/README.md")
    if (!file.exists(destfile)) {
      fileConn <- file(destfile)
      writeLines(c("### This folder is where reports notbook will be kept - it follows standard R package structure"), fileConn)
      close(fileConn)
    }


    ## Data folder creation ####
    subDir <- "data"

    if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
      cat("Data exists in mainDir and is a directory.\n")
    } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
      cat("Data directory exists in your project directory.\n")
      # you will probably want to handle this separately
    } else {
      cat("Data directory does not exist in your project directory - creating now! \n")
      dir.create(file.path(mainDir, subDir))
    }

    if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
      # By this point, the directory either existed or has been successfully created
      setwd(file.path(mainDir, subDir))
    } else {
      cat("")
      # Handle this error as appropriate
    }
    destfile = paste0(mainDir,"/data/README.md")
    if (!file.exists(destfile)) {
      fileConn <- file(destfile)
      writeLines(c("### This folder is the one where are stored data in Rdata format once it's been processed by kobo_load_data",
                   "# BE CAREFUL: DO NOT SHARE PROTECTION SENSITIVE DATA ON GITHUB!",
                   "",
                   "This project is only to keep track of your analysis workflow"), fileConn)
      close(fileConn)
    }



    ## Data-raw folder creation ####
    subDir <- "data-raw"

    if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
      cat("data-raw exists in mainDir and is a directory.\n")
    } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
      cat("data-raw directory exists in your project directory.\n")
      # you will probably want to handle this separately
    } else {
      cat("data-raw directory does not exist in your project directory - creating now! \n")
      dir.create(file.path(mainDir, subDir))
    }

    if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
      # By this point, the directory either existed or has been successfully created
      setwd(file.path(mainDir, subDir))
    } else {
      cat("")
      # Handle this error as appropriate
    }
    destfile = paste0(mainDir,"/data-raw/README.md")
    if (!file.exists(destfile)) {
      fileConn <- file(destfile)
      writeLines(c("### This folder is the one where are stored data in CSV format, the form in XLS format and geodata in SHP format",
                   "# BE CAREFUL: DO NOT SHARE PROTECTION SENSITIVE DATA ON GITHUB!",
                   "",
                   "This project is only to keep track of your analysis workflow"), fileConn)
      close(fileConn)
    }




    ## Out folder creation ####

    subDir <- "out"
    if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
      cat("Out directory exists in your project directory and is a directory.\n")
    } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
      cat("Ouput exists in your project directory but is a file.\n")
      # you will probably want to handle this separately
    } else {
      cat("Out directory does not exist in your project directory - creating now!\n ")
      dir.create(file.path(mainDir, subDir))
    }

    if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
      # By this point, the directory either existed or has been successfully created
      setwd(file.path(mainDir, subDir))
    } else {
      cat("")
      # Handle this error as appropriate
    }
    destfile = paste0(mainDir,"/out/README.md")
    if (!file.exists(destfile)) {
      fileConn <- file(destfile)
      writeLines(c("### This folder is where the analysis output will be generated",
                   "# BE CAREFUL: DO NOT SHARE PROTECTION SENSITIVE DATA ON GITHUB!"), fileConn)
      close(fileConn)
    }




    mainDirectory <- paste0(mainDir,"/out")
    subDir <- "/High_Frequency_Checks"
    if (file.exists(paste(mainDirectory, subDir, "/", sep = "/", collapse = "/"))) {
      cat("High_Frequency_Checks directory exists in out directory and is a directory.\n")
    } else if (file.exists(paste(mainDirectory, subDir, sep = "/", collapse = "/"))) {
      cat("High_Frequency_Checks directory exists in your out directory.\n")
      # you will probably want to handle this separately
    } else {
      cat("High_Frequency_Checks directory does not exist in your out directory - creating now!\n ")
      dir.create(file.path(mainDirectory, subDir))
    }



    ## reset the correct Working directory
    setwd(mainDir)
    cat("Please open now the file called run-check.R within the ++R++ folder, configure the xlsform and get your dataset. \n ")


}
NULL
