#' @name piechart
#' @rdname piechart
#' @title Create a donut chart with ggplot
#' @description This function allow to create a donut chart with ggplot.
#' @keywords internal
#'
#' @param data dataset as a data.frame object
#' @param graphTitle aes paramters
#'
#' @return  a ggplot object
piechart <- function(data, graphTitle){
  t1<-data
  t1$fraction = t1$Nb / sum(t1$Nb)
  t1 = t1[order(t1$fraction), ]
  t1$ymax = cumsum(t1$fraction)
  t1$ymin = c(0, utils::head(t1$ymax, n=-1))

  ggplot2::ggplot(t1, ggplot2::aes(fill=categories, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
    ggplot2::geom_rect(colour="grey30") +
    ggplot2::coord_polar(theta="y") +
    ggplot2::xlim(c(1, 4)) +
    ggplot2::theme_void() +
    ggplot2::theme(panel.grid=ggplot2::element_blank()) +
    ggplot2::theme(axis.text=ggplot2::element_blank()) +
    ggplot2::theme(axis.ticks=ggplot2::element_blank()) +
    ggplot2::theme(legend.position='none') +
    ggplot2::labs(title=graphTitle, subtitle = paste0("Errors: ", round(t1$fraction[t1$categories == 'NOK']*100,2), "%"))
}

#' @name booleanSum
#' @rdname booleanSum
#' @title Sum bollean value
#' @description This function allow to perform a Boolean sum (AND) over a vector of boolean values (TRUE/FALSE)
#' @keywords internal
#'
#' @param x vector of booleans
#'
#' @return boolean value
booleanSum <- function(x){
  result <- x[1]
  if(length(x)>1){
    for(i in 2:length(x)){
      result <- result & x[i]
    }
  }
  return(result)
}

#' @name NotIn
#' @rdname NotIn
#' @title Not in
#' @description This function create the not in function
#' @keywords internal
#'
#' @param x vector or NULL: the values to be matched. Long vectors are supported.
#' @param table vector or NULL: the values to be matched against. Long vectors are not supported.
#'
'%ni%' <- function(x, table) !(match(x, table, nomatch = 0) > 0)

#' @name mapFunctions
#' @rdname mapFunctions
#' @title Mapping of the function which can be used for the report
#' @description Mapping of the function which can be used for the report based on the variables configured
#' @keywords internal
#'
#' @param variablesConfig dataset where the variables are defined
#' @param reportType type of report wanted (N for text one, G for graphical one)
#'
#' @return list of possible functions
mapFunctions <- function(variablesConfig, reportType){
  functionsOutputs <- subset(functionsOutputs, !is.null(functionsOutputs$outputType) & !is.na(functionsOutputs$outputType) & functionsOutputs$outputType!="")
  functionsGraphics <- subset(functionsGraphics, !is.null(functionsGraphics$graph) & !is.na(functionsGraphics$graph) & functionsGraphics$graph!="")
  variablesConfig <- subset(variablesConfig, !is.null(variablesConfig$variableValue) & !is.na(variablesConfig$variableValue) & variablesConfig$variableValue!="")

  allFunctions <- list()
  for(i in functionsConfig$functionName){
    # Get the needed variables for the function
    variablesList <- names(functionsConfig[functionsConfig$functionName==i,])[unlist(lapply(functionsConfig[functionsConfig$functionName==i,], isTRUE))]
    variablesListNotOptional <- variablesList[mapply('%ni%', variablesList, variablesOptional)]
    # Check the variables are available in the configuration files provided
    variablesDefined <- variablesConfig[variablesConfig$variableName %in% variablesList,]
    if(!booleanSum(variablesListNotOptional %in% variablesConfig$variableName)){
      # All the necessary variables for this function are not defined
    } else if(booleanSum(variablesListNotOptional %in% variablesConfig$variableName)){
      # All the necessary variables for this function are defined
      # Remove the Necessary variables (the ones which have to be defined in any way but are not passed directly to the function)
      variableListNotNecessary <- names(variablesNecessary[variablesNecessary$functionName==i,])[unlist(lapply(variablesNecessary[variablesNecessary$functionName==i,], isTRUE))]
      if(!identical(variableListNotNecessary, character(0))){
        variablesDefined <- variablesDefined[variablesDefined$variableName %ni% variableListNotNecessary, ]
      }
      variablesDefined[variablesDefined$variableName %in% variablesDatasets$datasets, "variableValue"] <-
        variablesDefined[variablesDefined$variableName %in% variablesDatasets$datasets, "variableName"]
      # Building the function call
      functionCode <- paste0(i, "(", paste(paste0(variablesDefined$variableName, "=", variablesDefined$variableValue), collapse=", "), ")")
      # Put the function call in a list
      allFunctions[i] <- functionCode
    }
  }

  functionsList <- list()
  if(reportType=="N"){
    for(i in functionsConfig[with(functionsConfig, order(ord)), "functionName"]){
      functionsList[[i]] <- allFunctions[[i]]
    }
  } else if(reportType=="G"){
    for(i in functionsConfig[with(functionsConfig, order(ord)), "functionName"]){
      if(i %in% functionsGraphics$functionName){
        functionsList[[i]] <- allFunctions[[i]]
      }
    }
  }

  return(functionsList)
}
