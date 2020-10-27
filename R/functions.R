#' @name piechart
#' @rdname piechart
#' @title Create a donut chart with ggplot
#' @description This function allow to create a donut chart with ggplot.
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
    ggplot2::xlim(c(0, 4)) +
    ggplot2::annotate(geom = 'text', x = 0, y = 0,
                      label = paste0("Errors\n", round(t1$fraction[t1$categories == 'NOK']*100,0), "%"),
                      size = 10,
                      color = "red") +
    ggplot2::theme_void() +
    ggplot2::theme(panel.grid=ggplot2::element_blank()) +
    ggplot2::theme(axis.text=ggplot2::element_blank()) +
    ggplot2::theme(axis.ticks=ggplot2::element_blank()) +
    ggplot2::labs(title=graphTitle)
}

#' @name booleanSum
#' @rdname booleanSum
#' @title Sum bollean value
#' @description This function allow to perform a Boolean sum (AND) over a vector of boolean values (TRUE/FALSE)
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

#' @name ni
#' @rdname ni
#' @title Not in
#' @description This function create the not in function
#'
#' @param x vector or NULL: the values to be matched. Long vectors are supported.
#' @param table vector or NULL: the values to be matched against. Long vectors are not supported.
#'
'%ni%' <- function(x, table) !(match(x, table, nomatch = 0) > 0)

#' @name mapFunctions
#' @rdname mapFunctions
#' @title Mapping of the function which can be used for the report
#' @description Mapping of the function which can be used for the report based on the variables configured
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
    # Recuperer les variables necessaires
    # variablesList <- names(functionsConfig[functionsConfig$functionName==i,unlist(lapply(functionsConfig[functionsConfig$functionName==i,], isTRUE))])
    variablesList <- names(functionsConfig[functionsConfig$functionName==i,])[unlist(lapply(functionsConfig[functionsConfig$functionName==i,], isTRUE))]
    variablesListNotOptional <- variablesList[mapply('%ni%', variablesList, variablesOptional)]
    # Verifier que ces variables sont disponibles
    variablesDefined <- variablesConfig[variablesConfig$variableName %in% variablesList,]
    if(!booleanSum(variablesListNotOptional %in% variablesConfig$variableName)){
      # Toutes les variables necessaires pour cette fonction ne sont pas definies
    } else if(booleanSum(variablesListNotOptional %in% variablesConfig$variableName)){
      # Toutes les variables necessaires pour cette fonction sont definies
      # Supprimer les variables necessaires (variable qui doivent de toutes facon etre declarees mais ne font pas partie de l'appel)
      variableListNotNecessary <- names(variablesNecessary[variablesNecessary$functionName==i,])[unlist(lapply(variablesNecessary[variablesNecessary$functionName==i,], isTRUE))]
      if(!identical(variableListNotNecessary, character(0))){
        variablesDefined <- variablesDefined[variablesDefined$variableName %ni% variableListNotNecessary, ]
        # variablesDefined <- variablesDefined[mapply('%ni%', variablesDefined, variableListNotNecessary)]
      }
      # On construit l'appel pour la fonction
      variablesDefined[variablesDefined$variableName %in% variablesDatasets$datasets, "variableValue"] <-
        variablesDefined[variablesDefined$variableName %in% variablesDatasets$datasets, "variableName"]
      functionCode <- paste0(i, "(", paste(paste0(variablesDefined$variableName, "=", variablesDefined$variableValue), collapse=", "), ")")
      # Sauvegarde pour une utilisation ulterieure
      allFunctions[i] <- functionCode
    }
  }

  functionsList <- list()
  if(reportType=="N"){
    for(i in functionsConfig[with(functionsConfig, order(ord)), "functionName"]){
      functionsList[[i]] <- allFunctions[[i]]
    }
  } else if(reportType=="G"){
    # Liste les fonctions disponibles pour un affichage graphique
    for(i in functionsConfig[with(functionsConfig, order(ord)), "functionName"]){
      if(i %in% functionsGraphics$functionName){
        functionsList[[i]] <- allFunctions[[i]]
      }
    }
  }

  return(functionsList)
}
