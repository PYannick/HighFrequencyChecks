
#' @name assessmentProductivityGraphical
#' @rdname assessmentProductivityGraphical
#' @title Overall productivity histogram
#' @description This function create an histogram showing the overall productivity per consent status per day.
#'
#' @param ds dataset as a data.frame object
#' @param surveydate name as a string of the field in the dataset where the date of the survey is stored
#' @param dateformat format as a string used for the date ('\%m/\%d/\%Y')
#' @param survey_consent name as a string of the field in the dataset where the survey consent is stored
#'
#' @return graph  graphic as a plot.ly object
#'
#' @author Yannick Pascaud
#'
#' @examples
#' {
#' ds <- HighFrequencyChecks::sample_dataset
#' surveydate <- "survey_date"
#' dateformat <- "%m/%d/%Y"
#' survey_consent <- "survey_consent"
#'
#' assessmentProductivityGraphical(ds, surveydate, dateformat, survey_consent)
#'}
#' @export assessmentProductivityGraphical

assessmentProductivityGraphical <- function(ds = NULL,
                                            surveydate = NULL,
                                            dateformat = NULL,
                                            survey_consent = NULL){
  if(is.null(ds) | nrow(ds)==0 | !is.data.frame(ds)){
    stop("Please provide the dataset")
  }
  if(is.null(surveydate) | !is.character(surveydate)){
    stop("Please provide the field where the survey date is stored")
  }
  if(is.null(dateformat) | !is.character(dateformat)){
    stop("Please provide the format used for the date ('%m/%d/%Y')")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    stop("Please provide the field where the survey consent is stored")
  }

  tmp <- ds %>%
    #group_by_(surveydate=surveydate) %>%
    group_by(surveydate=.data[[surveydate]]) %>%
    count(survey_consent)

  colnames(tmp)[2] <- "survey_consent"
  tmp$surveydate <- as.Date(tmp$surveydate, dateformat)
  tmp <- tmp[with(tmp, order(surveydate)), ]
  tmp <- reshape2::dcast(tmp, surveydate ~ survey_consent, value.var="n")
  tmp[is.na(tmp)] <- 0

  graph <- plotly::plot_ly(type = 'bar', width = 800, height = 600)
  for(i in 2:length(tmp)){
    graph<-add_trace(graph, x=tmp[,1], y = tmp[,i], name = colnames(tmp)[i])
  }
  graph <- plotly::layout(graph,
                          yaxis = list(title = "Nb Survey"),
                          barmode = "stack")

  return(list(NULL,NULL,NULL,graph))
}
