\name{chk7aii_productivity_hist}
\alias{chk7aii_productivity_hist}
\title{
Overall productivity histogram
}
\description{
This function create an histogram showing the overall productivity per consent status per day.
}
\usage{
chk7aii_productivity_hist(ds,
                          surveydate,
                          dateformat,
                          survey_consent)
}
\arguments{
  \item{ds}{
dataset as a data.frame object
}
  \item{surveydate}{
name as a string of the field in the dataset where the date of the survey is stored
}
  \item{dateformat}{
format as a string used for the date ('\%m/\%d/\%Y')
}
  \item{survey_consent}{
name as a string of the field in the dataset where the survey consent is stored
}
}
\value{
\item{graph }{the graphic as a plot.ly object}
}
\author{
Yannick Pascaud
}
\examples{
  df<-sample_dataset
  sdte<-"survey_date"
  dtf<-"\%m/\%d/\%Y"
  sc<-"survey_consent"

  chk7aii_productivity_hist(df, sdte, dtf, sc)
}
