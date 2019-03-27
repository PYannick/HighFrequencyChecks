\name{chk7bi_nb_status}
\alias{chk7bi_nb_status}
\title{
Daily number of survey per consent status
}
\description{
This function display the number of surveys conducted per day per constent status.
}
\usage{
chk7bi_nb_status(ds,
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
\item{logf }{the report}
}
\author{
Yannick Pascaud
}
\examples{
  df<-sample_dataset
  sdte<-"survey_date"
  dtf<-"\%m/\%d/\%Y"
  sc<-"survey_consent"

  chk7bi_nb_status(df, sdte, dtf, sc)
}
