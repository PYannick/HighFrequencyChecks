\name{chk6f_productivity}
\alias{chk6f_productivity}
\title{
Check the surveyors with very low or high productivity
}
\description{
This function display the surveyors with very low or high productivity.
}
\usage{
chk6f_productivity(ds,
                   enumeratorID,
                   surveydate,
                   sdval)
}
\arguments{
  \item{ds}{
dataset as a data.frame object
}
  \item{enumeratorID}{
name as a string of the field in the dataset where the enumerator ID is stored
}
  \item{surveydate}{
name as a string of the field in the dataset where the date of the survey is stored
}
  \item{sdval}{
number of standard deviation for which the duration within is considered as normal
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
  eid<-"enumerator_id"
  sdte<-"survey_date"
  sdv<-2

  chk6f_productivity(df, eid, sdte, sdv)
}
