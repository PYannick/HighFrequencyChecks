\name{chk6b_duration}
\alias{chk6b_duration}
\title{
Check the average interview duration by enumerator
}
\description{
This function display the average interview duration per enumerator.
}
\usage{
chk6b_duration(ds,
               dates,
               enumeratorID)
}
\arguments{
  \item{ds}{
dataset as a data.frame object
}
  \item{dates}{
fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
}
  \item{enumeratorID}{
name as a string of the field in the dataset where the enumerator ID is stored
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
  dt<-c("survey_start","end_survey")
  eid<-"enumerator_id"

  chk6b_duration(df, dt, eid)
}
