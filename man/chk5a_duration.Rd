\name{chk5a_duration}
\alias{chk5a_duration}
\title{
Compute the average and total time for the surveys
}
\description{
This function compute the average and total time for the surveys
}
\usage{
chk5a_duration(ds,
               dates)
}
\arguments{
  \item{ds}{
dataset as a data.frame object
}
  \item{dates}{
fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
}
}
\value{
\item{avg }{average time per survey}
\item{tot }{total time}
}
\author{
Yannick Pascaud
}
\section{Warning }{
If there are uncorrected mistakes in the survey dates, it can lead to have the length of the survey in seconds and this check will not performed well
}
\examples{
  df<-sample_dataset
  dt<-c("survey_start","end_survey")

  chk5a_duration(df, dt)
}
