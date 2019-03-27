\name{chk5d_duration_outliers}
\alias{chk5d_duration_outliers}
\title{
Report the outlier durations for the surveys
}
\description{
This function report the outlier durations for the surveys
}
\usage{
chk5d_duration_outliers(ds,
                        dates,
                        sdval,
                        reportingcol)
}
\arguments{
  \item{ds}{
dataset as a data.frame object
}
  \item{dates}{
fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
}
  \item{sdval}{
number of standard deviation for which the duration within is considered as acceptable
}
  \item{reportingcol}{
columns as a list of string name from the dataset you want in the result (c('col1','col2',...))
}
}
\value{
\item{logf }{the report}
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
  sdv<-2
  rc<-c("enumerator_id","X_uuid")

  chk5d_duration_outliers(df, dt, sdv, rc)
}
