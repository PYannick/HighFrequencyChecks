\name{chk3a_date_mistake}
\alias{chk3a_date_mistake}
\title{
Check for surveys that do not end on the same day as they started
}
\description{
This function check that all interviews in the dataset start and end the same day.
There is an option to automatically mark for deletion the surveys which have different starting and ending dates.
}
\usage{
chk3a_date_mistake(ds,
                   survey_consent,
                   dates,
                   reportingcol,
                   delete)
}
\arguments{
  \item{ds}{
dataset as a data.frame object
}
  \item{survey_consent}{
name as a string of the field in the dataset where the survey consent is stored
}
  \item{dates}{
fields as a list of string where the survey start and end date is stored (c('start_date','end_date'))
}
  \item{reportingcol}{
columns as a list of string name from the dataset you want in the result (c('col1','col2',...))
}
  \item{delete}{
delete action to be done as a boolean (TRUE/FALSE)
}
}
\value{
\item{ds }{same dataset as the inputed one but with survey marked for deletion if errors are found and delete=TRUE}
\item{errors }{list of the errors found}
}
\author{
Yannick Pascaud
}
\examples{
  df<-sample_dataset
  sc<-"survey_consent"
  dt<-c("survey_start","end_survey")
  rc<-c("enumerator_id","X_uuid")
  dl<-FALSE

  chk3a_date_mistake(df, sc, dt, rc, dl)
}
