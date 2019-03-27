\name{chk3c_date_mistake}
\alias{chk3c_date_mistake}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check for surveys that show start date earlier than first day of data collection
}
\description{
This function check that all interviews in the dataset start after the actual first day of data collection.
There is an option to automatically mark for deletion the surveys which have started before the first day of data collection.
}
\usage{
chk3c_date_mistake(ds,
                   survey_consent,
                   dates,
                   start_collection,
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
  \item{start_collection}{
date as a string of the first day of data collection ('yyyy-mm-dd')
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
  st<-"2018-11-11"
  rc<-c("enumerator_id","X_uuid")
  dl<-FALSE

  chk3c_date_mistake(df, sc, dt, st, rc, dl)
}
