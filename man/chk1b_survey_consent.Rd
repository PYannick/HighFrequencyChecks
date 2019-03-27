\name{chk1b_survey_consent}
\alias{chk1b_survey_consent}
\title{
Check that all surveys have consent
}
\description{
This function check that all interviews in the dataset have information about the consent of the people surveyed, meaning all the field where this information is stored is not empty.
There is an option to automatically mark for deletion the surveys which have not consent information.
}
\usage{
chk1b_survey_consent(ds,
                     survey_consent,
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
  rc<-c("enumerator_id","X_uuid")
  dl<-FALSE

  chk1b_survey_consent(df, sc, rc, dl)
}
