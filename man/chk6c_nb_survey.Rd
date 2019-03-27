\name{chk6c_nb_survey}
\alias{chk6c_nb_survey}
\title{
Check the number of surveys by enumerator
}
\description{
This function display the total number of survey made and the average per day per enumerator.
}
\usage{
chk6c_nb_survey(ds,
                surveydate,
                enumeratorID)
}
\arguments{
  \item{ds}{
dataset as a data.frame object
}
  \item{surveydate}{
name as a string of the field in the dataset where the date of the survey is stored
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
  sdte<-"survey_date"
  eid<-"enumerator_id"

  chk6c_nb_survey(df, sdte, eid)
}
