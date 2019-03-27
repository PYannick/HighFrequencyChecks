\name{chk6a_refusal}
\alias{chk6a_refusal}
\title{
Check the percentage of survey refusals by enumerator
}
\description{
This function display the percentage of survey refusal per enumerator.
}
\usage{
chk6a_refusal(ds,
              survey_consent,
              enumeratorID)
}
\arguments{
  \item{ds}{
dataset as a data.frame object
}
  \item{survey_consent}{
name as a string of the field in the dataset where the survey consent is stored
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
  sc<-"survey_consent"
  eid<-"enumerator_id"

  chk6a_refusal(df, sc, eid)
}
