\name{chk4aiii_missing_pct}
\alias{chk4aiii_missing_pct}
\title{
Report the percentage of missing values (NA) per fields
}
\description{
This function provide a report showing the percentage of missing values (NA) for each fields.
This report can be global (all the surveys) or displayed for each enumerator ID
}
\usage{
chk4aiii_missing_pct(ds,
                     enumeratorID,
                     enumeratorcheck)
}
\arguments{
  \item{ds}{
dataset as a data.frame object
}
  \item{enumeratorID}{
name as a string of the field in the dataset where the enumerator ID is stored
}
  \item{enumeratorcheck}{
specify if the report has to be displayed for each enumerator or not as a boolean (TRUE/FALSE)
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
  ec<-FALSE

  chk4aiii_missing_pct(df, eid, ec)
}
