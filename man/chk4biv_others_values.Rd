\name{chk4biv_others_values}
\alias{chk4biv_others_values}
\title{
Report the values for other responses per fields
}
\description{
This function provide a report showing all distinct other values and the number of occurences for each fields "other".
This report can be global (all the surveys) or displayed for each enumerator ID
}
\usage{
chk4biv_others_values(ds,
                      otherpattern,
                      enumeratorID,
                      enumeratorcheck)
}
\arguments{
  \item{ds}{
dataset as a data.frame object
}
  \item{otherpattern}{
pattern as string to identify the fields containing others values ('_other$')
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
  op<-"_other$"
  eid<-"enumerator_id"
  ec<-FALSE

  chk4biv_others_values(df, op, eid, ec)
}
