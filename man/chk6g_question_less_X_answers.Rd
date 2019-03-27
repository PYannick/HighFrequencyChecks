\name{chk6g_question_less_X_answers}
\alias{chk6g_question_less_X_answers}
\title{
Check the enumerators who pick up less than X answers per specific question
}
\description{
This function display the surveyors who picked up less than a specified amount of answers per specific question.
}
\usage{
chk6g_question_less_X_answers(ds,
                              enumeratorID,
                              questions,
                              minnbanswers)
}
\arguments{
  \item{ds}{
dataset as a data.frame object
}
  \item{enumeratorID}{
name as a string of the field in the dataset where the enumerator ID is stored
}
  \item{questions}{
columns as a list of string name from the dataset you want to check against (c('col1','col2',...))
}
  \item{minnbanswers}{
minimum anumber of answers expected per question
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
  qu<-c("consent_received.shelter_nfi.non_food_items[.]",
        "consent_received.food_security.main_income[.]",
        "consent_received.child_protection.boy_risk[.]",
        "consent_received.child_protection.girl_risk[.]")
  mna<-3

  chk6g_question_less_X_answers(df, eid, qu, mna)
}
