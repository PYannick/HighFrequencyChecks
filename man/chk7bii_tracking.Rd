\name{chk7bii_tracking}
\alias{chk7bii_tracking}
\title{
Overall tracking sheet
}
\description{
This function display the overall tracking sheet.
}
\usage{
chk7bii_tracking(ds,
                 sf,
                 dssite,
                 sfsite,
                 survcons,
                 sftarget,
                 sfnbpts,
                 formul,
                 colorder)
}
\arguments{
  \item{ds}{
dataset as a data.frame object
}
  \item{sf}{
sampling frame as a data.frame object
}
  \item{dssite}{
name as a string of the field in the dataset where the site is stored
}
  \item{sfsite}{
name as a string of the field in the sampling frame where the site is stored
}
  \item{survcons}{
name as a string of the field in the dataset where the survey consent is stored
}
  \item{sftarget}{
name as a string of the field where the target number of survey is stored in the sampling frame
}
  \item{sfnbpts}{
name as a string of the field where the number of points generated is stored in the sampling frame
}
  \item{formul}{
formulas as a list of string used to compute the final number of eligible surveys and the variance from the target (C('formula1','formula2')).
the values/fields available are: done and the ones generated according the survey consent values (one per value)
}
  \item{colorder}{
column names as a list of string to order the colums in the result (C('col1','col2',...)).
the columns available are: site, done, final, variance and the ones generated according the survey consent values (one per value)
}
}
\value{
\item{logf }{the report}
}
\author{
Yannick Pascaud
}
\examples{
  df=sample_dataset
  sf=SampleSize
  dssite="union_name"
  sfsite="Union"
  sc="survey_consent"
  sftarget="SS"
  sfnbpts="TotPts"
  #formul=c("done-no-not_eligible-deleted","done-no-not_eligible-deleted-SS")
  #colorder=c("site","SS","Provision","done","not_eligible","no","deleted","yes","final","variance")
  formul=c("done-no-not_eligible","done-no-not_eligible-SS")
  colorder=c("site","SS","TotPts","done","not_eligible","no","yes","final","variance")

  chk7bii_tracking(df, sf, dssite, sfsite, sc, sftarget, sfnbpts, formul, colorder)
}
