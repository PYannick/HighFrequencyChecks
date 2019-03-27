\name{chk1dii_GIS_Xm}
\alias{chk1dii_GIS_Xm}
\title{
check if the surveys fall within Xm radius of a sampled point
}
\description{
This function check that all interviews in the dataset were made within a distance from a sampled point. It is based on a GIS shapefile providing the sample points for the assessment. The function is based on the GPS data filled in the survey to determine their location.
There is an option to automatically mark for deletion the surveys which are to far away from a sampled point.
}
\usage{
chk1dii_GIS_Xm(pts,
               ds,
               ds_coord,
               buff=10,
               survey_consent,
               reportingcol,
               delete)
}
\arguments{
  \item{pts}{
dataset containing the shapefile
}
  \item{ds}{
dataset as a data.frame object
}
  \item{ds_coord}{
columns as a list of string name from the dataset where the GPS coordinates are stored (c('Long','Lat'))
}
  \item{buff}{
value as an integer in meter to determine the buffer from a sampled point which is acceptable
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
\note{
One internal function "make_GeodesicBuffer" used to create the buffers is created by Valentin
https://stackoverflow.com/users/5193830/valentin
}
\section{Warning }{
Regardless the projection used for the shapefile, it is transformed to WGS84
}
\examples{
  pts<-SamplePts
  df<-sample_dataset
  df_coord<-c("X_gps_reading_longitude","X_gps_reading_latitude")
  bu<-10
  sc<-"survey_consent"
  rc<-c("enumerator_id","X_uuid")
  dl<-FALSE

  chk1dii_GIS_Xm(pts, df, df_coord, bu, sc, rc, dl)
}
