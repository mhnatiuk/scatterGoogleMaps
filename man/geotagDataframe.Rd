\name{geotagDataframe}
\alias{geotagDataframe}
\title{Geotag dataframe via Google geo tag service}
\usage{
  geotagDataframe(df, geovars)
}
\arguments{
  \item{df}{: dataframe to geocode}

  \item{geovars}{: character vector with variable names
  that you want geotagged}
}
\value{
  dataframe with longitude and latitude values of nrow()
  equal to dataframe that was passed
}
\description{
  Geotag dataframe via Google geo tag service
}
\examples{
geotagDataframe(data, c("Street", "City", "Country"))
}

