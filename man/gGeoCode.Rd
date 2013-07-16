\name{gGeoCode}
\alias{gGeoCode}
\title{Construct url for geocoding}
\usage{
  gGeoCode(address, verbose = FALSE)
}
\arguments{
  \item{address}{(character) to geocode}
}
\value{
  vector: c(latitude, longitude)
}
\description{
  Construct url for geocoding
}
\examples{
gGeoCode("Harvard Square, Cambridge, MA, USA")
}

