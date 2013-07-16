\name{construct.geocode.url}
\alias{construct.geocode.url}
\title{Construct url for geocoding}
\usage{
  construct.geocode.url(address, return.call = "json",
    sensor = "false")
}
\arguments{
  \item{address}{(character)}
}
\value{
  Encoded URL
}
\description{
  Construct url for geocoding
}
\examples{
construct.geocode.url("Harvard Square, Cambridge, MA, USA")
}
}
\seealso{
  \code{\link{gGeoCode}} which uses this function
}

