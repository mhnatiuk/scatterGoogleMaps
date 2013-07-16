\name{plotOnGoogleMap}
\alias{plotOnGoogleMap}
\title{Main function. Plot plot_var on Static Google Map}
\usage{
  plotOnGoogleMap(df, geovars, plot_var, zoom = 6,
    intervals_count = 7, maptype = "roadmap",
    style = "jenks", palette = "OrRd", cex = 2.5, ...)
}
\arguments{
  \item{df}{: dataframe}

  \item{geovars}{: character vector with variable names
  that you want geotagged}

  \item{plot_var}{: variable you want to plot}

  \item{zoom}{: zoom of google map. Remember to adjust it
  to your needs}

  \item{intervals_count}{: number of intervals in which you
  want your variable divided into.}

  \item{maptype}{: c("roadmap", "mobile", "satellite",
  "terrain", "hybrid",
  "mapmaker-roadmap","mapmaker-hybrid")}

  \item{style}{: argument of RColorBrewer::classIntervals
  function. Determines how intervals are computed, 'jenks'
  is the fastest}

  \item{palette}{: RColorBrewer palette}

  \item{cex}{: size of point (as in base::plot())}

  \item{...}{: additional arguments to PlotOnStaticMap
  (base::plot() arguments)}
}
\description{
  Main function. Plot plot_var on Static Google Map
}
\examples{
ulica <- c('"Bora" Komorowskiego', '"Boya" Żeleńskiego', '"Hubala" Dobrzańskiego',
          '"Lota" Pietraszewicza', '1 Maja', '1 Praskiego', '1 Praskiego Pułku', '1 Pułku',
          '1 Sierpnia', '11 Listopada', '15 Sierpnia', '17 Stycznia', '2 Armii', '21 Pułku',
          '21 Pułku Piechoty Dzieci Warszawy', '27 Grudnia', '29 Listopada', '6 Sierpnia',
          'A. Kamińskiego', 'Abrahama', 'Abramowskiego', 'Achera', 'Achillesa', 'Adama Pługa',
          'Adamieckiego')
miasto <- rep("Warszawa",length(ulica))
var <- rnorm(n=length(ulica),mean=50)
df <- data.frame(ulica,miasto,var)
plotOnGoogleMap(df,c("ulica","miasto"),df$var,zoom=11)
}

