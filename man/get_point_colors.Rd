\name{get_point_colors}
\alias{get_point_colors}
\title{Find color from a given pallete that divide variable into 'intervals_count' intervals}
\usage{
  get_point_colors(var, intervals_count, style, palette)
}
\arguments{
  \item{var}{: numeric vector - variable you want to plot}

  \item{intervals_count}{: number of intervals in which you
  want your variable divided into.}

  \item{style}{: argument of RColorBrewer::classIntervals
  function.Determines how intervals are computed, 'jenks'
  is the fastest}

  \item{palette}{: RColorBrewer palette}
}
\value{
  vector of colors to plot on google static map
}
\description{
  Find color from a given pallete that divide variable into
  'intervals_count' intervals
}
\examples{
get_point_colors(human_capital_in_NYC, intervals_count = 7, style="jenks",palette ="PuRd")
}

