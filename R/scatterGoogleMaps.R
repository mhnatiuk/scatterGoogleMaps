library(classInt)
library(RgoogleMaps)
library(RCurl)
library(RJSONIO)
library(RColorBrewer)

#' Construct url for geocoding
#'
#' @param address (character)
#' @return Encoded URL
#' @seealso \code{\link{gGeoCode}} which uses this function
#' @export
#' @examples
#' construct.geocode.url("Harvard Square, Cambridge, MA, USA")
#' }
construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}


#' Construct url for geocoding
#'
#' @param address (character) to geocode
#' @return vector: c(latitude, longitude)
#' @export
#' @examples
#' gGeoCode("Harvard Square, Cambridge, MA, USA")
gGeoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- construct.geocode.url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    return(c(lat, lng))
  } else {
    return(c(NA,NA))
  }
}


#' Geotag dataframe via Google geo tag service
#'
#' @param df : dataframe to geocode
#' @param geovars : character vector with variable names that you want geotagged
#' @return dataframe with longitude and latitude values of nrow() equal to dataframe that was passed
#' @export
#' @examples
#' geotagDataframe(data, c("Street", "City", "Country"))
geotagDataframe <- function(df, geovars) 
{
  longitude <- vector() #dlugos geo
  latitude <- vector() #szerokosc geo
  if (NROW(df) == 0L) 
    stop("dataframe has no rows")
  if (NCOL(df) == 0L) {
    stop("dataframe has no columns")
  }
  if (!is.vector(geovars)) 
    stop("'by' must be a vector")
  geovar <- do.call("paste",c(df[geovars], sep = ", "))
  for(i in 1:nrow(df)){
    if(is.na(longitude[i])){
      coor <- gGeoCode(paste(geovar[i]) )
      latitude[i] <- coor[1]
      longitude[i] <- coor[2]
    }  
  }
  geodata <- data.frame(longitude,latitude)
  geodata
}


#' Find color from a given pallete that divide variable into 'intervals_count' intervals
#'
#' @param var : numeric vector - variable you want to plot
#' @param intervals_count : number of intervals in which you want your variable divided into.
#' @param style : argument of RColorBrewer::classIntervals function.Determines how intervals are computed, 'jenks' is the fastest
#' @param palette : RColorBrewer palette
#' @return vector of colors to plot on google static map
#' @export
#' @examples
#' get_point_colors(human_capital_in_NYC, intervals_count = 7, style="jenks",palette ="PuRd")
#' 
get_point_colors <- function(var,intervals_count,style,palette)
{
  intervals <- classIntervals(var, intervals_count, style=style)
  pal <- brewer.pal(intervals_count,palette)
  col <- findColours(intervals, pal)
  col
}


#' Main function. Plot plot_var on Static Google Map
#'
#' @param df : dataframe
#' @param geovars : character vector with variable names that you want geotagged
#' @param plot_var : variable you want to plot
#' @param zoom : zoom of google map. Remember to adjust it to your needs
#' @param intervals_count : number of intervals in which you want your variable divided into.
#' @param maptype : c("roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap","mapmaker-hybrid")
#' @param style : argument of RColorBrewer::classIntervals function. Determines how intervals are computed, 'jenks' is the fastest
#' @param palette : RColorBrewer palette
#' @param cex : size of point (as in base::plot())
#' @param ... : additional arguments to PlotOnStaticMap (base::plot() arguments)
#' @export
#' @examples
#' 
#' ulica <- c('"Bora" Komorowskiego', '"Boya" Żeleńskiego', '"Hubala" Dobrzańskiego',
#'           '"Lota" Pietraszewicza', '1 Maja', '1 Praskiego', '1 Praskiego Pułku', '1 Pułku',
#'           '1 Sierpnia', '11 Listopada', '15 Sierpnia', '17 Stycznia', '2 Armii', '21 Pułku',
#'           '21 Pułku Piechoty Dzieci Warszawy', '27 Grudnia', '29 Listopada', '6 Sierpnia',
#'           'A. Kamińskiego', 'Abrahama', 'Abramowskiego', 'Achera', 'Achillesa', 'Adama Pługa',
#'           'Adamieckiego')
#' miasto <- rep("Warszawa",length(ulica))
#' var <- rnorm(n=length(ulica),mean=50)
#' df <- data.frame(ulica,miasto,var)
#' plotOnGoogleMap(df,c("ulica","miasto"),df$var,zoom=11)
#' 
#' 
plotOnGoogleMap <- function(df,geovars,plot_var,zoom=6,intervals_count=7
                            ,maptype="roadmap", style="jenks",palette="OrRd",cex=2.5, ...)
{
  geodata <- geotagDataframe(df,geovars)
  col = get_point_colors(plot_var,intervals_count,style,palette)
  
  MyMap <- GetMap(zoom=zoom,center=c(mean(geodata$latitude,na.rm=TRUE),
                                  mean(geodata$longitude,na.rm=TRUE)),
                  maptype=maptype,
                  destfile = "MyTile1.png");
  
  PlotOnStaticMap(MyMap, lon = geodata$longitude, 
                  lat = geodata$latitude, 
                  destfile = "MyTile2.png", add=FALSE, cex=cex,pch=20,col=col, ...);
  
}




