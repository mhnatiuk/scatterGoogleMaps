library(classInt)
library(RgoogleMaps)
library(RCurl)
library(RJSONIO)
library(RColorBrewer)

construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

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

geotagDataframe <- function(df, geovars) 
{
  longitude <- vector() #dlugos geo
  latitude <- vector() #szerokosc geo
  if (!is.data.frame(df)) 
    x <- as.data.frame(df)
  
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
  data.frame(longitude,latitude)
}

get_point_colors <- function(var,intervals_count,style,palette)
{
  intervals <- classIntervals(proba$liczba_wywiadow, intervals_count, style=style)
  pal <- brewer.pal(intervals_count,"PuRd")
  col <- findColours(intervals, pal)
  col
}

plotOnGoogleMap(df,geovars,plot_var,latitude,longitude,zoom=6,intervals_count=7,
                style="jenks",palette="PuRd", ...)
{
  geodata <- geotagDataframe(df,geovars)
  col = get_point_colors(plot_var,intervals_count,style,palette)
  
  MyMap <- GetMap(zoom=zoom,center=c(mean(geodata$latitude,na.rm=TRUE),
                                  mean(geodata$longitude,na.rm=TRUE)),
                  maptype="satellite",
                  destfile = "MyTile1.png");
  
  PlotOnStaticMap(MyMap, lon = geodata$longitude, 
                  lat = geodata$latitude, 
                  destfile = "MyTile2.png", add=FALSE, cex=1.5,pch=20,col=col, ...);
  
}





ulica <- c("Małej Łąki", "Pod Lipą", "Armii Ludowej")
miasto <- c("Warszawa","Warszawa","Warszawa")
var <- 1:3
df <- data.frame(ulica,miasto,var)
geo<- geotagDataframe(df,c("ulica","miasto"))




