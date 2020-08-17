#' Form a grid of coordinates that is inside of a map of an EpiVis data frame.
#' 
#' Given a proper EpiVis data frame (`epi`) and the number of points, form a grid coordinates 
#' in the map of the EpiVis data frame (`epi`).
#'       
#' @details Note that `pts` can be a positive non-integer, and the number of points in the 
#' output grid is generally strictly less than the floor of the `pts`, 
#' unless the polygon is exactly rectangular.
#' 
#' @importFrom sp point.in.polygon
#' @importFrom maps map
#' 
#' @param epi a proper EpiVis data frame.
#' @param pts a numeric denoting the tentative number of points in the set. Generally points will be less than the floor of that number. Note that points = -1 if and only if kernal density is used.
#' @return a set of points (a vector of coordinates)
#' @export
epi_grid <- function(epi=NULL, pts=-1){
  if (pts <= 0){
    warning(paste0("Expected positive pts, but received pts = ", pts))
    return("Epi_grid has failed.")
  }
  else{
    mymap <- map("world", as.character(epi$map[1]), plot=FALSE)
    ## We first create new points that we want to predict at.
    xpoints <- seq(min(na.omit(mymap$x)), max(na.omit(mymap$x)),length.out=floor(sqrt(pts)))
    ypoints <- seq(min(na.omit(mymap$y)), max(na.omit(mymap$y)),length.out=floor(sqrt(pts)))
    point <- expand.grid(x=xpoints, y=ypoints)
    point <- point[which(point.in.polygon(point[,1],point[,2], mymap$x, mymap$y)==1),]
    point
    }
}

