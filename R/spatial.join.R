#' A function to join point data to geojson
#' @param input Input dataframe
#' @param geojson Path to geojson
#' @keywords creation
#' @export
#' @examples
#' spatial.join()

spatial.join <- function(input, geojson) {
  input.sp <- input
  coordinates(input.sp) <- ~Var1+Var2
  input.proj <- sp::CRS("+proj=longlat + datum=WGS84")
  sp::proj4string(input.sp) <- input.proj

  rgdal::map <- readOGR(geojson)
  sp::map <- spTransform(map, input.proj)
  plot(map)
  map.join <- sp::over(input.sp, map)
  return(map.join)
}
