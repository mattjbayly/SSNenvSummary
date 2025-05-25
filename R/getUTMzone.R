#' Get UTM Zone from an sf Object
#'
#' This function calculates the UTM zone for a given sf object by computing its bounding box,
#' determining the centroid of that box, transforming the centroid to EPSG:4326 (WGS84),
#' and then calculating the UTM zone based on the centroid's longitude.
#'
#' @param sf_object An object of class \code{sf}.
#' @param epsg_code Boolean (FALSE/TRUE). If TRUE, the function will return the EPSG code of the UTM zone. Default is FALSE.
#'
#' @return An integer value representing the UTM zone.
#'
#' @details The UTM zone is computed using the formula:
#' \deqn{floor((longitude + 180) / 6) + 1,}
#' where \code{longitude} is extracted from the centroid of the bounding box after transforming
#' to EPSG:4326. This function works regardless of the original coordinate reference system of
#' the sf object.
#'
#' @examples
#' \dontrun{
#'   library(sf)
#'   my_sf <- st_read("path/to/your/shapefile.shp")
#'   utm_zone <- getUTMzone(my_sf)
#'   print(utm_zone)
#' }
#'
#' @import sf
#' @export
getUTMzone <- function(sf_object, epsg_code = FALSE) {
  # Get the bounding box of the sf object
  bbox <- st_bbox(sf_object)

  # Calculate the centroid of the bounding box
  center_lon <- (bbox["xmin"] + bbox["xmax"]) / 2
  center_lat <- (bbox["ymin"] + bbox["ymax"]) / 2
  center_point <- st_sfc(st_point(c(center_lon, center_lat)), crs = st_crs(sf_object))

  # Transform the point to EPSG:4326 (WGS84)
  center_point_wgs84 <- st_transform(center_point, crs = 4326)

  # Extract the longitude (X coordinate)
  coords <- st_coordinates(center_point_wgs84)
  lon <- coords[1, "X"]

  # Calculate UTM zone using the standard formula:
  # UTM zone = floor((longitude + 180)/6) + 1
  utm_zone <- floor((lon + 180) / 6) + 1
  utm_zone <- as.numeric(as.character(utm_zone))

  if(epsg_code) {
    # Calculate the EPSG code for the UTM zone
    if (utm_zone > 0 && utm_zone < 60) {
      epsg_code <- paste0("326", sprintf("%02d", utm_zone))
    } else if (utm_zone > 60 && utm_zone < 120) {
      epsg_code <- paste0("327", sprintf("%02d", utm_zone - 60))
    } else {
      stop("UTM zone out of range. Valid range is 1-60.")
    }
    epsg_code <- as.numeric(as.character(epsg_code))
    return(epsg_code)
  } else {

    return(utm_zone)
  }

}
