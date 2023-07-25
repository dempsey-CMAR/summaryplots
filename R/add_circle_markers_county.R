

#' Add circle markers to leaflet map
#'
#' @param map A map widget object created from leaflet().
#'
#' @param dat Data frame with 3 columns: \code{COUNTY}, \code{LONGITUDE}, and
#'   \code{LATITUDE}. Optional additional column for scaling \code{size}. The
#'   default inherits \code{dat} from \code{leaflet()}.
#'
#' @param county_pal Colour palette to assign to counties.
#'
#' @param pal_domain The possible values that can be mapped (e.g., counties).
#'   The default is the unique values of \code{COUNTY} in \code{dat}. For layers
#'   that do not include all counties (e.g., DO in mg/L), a character vector of
#'   all can be used so that the colours are consistent between layers.
#'
#' @param size Radius of the circle markers. Can be numeric or based on a column
#'   in \code{dat}, e.g., \code{~n_prop}.
#'
#' @param alpha Transparency of markers.
#'
#' @param group Name of the group the layer belongs to.
#'
#' @return a new map object
#'
#' @importFrom leaflet colorFactor addCircleMarkers getMapData
#'
#' @export

add_circle_markers_county <- function(
    map, dat = NULL, pal_domain = NULL, county_pal, size, alpha = 0.65, group = NULL
){

  if(is.null(pal_domain)) pal_domain <- unique(dat$COUNTY)

  if(is.null(dat)) dat <- getMapData(map)

  map_pal <- colorFactor(county_pal, domain = pal_domain)

  addCircleMarkers(
    map = map,
    data = dat,
    lng = ~LONGITUDE, lat = ~LATITUDE, weight = 1,
    color = ~map_pal(COUNTY),
    fillColor = ~map_pal(COUNTY),
    popup = ~popup,
    fillOpacity = alpha,
    radius = size,
    group = group
  )

}

#' Add legend for county colours to leaflet plot
#'
#' @inheritParams add_circle_markers_county
#'
#' @return a new map object
#'
#' @importFrom leaflet colorFactor addLegend
#'
#' @export

add_county_fill_legend <- function(map, dat = NULL, county_pal, alpha = 0.65) {

  if(is.null(dat)) dat <- getMapData(map)

  map_pal <- colorFactor(county_pal, domain = unique(dat$COUNTY))

  addLegend(
    map = map,
    "bottomright", pal = map_pal, values = ~COUNTY,
    title = "County",
    opacity = alpha
  )
}
