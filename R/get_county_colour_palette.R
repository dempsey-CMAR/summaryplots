#' Generate consistent colour palette for counties
#'
#' @param n_col Number of colours to generate (number of counties).
#'
#' @return n_col hex codes
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#'
#' @export


get_county_colour_palette <- function(n_col) {

  # set up colour palette - need to interpolate with colorRampPalette
  get_pal = colorRampPalette(brewer.pal(8, "Dark2"))

  get_pal(n_col)

}
