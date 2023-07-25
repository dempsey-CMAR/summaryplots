#' Plot the mean and +/- 1 standard deviation for each month
#'
#' @param dat Data frame with columns: \code{month}, \code{mean},
#'   \code{stdev}, \code{variable}, and optionally \code{county}.
#'
#' @param facet_county If TRUE, plot will be faceted by county.
#'
#' @param text_size Numeric value for the size of the text.
#'
#' @return ggplot object.
#'
#' @importFrom ggplot2 aes element_text facet_wrap geom_errorbar geom_point ggplot
#'   scale_x_discrete scale_y_continuous theme
#'
#' @export

plot_mean_sd_season <- function(dat, facet_county = TRUE, text_size = 14) {

  if(length(unique(dat$variable)) > 1) {
    stop("More than one variable found in dat")
  }

  var = unique(dat$variable)

  p <- ggplot(dat, aes(month, mean)) +
    geom_point(size = 1) +
    geom_errorbar(
      aes(ymin = mean - stdev, ymax = mean + stdev), width = 0
    ) +
    scale_x_discrete(
      name = "", breaks = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")
    ) +
    scale_y_continuous(paste0(var, " (mean +/- standard deviation)")) +
    theme(text = element_text(size = text_size))

  if(isTRUE(facet_county)) {
    p <- p +
      facet_wrap(~county, ncol = 3)
  }

  p
}

#' Plot the mean and +/- 1 standard deviation as a ribbon for each month
#'
#' @param dat Data frame with columns: \code{month}, \code{mean}, \code{stdev},
#'   \code{variable}, and  \code{county}.
#'
#' @param group Name of a column in \code{dat} that will be used to assign
#'   colour.
#'
#' @param facet_county If TRUE, plot will be faceted by county.
#'
#' @param n_group Number of unique groups in dat. Used to make colour scale.
#'
#' @param text_size Numeric value for the size of the text.
#'
#' @param y_limits Optional numeric vector specifying limits for the y-axis.
#'
#' @return ggplot object.
#'
#' @importFrom dplyr rename
#'
#' @importFrom ggplot2 aes element_text facet_wrap geom_line geom_point
#'   geom_ribbon ggplot guides scale_colour_manual scale_fill_manual
#'   scale_x_discrete scale_y_continuous theme
#'
#' @importFrom viridis viridis
#'
#' @export
#'

plot_mean_sd_season_ribbon <- function(
    dat, group, n_group = NULL, facet_county = TRUE, y_limits = NULL, text_size = 14
) {

  if(length(unique(dat$variable)) > 1) {
    stop("More than one variable found in dat")
  }

  var = unique(dat$variable)

  if(is.null(n_group)) n_group <- nrow(unique(dat[, group]))

  dat <- dat %>% rename(group_col = {{ group }})

  p <- ggplot(
    dat,
    aes(month, mean, group = group_col, col = group_col,
        text = paste0(group_col,
                      "\nmonth: ", month,
                      "\nmean: ", mean,
                      "\nstdev: ", stdev)
    )
  ) +
    geom_point(size = 3) +
    geom_line(aes(col = group_col)) +
    geom_ribbon(
      aes(ymin = mean - stdev, ymax = mean + stdev, fill = group_col),
      alpha = 0.25, col = NA
    ) +
    scale_x_discrete(name = "") +
    scale_y_continuous(
      paste0(var, " (mean +/- standard deviation)"), limits = y_limits
    ) +
    theme(text = element_text(size = text_size))

  if(group == "county") {
    county_pal <- get_county_colour_palette(n_group)

    p <- p +
      scale_colour_manual("County", values = county_pal) +
      scale_fill_manual("County", values = county_pal)

  } else {
    p <- p +
      scale_colour_manual(group, values = viridis(n_group, option = "C"), drop = FALSE) +
      scale_fill_manual(group, values = viridis(n_group, option = "C"), drop = FALSE)
  }


  if(isTRUE(facet_county))  p <- p + facet_wrap(~county)

  p
}
