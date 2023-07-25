#' Plot the mean and +/- 1 standard deviation for each depth
#'
#' @param dat Data frame with columns: \code{depth}, \code{mean}, \code{stdev},
#'   \code{variable}, and optionally \code{county}.
#'
#' @param facet_county If TRUE, plot will be faceted by county.
#'
#' @param text_size Numeric value for the size of the text.
#'
#' @return ggplot object.
#'
#' @importFrom ggplot2 aes element_text facet_wrap geom_errorbar geom_point
#'   ggplot scale_color_discrete scale_x_continuous scale_y_reverse theme unit
#'
#' @export

plot_mean_sd_depth <- function(dat, facet_county = TRUE, text_size = 14) {

  if(length(unique(dat$variable)) > 1) {
    stop("More than one variable found in dat")
  }

  var = unique(dat$variable)

 # dat <- dat %>% mutate(depth = factor(depth))

  p <- ggplot(dat,
              aes(mean, depth, col = factor(depth),
                  text = (paste0("mean: ", mean, "\ndepth: ", depth)))
  ) +
    geom_point(size = 1) +
    geom_errorbar(aes(xmin = mean - stdev, xmax = mean + stdev), width = 0) +
    scale_y_reverse(
      name = "Sensor Depth Below the Surface at Low Tide (m)"
    ) +
    scale_x_continuous(paste0(var, " (mean +/- standard deviation)")) +
    scale_color_discrete("depth") +
    theme(text = element_text(size = text_size))

  if(isTRUE(facet_county)) {
    p <- p +
      facet_wrap(~county, ncol = 3)
  }

  p
}
