#' Plot the mean and +/- 1 standard deviation for each county
#'
#' @param dat Data frame with at least four columns: \code{county}, \code{mean},
#'   \code{stdev}, and \code{variable}.
#'
#' @param ordered If TRUE, orders the counties based on the mean. If FALSE,
#'   orders counties alphabetically.
#'
#' @param text_size Numeric value for the size of the text.
#'
#' @return ggplot object.
#'
#' @importFrom ggplot2 aes element_text geom_errorbar geom_point ggplot
#'   scale_x_continuous scale_y_discrete theme
#'
#' @importFrom stats reorder
#'
#' @export

plot_mean_sd_county <- function(dat, ordered = TRUE, text_size = 14) {

  if(length(unique(dat$variable)) > 1) {
    stop("More than one variable found in dat")
  }

  var = unique(dat$variable)

  if(isTRUE(ordered)) {

    p <- ggplot(
      dat,
      aes(mean, reorder(county, mean), col = mean,
          text = paste("county: ", county, "\nmean: ", mean))
      ) +
      scale_y_discrete(name = "")

  } else{

    p <-  ggplot(dat, aes(mean, county)) +
      scale_y_discrete(name = "", limits = rev)
  }

  p +
    geom_point(size = 4) +
    geom_errorbar(
      aes(xmin = mean - stdev, xmax = mean + stdev), width = 0
    ) +
    scale_x_continuous(paste0(var, " (mean +/- standard deviation)")) +
    theme(
      text = element_text(size = text_size),
      legend.position = "none"
    )

}
