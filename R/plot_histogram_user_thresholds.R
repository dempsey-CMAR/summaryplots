#' Plot distribution of observations
#'
#' @param dat Data frame with at least one column.
#'
#' @param hist_col Name of the column to plot (in quotes).
#'
#' @param binwidth Width of the bins. Passed to \code{geom_histogram()}.
#'
#' @param fill_col Name of the column used to for the fill colour. If
#'   \code{fill_col = NULL}, the default fill colour will be used.
#'
#' @param pal Colour palette used for the fill.
#'
#' @param legend_name Name of the legend. Default is \code{fill_col}. There will
#'   not be a legend if \code{fill_col = NULL}.
#'
#' @param user_min user_min threshold
#' @param user_max user_max threshold
#'
#' @return Returns a gpplot object.
#'
#' @importFrom dplyr count rename
#' @importFrom ggplot2 ggplot aes after_stat annotate element_blank element_text
#'   element_rect ensym geom_histogram scale_fill_manual scale_y_continuous
#'   theme
#'
#' @export

plot_histogram_user_thresholds <- function(
    dat, hist_col, binwidth = NULL,
    user_min = -Inf, user_max = Inf,
    fill_col = NULL, pal = "grey50", legend_name = NULL) {

  dat <- dat %>%
    rename(hist_col = {{ hist_col }})

  dat <- rename(dat, fill_col = {{ fill_col }})

  if(!("fill_col" %in% colnames(dat))) {
    dat <- dat %>% mutate(fill_col = factor(-111))
  }

  p <- ggplot(dat, aes(hist_col, fill = fill_col)) +
    annotate(
      "rect", fill = "#009200",
      ymin = -Inf, ymax = Inf, alpha = 0.5,
      xmin = user_min, xmax = user_max
    ) +
    # annotate(
    #   "rect", fill = "#EDA247",
    #   ymin = -Inf, ymax = Inf, alpha = 0.5,
    #   xmin = -Inf, xmax = user_min
    # ) +
    # annotate(
    #   "rect", fill = "#EDA247",
    #   ymin = -Inf, ymax = Inf, alpha = 0.5,
    #   xmin = Inf, xmax = user_max
    # ) +
    geom_histogram(
      aes(y = 100 * after_stat(count / sum(count))),
      binwidth = binwidth, col = "grey10", fill = "grey50"
    ) +
    scale_y_continuous(
      "Percent of Observations", expand = expansion(mult = c(0, 0.1))
    ) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(colour = "grey10"),
      text = element_text(size = 14)
    )

  if(all(unique(dat$fill_col) == -111)) {
    p <- p +
      scale_fill_manual(values = pal, drop = FALSE) +
      theme(legend.position = "none")
  } else{
    if(is.null(legend_name)) legend_name <- ensym(fill_col)
    p <- p +
      scale_fill_manual(legend_name, values = pal, drop = FALSE)
  }

  p
}
