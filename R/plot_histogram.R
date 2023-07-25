#' Plot distribution of observations
#'
#' @param dat Data frame with at least one column.
#'
#' @param hist_col Name of the column to plot (in quotes).
#'
#' @param binwidth Width of the bins. Passed to \code{geom_histogram()}.
#'
#' @return Returns a gpplot object.
#'
#' @importFrom dplyr count rename
#' @importFrom ggplot2 ggplot aes element_blank element_text element_rect
#'   geom_histogram after_stat scale_y_continuous theme
#'
#' @export

plot_histogram <- function(dat, hist_col, binwidth = NULL) {

  dat <- dat %>%
    rename(hist_col = {{ hist_col }})

  ggplot(dat, aes(hist_col)) +
    geom_histogram(
      aes(y = 100 * after_stat(count / sum(count))),
      binwidth = binwidth, col = "grey10", fill = "#B0E0E6"
    ) +
    scale_y_continuous(
      "Percent of Observations", expand = expansion(mult = c(0, 0.1))
    ) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(colour = "grey10"),
      text = element_text(size = 14)
    )

}
