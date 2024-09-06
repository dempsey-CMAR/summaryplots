#' Plot distribution of observations
#'
#' @param dat Data frame with at least one column.
#'
#' @param hist_col Name of the column to plot (in quotes).
#'
#' @param binwidth Width of the bins. Passed to \code{geom_histogram()}.
#'
#' @param user_thresh Optional threshold value(s) that will be plotted as
#'   vertical orange dotted lines.
#'
#' @param fill_col Name of the column used to for the fill colour. If
#'   \code{fill_col = NULL}, the default fill colour will be used.
#'
#' @param pal Colour palette used for the fill.
#'
#' @param bar_outline Single colour for the outline of the histogram bars.
#'   Default is "grey10". May leave artifacts (lines at the top of some groups)
#'   in the \code{ggplotly()} version of the figure. Use \code{bar_outline = NA}
#'   to avoid.
#'
#' @param legend_name Name of the legend. Default is \code{fill_col}. There will
#'   not be a legend if \code{fill_col = NULL}.
#'
#' @return Returns a gpplot object.
#'
#' @importFrom dplyr count rename
#' @importFrom ggplot2 ggplot aes after_stat element_blank element_text
#'   element_rect ensym geom_histogram scale_fill_manual geom_vline
#'   scale_y_continuous theme
#'
#' @export

plot_histogram <- function(
    dat, hist_col, binwidth = NULL, user_thresh = NULL,
    fill_col = NULL, pal = "#B0E0E6", bar_outline = "grey10", legend_name = NULL) {

  dat <- dat %>%
    rename(
      hist_col = {{ hist_col }},
      fill_col = {{ fill_col }}
    )

  #dat <- rename(dat, fill_col = {{ fill_col }})

  if(!("fill_col" %in% colnames(dat))) {
    dat <- dat %>% mutate(fill_col = factor(-111))
  }

  p <- ggplot(dat, aes(hist_col, fill = fill_col)) +
    geom_histogram(
      aes(
        y = 100 * after_stat(count / sum(count)),
        text = after_stat(
          paste(
            "percent:", round(100 * after_stat(count / sum(count)), digits = 2),
            "\nbin:", x
          ))
      ),
      binwidth = binwidth, col = bar_outline
    ) +
    scale_y_continuous(
      "Percent of Observations", expand = expansion(mult = c(0, 0.1))
    ) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(colour = "grey10", fill = NA),
      text = element_text(size = 14)
    )

  if(!is.null(user_thresh)) {
    p <- p +
      geom_vline(
        xintercept = user_thresh,
        linetype = 3, col = "#EDA247"
      )

  }

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
