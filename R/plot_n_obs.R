
#' Plot the number of observations for each county
#'
#' @param dat Data frame with at least three columns: \code{n}, \code{county},
#'   \code{units}.
#'
#' @param text_size Numeric value for the size of the text.
#'
#' @return ggplot object.
#'
#' @importFrom ggplot2 aes element_text geom_col geom_vline ggplot
#'   position_dodge2 scale_fill_manual scale_x_continuous scale_y_discrete theme
#'
#' @export

plot_n_obs_county <- function(dat, text_size = 14) {

  if("variable" %in% colnames(dat)) {

    if(length(unique(dat$variable)) > 1) {
      stop("More than one variable found in dat")
    }

  }

  p <- ggplot(dat, aes(n, county, fill = units)) +
    geom_col(
      col = 1,
      position = position_dodge2(preserve = "single", reverse = TRUE, padding = 0)
    ) +
    geom_vline(xintercept = 0) +
    scale_x_continuous(
      "Number of Observations", expand = expansion(mult = c(0, 0.1))
    ) +
    scale_y_discrete(name = "", limits = rev) +
    theme(text = element_text(size = text_size))


  if(length(unique(dat$units)) == 1) {
    p <- p +
      scale_fill_manual("Units", values = "#AE1759FF") +
      theme(legend.position = "none")
  }


  if(length(unique(dat$units)) == 2) {
    p <- p +
      scale_fill_manual("Units", values = c("#F6B48EFF", "#AE1759FF"))
  }

  p

}

#' Plot the number of observations for each depth
#'
#' @param dat Data frame with at least two columns: \code{n}, \code{depth}.
#'
#' @param facet_county If TRUE, plot will be faceted by county.
#'
#' @param text_size Numeric value for the size of the text.
#'
#' @return ggplot object
#'
#' @importFrom ggplot2 aes element_text expansion geom_col geom_vline ggplot
#'   position_dodge2 scale_color_discrete scale_x_continuous scale_y_reverse theme
#' @export

plot_n_obs_depth <- function(dat, facet_county = TRUE, text_size = 14) {

  if("variable" %in% colnames(dat)) {

    if(length(unique(dat$variable)) > 1) {
      stop("More than one variable found in dat")
    }

  }

  p <- ggplot(
    dat, aes(n, depth, fill = factor(depth),
             text = paste("n: ", n, "\n", "depth: ", depth, "m"))
  ) +
    geom_col(orientation = "y")+
    #geom_vline(xintercept = 0) +
    scale_x_continuous(
      "Number of Observations",
      expand = expansion(mult = c(0, 0.1))
    ) +
    scale_y_reverse(name = "Sensor Depth Below Surface at Low Tide (m)") +
    scale_color_discrete("depth") +
    theme(
      legend.position = "none",
      text = element_text(size = text_size)
    )

  if(isTRUE(facet_county)) p <- p + facet_wrap(~county, ncol = 3)

  p
}



#' Plot the number of observations for each month
#'
#' @param dat Data frame with at least three columns: \code{n}, \code{month},
#'   and optionally \code{county}.
#'
#' @param facet_county If TRUE, plot will be faceted by county.
#'
#' @param text_size Numeric value for the size of the text.
#'
#' @param fill Hex code. Colour to fill bars.
#'
#' @return ggplot object.
#'
#' @importFrom ggplot2 aes element_text geom_col geom_vline ggplot
#'   position_dodge2 scale_fill_manual scale_x_continuous scale_y_discrete theme
#'
#' @export

plot_n_obs_month <- function(
    dat, facet_county = TRUE, text_size = 14, fill = "#AE1759FF"

    ) {

  if("variable" %in% colnames(dat)) {

    if(length(unique(dat$variable)) > 1) {
      stop("More than one variable found in dat")
    }

  }

  p <- ggplot(dat, aes(n, month)) +
    geom_col(
      col = 1, fill = fill,
      position = position_dodge2(preserve = "single", reverse = TRUE, padding = 0)
    ) +
    geom_vline(xintercept = 0) +
    scale_x_continuous(
      "Number of Observations", expand = expansion(mult = c(0, 0.1))
    ) +
    scale_y_discrete(name = "", limits = rev) +
    theme(text = element_text(size = text_size))

  if(isTRUE(facet_county)) p <- p + facet_wrap(~county, ncol = 3)

  p

}
