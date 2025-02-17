% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_mean_sd_season.R
\name{plot_mean_sd_season_ribbon}
\alias{plot_mean_sd_season_ribbon}
\title{Plot the mean and +/- 1 standard deviation as a ribbon for each month}
\usage{
plot_mean_sd_season_ribbon(
  dat,
  group,
  n_group = NULL,
  facet_county = TRUE,
  y_limits = NULL,
  text_size = 14
)
}
\arguments{
\item{dat}{Data frame with columns: \code{month}, \code{mean}, \code{stdev},
\code{variable}, and  \code{county}.}

\item{group}{Name of a column in \code{dat} that will be used to assign
colour.}

\item{n_group}{Number of unique groups in dat. Used to make colour scale.}

\item{facet_county}{If TRUE, plot will be faceted by county.}

\item{y_limits}{Optional numeric vector specifying limits for the y-axis.}

\item{text_size}{Numeric value for the size of the text.}
}
\value{
ggplot object.
}
\description{
Plot the mean and +/- 1 standard deviation as a ribbon for each month
}
