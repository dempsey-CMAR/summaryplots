
#' Calculate percent of observations flagged by user thresholds
#'
#' @param dat Data frame with a least one column: \code{VALUE}.
#'
#' @param user_thresh Data frame with columns \code{mean}, \code{stdev},
#'   \code{user_min} and \code{user_max}.
#'
#' @param group Identifier for how the thresholds were calculated
#'
#' @importFrom dplyr %>% bind_cols summarise select mutate
#'
#' @export

summarise_n_user_flags <- function(dat, user_thresh, group) {

  dat %>%
    bind_cols(user_thresh) %>%
    summarise(
      percent_greater = 100 * sum(VALUE  > user_max) / n(),
      percent_less = 100 * sum(VALUE < user_min) / n(),
      percent_flagged = 100 * (sum(VALUE > user_max) + sum(VALUE < user_min)) / n()
    ) %>%
    bind_cols(user_thresh) %>%
    select(
      mean, stdev, user_min, user_max, percent_greater, percent_less, percent_flagged
    ) %>%
    mutate(group = group)

}
