# Need this so that package will play nice with dplyr package
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
# more technical solution here: https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html

# other technical solution here:
# https://dplyr.tidyverse.org/articles/programming.html


utils::globalVariables(
  c(
   "county",
   "depth",
   "n",
   "stdev",
   "month",

   "VALUE",
   "percent_greater",
   "percent_less",
   "percent_flagged",
   "user_max",
   "user_min",

   "n_group",
   "group_col",
   "x"

  )
)




