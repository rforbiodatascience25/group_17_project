library(tidyverse)
library(broom)

get_model_stats <- function(model){
  summ <- model |>
    summary()
  slope_est <- summ$coefficients[2,1]
  slope_std <- summ$coefficients[2,2]
  slope_p   <- summ$coefficients[2,4]
  R_squared <- summ$adj.r.squared

  out_tible <- tibble("slope_est"=slope_est,
                      "slope_std"=slope_std,
                      "slope_p"  =slope_p,
                      "R_squared"=R_squared)

  return(out_tible)
}

get_model_stats(model_white)
