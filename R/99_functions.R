library(tidyverse)
library(broom)
library(glue)

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

#render a qmd file to a folder. Only write the name of the file inside the R folder. and no suffix
render_to_results <- function(file_name){
  ren_path <- here(glue("R/{file_name}.qmd"))
  dump_path <- here(glue("R/{file_name}.html"))
  results_path <- here(glue("results/{file_name}.html"))


  quarto_render(ren_path)

  file.rename(
    from = dump_path,
    to = results_path)

  file.rename(
    from=here(glue("R/{file_name}_files")),
    to = here(glue("results/{file_name}_files"))
    )
}
