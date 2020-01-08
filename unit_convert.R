unit_convert <- function(unit) {
  y <- case_when(unit=="cm" ~ 1/0.032808, # feet to cm
                 TRUE ~ 1)
  return(y)
}