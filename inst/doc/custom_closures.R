## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  echo = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(SimSurvey)

## -----------------------------------------------------------------------------

sim_logistic <- function(k = 2, x0 = 3, plot = FALSE) {
  function(x = NULL) {
    y <- 1 / (1 + exp(-k * (x - x0)))
    if (plot) plot(x, y, type = "b")
    y
  }
}


## -----------------------------------------------------------------------------
ages <- 1:20
logistic_fun <- sim_logistic(k = 1, x0 = 5)
logistic_curve <- logistic_fun(ages)
plot(x = ages, y = logistic_curve, xlab = "Age", ylab = "Catchability", type = "l")

## ---- eval = FALSE------------------------------------------------------------
#  
#  set.seed(438)
#  sim <- sim_abundance() %>%
#    sim_distribution() %>%
#    sim_survey(n_sims = 5, q = sim_logistic(k = 1, x0 = 5))
#  

## -----------------------------------------------------------------------------

sim_gaussian <- function(a = 1, b = 10, c = 5, plot = FALSE) {
  function(x = NULL) {
    y <- a * exp(-((x - b) ^ 2) / (2 * c ^ 2))
    if (plot) plot(x, y, type = "b")
    y
  }
}

gaussian_fun <- sim_gaussian(b = 15, c = 5)
gaussian_curve <- gaussian_fun(ages)
plot(x = ages, y = gaussian_curve, xlab = "Age", ylab = "Catchability", type = "l")


## ---- eval = FALSE------------------------------------------------------------
#  
#  set.seed(438)
#  sim <- sim_abundance() %>%
#    sim_distribution() %>%
#    sim_survey(n_sims = 5, q = sim_gaussian(b = 15, c = 5))
#  

