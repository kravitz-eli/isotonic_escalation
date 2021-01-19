# Libraries --------------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(here)

# Functions --------------------------------------------------------------------
source(here("get_dose_response.R"))

# Parameters -------------------------------------------------------------------
n_per_dose <- 6
alpha <- qlogis(0.30) 
beta <- 2.75
n_doses <- 5
dose_response <- get_dose_response(reference = 3, n_doses = 5, alpha = alpha, beta = beta) 
plot(dose_response, type = "b")

# Simulation ------------------------------------------------------------------
patient_df <- tibble(
  dose = seq(1, n_doses, by = 1),
  n_per_dose = n_per_dose,
  pr_tox = dose_response
) %>% 
  mutate(
    n_dlt = rbinom(n(), n_per_dose, pr_tox),
    dlt_rate = n_dlt / n_per_dose
  )

# Isotonic regression
isotonic_fit <- isoreg(patient_df$dose, patient_df$dlt_rate)
plot(isotonic_fit)

