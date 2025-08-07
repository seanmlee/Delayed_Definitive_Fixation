

# libraries --------------------------------------------------------------------
library(tidyverse)
library(sjPlot)


# full model -------------------------------------------------------------------
mod_selected_interactions <- glm(
  exfix_orif ~ 
    Gender + 
    Race + 
    Insurance +
    scale(ISS, scale = FALSE) + 
    GCS + 
    Open + 
    Extremity +
    scale(Age, scale = FALSE) + 
    Smoke + 
    Drugs + 
    Alcohol +
    COPD + 
    Renal + 
    Cirrhosis + 
    CHF + 
    Diabetes +
    
    # 2-way interactions
    Gender:Open +
    Race:Open +
    Gender:Extremity +
    Race:Extremity +
    Gender:Race
  ,
    
  family = poisson,
  data = orif
)

mod_stepwise <- step(mod_selected_interactions, direction = "both", trace = TRUE)
formula(mod_stepwise)
tab_model(mod_stepwise)
