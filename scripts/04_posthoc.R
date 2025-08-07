

# libraries --------------------------------------------------------------------
library(tidyverse)
library(emmeans)


# pairwise comparisons with specific levels ------------------------------------
emm_gender <- emmeans(
  mod_stepwise,
  pairwise ~ Gender | Extremity,
  type = "response",
  at = list(
    Race = "Black or African American",
    GCS = "15",
    Open = "Yes",
    Smoke = "No",
    Alcohol = "No",
    COPD = "No",
    Renal = "No",
    Cirrhosis = "No"
  )
)

emm_race <- emmeans(
  mod_stepwise,
  pairwise ~ Race | Extremity,
  type = "response",
  at = list(
    Gender = "Male",
    GCS = "15",
    Open = "Yes",
    Smoke = "No",
    Alcohol = "No",
    COPD = "No",
    Renal = "No",
    Cirrhosis = "No"
  )
)



# formatted tables -------------------------------------------------------------
contrasts_gender <- summary(emm_gender$contrasts, infer = c(TRUE, TRUE)) %>%
  mutate(
    log_lower = log(ratio) - 1.96 * SE,
    log_upper = log(ratio) + 1.96 * SE,
    lower.CL = round(exp(log_lower), 2),
    upper.CL = round(exp(log_upper), 2),
    `Rate Ratio` = round(ratio, 2),
    `95% CI` = paste0(lower.CL, " – ", upper.CL),
    `p-value` = ifelse(p.value < 0.001, "<0.001", format(round(p.value, 3), nsmall = 3))
  ) %>%
  select(Extremity, Contrast = contrast, `Rate Ratio`, `95% CI`, `p-value`)

contrasts_race <- summary(emm_race$contrasts, infer = c(TRUE, TRUE)) %>%
  mutate(
    log_lower = log(ratio) - 1.96 * SE,
    log_upper = log(ratio) + 1.96 * SE,
    lower.CL = round(exp(log_lower), 2),
    upper.CL = round(exp(log_upper), 2),
    `Rate Ratio` = round(ratio, 2),
    `95% CI` = paste0(lower.CL, " – ", upper.CL),
    `p-value` = ifelse(p.value < 0.001, "<0.001", format(round(p.value, 3), nsmall = 3))
  ) %>%
  select(Extremity, Contrast = contrast, `Rate Ratio`, `95% CI`, `p-value`)


# write ------------------------------------------------------------------------
write.csv(contrasts_gender, "out/posthoc_gender.csv", row.names = FALSE)
write.csv(contrasts_race, "out/posthoc_race.csv", row.names = FALSE)
