

# libraries --------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(sjPlot)
library(survival)


# read data --------------------------------------------------------------------
orif <- read_excel("data/Ex-fix to ORIF 4.23.25.xlsx", sheet = "Sheet1")

# remove stuff at the bottom of the sheet
first_blank_row <- which(apply(orif, 1, function(x) all(is.na(x) | x == "")))[1]
orif <- orif[1:(first_blank_row - 1), ]
rm(first_blank_row)


# format -----------------------------------------------------------------------
orif <- orif %>%
  
  rename(
    admission_exfix = `Days from admission to Ex-fix`,
    exfix_orif = `Days of ORIF to Ex-fix`
  ) %>%
   
  filter(
    
    exfix_orif < 60,
    `Discharge Status` == "Alive"
    
  ) %>%
  
  mutate(
    
    # numeric
    Age = as.numeric(Age),
    ISS = as.numeric(ISS),
    
    # Race
    Race = case_when(
      Race == "Asian" ~ "Other Race",
      TRUE ~ Race
    ),
    
    Race = factor(
      Race,
      levels = c(
        "Black or African American",
        "White",
        "Asian",
        "Other Race"
      )
    ),
    
    # Extremity
    Extremity = case_when(
      str_detect(`Ex-fixe'd Extremity (Catgories)`, regex("ankle", ignore_case = TRUE)) ~ "Ankle",
      str_detect(`Ex-fixe'd Extremity (Catgories)`, regex("pilon", ignore_case = TRUE)) ~ "Ankle",
      str_detect(`Ex-fixe'd Extremity (Catgories)`, regex("femur", ignore_case = TRUE)) ~ "Femur",
      str_detect(`Ex-fixe'd Extremity (Catgories)`, regex("tibia", ignore_case = TRUE)) ~ "Tibia/Fibula",
      str_detect(`Ex-fixe'd Extremity (Catgories)`, regex("fibula", ignore_case = TRUE)) ~ "Tibia/Fibula",
      TRUE ~ "Other"
    ),
    
    Extremity = factor(Extremity, levels = c("Tibia/Fibula", "Femur", "Ankle", "Other")),
    
    # Insurance
    Insurance = case_when(
      `Primary Payor` == "Blue Cross Blue Shield" ~ "Commercial",
      `Primary Payor` == "HMO" ~ "Commercial",
      `Primary Payor` == "PPO" ~ "Commercial",
      `Primary Payor` == "Other Commercial" ~ "Commercial",
      `Primary Payor` == "Medicaid" ~ "Government",
      `Primary Payor` == "Medicare" ~ "Government",
      `Primary Payor` == "Other Government" ~ "Government",
      `Primary Payor` == "Self Pay" ~ "Other",
      `Primary Payor` == "Workers Compensation" ~ "Other",
      `Primary Payor` == "Automobile" ~ "Other",
      `Primary Payor` == "Military (Tricare)" ~ "Other",
      TRUE ~ NA
    ),
    
    Insurance = factor(Insurance, levels = c("Government", "Commercial", "Other")),
    
    # GCS
    GCS = case_when(
      str_detect(`GCS on Admission`, regex("15", ignore_case = TRUE)) ~ "15",
      TRUE ~ "<15"
    ),
    
    # Open
    Open = case_when(
      `Open (Y/N)` == "N" ~ "No",
      `Open (Y/N)` == "Y" ~ "Yes",
      TRUE ~ NA
    ),
    
    # Smoke
    Smoke = case_when(
      `Smoker (Y/N)` == "N" ~ "No",
      `Smoker (Y/N)` == "Y" ~ "Yes",
      TRUE ~ NA
      ),
    
    # Drugs
    Drugs = case_when(
      `Drug use (Y/N)` == "N" ~ "No",
      `Drug use (Y/N)` == "Y" ~ "Yes",
      TRUE ~ NA
    ),
    
    # Alcohol
    Alcohol = case_when(
      `Alcohol use (Y/N/Social)` == "N" ~ "No",
      `Alcohol use (Y/N/Social)` == "n" ~ "No",
      `Alcohol use (Y/N/Social)` == "Y" ~ "Yes",
      `Alcohol use (Y/N/Social)` == "Social" ~ "Yes",
      `Alcohol use (Y/N/Social)` == "S" ~ "Yes",
      TRUE ~ NA
    ),
    
    # COPD
    COPD = case_when(
      `COPD (Y/N)` == "N" ~ "No",
      `COPD (Y/N)` == "Y" ~ "Yes",
      TRUE ~ NA
    ),
    
    # Renal
    Renal = case_when(
      `Renail failure (Y/N)` == "N" ~ "No",
      `Renail failure (Y/N)` == "Y" ~ "Yes",
      TRUE ~ NA
    ),
    
    # Cirrhosis
    Cirrhosis = case_when(
      `Cirrhosis (Y/N)` == "N" ~ "No",
      `Cirrhosis (Y/N)` == "Y" ~ "Yes",
      TRUE ~ NA
    ),
    
    # CHF
    CHF = case_when(
      `CHF (Y/N)` == "N" ~ "No",
      `CHF (Y/N)` == "Y" ~ "Yes",
      TRUE ~ NA
    ),
    
    # Diabetes
    Diabetes = case_when(
      `DM(Y/N)` == "N" ~ "No",
      `DM(Y/N)` == "Y" ~ "Yes",
      TRUE ~ NA
    ),
    
    # PAD
    PAD = case_when(
      `PAD(Y/N0` == "N" ~ "No",
      `PAD(Y/N0` == "Y" ~ "Yes",
      TRUE ~ NA
    ),
    
    # Discharge Status
    `Discharge Status` = factor(
      `Discharge Status`,
      c("Deceased", "Alive")
    )
    
  ) %>%
  
  dplyr::select(
    # primary predictors
    Gender,
    Race,
    Insurance,
    
    # procedure characteristics
    ISS,
    GCS,
    Open,
    Extremity,
    
    # patient characteristics
    Age,
    Smoke,
    Drugs,
    Alcohol,
    COPD,
    Renal,
    Cirrhosis,
    CHF,
    Diabetes,
    PAD,
    
    # outcomes
    admission_exfix,
    exfix_orif,
    
    # additional
    `Discharge Status`
  ) %>%
  
  filter(
    if_all(everything(), ~ !is.na(.)),
    Extremity != "Other",
    Race != "Other Race",
    Insurance != "Other"
  )
