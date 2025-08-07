

# libraries --------------------------------------------------------------------
library(table1)


# unstratified -----------------------------------------------------------------
table1(
  ~ 
    # patient characteristics
    Age +
    Gender +
    Race +
    Insurance +
    Smoke +
    Drugs +
    Alcohol +
    COPD +
    Renal +
    Cirrhosis +
    CHF +
    Diabetes +
    PAD +
    
    # procedure characteristics
    ISS +
    GCS +
    Open +
    Extremity +
    
    # additional
    `Discharge Status` +
      
    # outcomes
    admission_exfix +
    exfix_orif
    ,
  data = orif
)


# stratified -------------------------------------------------------------------
table1(
  ~ 
    # patient characteristics
    Age +
    Gender +
    Race +
    Insurance +
    Smoke +
    Drugs +
    Alcohol +
    COPD +
    Renal +
    Cirrhosis +
    CHF +
    Diabetes +
    PAD +
    
    # procedure characteristics
    ISS +
    GCS +
    Open +
    Extremity +
    
    # additional
    `Discharge Status` +
    
    # outcomes
    admission_exfix +
    exfix_orif | Race * Gender
  ,
  overall = FALSE,
  data = orif
)
