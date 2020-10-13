# chnsdata

This resp is for processing data from CHNS (China Health and Nutrition Survey). 

## How it works

### Preparing and cleaning the data

#### Baseline charicteristics preparing

Diet.R: Food data preprocessing and Plant-based Diet Scores computing

CHNS.R: Main program, including covariate preprocessing

#### X: plant-based diet score

#### Y: Type II diabetes

### Applying cox model to estimate the HR (Hazard Ratio)

### Predicting the disease burden change because of higher plant-based diet score

## What has been finished
- 1013

Diet.1013.R: PDI, hPDI, uPDI were calculated.

- 1011

Diet.1011.R: Foodgroup dictionary is updated by grouping foods into 17 categories. 


- 1010

Diet.1009.R: Foodgroup dictionary of FCT2002 is constructed.


- 1009

Diet.1009.R: Foodgroup dictionary of FCT1991 is constructed.

- 1001

CHNS.1001.R: Minor adjustment.

- 0930

CHNS.0930.R: Date of birth, gender, education, smoking, alcohol intake, BMI and total energy are accessed. Diagnosis of diabetes is accessed. Food intake data is found but needs further process. Physical activity METS needs to be constructed.


- 0929

CHNS.0929.R: Date of birth, gender, education and total energy are accessed. Diagnosis of diabetes is accessed.
