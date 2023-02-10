# GraduationThesisOpen

## description
This repository has R codes for our graduation thesis. I cannot share raw data used in the thesis. Acutually results in the thesis are computed by Matlab.

## codes

- Latest_progress.R
  - run this file. remaining fules are run automatically.
- DataCleaning.R
   clean raw data
- J_ht_new.R 
  - calculate expected demand at h=1...T, t=1...T and then, compute objective function of simulated GMM 
  - number of draw from error terms is fixed at 30.
- Epsilon_ht_support_new.R
  - functions used in J_ht_new.R
- Epsilon_ht.R
  - calculate expected demand at h=1...T, t=1...T and then, compute prediction error 
- Epsilon_ht_support.R
  - functions used in Epsilon_ht.R
- DataUnderstanding.R
  - make figures
