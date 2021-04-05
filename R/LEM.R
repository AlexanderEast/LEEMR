# Lorber Egeghy Model
#
# Learn more by entering "?LorberEgeghyModel::LEM"
# into the console or running the line in R.

## AE: MENTION UNITS

library('rio')
data <- rio::import("PFOA Drinking Water.xlsx",sheet ="PFOA Drinking Water" )
waterfactors <- 1.4*0.9 #(L/DAY INTAKE * Absorption Factor)



LEM <- function(df,wt = NULL,ef,n){
  if(is.null(wt)){
    wt <- 1
  }

  return(wt)
}

LEM(df = 5, ef = 2, n = 6)



