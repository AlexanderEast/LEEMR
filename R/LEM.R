library('rio')
library('dplyr')
library('stringr')




rm(list=ls())
wtcol <- "Sample Size"
x <- import_list("LEP Dev.xlsx")
data <- x$Data
factors <- x$Factors

colnames(data)<- tolower(colnames(data))
wtcol<-tolower(wtcol)



test <- c(as.character(data$media), "Food")


if (length(setdiff(unique(data$media), unique(factors$Media))) > 0){
  stop((str_c("Media '", setdiff(unique(test), unique(factors$Media)),
                                 "' not detected in factors media column.")))
}





if (!tolower(wtcol) %in% tolower(colnames(data))){
  stop((str_c("Column '", wtcol, "' not detected in data column names.")))
}













namer <- function(name){

  date   <- Sys.time()
  year   <- substr(date,1,4)
  month  <- substr(date,6,7)
  day    <- substr(date,9,10)
  hour   <- as.numeric(substr(date,12,13))
  minute <- substr(date,15,16)

  if (hour > 12){
    hour <- as.character(hour-12)
    minute <- paste0(minute,"PM")
  } else {
    minute <- paste0(minute,"AM")
  }

  filename <- paste0(name," ",hour,minute," ",month,"",day,"",year)

  return(filename)
}
time  <- namer('LEM Results')
time
