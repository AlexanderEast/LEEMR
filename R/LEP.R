library('rio')
LEM <- function(data,n){
  return(x)
}



# A. Set seed.
set.seed(12345)

# B. Column check. needs columns (Units, Sample Size, Media,)

#1. Units to ng/m3,ng/L, or ng/g.
data<- data %>% dplyr::mutate(UNITFACTOR = case_when(
    (Units %in% c("ng/m3","ng/L","ng/g","µg/kg","ug/kg","pg/mL","pg/ml")) ~ 1,
    (Units %in% c("pg/m3","pg/g")) ~ 0.001,
    (Units %in% c("ng/mL","ug/l","µg/L","ug/m³","µg/m³","ug/m3")) ~ 1000)) %>%
  mutate_at(c("Min","Max","Median","Mean","SD","GM","GSD","P10","P25","P75","P90","P95","P99"),~.*UNITFACTOR) %>%
  mutate(Units = case_when(
    (Units %in% c("ug/m3","µg/m3","pg/m3","ng/m3")) ~ "ng/m3",
    (Units %in% c("ng/mL","ug/l","ug/L","µg/l","µg/L","pg/ml","pg/mL","ng/L")) ~ "ng/L",
    (Units %in% c("pg/g","µg/kg","ug/kg","ng/g")) ~ "ng/g")) %>%
  select(-UNITFACTOR)

#2. GM/GSD Estimator
# A. Estimate GM using Pleil 1.
data <- data %>% mutate(GM = if_else(!is.na(GM),GM , Median))

# B. Estimate GM using Pleil 2.
data <- data %>% mutate(GM = if_else(!is.na(GM),GM , Mean/(1+0.5 *(SD/Mean)^2)))

# C. Estimate GSD using Pleil 1.
data <- data %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P10/GM)/qnorm(.10)))) %>%
  mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P25/GM)/qnorm(.25)))) %>%
  mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P75/GM)/qnorm(.75)))) %>%
  mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P90/GM)/qnorm(.90)))) %>%
  mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P95/GM)/qnorm(.95)))) %>%
  mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P99/GM)/qnorm(.99))))

# D. Estimate GSD using Pleil 2.
data <- suppressWarnings(data %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(sqrt(2 * log(Mean/GM))))))
# E. Estimate GM using Pleil 3.
data <- data %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(Max/GM)/qnorm(1-1/`Sample Size`))))
data <- data %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(Min/GM)/qnorm(1/`Sample Size`))))

# F. Estimate Mean and SD using "Estimating.datalsdata" Methods (5) and (16). Mean calculated from min, median, Maximum,
# and SD from minimum, median, Maximum, and range.

data <- data %>% mutate(Mean = if_else(!is.na(Mean),Mean, (Min+2*Median+Max)/4))
data <- data %>% mutate(SD = if_else(!is.na(SD),SD, sqrt ((1/12) * ((Min-2*Median+Max)^2)/4 + (Max-Min)^2)))


# G. Estimate SD using Ramirez & Codata Method and range rule. Applied only if sample size  > 10.
data <- data %>% mutate(SD = if_else((!is.na(SD) & `Sample Size` > 10),SD, (Max-Min)/ (3*sqrt(log(`Sample Size`))-1.5)))
data <- data %>% mutate(SD = if_else((!is.na(SD) & `Sample Size` > 10),SD, (Max-Min)/4))

# ______________________________ Repeat A - E. ______________________________ #


# H. Estimate GM using Pleil 1.
data <- data %>% mutate(GM = if_else(!is.na(GM),GM , Median))

# I. Estimate GM using Pleil 2.
data <- data %>% mutate(GM = if_else(!is.na(GM),GM , Mean/(1+0.5 *(SD/Mean)^2)))

# J. Estimate GSD using Pleil 1.
data <- data %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P10/GM)/qnorm(.10)))) %>%
  mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P25/GM)/qnorm(.25)))) %>%
  mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P75/GM)/qnorm(.75)))) %>%
  mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P90/GM)/qnorm(.90)))) %>%
  mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P95/GM)/qnorm(.95)))) %>%
  mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P99/GM)/qnorm(.99))))

# K. Estimate GSD using Pleil 2.
data <- suppressWarnings(data %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(sqrt(2 * log(Mean/GM))))))

# L. Estimate GM using Pleil 3.
data <- data %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(Max/GM)/qnorm(1-1/`Sample Size`))))
data <- data %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(Min/GM)/qnorm(1/`Sample Size`))))

# M. Remove Infs.
data$GM[is.infinite(data$GM)]<- NA
data$GSD[is.infinite(data$GSD)]<- NA

# Split by media
