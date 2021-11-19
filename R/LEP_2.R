library('rio')
library('dplyr')
library('stringr')



rm(list=ls())
wtcol <- "Sample Size"
x <- import_list("LEP Dev.xlsx")
data <- x$Data
factors <- x$Factors
n <- 2000
rm(x)



LEM <- function(data,factors,wtcol,n){


# A. Set seed.
set.seed(12345)
# B. Column check. needs columns (Units, weight, Media,)

colnames(data)<- tolower(colnames(data))
wtcol<-tolower(wtcol)
#1. Units to ng/m3,ng/L, or ng/g.
data<- data %>% dplyr::mutate(UNITFACTOR = case_when(
    (units %in% c("ng/m3","ng/L","ng/g","µg/kg","ug/kg","pg/mL","pg/ml")) ~ 1,
    (units %in% c("pg/m3","pg/g")) ~ 0.001,
    (units %in% c("ng/mL","ug/l","µg/L","ug/m³","µg/m³","ug/m3")) ~ 1000)) %>%
  mutate_at(c("min","max","median","mean","sd","gm","gsd","p10","p25","p75","p90","p95","p99"),~.*UNITFACTOR) %>%
  mutate(Units = case_when(
    (units %in% c("ug/m3","µg/m3","pg/m3","ng/m3")) ~ "ng/m3",
    (units %in% c("ng/mL","ug/l","ug/L","µg/l","µg/L","pg/ml","pg/mL","ng/L")) ~ "ng/L",
    (units %in% c("pg/g","µg/kg","ug/kg","ng/g")) ~ "ng/g")) %>%
  select(-UNITFACTOR)

#2. gm/gsd Estimator
# A. Estimate gm using Pleil 1.
data <- data %>% mutate(gm = if_else(!is.na(gm),gm , median))

# B. Estimate gm using Pleil 2.
data <- data %>% mutate(gm = if_else(!is.na(gm),gm , mean/(1+0.5 *(sd/mean)^2)))

# C. Estimate gsd using Pleil 1.
data <- data %>% mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p10/gm)/qnorm(.10)))) %>%
  mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p25/gm)/qnorm(.25)))) %>%
  mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p75/gm)/qnorm(.75)))) %>%
  mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p90/gm)/qnorm(.90)))) %>%
  mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p95/gm)/qnorm(.95)))) %>%
  mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p99/gm)/qnorm(.99))))

# D. Estimate gsd using Pleil 2.
data <- suppressWarnings(data %>% mutate(gsd = if_else(!is.na(gsd),gsd ,exp(sqrt(2 * log(mean/gm))))))
# E. Estimate gm using Pleil 3.
data <- data %>% mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(max/gm)/qnorm(1-1/`sample size`))))
data <- data %>% mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(min/gm)/qnorm(1/`sample size`))))

# F. Estimate mean and sd using "Estimating.datalsdata" Methods (5) and (16). mean calculated from min, median, maximum,
# and sd from minimum, median, maximum, and range.

data <- data %>% mutate(mean = if_else(!is.na(mean),mean, (min+2*median+max)/4))
data <- data %>% mutate(sd = if_else(!is.na(sd),sd, sqrt ((1/12) * ((min-2*median+max)^2)/4 + (max-min)^2)))


# G. Estimate sd using Ramirez & Codata Method and range rule. Applied only if weight  > 10.
data <- data %>% mutate(sd = if_else((!is.na(sd) & `weight` > 10),sd, (max-min)/ (3*sqrt(log(`weight`))-1.5)))
data <- data %>% mutate(sd = if_else((!is.na(sd) & `weight` > 10),sd, (max-min)/4))

# ______________________________ Repeat A - E. ______________________________ #


# H. Estimate gm using Pleil 1.
data <- data %>% mutate(gm = if_else(!is.na(gm),gm , median))

# I. Estimate gm using Pleil 2.
data <- data %>% mutate(gm = if_else(!is.na(gm),gm , mean/(1+0.5 *(sd/mean)^2)))

# J. Estimate gsd using Pleil 1.
data <- data %>% mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p10/gm)/qnorm(.10)))) %>%
  mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p25/gm)/qnorm(.25)))) %>%
  mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p75/gm)/qnorm(.75)))) %>%
  mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p90/gm)/qnorm(.90)))) %>%
  mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p95/gm)/qnorm(.95)))) %>%
  mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p99/gm)/qnorm(.99))))

# K. Estimate gsd using Pleil 2.
data <- suppressWarnings(data %>% mutate(gsd = if_else(!is.na(gsd),gsd ,exp(sqrt(2 * log(mean/gm))))))

# L. Estimate gm using Pleil 3.
data <- data %>% mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(max/gm)/qnorm(1-1/`sample size`))))
data <- data %>% mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(min/gm)/qnorm(1/`sample size`))))

# M. Remove Infs.
data$gm[is.infinite(data$gm)]<- NA
data$gsd[is.infinite(data$gsd)]<- NA

# Remove co



# Split by media and chemical
md <- split(data,list(data$media,data$chemical),drop = TRUE)


wgmwgsd <- function(x){

# filter out NAs
x <- x[!is.na(x[wtcol]),]

# wgm wgsd
wgm  <- weighted.mean(x$gm[complete.cases(x$gsd,x$gm)],
                      x[,wtcol][complete.cases(x$gsd,x$gm)])
wgsd <- weighted.mean(x$gsd[complete.cases(x$gsd,x$gm)],
                      x[,wtcol][complete.cases(x$gsd,x$gm)])

y <- data.frame(wgm,wgsd)

return(y)
}


md<-lapply(md,wgmwgsd)

# Create concentration curves
distributions <- function(x){
set.seed(seed = 12345)
Concentration<- rlnorm(n,log(x$wgm),abs(log(x$wgsd)))
Concentration <- data.frame(Concentration)
return(Concentration)
}
conc <- lapply(md,distributions)

# Add names
conc <- bind_rows(conc, .id = "Media Chemical")


# Load Exposure Factors
factors <-split(factors,list(factors$Individual,factors$Media,factors$Path),drop = TRUE)


exposurefactors<- function(x){

myname <- str_c(unique(x$Individual)," ",
                unique(x$Media)," ",
                unique(x$Path))

y <- data.frame(prod(x$Factor),unique(x$Media))
colnames(y)<- c(myname,"Media")

return(y)
}

factors<-lapply(factors,exposurefactors)


# Apply Exposure Factors

conc2exposure <- function(x){
# Match Media across factors and concentration
y<- conc[grepl(as.character(x$Media),conc$`Media Chemical`),]

# Tidy
y$`Media Chemical` <- gsub("\\.", " ", y$`Media Chemical` )

# Create columns for export df
name <- str_remove(y$`Media Chemical`,as.character(x$Media))
name <- str_c(colnames(x)[1],name)
conc <- as.numeric(unlist(y$Concentration))
exp  <- as.numeric(unlist(x[1]))*conc

# Bind and export
y <- data.frame(cbind(name,conc,exp))
return(y)
}


result <- lapply(factors,conc2exposure)
result <- bind_rows(result)


# Summarize Exposure Data
thesummary <- split(result,result$name)


getsummary <- function(x){

Output <- t(c("Exposure", as.character(unique(x$name)), quantile(as.numeric(as.character(x$exp)),c(0,.10,.5,.75,.95,1))))
colnames(Output)[1:8]<-c("Output","Scenario","Min","10th%","Median","75th%","95th%","Max")
Concentration <- t(c("Concentration", as.character(unique(x$name)), quantile(as.numeric(as.character(x$conc)),c(0,.10,.5,.75,.95,1))))

y <- data.frame(rbind(Output,Concentration))

return(y)
}


thesummary <- lapply(thesummary,getsummary)
thesummary  <- bind_rows(thesummary)

colnames(thesummary)[1:8]<-c("Output","Scenario","Min","10th%","Median","75th%","95th%","Max")
thesummary$Units <- "ng/day"


colnames(result) <- c("Scenario","Concentration","Exposure")

finished <- list("Summary" = thesummary,
                 "Data"    = data,
                 "Raw"     = result)

export(finished,"LEM_RESULTS.xlsx")

cat("LEM Complete. See Working Directory for LEM_Results.xlsx.")
}


LEM(data,factors,"Sample Size",n)


