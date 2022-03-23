# Lorber-Egeghy-East Model: Concentration only
#
# Learn more by entering "?LorberEgeghyModel::LEEM_Concentration"
# into the console or running the line in R.

LEEM_Concentration <- function(data, wtcol, n, seed=NULL){

  # Testing
  # 1A. Convert all inputs to lowercase and check for column names.
  colnames(data)<- tolower(colnames(data))
  wtcol <- tolower(wtcol)

  # 1B. Check "data" for column names.
  requiredcols <- c("sample size","chemical","units",
                    "min","max","median","mean","sd","gm","gsd",
                    "p10","p25","p75","p90","p95","p99")
  if (sum(requiredcols %in% colnames(data)) != length(requiredcols)){
    mssng <- paste(str_c(setdiff(requiredcols,colnames(data))), collapse = ", ")
    stop(str_c("Missing data input column name(s): ",mssng ,
               ". Please rename or add column to data input dataframe."))
  }

  #1C. Make sure one media entered.
  if (length(unique(data$media)) > 1){
    stop(str_c("Only one media allowed at a time in LEEM_Concentration. The following media are listed in the input dataframe: ",
               paste(unique(data$media),collapse = ", "),"."))
  }



  # 2. Set seed if none specified.
  if(is.null(seed)) seed <- 12345
  set.seed(seed)

  # 3. Make sure Weighting Column is a numeric
  if(!is.numeric(data[,(wtcol)])){
    stop((str_c("Column '", wtcol, "' is not a numeric. Please convert or specify a different column.")))
  }


  #4. Units
  data<- data %>% dplyr::mutate(UNITFACTOR = case_when(
    (units %in% c("ng/m³","ng/L","ng/g","µg/kg","ug/kg","pg/mL","pg/ml","ng/m3")) ~ 1,
    (units %in% c("pg/m³","pg/g")) ~ 0.001,
    (units %in% c("ng/mL","ug/l","µg/L","ug/m³","µg/m³","ug/m3")) ~ 1000)) %>%
    mutate_at(c("min","max","median","mean","sd","gm","gsd","p10","p25","p75","p90","p95","p99"),~.*UNITFACTOR) %>%
    mutate(units = case_when(
      (units %in% c("ug/m3","µg/m³","pg/m³","ng/m³","ng/m3")) ~ "ng/m3",
      (units %in% c("ng/mL","ug/l","ug/L","µg/l","µg/L","pg/ml","pg/mL","ng/L")) ~ "ng/L",
      (units %in% c("pg/g","µg/kg","ug/kg","ng/g")) ~ "ng/g")) %>%
    select(-UNITFACTOR)

  #4A. Units test.
  if (length(unique(data$units)) > 1){
    stop(str_c("Different unit metrics for media. ",unique(data$media)," units are: ",
               paste(unique(data$units),collapse = ", "),". Please resolve to one metric unit in input."))
  }

  # 5. GM/GSD estimation sequence

  # 5A. Estimate gm using Pleil 1.
  data <- data %>% mutate(gm = if_else(!is.na(gm),gm , median))

  # 5B. Estimate gm using Pleil 2.
  data <- data %>% mutate(gm = if_else(!is.na(gm),gm , mean/(1+0.5 *(sd/mean)^2)))

  # 5C. Estimate gsd using Pleil 1.
  data <- data %>% mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p10/gm)/qnorm(.10)))) %>%
    mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p25/gm)/qnorm(.25)))) %>%
    mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p75/gm)/qnorm(.75)))) %>%
    mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p90/gm)/qnorm(.90)))) %>%
    mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p95/gm)/qnorm(.95)))) %>%
    mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p99/gm)/qnorm(.99))))

  # 5D. Estimate gsd using Pleil 2.
  data <- suppressWarnings(data %>% mutate(gsd = if_else(!is.na(gsd),gsd ,exp(sqrt(2 * log(mean/gm))))))
  # 5E. Estimate gm using Pleil 3.
  data <- data %>% mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(max/gm)/qnorm(1-1/`sample size`))))
  data <- data %>% mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(min/gm)/qnorm(1/`sample size`))))

  # 5F. Estimate mean and sd using "Estimating.datalsdata" Methods (5) and (16). mean calculated from min, median, maximum,
  # and sd from minimum, median, maximum, and range.

  data <- data %>% mutate(mean = if_else(!is.na(mean),mean, (min+2*median+max)/4))
  data <- data %>% mutate(sd = if_else(!is.na(sd),sd, sqrt ((1/12) * ((min-2*median+max)^2)/4 + (max-min)^2)))

  # 5G. Estimate sd using Ramirez & Cox Method and range rule. Applied only if weight  > 10.
  data <- data %>% mutate(sd = if_else((!is.na(sd) & `sample size` > 10),sd, (max-min)/ (3*sqrt(log(`sample size`))-1.5)))
  data <- data %>% mutate(sd = if_else((!is.na(sd) & `sample size` > 10),sd, (max-min)/4))

  # ______________________________ Repeat A - E. ______________________________ #


  # 5H. Estimate gm using Pleil 1.
  data <- data %>% mutate(gm = if_else(!is.na(gm),gm , median))

  # 5I. Estimate gm using Pleil 2.
  data <- data %>% mutate(gm = if_else(!is.na(gm),gm , mean/(1+0.5 *(sd/mean)^2)))

  # 5J. Estimate gsd using Pleil 1.
  data <- data %>% mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p10/gm)/qnorm(.10)))) %>%
    mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p25/gm)/qnorm(.25)))) %>%
    mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p75/gm)/qnorm(.75)))) %>%
    mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p90/gm)/qnorm(.90)))) %>%
    mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p95/gm)/qnorm(.95)))) %>%
    mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(p99/gm)/qnorm(.99))))

  # 5K. Estimate gsd using Pleil 2.
  data <- suppressWarnings(data %>% mutate(gsd = if_else(!is.na(gsd),gsd ,exp(sqrt(2 * log(mean/gm))))))

  # 5L. Estimate gm using Pleil 3.
  data <- data %>% mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(max/gm)/qnorm(1-1/`sample size`))))
  data <- data %>% mutate(gsd = if_else(!is.na(gsd),gsd ,exp(log(min/gm)/qnorm(1/`sample size`))))

  # 5M. Remove Infs.
  data$gm[is.infinite(data$gm)]<- NA
  data$gsd[is.infinite(data$gsd)]<- NA

  # 6. Wgm and Wgsd
  wgm  <- weighted.mean(data$gm[complete.cases(data$gsd,data$gm)],
                        data[,wtcol][complete.cases(data$gsd,data$gm)])
  wgsd <- weighted.mean(data$gsd[complete.cases(data$gsd,data$gm)],
                        data[,wtcol][complete.cases(data$gsd,data$gm)])

  # 7. Generate exposure and concentration curves
  Concentration<- rlnorm(n,log(wgm),abs(log(wgsd)))

  Concentration <- data.frame(Concentration)
  Concentration$Units <- names(table(data$units))


  # 8. Subset used and not used data
  used     <-  data[complete.cases(data$gsd,data$gm,data[,(wtcol)]),]
  notused  <-  data[!complete.cases(data$gsd,data$gm,data[,(wtcol)]),]


  # 9. Summarize output
  sumstats<- function(x){
    sumstats <- t(quantile(x,c(0,.10,.5,.75,.95,1)))
    mymean   <- mean(x)
    sumstats <- cbind(mymean,sumstats)
    colnames(sumstats)<-c("Mean","Min","10th%","Median","75th%","95th%","Max")
    sumstats<- signif(sumstats,5)
    return(sumstats)
  }
  summary <- sumstats(Concentration$Concentration)
  summary <- data.frame(summary)
  colnames(summary)<-c("Mean","Min","10th%","Median","75th%","95th","Max")

  # Count of datasets used

scount <- data.frame(t(table(used$chemical)))[2:3]
colnames(scount)<- c("Chemical","Stuides Used")

  # 10. Create Metadata
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
  filename  <- namer('LEM Concentration Results')


  metadata <- data.frame("Date" = filename,
                         "n" = n,
                         "Seed" = seed,
                         "Chemicals" = paste(unique(data$chemical),collapse = ", "))

  metadata[]<- lapply(metadata, as.character)

  # 11. Compile list of results
  finished <- list("Summary"   = summary,
                   "Data"      = used,
                   "Not Used"  = notused,
                   "Used Row Count" = scount,
                   "Raw"       = Concentration,
                   "Metadata"  = metadata)


  cat(str_c("LEEM-R Concentration complete."))
  return(finished)
}

