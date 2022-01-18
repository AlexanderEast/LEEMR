# Lorber-Egeghy-East-Model: Concentration Summary Statistics
# to Exposure Distributions
#
# Learn more by entering "?LorberEgeghyModel::LEEM"
# into the console or running the line in R.


LEEM <- function(data,factors,wtcol,n,seed = NULL){

  # 1A. Convert all inputs to lowercase and check for column names.
  colnames(data)<- tolower(colnames(data))
  colnames(factors) <-tolower(colnames(factors))
  wtcol<-tolower(wtcol)

  # 1B. Check "data" for column names.
  requiredcols <- c("sample size","media","chemical","units",
                    "min","max","median","mean","sd","gm","gsd",
                    "p10","p25","p75","p90","p95","p99")
  if (sum(requiredcols %in% colnames(data)) != length(requiredcols)){
    mssng <- paste(str_c(setdiff(requiredcols,colnames(data))), collapse = ", ")
    stop(str_c("Missing data input column name(s): ",mssng ,
          ". Please rename or add column to data input dataframe."))
  }

  # 1C. Check "factors" for column names.
  requiredcols <- c("path","media","individual","factor")
  if (sum(requiredcols %in% colnames(factors)) != length(requiredcols)){
    mssng <- paste(str_c(setdiff(requiredcols,colnames(factors))), collapse = ", ")
    stop(str_c("Missing factors input column name(s): ",mssng ,
               ". Please rename or add column to factors input dataframe."))
  }

  # 2. Set seed if none specified.
  if(is.null(seed)) seed <- 12345
  set.seed(seed)

  # Check Inputs
  # 3A. Detect Weighting Column in Data
  if (!tolower(wtcol) %in% tolower(colnames(data))){
    stop((str_c("Column '", wtcol, "' not detected in data column names.")))
  }

  # 3B. Make sure Weighting Column is a numeric
  if(!is.numeric(data[,(wtcol)])){
    stop((str_c("Column '", wtcol, "' is not a numeric. Please convert or specify a different column.")))
  }

  # 3C. Detect Factors for all media
  if (length(setdiff(unique(data$media), unique(factors$media))) > 0){
    stop((str_c("Media '", setdiff(unique(data$media), unique(factors$media)),
                "' not detected in factors media column.")))
  }

  #- ID unique individuals for step # 15.
  id <- factors[c("individual","media","path")]

  # 4. Units to ng/m3,ng/L,or ng/g
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

  # 5. GM/GSD Estimation sequence

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


  # 6. Split by media and chemical
  md <- split(data,list(data$media,data$chemical),drop = TRUE)


  # 7. Estimate weighted geometric mean and weighted geometric standard deviation
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

  # 8. Create concentration curves
  distributions <- function(x){
    set.seed(seed)
    Concentration<- rlnorm(n,log(x$wgm),abs(log(x$wgsd)))
    Concentration <- data.frame(Concentration)
    return(Concentration)
  }
  conc <- lapply(md,distributions)

  # Add names and remove NAs.
  conc <- bind_rows(conc, .id = "Media Chemical")
  conc <- conc[!is.na(conc$Concentration),]

  # 9. Load Exposure Factors
  factors <-split(factors,list(factors$individual,factors$media,factors$path),drop = TRUE)


  exposurefactors<- function(x){

    myname <- str_c(unique(x$individual)," ",
                    unique(x$media)," ",
                    unique(x$path))

    y <- data.frame(prod(x$factor),unique(x$media))
    colnames(y)<- c(myname,"Media")

    return(y)
  }

  factors<-lapply(factors,exposurefactors)


  # 10. Apply Exposure Factors

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


  # 11. Summarize Exposure Data
  thesummary <- split(result,result$name)

bind_rows(md)
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
  thesummary$WeightedBy <- wtcol


  colnames(result) <- c("Scenario","Concentration","Exposure")


  # 12. Subset used and not used data
  used     <-  data[complete.cases(data$gsd,data$gm,data[,(wtcol)]),]
  notused  <-  data[!complete.cases(data$gsd,data$gm,data[,(wtcol)]),]


  # 13. Convert classes and apply 4 significant figures

  # 13A. Raw
  result$Concentration <- signif(as.numeric(as.character(result$Concentration)),4)
  result$Exposure <- signif(as.numeric(as.character(result$Exposure)),4)

  # 13B. Summary
  thesummary[c("Min","10th%","Median","75th%","95th%","Max")]<-
    lapply(thesummary[c("Min","10th%","Median","75th%","95th%","Max")],as.character)

  thesummary[c("Min","10th%","Median","75th%","95th%","Max")]<-
    lapply(thesummary[c("Min","10th%","Median","75th%","95th%","Max")],as.numeric)

  thesummary[c("Min","10th%","Median","75th%","95th%","Max")]<-
    signif(thesummary[c("Min","10th%","Median","75th%","95th%","Max")],4)

  # 14. Count of datasets used

  counted <- split(used, list(used$chemical))
  names(counted)

  numberofstudies <- function(x){

  scount<-data.frame(t(c(unique(x$chemical),table(x$media))))
  colnames(scount) <-  c("Chemical",names(table((x$media))))

  return(scount)
  }

  scount <- bind_rows(lapply(counted,numberofstudies))


  # 15. Parse "Scenario" column for raw data and summary


  # 15A. Construct Legend ID
  unique(id)
  unic <- unique(used$chemical)

  chemname <- function(x){
  return(cbind(unique(id),"chemical" = x))
  }

  id <- bind_rows(lapply(unic,chemname))
  id$Scenario <- str_c(id$individual," ",
                         id$media," ",
                         id$path," ",
                         id$chemical)
  colnames(id)<- c("Individual","Media","Path","Chemical","Scenario")


  # 15B. Merge and tidy results and summary
  result     <- merge(id,result)
  thesummary <- merge(id,thesummary)

  thesummary <-thesummary[!names(thesummary) %in% c("Scenario")]
  result     <- result[!names(result) %in% c("Scenario")]


  # 16. Create metadata
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
  filename  <- namer('LEM Results')


  metadata <- data.frame("Date" = filename,
  "n" = n,
  "Seed" = seed,
  "Chemicals" = paste(unique(data$chemical),collapse = ", "),
  "Media" = paste(unique(data$media),collapse = ", "))

  metadata[]<- lapply(metadata, as.character)

  # 17. Compile list of results
  finished <- list("Summary"   = thesummary,
                   "Data"      = used,
                   "Not Used"  = notused,
                   "Used Row Count" = scount,
                   "Raw"       = result,
                   "Metadata"  = metadata)


  cat(str_c("LEEM-R Complete."))

  return(finished)
}



