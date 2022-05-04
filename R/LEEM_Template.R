# Lorber Egeghy Model: Template Generator
#
# Learn more by entering "?LorberEgeghyModel::LEEM_Template"
# into the console or running the line in R.

LEEM_Template<- function(){

tnames <- c("Chemical","Weight","Media","Min","Max","Median","Mean","SD",
  "GM","GSD","P10","P25","P75","P90","P95","P99")
fnames<- c("Individual","Media","Path","Factor","Value")
anames <- c("Chemical","Media","Path","Absorption")

Template <- data.frame(cbind(
matrix("A string",nrow = 3, ncol = 1),
matrix("A numeric",nrow = 3, ncol = 1),
c("String here must be detected in Factors sheet. Examples Below.","Dust","Water"),
matrix("A numeric",nrow = 3, ncol = length(tnames)-3)))

Factors <- data.frame(t(c("Name of group (adults, women, children, etc.)","Media column to match data input sheet (e.g. Dust)",
                          "Grouping of factors (e.g. Ingestion)","Name of one of many variables used to transform concentration to a result. (e.g. Inhalation Rate)",
                          "A numeric")))


Absorption <- data.frame(t(c("Chemical Name","Media column to match data input sheet (e.g. Dust)","Pathway concurrent with factors sheet",
                             "The absorption fraction for this chemical/media/pathway.")))

colnames(Template)   <- tnames
colnames(Factors)    <-fnames
colnames(Absorption) <- anames

Template$Other   <- "Add any number of identifying columns. (citation, location, etc.). These will be carried to results."
Factors$Other    <- "Add any number of columns (citation, notes, etc.)."
Absorption$Other <- "Add any number of columns (citation, notes, etc.)."

Template<-list("Data Template"   = Template,
               "Factors"    = Factors,
               "Absorption" = Absorption)

cat("LEEM-R Template Generated.")
return(Template)
}


