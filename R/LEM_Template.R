# Lorber Egeghy Model: Template Generator
#
# Learn more by entering "?LorberEgeghyModel::LEM_Template"
# into the console or running the line in R.


LEM_Template<- function(){

tnames <- c("Chemical","Weight","Media","Min","Max","Median","Mean","SD",
  "GM","GSD","P10","P25","P75","P90","P95","P99")

Template <- data.frame(cbind(
matrix("A string",nrow = 3, ncol = 1),
matrix("A numeric",nrow = 3, ncol = 1),
c("String here must be detected in Factors sheet. Examples Below.","Dust","Water"),
matrix("A numeric",nrow = 3, ncol = length(tnames)-3)))


fnames<- c("Individual","Media","Path","Factor","Value")

Factors <- data.frame(t(c("Name of group (adults, women, children, etc.)","Media column to match data input sheet (e.g. Dust)",
                          "Grouping of factors (e.g. Ingestion)","Name of one of many variables used to transform concentration to a result. (e.g. Inhalation Rate)",
                          "A numeric")))

colnames(Template)<- tnames
colnames(Factors)<-fnames

Template$Other <- "Add any number of identifying columns. (citation, location, etc.). These will be carried to results."

Template<-list("Template" = Template,
               "Factors"  = Factors)
export(Template,"LEM Template.xlsx")

return(cat("Template Generated. Check your working directory for the LEM_Template.xlsx file."))
}


LEM_Template()
