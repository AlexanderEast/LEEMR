# Lorber Egeghy Model: Template Generator
#
# Learn more by entering "?LorberEgeghyModel::LEM_Template"
# into the console or running the line in R.
library('rio')
rm(list=ls())

LEM_Template<- function(){

tnames <- c("Chemical","Weight","Media","Min","Max","Median","Mean","SD",
  "GM","GSD","P10","P25","P75","P90","P95","P99")

Template <- data.frame(cbind(
matrix("enter string here",nrow = 3, ncol = 1),
matrix("enter numeric here",nrow = 3, ncol = 1),
c("string here must be detected in Factors sheet. Examples Below.","Indoor Air","Water"),
matrix("enter numeric here",nrow = 3, ncol = length(tnames)-3)))

fnames<- c("Individual","n","Indoor Air: Inhalation Rate",
           "Indoor Air: Time spent indoors (h)",
           "Indoor Air: Absorption Factor",
           "Water: Intake per day (L/day)","Water AF")
Factors <- data.frame(t(c("Name of group (adults, women, children, etc.)","number of exposures generated",
  rep("Create any number of exposure factor columns. column name must be detected in Media column in data sheet.",5))))

colnames(Template)<- tnames
colnames(Factors)<-fnames

Template$Other <- "Add any number of identifying columns. (citation, location, etc.)"

Template<-list("Template" = Template,
               "Factors"  = Factors)
export(Template,"LEM_Template.xlsx")

return(cat("Template Generated. Check your working directory for the LEM_Template.xlsx file."))
}

LEM_Template()


