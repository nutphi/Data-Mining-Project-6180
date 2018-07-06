install.packages('Boruta')
library(Boruta)

featureSelection<-function(dat){
  
# Cleaning up the data and only use the 24 columns
#drops <- c("site_id","site_domain","site_category","app_id","app_domain","app_category","device_id","device_ip","device_model",
#           "site_id_dec","site_domain_dec","site_category_dec","app_id_dec","app_domain_dec","app_category_dec","device_id_dec","device_ip_dec","device_model_dec")
#dat<-dat[ , !(names(dat) %in% drops)]
dat <- data.frame(lapply(dat, as.character), stringsAsFactors=FALSE)
dat <- data.frame(lapply(dat, as.numeric))

dat[is.na(dat)] <- 0


#start implementing random forest 
#Kursa M., Rudnicki W. (2010), Feature Selection with the Boruta Package, Journal of Statistical Software, 36(11), 1 - 13
set.seed(1)
label <- dat$click


#use Boruta algorithm to get feature selection
#If you want to track the progress of the algorithm, specify doTrace = 1
feature.selection <- Boruta(click ~ ., data = dat, doTrace = 1)

feature.selection$timeTaken
table(feature.selection$finalDecision)
fNames <- getSelectedAttributes(feature.selection) # withTentative = TRUE
fNames

dat <- dat[, fNames]
dat$click<-label
dim(dat)


# Making dependent variable factor and label values
dat$click <- as.factor(dat$click)
dat$click <- factor(dat$click,levels = c("0","1"),labels = c("No", "Yes"))

return(dat)
}