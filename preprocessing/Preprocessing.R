preprocessing<-function(dat){
  
#source("~/INSE6180_Project/preprocessing/RandomForestFeatureSelection.R")
#dat<-featureSelection(dat)
  
# to change nominal to bernouli
# add attribute on keeps,
# add attribute on newframe<- ,with(dat,model.matrix(~that attribute))
# add attribute on drop2

# Cleaning up the unused column on data
keeps <- c("click","C1","banner_pos","site_domain","site_category","device_type","C14","C15","C16","C17","C18","C19","C20","C21")

dat<-dat[ , (names(dat) %in% keeps)]

newframe<-as.data.frame(cbind(with(dat,model.matrix(~site_domain)),with(dat, model.matrix(~site_category)),with(dat,model.matrix(~device_type))))
drop2 <- c("(Intercept)","site_domain","site_category","device_type")
dat<-dat[ , !(names(dat) %in% drop2)]
newframe<-newframe[ , !(names(newframe) %in% drop2)]
dat<-cbind(dat,newframe)


#drops <- c("site_domain","site_category","device_type","id","hour","site_id","app_id","app_domain","app_category","device_id","device_ip","device_model","device_conn_type")
#dat<-dat[ , !(names(dat) %in% drops)]

# Making dependent variable factor and label values
dat$click <- as.factor(dat$click)
dat$click <- factor(dat$click,levels = c("0","1"),labels = c("No", "Yes"))


sampling_attr<-c("C1","C14","C15","C16","C17","C18","C19","C20","C21")

dat[,sampling_attr] <- as.data.frame(sapply(dat[,sampling_attr], function(i) if(is.numeric(i)) scale(i) else i))

#dat <- data.frame(lapply(dat, as.character), stringsAsFactors=FALSE)
#dat <- data.frame(lapply(dat, as.numeric))

#dat[is.na(dat)] <- 0
return(dat)
}