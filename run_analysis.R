#install useful packages
#install.packages("data.table")
#install.packages("reshape2")
#install.packages("XLConnect")
#install.packages("XLConnectJars")

library(data.table)
library(reshape2)
library(plyr)
#library(XLConnect)
#library(XLConnectJars)

url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#zip<-"data_zip.zip"
#download.file(url, zip)

#unzip
#unzip(zip, list = FALSE, overwrite = TRUE)

#sets the path for the job
path <- getwd()
location <- file.path(path, "UCI HAR Dataset")

#Read the files
TrainSubject <- read.table(file.path(location, "train", "subject_train.txt"))
TestSubject <- read.table(file.path(location, "test", "subject_test.txt"))

#Read the activity files
TrainActivity <- read.table(file.path(location, "train", "Y_train.txt"))
TestActivity <- read.table(file.path(location, "test", "Y_test.txt"))

TrainData <- read.table(file.path(location, "train", "X_train.txt"))
TestData <- read.table(file.path(location, "test", "X_test.txt"))

#Merge files
Subject <- rbind(TrainSubject, TestSubject)
setnames(Subject, "V1", "subject")
Activity <- rbind(TrainActivity, TestActivity)
setnames(Activity, "V1", "activityNum")
MergedData <- rbind(TrainData, TestData)

Subject <- cbind(Subject, Activity)
MergedData <- cbind(Subject, MergedData)
print(head(MergedData))
#setkey(MergedData, subject, activityNum)

#mean and standard deviation
Fe <- fread(file.path(location, "features.txt"))
setnames(Fe, names(Fe), c("featureNum", "featureName"))
Fe <- Fe[grepl("mean\\(\\)|std\\(\\)", featureName)]
Fe$featureCode <- Fe[, paste0("V", featureNum)]

select <- c(key(MergedData), Fe$featureCode)
MergedData <- MergedData[, select]

saveresults <- function (data,name){
  print(paste("saving", name))
  file <- paste(location, "/", name,"txt" ,sep="")
  write.table(data,file)
}

saveresults(MergedData,"mean_and_std")

#3) Uses descriptive activity names to name the activities in the data set
ActiNames <- fread(file.path(location, "activity_labels.txt"))
setnames(ActiNames, names(ActiNames), c("activityNum", "activityName"))
print(ActiNames)

MergedData <- merge(MergedData, ActiNames, all = TRUE)

#Step 4: "Appropriately labels the data set with descriptive activity names." 
Fe$featureName = gsub("\\(\\)", "", Fe$featureName)
Fe$featureName = gsub("-", ".", Fe$featureName)
for (i in 1:length(Fe$featureName)) {
  colnames(MergedData)[i + 2] <- Fe$featureName[i]
}

#write.table(file="raw.txt",x=MergedData,row.names=F)
#Step 5: "Creates a second, independent tidy data set with the average of each variable for each activity and each subject."
melted <- melt(MergedData, id=c("V1","V2"))
tidy <- dcast(melted, V1+V2 ~ variable, mean)


write.table(file="tidy.txt",x=tidy,row.names=F)


