#Course Project 
#Written by Liz
#9/26/2015

#data link:
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

#This code will perform the following analysis: 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject.



rm(list=ls())

library(reshape2)

#download and unzip the dataset

url="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f=file.path(getwd(), "da.zip")
download.file(url, f)

unzip(f)

#1. merge the training and test sets

#read data from the unzipped file
activity.labels=read.table("UCI HAR Dataset/activity_labels.txt")
features=read.table("UCI HAR Dataset/features.txt")

names(activity.labels)=c("activityID", "activityName")
features[,2]=as.character(features[,2])

#test set
subject.test=read.table("UCI HAR Dataset/test/subject_test.txt")
x.test=read.table("UCI HAR Dataset/test/X_test.txt")
y.test=read.table("UCI HAR Dataset/test/y_test.txt")

#assign column names to each test dataset
names(subject.test)="subjectID"
names(x.test)=features[,2]
names(y.test)="activityID"


#training sets
subject.train=read.table("UCI HAR Dataset/train/subject_train.txt")
x.train=read.table("UCI HAR Dataset/train/X_train.txt")
y.train=read.table("UCI HAR Dataset/train/y_train.txt")

#assign column names to each train dataset
names(subject.train)="subjectID"
names(x.train)=features[,2]
names(y.train)="activityID"


#merge test sets
test=cbind(subject.test, x.test, y.test)

#merge training sets
train=cbind(subject.train, x.train, y.train)

#merge test and train
da=rbind(train, test)

#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
names.index=c(grep("subjectID" , names(da)), 
              grep("activityID", names(da)), 
              grep(".*mean.*", names(da)), 
              grep(".*std.*", names(da)))
final.data=da[, names.index]

#3. Uses descriptive activity names to name the activities in the data set
#merge with the table activity.lables, which includes the descriptive activity name 
final.data=merge(final.data, activity.labels, by="activityID", all.x=T)


#4. Appropriately labels the data set with descriptive variable names. 
label=names(final.data)
label=gsub("-mean", "Mean", label)
label=gsub("-std", "StdDev", label)
label=gsub("^t", "Time", label)
label=gsub("^f", "Freq", label)

names(final.data)=label


#5. Create a tidy data set with the average of each variable for each activity and each subject
da.noActivityName=subset(final.data, select=(names(final.data)!="activityName"))
da.noActivityName$activityID=as.factor(da.noActivityName$activityID)
da.noActivityName$subjectID=as.factor(da.noActivityName$subjectID)

da.melted=melt(da.noActivityName, id=c("activityID", "subjectID"))
da.mean=dcast(da.melted, activityID+subjectID ~ variable, mean)

write.table(da.mean, "tidy.txt", row.names=F)

