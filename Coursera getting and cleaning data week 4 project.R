
        ##INSTALL PACKAGES

install.packages(dplyr)
library(dplyr)
install.packages("stringr")
library(stringr)


        ##IMPORT DATA


        ##set working directory to location of folder containing feature and label data
setwd("C:/Users/azuranewman/Documents/R/Coursera/Getting and Cleaning Data/week 4 project/Dataset/UCI HAR Dataset")

        ##read in actlabel data to get activity descriptions to use to identify activity on each row.
actlabels<-read.table("activity_labels.txt")
        ##read in features data to use to label the columns in the xtet and xtrain tables.
features<-read.table("features.txt")


setwd("C:/Users/azuranewman/Documents/R/Coursera/Getting and Cleaning Data/week 4 project/Dataset/UCI HAR Dataset/test")

        ##read in subjecttest data to identify test participants
subjecttest<-read.table("subject_test.txt")
xtest<-read.table("x_test.txt")
ytest<-read.table("y_test.txt")

        ##setting working directory to train folder to get train data
setwd("C:/Users/azuranewman/Documents/R/Coursera/Getting and Cleaning Data/week 4 project/Dataset/UCI HAR Dataset/train")

        ##read in subjecttrain data to identify train participants
subjecttrain<-read.table("subject_train.txt")

xtrain<-read.table("x_train.txt")
ytrain<-read.table("y_train.txt")


        ##MERGE DATA

##merging test and training data together
xall<-rbind(xtest,xtrain)
yall<-rbind(ytest,ytrain)
subjectsall<-rbind(subjecttest,subjecttrain)


        ##CLEAN UP DATA

##fixing xall names to match values from the feature data.
featurenames<-features$V2
names(xall)<-featurenames

##selecting columns that contain mean or std
xcols<-xall[,grepl("(.+)mean\\(\\)(.+)|(.+)std(.+)",names(xall))==TRUE]

##creating flag to indicate whether a subject is in the test or train group in case it is needed later.
groupindicator<-c(rep("test",nrow(xtest)),rep("train",nrow(xtrain)))
xcols<-cbind(groupindicator,xcols)

##creating name for subject data frame.
names(subjectsall)<-"subject"

##merge yall with act labels to get activity descriptions and name columns.
yallwithlabels<-merge(yall,actlabels)
names(yallwithlabels)<-c("activitykey","activity")

##cbind subjectsall and yallwithlabels$activity with xall to get subject id, activity, and results into one table.
results<-cbind(subjectsall,yallwithlabels,xcols)

##changing class of subject column to factor as we do not want it treated as an integer data type.
results$subject<-as.factor(results$subject)

##creating results2 table to remove activity key as it is the final unnecessary column.
##keeping results table for data checking and to speed up rerunning the code.
results2<-select(results,-activitykey)

##creating descriptive names for results2 data frame
names(results2)<-gsub("[-()]","",names(results2))
names(results2)<-gsub("^t","timedomain",names(results2))
names(results2)<-gsub("^f","frequencydomain",names(results2))
names(results2)<-gsub("Body","bodycomponent",names(results2))
names(results2)<-gsub("Gravity","gravitycomponent",names(results2))
names(results2)<-gsub("Acc","acceleration",names(results2))
names(results2)<-gsub("Gryo","gyroscope",names(results2))
names(results2)<-gsub("Jerk","jerk",names(results2))

##creating key out of subject id and activity for easier use of tapply.
results2<-mutate(results2,key=paste(subject,activity,sep="_"))

##results 2 table is considered the output for part 4 of the assignment


        ##setting up final output

##using loop combined with tapply to get mean calculations for all key values.
output<-data.frame(row.names=c("subjectid_activity","feature","mean"))
for(i in 4:60){
        test<-tapply(results2[,i],results2$key,mean)
        for(j in 1:length(test)){
                test1<-cbind(testnames[j],names(results2)[i],test[[j]])
                output<-rbind(output,test1)
        }
}

##setting names for output columns
names(output)<-c("subjectid_activity","feature","mean")

##separating the subject id and activity info back into two columns
output<-mutate(output,subject_id=str_split_fixed(output$subjectid_activity,"_",2)[,1])
output<-mutate(output,activity=str_split_fixed(output$subjectid_activity,"_",2)[,2])
output<-select(output,2:5)

##output is considered the results for part 5 of the assignment
