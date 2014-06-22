#The purpose of this project is to demonstrate your ability to collect, work with, 
#and clean a data set. The goal is to prepare tidy data that can be used for later 
#analysis. You will be graded by your peers on a series of yes/no questions related 
#to the project. You will be required to submit: 1) a tidy data set as described 
#below, 2) a link to a Github repository with your script for performing the analysis,
#and 3) a code book that describes the variables, the data, and any transformations 
#or work that you performed to clean up the data called CodeBook.md. 
#You should also include a README.md in the repo with your scripts. This repo explains
#how all of the scripts work and how they are connected.  
#
#One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 
#        
#        http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
#
#Here are the data for the project: 
#        
#        https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#
#You should create one R script called run_analysis.R that does the following. 
#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement. 
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names. 
#Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 


#C:/Users/Harold/Documents/Coursera/Data Science/Specialization in Data Science/Getting and Cleaning Data/project

#setwd("C:/Users/Harold/Documents/Coursera/Data Science/Specialization in Data Science/Getting and Cleaning Data/project")


# 1.Merges the training and the test sets to create one data set.


training_x = read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
testing_x = read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
X <- rbind(training_x, testing_x)

training_y = read.csv("UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE)
testing_y = read.csv("UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE)
Y <- rbind(training_y, testing_y)

# Add the column name to Y data
names(Y) <- "activity"

training_s = read.csv("UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)
testing_s = read.csv("UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)
S <- rbind(training_s, testing_s)

# Add the column name to Subject data
names(S) <- "subject"


# Combine all data sets 
TotalData=cbind(X,Y,S)



# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

features = read.csv("UCI HAR Dataset/features.txt", sep="", header=FALSE)
names(features) <- c('id', 'name')

meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])

X<-X[,meanStdIndices]
names(X) <- features[meanStdIndices, 2]

# Add the column names to X data
names(X) <- gsub("\\(|\\)", "", names(X)) #Clean the names

#3. Uses descriptive activity names to name the activities in the data set

activityLabels = read.csv("UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)
names(activityLabels) <- c('id', 'name')

activityLabels[, 2] =gsub("_", "", tolower(as.character(activityLabels[, 2])))

Y[,1] = activityLabels[Y[,1], 2]


# 4. Appropriately labels the data set with descriptive activity names.

# Combine all data
DataCleaned <- cbind(S,Y,X)

# write file to output
write.table(DataCleaned, "DataCleaned.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable 
# for each activity and each subject.

uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activityLabels[,1])
numCols = dim(DataCleaned)[2]
OutputFile = DataCleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
        for (a in 1:numActivities) {
                OutputFile[row, 1] = uniqueSubjects[s]
                OutputFile[row, 2] = activityLabels[a, 2]
                tmp <- DataCleaned[DataCleaned$subject==s & DataCleaned$activity==activityLabels[a, 2], ]
                OutputFile[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
                row = row+1
        }
}

write.table(OutputFile, "DataCelaned.txt")
