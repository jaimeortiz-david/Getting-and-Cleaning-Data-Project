library(data.table)
library(reshape2)
path <- getwd()
path

#Download file

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipdata <- "Dataset.zip"
if (!file.exists(path)) {dir.create(path)}
download.file(url, file.path(path, zipdata))

unzip("Dataset.zip")

# Read files

pathdata <- file.path(path, "UCI HAR Dataset")
list.files(pathdata)

#read subject files
subject.train <- fread(file.path(pathdata, "train", "subject_train.txt"))
subject.test  <- fread(file.path(pathdata, "test" , "subject_test.txt" ))

#read activity files
activity.train <- fread(file.path(pathdata, "train", "Y_train.txt"))
activity.test  <- fread(file.path(pathdata, "test" , "Y_test.txt" ))

# read data files
train.data <- fread(file.path(pathdata,"train", "X_train.txt"))
test.data <- fread(file.path(pathdata, "test", "X_test.txt"))

# Merging training and test sets

subject.data <- rbind(subject.train, subject.test)
setnames(subject.data, "V1", "subject")
activity.data <- rbind(activity.train, activity.test)
setnames(activity.data, "V1", "activityNumber")
full.data <- rbind(train.data, test.data)

# merge columns

subject.data <- cbind(subject.data, activity.data)
full.data <- cbind(subject.data, full.data)
#set key
setkey(full.data, subject, activityNumber)

#Extract the information: only the mean and standard deviation 
#read the features.txt to know which variables correspond to mean and sd

features <- fread(file.path(pathdata, "features.txt"))
setnames(features, names(features), c("featureNumber", "featureName"))

#get the measurements fro mean and sd

features <- features[grepl("mean\\(\\)|std\\(\\)", featureName)]

# convert the column numbers to a vector of variable names matching columns in full.data

features$featureCode <- features[, paste0("V", featureNumber)]
head(features)
features$featureCode

#subset variables using variable names

a <- c(key(full.data), features$featureCode)
full.data <- full.data[, a, with=FALSE]

#Add descriptive activity names
#Use activity_labels.txt to add descriptive names to the activities

activity.names <- fread(file.path(pathdata, "activity_labels.txt"))
setnames(activity.names, names(activity.names), c("activityNumber", "activityName"))

#merge activity labels
full.data <- merge(full.data, activity.names, by="activityNumber", all.x=TRUE)

setkey(full.data, subject, activityNumber, activityName)

#reshape data table to a tall and narrow format
full.data <- data.table(melt(full.data, key(full.data), variable.name="featureCode"))

#Merge activity name

full.data <- merge(full.data, features[, list(featureNumber, featureCode, featureName)], by="featureCode", all.x=TRUE)

#Create factor class variables for activityName and featureName
full.data$activity <- factor(full.data$activityName)
full.data$feature <- factor(full.data$featureName)

# separate features from featureName

getthis <- function (regex) {
        grepl(regex, full.data$feature)
}
## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(getthis("^t"), getthis("^f")), ncol=nrow(y))
full.data$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))

x <- matrix(c(getthis("Acc"), getthis("Gyro")), ncol=nrow(y))
full.data$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(getthis("BodyAcc"), getthis("GravityAcc")), ncol=nrow(y))
full.data$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(getthis("mean()"), getthis("std()")), ncol=nrow(y))
full.data$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
## Features with 1 category
full.data$featJerk <- factor(getthis("Jerk"), labels=c(NA, "Jerk"))
full.data$featMagnitude <- factor(getthis("Mag"), labels=c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(getthis("-X"), getthis("-Y"), getthis("-Z")), ncol=nrow(y))
full.data$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

#check results to amke sure all combinations of the factor class are accounted for
r1 <- nrow(full.data[, .N, by=c("feature")])
r2 <- nrow(full.data[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2

#Tidy dataset
setkey(full.data, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
full.tidy <- full.data[, list(count = .N, average = mean(value)), by=key(full.data)]

#Codebook
knit("codebookJO.Rmd", output="codebook.md", encoding="ISO8859-1", quiet=TRUE)
markdownToHTML("codebook.md", "codebook.html")








