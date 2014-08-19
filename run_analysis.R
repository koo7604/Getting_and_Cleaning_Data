# 1. Merges the training and the test sets to create one data set.
setwd("/Users/koo7604/Desktop/R/Johns_Hopkins/GettingandCleaningData/week3/Getting_and_Cleaning_Data")

## get training data
x_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")
subject_train <- read.table("./train/subject_train.txt")
## get test data
x_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")
subject_test <- read.table("./test/subject_test.txt")
## combine
x_all <- rbind(x_train, x_test)
y_all <- rbind(y_train, y_test)
subject_all <- rbind(subject_train, subject_test)

## get common data
features <- read.table("./features.txt")
act_label <- read.table("activity_labels.txt")
names(act_label) <- c("act_label_num", "act_label_name")
act_label$act_label_name <- as.character(act_label$act_label_name)


colnames(allData)
featurenames <- as.character(features$V2)
names(x_all) <- c(featurenames)
names(y_all) <- "act_label_num"
names(subject_all) <- "subject"

allData <- cbind(subject_all, y_all, x_all)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
pickuplist <- grepl("-mean()", names(allData)) - grepl("-meanFreq()", names(allData)) + grepl("-std()", names(allData))

columnnames <- names(allData)
namelist <- c("subject", "act_label_num", "act_label_name", columnnames[pickuplist==1])

extractedData <- data.frame(allData$subject, allData$act_label_num, 0)

j = 4
for (i in 3:ncol(allData)){
        if (pickuplist[i] == 1){
                extractedData[,j] <- allData[,i]
                j <- j + 1
        }
}
colnames(extractedData) <- namelist ##assign column names

## 3. Uses descriptive activity names to name the activities in the data set
for (i in 1:nrow(extractedData)){
        extractedData$act_label_name[i] <- act_label$act_label_name[extractedData$act_label_num[i]]
}

# 4. Appropriately labels the data set with descriptive variable names.
## This has already done above. The dataset has appropriate names.

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
head(extractedData)

## calculate average values by subjects
secData1 <- aggregate(extractedData, by = list(extractedData$subject), FUN = mean)
secData1 <- cbind(secData1$Group.1, secData1[, 5:ncol(secData1)])
names(secData1) <- c("id(subject/activity)", names(secData1[2:ncol(secData1)])) 

## calculate average values by activity
secData2 <- aggregate(extractedData, by = list(extractedData$act_label_name), FUN = mean)
secData2 <- cbind(secData2$Group.1, secData2[, 5:ncol(secData2)])
names(secData2) <- c("id(subject/activity)", names(secData2[2:ncol(secData2)])) 

## combine
secData <- rbind(secData1, secData2)

## output
write.table(secData, file="./tidydataset.txt", row.name=FALSE)

