

## Q1
# activity label: WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING

#'train/X_train.txt': Training set.
#'train/y_train.txt': Training labels.
#'test/X_test.txt': Test set.
#'test/y_test.txt': Test labels.

labels <- read.table("./UCI HAR Dataset/activity_labels.txt") # 6 by 2
features <- read.table("./UCI HAR Dataset/features.txt")    # 561 by 2    see features_info.txt for interpretations.

trainX <- read.table("./UCI HAR Dataset/train/X_train.txt")             # 7352 by 561 
trainY <- read.table("./UCI HAR Dataset/train/Y_train.txt")             # 7352 by   1
subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt") # 7352 by   1
train <- cbind(trainY, subjectTrain ,trainX)                            # 7352 by 563

testX <- read.table("./UCI HAR Dataset/test/X_test.txt")                # 2947 by 561
testY <- read.table("./UCI HAR Dataset/test/Y_test.txt")                # 2947 by   1
subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")    # 2947 by   1
test <- cbind(testY, subjectTest, testX)                                # 2947 by 563 

one <- rbind(train, test)  # 10299 by  563

## Q2
features$char <- levels(features[, 2])[features[, 2]]  # factor to char, used next for naming. 
names(one) <- c("label", "id", features$char)  # names    
 
valid_column_names <- make.names(names=names(one), unique=TRUE, allow_ = TRUE) # prepare valid names for later use
names(one) <- valid_column_names 

select_mean_std <- select(one, contains("mean.."), contains("std.."))  # 10299 by  67
select_mean_std <- select_mean_std[, -34] # remove "angle.tBodyAccJerkMean..gravityMean."   # 10299 by  66 = 2*(3*8 + 9), see features.info



## Q3
select_mean_std$label <- one$label # label as integers 

newLabels <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
select_mean_std$labelChar <- newLabels[select_mean_std$label]  # create column of descriptive activity names

## Q4
# done in Q2 names(one) <- c("label", "id", features$char) 


## Q5
select_mean_std$subject <- c(subjectTrain[, 1], subjectTest[, 1]) # create subject id column # note "[, 1]", even though they only have one column

tidy <- matrix(nrow = 180, ncol = 66)  # 6 labels * 30 subjects = 180 categories #  66 variables obtained in Q2 
for (i in 1:66) {
	tidy[, i] <- tapply(select_mean_std[, i], INDEX = list(select_mean_std$label, select_mean_std$subject), mean) 
}

tidy <- as.data.frame(tidy)
names(tidy) <- names(select_mean_std)[1:66]

write.table(tidy, file = "tidy.txt", row.name=FALSE)


