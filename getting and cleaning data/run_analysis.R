run_analysis <- function(){
  
  ## Reading data
        
        test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
        test_x <- read.table("./UCI HAR Dataset/test/X_test.txt")
        test_y <- read.table("./UCI HAR Dataset/test/Y_test.txt")
        train_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
        train_x <- read.table("./UCI HAR Dataset/train/X_train.txt")
        train_y <- read.table("./UCI HAR Dataset/train/Y_train.txt")
        activity <- read.table("./UCI HAR Dataset/activity_labels.txt")
        features <- read.table("./UCI HAR Dataset/features.txt")
 
        ## Name columns
        features_label <- features[,2]
        colnames(test_x) <- features_label
        colnames(train_x) <- features_label
        
        ## Select measurements on the mean and standard deviation
        col_select <- grep("*mean\\(|*std\\(", features_label)
        features_label_select <- features_label[col_select]
        features_label_select <- sub("\\(\\)", "", features_label_select)
        
        test_select <- test_x[, col_select]
        train_select <- train_x[, col_select]
        
        ## add activity and subject columns to the data sets
        test_activity <- cbind(test_y, test_select)
        test_activity_subject <- cbind(test_subject, test_activity)
        
        train_activity <- cbind(train_y, train_select)
        train_activity_subject <- cbind(train_subject, train_activity)
        
        
        ## combine both test and training data
        dat <- rbind(test_activity_subject, train_activity_subject)
        colnames(dat) <- c("subject", "activity", features_label_select)
        
        library(dplyr)
        data <- arrange(dat, subject, activity)

        ## insert descriptive activity names        
        activity_label <- gsub("_", " ", activity[,2])
        for (i in 1:6) {
                data$activity <- sub(i, activity_label[i], data$activity)
                }

        ## 2. part (step 5)   

        
        result_all <- NULL
        activity_name <- sort(activity_label)
        
        ## separate datasets for each subject and calculate the mean for each 
        ## activity and variable 
        for (i in 1:30) {
          subject_data <- filter(data, subject == i)
          activity_data <- split(subject_data, subject_data$activity)
          
          activity_means <- lapply(activity_data, function(x) colMeans(x[, features_label_select]))
          
          result <- as.data.frame(activity_means)
          result_trans <- as.data.frame(t(as.matrix(result)))
          
          sub <- c(i, i, i, i, i, i)
          
          # result_trans <- cbind(sub, activity_label, result_trans)
          
          ## combine subject, activity and calculated means in a single data frame, 
          ## added for each subject
          result_trans <- cbind(sub, activity_name, result_trans)
          coln <- paste("mean(", features_label_select, ")", sep = "")
          colnames(result_trans) <- c("subject", "activity", coln)
          
          result_all <- rbind(result_all, result_trans)
        
        }
        
        ## write output file
        write.table(result_all, file = "average.txt", row.names = FALSE)
        
}