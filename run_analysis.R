setwd("~/GitHub/Getting_and_Cleaning_Data_Course_Project")

#Merges the training and the test sets
test <- read.table("./UCI HAR Dataset/test/X_test.txt")
train <- read.table("./UCI HAR Dataset/train/X_train.txt")
record <- rbind(train,test)
rm(test,train)

stest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
strain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
subject <- rbind(strain,stest)
rm(stest,strain)

atest <- read.table("./UCI HAR Dataset/test/y_test.txt")
atrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
activity <- rbind(atrain,atest)
rm(atest,atrain)

#Extracts only the measurements on the mean and standard deviation
#for each measurement
feature <- read.table("./UCI HAR Dataset/features.txt")
fid <- sort(c(grep("mean",feature$V2),grep("std",feature$V2)))
fname <- as.character(feature$V2[fid])
fname <- gsub("-","_",fname)
fname <- gsub("\\(\\)","",fname)

record <- record[,fid]
colnames(record) <- fname

#Uses descriptive activity names to name the activities in the data set
aname <- read.table("./UCI HAR Dataset/activity_labels.txt")
aname$V2 <- as.character(aname$V2)
for(i in 1:6) {
    activity$V1 <- gsub(i,aname[i,2],activity$V1)
}

#Combine datasets together
all_data <- cbind(subject,activity,record)

#Appropriately labels the data set with descriptive variable names
colnames(all_data) <- c("Subject","Activity",fname)

#store new dataset in file
write.table(all_data,file="all_data.txt",row.names = FALSE)



#Creat dataset by variable subject and activity
a <- all_data
a$sub_act <- paste(a$Subject,"@",a$Activity)
a <- a[,c(ncol(a),1:ncol(a)-1)]
a <- subset(a,select = - c(Subject,Activity))
a_sub <- split(a,a$sub_act)

mean_data <- matrix(ncol = ncol(a)-1, nrow = length(a_sub))
for (i in 1:length(a_sub)){
    mean_data[i,] <- colMeans(a_sub[[i]][,-1])
}

mean_data <- as.data.frame(mean_data)
mean_data <- cbind(names(a_sub),mean_data)
colnames(mean_data) <- colnames(a)
mean_data$sub_act <- as.character(mean_data$sub_act)

mean_data$Subject <- gsub("@.*",'',mean_data$sub_act)
mean_data$Activity <- gsub(".*@ ",'',mean_data$sub_act)

mean_data <- mean_data[,c(ncol(mean_data)-1,ncol(mean_data),1:ncol(mean_data))]
mean_data <- mean_data[,-c(ncol(mean_data)-1,ncol(mean_data))]
mean_data <- subset(mean_data,select=-sub_act)

mean_data$Subject <- as.numeric(mean_data$Subject)
mean_data <- mean_data[order(mean_data$Subject),]

#write this dataset into file
write.table(mean_data,file="mean_data.txt",row.names = FALSE)

write.table(colnames(mean_data),file="variable_name.txt", quote=F, col.names=F)