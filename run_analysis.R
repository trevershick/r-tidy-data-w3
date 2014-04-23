library(reshape2)
library(plyr)

downloadAndExtract <- function() {
  
  downloaded <- function() { file.exists("dataset.zip"); }
  if (!downloaded()) {
    print("Downloading the dataset zip file")
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile="dataset.zip",method="curl")
  }
  stopifnot(downloaded())
  
  extracted <- function() { 
      files <- c("X_train.txt","X_test.txt","y_train.txt",
                 "y_test.txt","subject_train.txt","subject_test.txt",
                 "activity_labels.txt","features.txt")
      all(sapply(files, file.exists))
  }
  
  if (!extracted()) {
    unzip(zipfile="dataset.zip",
          files=c("UCI HAR Dataset/test/X_test.txt",
                  "UCI HAR Dataset/train/X_train.txt",
                  "UCI HAR Dataset/test/y_test.txt",
                  "UCI HAR Dataset/train/y_train.txt",                  
                  "UCI HAR Dataset/test/subject_test.txt",
                  "UCI HAR Dataset/train/subject_train.txt",                  
                  "UCI HAR Dataset/features.txt",
                  "UCI HAR Dataset/activity_labels.txt"),
          exdir=".",
          junkpaths=TRUE,
          overwrite=TRUE)
  }
  stopifnot(extracted())
}

mergeFiles <- function() {
  m <- function(prefix) {
    merged <- paste(prefix, "_merged.txt", sep="")
    file.create(merged)
    file.append(merged, paste(prefix, "_test.txt", sep=""))
    file.append(merged, paste(prefix, "_train.txt", sep=""))
  }
  lapply(c("y","X","subject"), m)
}

mergedX <- function() {
  if (!exists("theMergedX")) {
    print("loading merged set")
    theMergedX <<- read.table(file="X_merged.txt")  
  }
  theMergedX
}

extractMeanAndStdDevs <- function() {
  if (!exists("theExtractMeanAndStdDevs")){
    featuresDef <- read.table("features.txt",sep=" ",col.names=c("index","feature"))
    meansAndStdDevIndexes <- featuresDef[grep("mean\\(\\)$|std\\(\\)$", featuresDef$feature, ignore.case=TRUE),1]
    meansAndStdDevLabels <- featuresDef[grep("mean\\(\\)$|std\\(\\)$", featuresDef$feature, ignore.case=TRUE),2]
    df <- data.frame(mergedX()[,meansAndStdDevIndexes])
    colnames(df) <- meansAndStdDevLabels
    theExtractMeanAndStdDevs <<- df
  }
  theExtractMeanAndStdDevs
}

labelRecords <- function(data) {
  theMergedY <- read.table("y_merged.txt")
  colnames(theMergedY) <- c("id")
  
  activityLabels <- read.table("activity_labels.txt",col.names=c("id","activity"))
  activities <- merge(x=theMergedY, y=activityLabels, by.x="id", by.y="id")[,2]
  tmp <- data.frame(activities, data)
  colnames(tmp)[1] <- "Activity"
  tmp
}
personifyRecords <- function(data) {
  theMergedSubject <- read.table("subject_merged.txt")
  tmp <- data.frame(theMergedSubject, data)
  colnames(tmp)[1] <- "Subject"
  tmp
}

summarizeIt <- function(data) {
  mdata <- melt(data, id=c("Activity","Subject"))
  ddply(mdata, .(Subject,Activity,variable), summarize, Mean=mean(value))
}

runIt <- function() {
  # download if they're not there
  downloadAndExtract()
  
  # merge all x,y and subject files together (union them)
  mergeFiles()
  
  # pull out the stddev and mean columns
  tmp1 <- extractMeanAndStdDevs()
  
  # add the activity labels
  tmp2 <- labelRecords(tmp1)
  
  # add the subject data to the data
  tmp3 <- personifyRecords(tmp2)
  write.csv(x=tmp3,file="RESULT_unsummarized.txt",quote=FALSE,row.names=FALSE)
  
  # finally summarize the data and output
  summarizedData <- summarizeIt(tmp3)
  write.csv(x=summarizedData,file="RESULT_summarized.txt",quote=FALSE,row.names=FALSE)
  
  summarizedCrosstab <- dcast(summarizedData, Subject+Activity ~ variable, sum,drop=TRUE,value.var="Mean")
  write.csv(x=summarizedData,file="RESULT_summarized_xtab.txt",quote=FALSE,row.names=FALSE)  
  
  print("RESULT_unsummarized.txt, RESULT_summarized.txt and RESULT_summarized_xtab.txt files are created")
}

runIt()