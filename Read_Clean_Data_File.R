##############################################################################
#
# FILE  :   Read_Clean_Data.R
# Author:   Leena Sonpethkar
#
# OVERVIEW
#
##############################################################################

library(dplyr)
library(R.utils)

zipUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
zipFile <- paste("Data", "Coursera-SwiftKey.zip", sep = "/")

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

# unzip zip file 
dataPath <- "Data/final/en_US"
if (!file.exists(file.path(paste(dataPath, "en_US.twitter.txt", sep = "/")))) {
  unzip(zipFile, exdir = "/Data")
}

## PreStep 2: Read data test, train, feature, activity 
##############################################################################
#Create a Test dataset, by adding columns from subject, activity and test data provided

#Read Data
filePath <- paste(getwd(), "Data/final/en_US", sep = "/")
#testactdata <- read.table(file.path(dataPath, "test", "y_test.txt"))

if (!file.exists(file.path(filePath, "en_US.twitter.txt"))) {
#  xx <- 1
   #Print error here
}

conBL <- file(file.path(filePath, "en_US.blogs.txt"), "r") 
conNW <- file(file.path(filePath, "en_US.news.txt"), "r") 
conTW <- file(file.path(filePath, "en_US.twitter.txt"), "r") 

rdTW <- readLines(conTW) 
rdBL <- readLines(conBL)
rdNW <- readLines(conNW)

lenBL <- nchar(readLines(conBL))
max_lenBL <- max(lenBL)

lenNW <- nchar(readLines(conNW))
max_lenNW <- which.max(lenNW)

zz <- file.info(file.path(getwd(),dataPath, "en_US.blogs.txt"))



BL_len <- NULL
for (i in (1:rdBL)){
  BL_len <- stri_length(readLines(conBL,1))
}
max_lenBL <- max(BL_len)



## Read the first line of text readLines(con, 1) 
## Read the next line of text readLines(con, 5) 
## Read in the next 5 lines of text close(con) 
## It's important to close the connection when you are done. See the connections help page for more information.

close(conTW)
close(conBL)
close(conNW)

#ActivityTidyData <- humanActivity %>% group_by(subject, activity) %>%  summarise_each(funs(mean))

#View(ActivityTidyData)

# New output file "Activity_tidy_data.txt"
#write.table(ActivityTidyData, "Activity_tidy_data.txt", row.names = FALSE, 
#            quote = FALSE)

