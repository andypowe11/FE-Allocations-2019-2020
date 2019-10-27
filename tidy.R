#' Replace Commas Function
#'
#' This function converts a character representation of a number that contains a comma separator with a numeric value.
#' @keywords read data
#' @export
replaceCommas<-function(x){
  x<-as.numeric(gsub("\\,", "", x))
}

# Read in the funding data
df <- read.csv("FE-Allocations-2019-2020.csv")
orig <- df

# Tidy up the column names
names(df)[1]<-"Region"
names(df)[2]<-"LA"
names(df)[3]<-"LANum"
names(df)[4]<-"Institution"
names(df)[8]<-"TotalStudents"
names(df)[9]<-"TotalHighNeedsStudents"
names(df)[10]<-"TotalProgrammeFunding"
names(df)[11]<-"DisadvantageFunding"
names(df)[12]<-"TotalStudentsNotMeetingConditionOfFunding"
names(df)[13]<-"ConditionOfFundingAdjustment"
names(df)[14]<-"HighNeedsElement2Funding"
names(df)[15]<-"TotalStudentFinancialSupportFunding"
names(df)[16]<-"DiscretionaryBursaryFunding"
names(df)[17]<-"FreeMealsFunding"
names(df)[18]<-"IndustryPlacementsCapacityAndDeliveryFunding"
names(df)[19]<-"AdvancedMathsPremiumFunding"
names(df)[20]<-"AlternativeCompletionsFunding"
names(df)[21]<-"TotalFundingAllocation"
# colnames(df)

# Change column values to numeric
df$TotalStudents <- replaceCommas(df$TotalStudents)
df$TotalHighNeedsStudents <- replaceCommas(df$TotalHighNeedsStudents)
df$TotalProgrammeFunding <- replaceCommas(df$TotalProgrammeFunding)
df$DisadvantageFunding <- replaceCommas(df$DisadvantageFunding)
df$TotalStudentsNotMeetingConditionOfFunding <- replaceCommas(df$TotalStudentsNotMeetingConditionOfFunding)
df$ConditionOfFundingAdjustment <- replaceCommas(df$ConditionOfFundingAdjustment)
df$HighNeedsElement2Funding <- replaceCommas(df$HighNeedsElement2Funding)
df$TotalStudentFinancialSupportFunding <- replaceCommas(df$TotalStudentFinancialSupportFunding)
df$DiscretionaryBursaryFunding <- replaceCommas(df$DiscretionaryBursaryFunding)
df$FreeMealsFunding <- replaceCommas(df$FreeMealsFunding)
df$IndustryPlacementsCapacityAndDeliveryFunding <- replaceCommas(df$IndustryPlacementsCapacityAndDeliveryFunding)
df$AdvancedMathsPremiumFunding <- replaceCommas(df$AdvancedMathsPremiumFunding)
df$AlternativeCompletionsFunding <- replaceCommas(df$AlternativeCompletionsFunding)
df$TotalFundingAllocation <- replaceCommas(df$TotalFundingAllocation)

# Remove any rows with NA value
df <- df[complete.cases(df),]

# Remove any rows with 0 students
df <- df[df$TotalStudents != 0, ]

# Create some helper columns
df$FundingPerStudent <- round(df$TotalFundingAllocation / df$TotalStudents)

# Compute some averages by Region and LA

#Regions <- table(df$Region)
#Regions <- Regions[order(-Regions)]
#LAs <- table(df$LA)
#LAs <- LAs[order(-LAs)]
#RegionAverage <- tapply(df$FundingPerStudent, df$Region, mean)
#LAAverage <- tapply(df$FundingPerStudent, df$LA, mean)

# install.packages("tidyverse")
library(dplyr)

regions_df <- df %>%
group_by(Region) %>%
summarize(Count=n(), AverageFundingPerStudent=mean(FundingPerStudent)) %>%
mutate(AverageFundingPerStudent=round(AverageFundingPerStudent)) %>%
arrange(desc(AverageFundingPerStudent))

las_df <- df %>%
group_by(LA) %>%
summarize(Count=n(), AverageFundingPerStudent=mean(FundingPerStudent)) %>%
mutate(AverageFundingPerStudent=round(AverageFundingPerStudent)) %>%
arrange(desc(AverageFundingPerStudent))
