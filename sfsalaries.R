#install.packages("ggplot2")
#library(ggplot2)

filepath <- '/Users/sindhurao/Desktop/Rfiles/SFSALARIES'
file <- list.files(path=filespath, pattern="*.csv")
nooffiles <- length(files)
sfsalaries <- read.csv('/Users/sindhurao/Desktop/Rfiles/SFSALARIES/Salaries.csv', header = TRUE, na.strings = c("Not Provided", "NotProvided"), colClasses = c('integer', 'character', 'factor', rep('numeric', 6), 'factor', 'NULL', 'factor', 'factor'))
str(sfsalaries)
head(sfsalaries)
columns <- length(sfsalaries)
rows <- nrow(sfsalaries)
summary(sfsalaries)
# removes rows with NA values in any column
sfsalaries_filtered_na <- na.omit(sfsalaries) 
summary(sfsalaries_filtered_na)
hist(sfsalaries$BasePay, xlab = "Base pay", main = "Total Earnings")

title_meager_pay <- unique(sfsalaries_filtered_na[sfsalaries_filtered_na$BasePay < 0, c("JobTitle")])
#mean(sfsalaries_filtered_na[sfsalaries_filtered_na$JobTitle == "Clerk", ]$BasePay)

for (title in title_meager_pay){
   mean_title <- mean(sfsalaries_filtered_na[sfsalaries_filtered_na$JobTitle == title, ]$BasePay)
   sfsalaries_filtered_na[(sfsalaries_filtered_na$JobTitle == title) & (sfsalaries_filtered_na$BasePay < 0), "BasePay"] <- mean_title
}
Title_meager_Totalpay <- unique(sfsalaries_filtered_na[sfsalaries_filtered_na$TotalPay < 0, c("JobTitle")])
for (title in Title_meager_Totalpay){
    mean_title <- mean(sfsalaries_filtered_na[sfsalaries_filtered_na$JobTitle == title, ]$TotalPay)
    sfsalaries_filtered_na[(sfsalaries_filtered_na$JobTitle == title) & (sfsalaries_filtered_na$TotalPay < 0), "TotalPay"] <- mean_title
}

Title_meager_Otherpay <- unique(sfsalaries_filtered_na[sfsalaries_filtered_na$OtherPay < 0, c("JobTitle")])
for (title in Title_meager_Otherpay){
    mean_title <- mean(sfsalaries_filtered_na[sfsalaries_filtered_na$JobTitle == title, ]$OtherPay)
    sfsalaries_filtered_na[(sfsalaries_filtered_na$JobTitle == title) & (sfsalaries_filtered_na$OtherPay < 0), "OtherPay"] <- mean_title
}

Title_meager_Benefits <- unique(sfsalaries_filtered_na[sfsalaries_filtered_na$Benefits < 0, c("JobTitle")])
for (title in Title_meager_Benefits){
  mean_title <- mean(sfsalaries_filtered_na[sfsalaries_filtered_na$Benefits == title, ]$Benefits)
  sfsalaries_filtered_na[(sfsalaries_filtered_na$JobTitle == title) & (sfsalaries_filtered_na$Benefits < 0), "Benefits"] <- mean_title
}

Title_meager_TotalPayBenefits <- unique(sfsalaries_filtered_na[sfsalaries_filtered_na$TotalPayBenefits < 0, c("JobTitle")])
for (title in Title_meager_TotalPayBenefits){
  mean_title <- mean(sfsalaries_filtered_na[sfsalaries_filtered_na$TotalPayBenefits == title, ]$TotalPayBenefits)
  sfsalaries_filtered_na[(sfsalaries_filtered_na$JobTitle == title) & (sfsalaries_filtered_na$TotalPayBenefits < 0), "TotalPayBenefits"] <- mean_title
}
# to replace the negative salaries with the avg salary for a job title 
#tapply(sfsalaries2$JobTitle, sfsalaries2$BasePay, mean) 

install.packages("plotly")
library(plotly)
plot(sfsalaries_filtered_na$Year, sfsalaries_filtered_na$BasePay, col = "light Blue")

h <- hist(sfsalaries_filtered_na$BasePay, breaks = 20, col = "Green")
xfit<-seq(min(sfsalaries_filtered_na$BasePay),max(sfsalaries_filtered_na$BasePay),length=25)
yfit<-dnorm(xfit,mean=mean(sfsalaries_filtered_na$BasePay),sd=sd(sfsalaries_filtered_na$BasePay))
yfit <- yfit*diff(h$mids[1:2])*length(sfsalaries_filtered_na$BasePay) 
lines(xfit, yfit, col="blue", lwd=2)

library(e1071)
skewness(sfsalaries_filtered_na$BasePay)
skewness(sfsalaries_filtered_na$TotalPayBenefits)

sfsalaries_filtered_na[sfsalaries_filtered_na$OvertimePay < 0, c("JobTitle", "EmployeeName")]
sfsalaries_filtered_na[sfsalaries_filtered_na$OvertimePay < 0, c("JobTitle", "EmployeeName", "OvertimePay")]
# Below one inserts NA and converts numeric to character values
sfsalaries_filtered_na[sfsalaries_filtered_na$OvertimePay < 0, "OvertimePay"] <- "NA"
# to convert OvertimePay back to numeric
sfsalaries_filtered_na$OvertimePay <- as.numeric(sfsalaries_filtered_na$OvertimePay)
sfsalaries_filtered_na <- na.omit(sfsalaries_filtered_na)
summary(sfsalaries_filtered_na)
means_totalpaybenefits <- tapply(sfsalaries_filtered_na$TotalPayBenefits, sfsalaries_filtered_na$Year, mean)
means_totalpaybenefits
# toupper - to convert all job titles to upper case to avoid duplicates
sfsalaries_filtered_na$JobTitle <- toupper(sfsalaries_filtered_na$JobTitle)
# calculate mean salary for each job title
mean_salary_title <- tapply(sfsalaries_filtered_na$TotalPayBenefits, sfsalaries_filtered_na$JobTitle, mean)
#least salaries and their titles
head(sort(mean_salary_title))
# highest salaries and their titles
tail(sort(mean_salary_title))
