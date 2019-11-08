#Maunaloa data reading .RData files
#url<- "https://github.com/levvers/dsemr/blob/master/data/week2/esktem.RData?raw=true"
url <- "https://github.com/levvers/dsemr/raw/master/data/week2/maunaloa.RData"
myFile <- "C://Users/Net57-36/Documents/R/2019 R Practice/Environmental Modelling in R/maunaloa.RData"
download.file(url,destfile=myFile)
load(myFile) # this loads the data to the global environment and will appear as loadata
head(loadata)
save(loadata,file=myFile)

#Maunaloa data reading csv files
loadata<- read.csv("https://raw.githubusercontent.com/levvers/dsemr/master/data/week2/maunaloa.csv")
head(loadata)
loadata <- ts(loadata$average, start=c(1958,3), frequency=12)
loadata
save(loadata, file="C://Users/Net57-36/Documents/R/2019 R Practice/Environmental Modelling in R/maunaloa2.RData")

plot(loadata)
plot(loadata, col=2, type="p", pch=16, cex=0.5,
     xlim=c(2010, 2018),
     ylim=c(390,410),
     xlab="Year", ylab="Carbon dioxide level (ppmnv)")
title(expression("Mauna Loa CO"[2]*" levels"))

#install.packages("forecast")
library(forecast)
loashort <- subset(loadata, start=623, end=718)
plot(loashort, col=2, type="p", pch=16, cex=0.5,
     xlab="Year", ylab="Carbon dioxide level (ppmnv)")
title(expression("Mauna Loa CO"[2]*" levels"))


#Windspeed
windata<- read.csv("https://raw.githubusercontent.com/levvers/dsemr/master/data/week2/windspeed.csv")
head(windata,20)
library(tidyverse)
glimpse(windata)
windata$Newdate<- as.Date(windata$Newdate)#convert Newdate from factor to date

library(lubridate)
windata%>%
  select(Newdate,WindSpeed)%>%
  #filter(year(Newdate) == 2011)%>%
  group_by(Newdate)%>%
  summarize(WindSpeed = mean(WindSpeed))%>%
  plot(col="purple",cex=0.2,pch=19,type = "l"
       ,ylab="Wind Speed",xlab="Year")
title("Wind Speed")
# plot( ylab="Wind Speed", xlab="Year", 
#      type="l", col=6)
# title("Wind Speed")

# course.end <- make_datetime(2018, 11, 26) - seconds(1)
# course.end
# course.end <- "2018-11-25 23:59:59"
# decimal_date(course.end)
# date_decimal(2018.9013)

windata_monthly <- windata%>%
  select(Newdate,WindSpeed)%>%
  filter(year(Newdate) == 2013)%>%
  group_by(Newdate)%>%
  summarize(WindSpeed = mean(WindSpeed))

windata_monthly%>%
  plot(col="purple",cex=0.2,pch=19,type = "l"
       ,ylab="Wind Speed",xlab="Year 2013") 
title("Wind Speed")


windata_missing <- subset(windata, Newdate>="2013-01-01" & Newdate<="2013-12-31")
mean_speed <- aggregate(WindSpeed ~ Newdate, data= windata_missing, mean, na.action=na.pass)

plot(WindSpeed ~ Newdate, data=mean_speed, xlab="Year 2013", type="l", col=2)

#missing values in environmental data/modelling
install.packages("imputeTS")
library(imputeTS)
#plot the missing values
plotNA.distribution(x=mean_speed$WindSpeed, colPoints=2, colBackgroundMV=3)
#The green vertical lines indicate where there is missing values 

#Another helpful plot is the distribution bar, which shows where observations are missing.
plotNA.distributionBar(x=mean_speed$WindSpeed)

#We can impute the overall mean. This is the simplest, but not a very sophisticated approach.
mean_speed_imp <- na.mean(mean_speed)
mean_speed_imp  

#We could now work with this data set, but let's first visualise the imputed values.
plotNA.imputations(mean_speed$WindSpeed,mean_speed_imp$WindSpeed)

#We can also impute values, based on a time series model. This usually gives more realistic results, but is highly dependent on whether the time series model is suitable or not.
mean_speed_imp <- na.kalman(mean_speed)
plotNA.imputations(mean_speed$WindSpeed,mean_speed_imp$WindSpeed)  

#Instead, we could simply calculate the daily average from the other hours of the day, so that we would only get missing values if we had no measurements at all for that day.
mean_speed <- aggregate(WindSpeed ~ Newdate, data=windata_missing, mean, na.action=na.omit)
#This dataset now contains no missing values at all.
plot(WindSpeed ~ Newdate, data=mean_speed, xlab="Year 2013", type="l", col=2)


# How to load .RData files from github
#url<- "https://github.com/levvers/dsemr/blob/master/data/week2/esktem.RData?raw=true"
url <- "https://github.com/levvers/dsemr/raw/master/data/week2/esktem.RData"
myFile <- "C://Users/Net57-36/Documents/R/2019 R Practice/Environmental Modelling in R/esktem.RData"
download.file(url,destfile=myFile)
load(myFile) # this will load esktem to the global Environment

# Another way to load .rdata into r from github
library(repmis)
install.packages("repmis")
source_data("https://github.com/levvers/dsemr/blob/master/data/week2/esktem.RData?raw=true")
save(esktem,file=myFile)

# esktem is already loaded in the global Environment so you can call and manipulate it directly
head(esktem)
plot(esktem)

#STL Decomposition of time series data
plot(stl(esktem, "periodic"))
plot(stl(esktem, "periodic", t.window=240))

#Decomposing the maunaloa data for Atmospheric CO2
url <- "https://github.com/levvers/dsemr/raw/master/data/week2/maunaloa.RData"
myFile <- "C://Users/Net57-36/Documents/R/2019 R Practice/Environmental Modelling in R/maunaloa.RData"
download.file(url,destfile=myFile)
load(myFile) #this will show as loadata

head(loadata)
plot(loadata)
plot(stl(loadata,"periodic")) # with stl decomposition


#Minimum Daily Temperatures Dataset
#Daily minimum temperatures in Melbourne, Australia, 1981-1990

tempData <- read.csv("https://raw.githubusercontent.com/jbrownlee/Datasets/master/daily-min-temperatures.csv")
tail(tempData)
#temp_data%>%
    plot(tempData, col=2, type="l", pch=16, cex=0.5)
    title(expression("Daily minimum temperatures in Melbourne, Australia, 1981-1990"))
library(forecast)
tsdata <- ts(tempData$Temp,start=c(1981,1),end=c(1990,12),frequency=12)
 plot(tsdata,col="skyblue", type="l", pch=16, cex=0.5,xlab = "Year", ylab = "Temperature")
 title(expression("Daily minimum temperatures in Melbourne, Australia, 1981-1990"))
plot(stl(tsdata,"periodic",xlab = "Year")) 
title(expression("Daily minimum temperatures in Melbourne, Australia, 1981-1990"))


