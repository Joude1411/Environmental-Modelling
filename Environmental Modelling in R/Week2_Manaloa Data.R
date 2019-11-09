
#Maunaloa data
loadata<- read.csv("https://raw.githubusercontent.com/levvers/dsemr/master/data/week2/maunaloa.csv")
head(loadata)
loadata <- ts(loadata$average, start=c(1958,3), frequency=12)
loadata
save(loadata, file="C:\Users\Net57-36\Documents\Qlikview Apps Folder\RFH Performance\2019 R Practice\Environmental Modelling in R\maunaloa.RData")
save(loadata, file="C://Users/Net57-36/Documents/Qlikview Apps Folder/RFH Performance/2019 R Practice/Environmental Modelling in R/maunaloa2.RData")

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
glimpse(windata)
windata$Newdate<- as.Date(windata$Newdate)#convert Newdate from factor to date

library(lubridate)
windata%>%
  select(Newdate,WindSpeed)%>%
  filter(year(Newdate) = 2013)%>%
  plot()


#It is now time to try pull request. Lets see if this works too.
#This also worked perfectly

#Lets see what happens with branching
