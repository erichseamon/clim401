#--aacs_1.R
#
#--This is the data loading for weather data for a station
#--in this instance, we are using Corvallis State University Station, OR
#--the code loads the data, 



corvallis_temp <- read.csv("/dmine/code/git/clim401/CorvallisStateUniversity_temp_1895-2017_data.csv", header = TRUE)
corvallis_precip <- read.csv("/dmine/code/git/clim401/CorvallisStateUniversity_precip_1895_2017_data.csv", header = TRUE)

library(tidyr)
library(car)
library(seas)
library(hydroTSM)
library(latticeExtra)
library(plotrix)
library(weathermetrics)
library(gridExtra)
library(measurements)
library(dplyr)

#--Table for Latitude, Longitude, and Elevation for Corvallis Station

Description <- data.frame(c("Name", "Station", "Station State", "Latitude", "Longitude"))
Values <- data.frame(c("351862", "Corvallis State Univ", "OR", "44.6333", "-123.19"))

Desc <- cbind(Description, Values)
colnames(Desc) <- c("Description", "Values")

#plotting description table

grid.table(Desc, rows = NULL)

#--subsetting temp and precipitation for 1981-2010

corvallis_temp_normals <- subset(corvallis_temp, Year >= 1981 & Year <= 2010)
corvallis_precip_normals <- subset(corvallis_precip, Year >= 1981 & Year <= 2010)

#--subsetting temp and precipitation for 2016

corvallis_temp_2016 <- subset(corvallis_temp, Year == 2016)
corvallis_precip_2016 <- subset(corvallis_precip, Year == 2016)

#--averaging temp and converting to C

cor1_temp <- data.frame(corvallis_temp_normals[,2:13])
cor1_temp_normals <- colMeans(cor1_temp)
cor1_temp_normals_celsius <- t(data.frame(fahrenheit.to.celsius(cor1_temp_normals)))

#--for plotting just 2016 and converting to celsisu

cor1_temp_normals_celsius_2016 <- fahrenheit.to.celsius(corvallis_temp_2016[,2:13])
cor1_precip_normals_mm_2016 <- corvallis_precip_2016[,2:13] * 25.4

#--averaging precip and converting to mm

cor1_precip <- data.frame(corvallis_precip_normals[,2:13])
cor1_precip_normals <- colMeans(cor1_precip)
cor1_precip_normals_mm <- cor1_precip_normals * 25.4


finalnormals2016 <- rbind(cor1_precip_normals_mm_2016, cor1_temp_normals_celsius_2016)
finalnormals2016 <- t(finalnormals2016)



#--Plotting


#--Creating table for Climograph 1981-2010
finalnormals <- rbind(cor1_precip_normals_mm, cor1_temp_normals_celsius)
finalnormals <- t(finalnormals)
months <- data.frame(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
finalnormals <- cbind(months, finalnormals)
colnames(finalnormals) <- c("Months", "Precipitation (mm)", "Temperature (C)")
grid.table(finalnormals[,1:3], rows = NULL)


#--Creating climograph for Corvallis, 1981-2010
twoord.plot(c(1:12), cor1_temp_normals_celsius, c(1:12), cor1_precip_normals_mm, ylab = "Temperature (Celsius)", xticklab=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), rylab = "Precipitation (mm)", xlab = "Months", type=c("bar", "b"), lcol = "green", rcol = "blue", main = "Climograph for Corvallis State University Station, 1981-2010")

#--Creating climograph for Corvallis, 2016
twoord.plot(c(1:12), cor1_temp_normals_celsius_2016, c(1:12), cor1_precip_normals_mm_2016, ylab = "Temperature (Celsius)", xticklab=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), rylab = "Precipitation (mm)", xlab = "Months", type=c("bar", "b"), lcol = "green", rcol = "blue", main = "Climograph for Corvallis State University Station, 2016")


#calculating anomlies between 2016 and 1981-2010

#Anomaly Plot for precipitation (percentage)

anomaly_precip <- rbind(cor1_precip_normals_mm_2016, cor1_precip_normals_mm )
anomaly_precip1 <- (anomaly_precip[1,1] / anomaly_precip[2,1]) - 1
anomaly_precip2 <- (anomaly_precip[1,2] / anomaly_precip[2,2]) -1
anomaly_precip3 <- (anomaly_precip[1,3] / anomaly_precip[2,3]) - 1
anomaly_precip4 <- (anomaly_precip[1,4] / anomaly_precip[2,4]) - 1
anomaly_precip5 <- (anomaly_precip[1,5] / anomaly_precip[2,5]) - 1
anomaly_precip6 <- (anomaly_precip[1,6] / anomaly_precip[2,6]) - 1
anomaly_precip7 <- (anomaly_precip[1,7] / anomaly_precip[2,7]) - 1
anomaly_precip8 <- (anomaly_precip[1,8] / anomaly_precip[2,8]) - 1
anomaly_precip9 <- (anomaly_precip[1,9] / anomaly_precip[2,9]) - 1
anomaly_precip10 <- (anomaly_precip[1,10] / anomaly_precip[2,10]) - 1
anomaly_precip11 <- (anomaly_precip[1,11] / anomaly_precip[2,11]) - 1
anomaly_precip12 <- (anomaly_precip[1,12] / anomaly_precip[2,12]) - 1
anomalyfinal_precip <- cbind(anomaly_precip1, anomaly_precip2, anomaly_precip3, anomaly_precip4, anomaly_precip5, anomaly_precip6, anomaly_precip7, anomaly_precip8, anomaly_precip9, anomaly_precip10, anomaly_precip11, anomaly_precip12)
colnames(anomalyfinal_precip) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
barplot(anomalyfinal_precip, col = "green", main = "Corvallis State University Station, OR \n Precipitation (percentage) Anomalies for 2016 \n (% Departure from 1981-2010 Normals)", ylab = "Percentage Departure from Normal (1981-2010 average)" )

#--Anomaly Plot for Temperature

anomaly <- rbind(cor1_temp_normals_celsius,cor1_temp_normals_celsius_2016)
rownames(anomaly) <- c("1981-2010", "2016")

anomaly1<- anomaly[2,1] - anomaly[1,1]
anomaly2<- anomaly[2,2] - anomaly[1,2]
anomaly3<- anomaly[2,3] - anomaly[1,3]
anomaly4<- anomaly[2,4] - anomaly[1,4]
anomaly5<- anomaly[2,5] - anomaly[1,5]
anomaly6<- anomaly[2,6] - anomaly[1,6]
anomaly7<- anomaly[2,7] - anomaly[1,7]
anomaly8<- anomaly[2,8] - anomaly[1,8]
anomaly9<- anomaly[2,9] - anomaly[1,9]
anomaly10<- anomaly[2,10] - anomaly[1,10]
anomaly11<- anomaly[2,11] - anomaly[1,11]
anomaly12<- anomaly[2,12] - anomaly[1,12]
anomalyfinal <- cbind(anomaly1, anomaly2, anomaly3, anomaly4, anomaly5, anomaly6, anomaly7, anomaly8, anomaly9, anomaly10, anomaly11, anomaly12)
colnames(anomalyfinal) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
barplot(anomalyfinal, col = "green", main = "Corvallis State University Station, OR \n Temperature (Celsius) Anomalies for 2016 \n (Departure from 1981-2010 Normals)", ylab = "Departure from Normal (1981-2010 average)" )

