
#palousenormals <- function(state, county, year, climatevariable) {

  
  


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

corvallis_temp_2016 <- subset(corvallis_temp, Year == 2017)
corvallis_precip_2016 <- subset(corvallis_precip, Year == 2017)

#--averaging temp and converting to C

cor1_temp <- data.frame(corvallis_temp_normals[,2:13])
cor1_temp_normals <- colMeans(cor1_temp)
cor1_temp_normals_celsius <- t(data.frame(fahrenheit.to.celsius(cor1_temp_normals)))

#--for plotting just 2016 and converting to celsisu

cor1_temp_normals_celsius_2016 <- fahrenheit.to.celsius(corvallis_temp_2016[,2:13])
cc <- rbind(corvallis_precip_2016, corvallis_precip_2016)
cor1_precip_normals_mm_2016 <- colMeans(cc)
cor1_precip_normals_mm_2016 <- cor1_precip_normals_mm_2016[2:13] * 25.4

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
colnames(finalnormals) <- c("Months", "1981-2010 Average Precipitation (mm)", "1981-2010 Average Temperature (C)")
grid.table(finalnormals[,1:3], rows = NULL)

#--Creating table for 2017
finalnormals <- rbind(cor1_precip_normals_mm_2016, cor1_temp_normals_celsius_2016)
finalnormals <- t(finalnormals)
months <- data.frame(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
finalnormals <- cbind(months, finalnormals)
colnames(finalnormals) <- c("Months", "2017 Precipitation (mm)", "2017 Temperature (C)")
grid.table(finalnormals[,1:3], rows = NULL)

#layout(matrix(c(1,2,3,4,5,6,7,8), 2, 4, byrow = TRUE))

#--Creating climograph for Corvallis, 1981-2010
twoord.plot(c(1:12), cor1_precip_normals_mm, c(1:12), rylim=c(0,20), lylim=c(0,250), cor1_temp_normals_celsius, ylab = "Precipitation (mm)", xticklab=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), rylab = "Temperature (Celsius)", xlab = "Months", type=c("bar", "b"), lcol = "green", rcol = "blue", main = "Climograph for Corvallis State University Station, 1981-2010")

#--Creating climograph for Corvallis, 2016
twoord.plot(c(1:12), cor1_precip_normals_mm_2016, c(1:12), cor1_temp_normals_celsius_2016, lylim=c(0,350), rylim=c(0,25), ylab = "Precipitation (mm)", xticklab=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), rylab = "Temperature (Celsius)", xlab = "Months", type=c("bar", "b"), lcol = "green", rcol = "blue", main = "Climograph for Corvallis State University Station, 2017")


#calculating anomlies between 2016 and 1981-2010

#Anomaly Plot for precipitation (percentage)

anomaly_precip <- rbind(cor1_precip_normals_mm_2016, cor1_precip_normals_mm )
anomaly_precip1 <- (anomaly_precip[1,1] / anomaly_precip[2,1]) 
anomaly_precip2 <- (anomaly_precip[1,2] / anomaly_precip[2,2])
anomaly_precip3 <- (anomaly_precip[1,3] / anomaly_precip[2,3]) 
anomaly_precip4 <- (anomaly_precip[1,4] / anomaly_precip[2,4]) 
anomaly_precip5 <- (anomaly_precip[1,5] / anomaly_precip[2,5]) 
anomaly_precip6 <- (anomaly_precip[1,6] / anomaly_precip[2,6]) 
anomaly_precip7 <- (anomaly_precip[1,7] / anomaly_precip[2,7]) 
anomaly_precip8 <- (anomaly_precip[1,8] / anomaly_precip[2,8]) 
anomaly_precip9 <- (anomaly_precip[1,9] / anomaly_precip[2,9])
anomaly_precip10 <- (anomaly_precip[1,10] / anomaly_precip[2,10])
anomaly_precip11 <- (anomaly_precip[1,11] / anomaly_precip[2,11])
anomaly_precip12 <- (anomaly_precip[1,12] / anomaly_precip[2,12])
anomalyfinal_precip <- cbind(anomaly_precip1, anomaly_precip2, anomaly_precip3, anomaly_precip4, anomaly_precip5, anomaly_precip6, anomaly_precip7, anomaly_precip8, anomaly_precip9, anomaly_precip10, anomaly_precip11, anomaly_precip12)
colnames(anomalyfinal_precip) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

library(scales)

yticks_val <- pretty_breaks(n=6)(anomalyfinal_precip)

barplot(anomalyfinal_precip, col = "green", xlab = "Months", main = "Corvallis State University Station, OR \n Precipitation (mm) Anomalies for 2017 \n (% Departure from 1981-2010 Normals)", yaxt="n", ylab = "Percentage Departure from Normal (1981-2010 average)" )
axis(2, at=yticks_val, lab=percent(yticks_val))
abline(h = 1, lty = 2, col = "red")
#--Anomaly Plot for Temperature

anomaly <- rbind(cor1_temp_normals_celsius,cor1_temp_normals_celsius_2016)
rownames(anomaly) <- c("1981-2010", "2017")

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
barplot(anomalyfinal, col = "green", xlab = "Months", main = "Corvallis State University Station, OR \n Temperature (Celsius) Anomalies for 2017 \n (Departure from 1981-2010 Normals)", ylim=c(-3, 3), ylab = "Departure from Normal (1981-2010 average) - degrees Celsius" )


#--subsetting temp and precipitation for 1950-2017

corvallis_temp_normals_annual <- subset(corvallis_temp, Year >= 1950 & Year <= 2017)
corvallis_precip_normals_annual <- subset(corvallis_precip, Year >= 1949 & Year <= 2017)

corvallis_precip_normals_annual['Oct'] <- c(NA, head(corvallis_precip_normals_annual['Oct'], dim(corvallis_precip_normals_annual)[1] - 1)[[1]])
corvallis_precip_normals_annual['Nov'] <- c(NA, head(corvallis_precip_normals_annual['Nov'], dim(corvallis_precip_normals_annual)[1] - 1)[[1]])
corvallis_precip_normals_annual['Dec'] <- c(NA, head(corvallis_precip_normals_annual['Dec'], dim(corvallis_precip_normals_annual)[1] - 1)[[1]])
corvallis_precip_normals_annual = corvallis_precip_normals_annual[-1,]


ctnas <- as.data.frame(rowMeans(corvallis_temp_normals_annual[,2:13]))
ctnas <- cbind(c(1950:2017), ctnas)
colnames(ctnas) <- c("Year", "Temp_Annual_Average")
cpnas <- as.data.frame(rowMeans(corvallis_precip_normals_annual[,2:13]))
cpnas <- cbind(c(1950:2017), cpnas)
colnames(cpnas) <- c("Year", "Precip_Annual_Average")

plot(ctnas$Year, ctnas$Temp_Annual_Average, pch=16, main = "Corvallis State University Station \n Annual Temperature (F) - 1950-2017", las=2, xlab = "Year", ylab = "Temperature (F)")

mod<-lm(ctnas$Temp_Annual_Average~ctnas$Year)
abline(coefficients(mod), lwd=2, lty=2, col="red")

text(2008, 50, paste("annual trend =", round(mod$coefficients[2], 4),  "\n decadal trend =", round(mod$coefficients[2]*10, 4), "\n 1950-2017 trend =", round(mod$coefficients[2]*10*6, 4), "\n p-value =", round(summary(mod)$coefficients[,4][2], 7)))

plot(cpnas$Year, cpnas$Precip_Annual_Average, pch=16, main = "Corvallis State University Station \n Annual Precipitation (mm) - 1950-2017", las=2, xlab = "Year", ylab = "Precipitation (mm)")

mod<-lm(cpnas$Precip_Annual_Average~cpnas$Year)
abline(coefficients(mod), lwd=2, lty=2, col="red")

text(1961, 2.5, paste("annual trend =", round(mod$coefficients[2], 4),  "\n decadal trend =", round(mod$coefficients[2]*10, 4), "\n 1950-2017 trend =", round(mod$coefficients[2]*10*6, 4), "\n p-value =", round(summary(mod)$coefficients[,4][2], 7)))


#---june.july.aug and dec.jan.feb

corvallis_temp_normals_annual <- subset(corvallis_temp, Year >= 1950 & Year <= 2017)
ctna_jja <- cbind(corvallis_temp_normals_annual$Jun, corvallis_temp_normals_annual$Jul, corvallis_temp_normals_annual$Aug)
ctna_djf <- cbind(corvallis_temp_normals_annual$Dec, corvallis_temp_normals_annual$Jan, corvallis_temp_normals_annual$Feb)


corvallis_precip_normals_annual <- subset(corvallis_precip, Year >= 1950 & Year <= 2017)
cpna_jja <- cbind(corvallis_precip_normals_annual$Jun, corvallis_precip_normals_annual$Jul, corvallis_precip_normals_annual$Aug)
cpna_djf <- cbind(corvallis_precip_normals_annual$Dec, corvallis_precip_normals_annual$Jan, corvallis_precip_normals_annual$Feb)

ctna_jja_sum <- as.data.frame(rowMeans(ctna_jja))
ctna_jja_sum <- cbind(c(1950:2017), ctna_jja_sum)
colnames(ctna_jja_sum) <- c("Year", "Temp_Annual_Average_JJA")
plot(ctna_jja_sum$Year, ctna_jja_sum$Temp_Annual_Average_JJA, main = "Corvallis State University Station \n Annual Temperature (F) - 1950-2017 - June/July/August", pch=16, las=2, xlab = "Year", ylab = "Temperature (F)")
mod<-lm(ctna_jja_sum$Temp_Annual_Average_JJA~ctna_jja_sum$Year)
abline(coefficients(mod), lwd=2, lty=2, col="red")

text(2008, 62, paste("annual trend =", round(mod$coefficients[2], 4),  "\n decadal trend =", round(mod$coefficients[2]*10, 4), "\n 1950-2017 trend =", round(mod$coefficients[2]*10*6, 4), "\n p-value =", round(summary(mod)$coefficients[,4][2], 7)))



cpna_jja_sum <- as.data.frame(rowMeans(cpna_jja))
cpna_jja_sum <- cbind(c(1950:2017), cpna_jja_sum)
colnames(cpna_jja_sum) <- c("Year", "Precip_Annual_Average_JJA")
plot(cpna_jja_sum$Year, cpna_jja_sum$Precip_Annual_Average_JJA, main = "Corvallis State University Station \n Annual Precipitation (mm) - 1950-2017 - June/July/August", pch=16, las=2, xlab = "Year", ylab = "Precipitation (mm)")
mod<-lm(cpna_jja_sum$Precip_Annual_Average_JJA~cpna_jja_sum$Year)
abline(coefficients(mod), lwd=2, lty=2, col="red")

text(2008, 2, paste("annual trend =", round(mod$coefficients[2], 4),  "\n decadal trend =", round(mod$coefficients[2]*10, 4), "\n 1950-2017 trend =", round(mod$coefficients[2]*10*6, 4), "\n p-value =", round(summary(mod)$coefficients[,4][2], 4)))



ctna_djf_sum <- as.data.frame(rowMeans(ctna_djf))
ctna_djf_sum <- cbind(c(1950:2017), ctna_djf_sum)
colnames(ctna_djf_sum) <- c("Year", "Temp_Annual_Average_DJF")
plot(ctna_djf_sum$Year, ctna_djf_sum$Temp_Annual_Average_DJF, pch=16, las=2, main = "Corvallis State University Station \n Annual Temperature (F) - 1950-2017 - Dec/Jan/Feb", xlab = "Year", ylab = "Temperature (F)")
mod<-lm(ctna_djf_sum$Temp_Annual_Average_DJF~ctna_djf_sum$Year)
abline(coefficients(mod), lwd=2, lty=2, col="red")

text(2008, 37, paste("annual trend =", round(mod$coefficients[2], 4),  "\n decadal trend =", round(mod$coefficients[2]*10, 4), "\n 1950-2017 trend =", round(mod$coefficients[2]*10*6, 4), "\n p-value =", round(summary(mod)$coefficients[,4][2], 4)))


cpna_djf_sum <- as.data.frame(rowMeans(cpna_djf))
cpna_djf_sum <- cbind(c(1950:2017), cpna_djf_sum)
colnames(cpna_djf_sum) <- c("Year", "Precip_Annual_Average_DJF")
plot(cpna_djf_sum$Year, cpna_djf_sum$Precip_Annual_Average_DJF, pch=16, las=2, main = "Corvallis State University Station \n Annual Precipitation (mm) - 1950-2017 - Dec/Jan/Feb", xlab = "Year", ylab = "Precipitation (mm)")
mod<-lm(cpna_djf_sum$Precip_Annual_Average_DJF~cpna_djf_sum$Year)
abline(coefficients(mod), lwd=2, lty=2, col="red")

text(2008, 12, paste("annual trend =", round(mod$coefficients[2], 4),  "\n decadal trend =", round(mod$coefficients[2]*10, 4), "\n 1950-2017 trend =", round(mod$coefficients[2]*10*6, 4), "\n p-value =", round(summary(mod)$coefficients[,4][2], 4)))

