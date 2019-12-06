
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

readline(prompt="Press [enter] to continue")
dev.off()

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

readline(prompt="Press [enter] to continue")
dev.off()

#--Creating table for 2017
finalnormals <- rbind(cor1_precip_normals_mm_2016, cor1_temp_normals_celsius_2016)
finalnormals <- t(finalnormals)
months <- data.frame(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
finalnormals <- cbind(months, finalnormals)
colnames(finalnormals) <- c("Months", "2017 Precipitation (mm)", "2017 Temperature (C)")
grid.table(finalnormals[,1:3], rows = NULL)

readline(prompt="Press [enter] to continue")
dev.off()


#layout(matrix(c(1,2,3,4,5,6,7,8), 2, 4, byrow = TRUE))

#--Creating climograph for Corvallis, 1981-2010
twoord.plot(c(1:12), cor1_precip_normals_mm, c(1:12), rylim=c(0,20), lylim=c(0,250), cor1_temp_normals_celsius, ylab = "Precipitation (mm)", xticklab=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), rylab = "Temperature (Celsius)", xlab = "Months", type=c("bar", "b"), lcol = "green", rcol = "blue", main = "Climograph for Corvallis State University Station, 1981-2010")

readline(prompt="Press [enter] to continue")


#--Creating climograph for Corvallis, 2016
twoord.plot(c(1:12), cor1_precip_normals_mm_2016, c(1:12), cor1_temp_normals_celsius_2016, lylim=c(0,350), rylim=c(0,25), ylab = "Precipitation (mm)", xticklab=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), rylab = "Temperature (Celsius)", xlab = "Months", type=c("bar", "b"), lcol = "green", rcol = "blue", main = "Climograph for Corvallis State University Station, 2017")

readline(prompt="Press [enter] to continue")
dev.off()

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

readline(prompt="Press [enter] to continue")
dev.off()

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

readline(prompt="Press [enter] to continue")
dev.off()

#--subsetting temp and precipitation for 1950-2017

corvallis_temp_normals_annual <- subset(corvallis_temp, Year >= 1950 & Year <= 2017)
corvallis_temp_normals_annual <- cbind(corvallis_temp_normals_annual[,1],data.frame(fahrenheit.to.celsius(corvallis_temp_normals_annual[,2:13])))
colnames(corvallis_temp_normals_annual) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
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

plot(ctnas$Year, ctnas$Temp_Annual_Average, pch=16, main = "Corvallis State University Station \n Annual Temperature (C) - 1950-2017", las=2, xlab = "Year", ylab = "Temperature (C)")
lines(ctnas)

mod1<-lm(ctnas$Temp_Annual_Average~ctnas$Year)
abline(coefficients(mod1), lwd=2, lty=2, col="red")

text(2008, 10.25, paste("annual trend =", round(mod1$coefficients[2], 4),  "\n decadal trend =", round(mod1$coefficients[2]*10, 4), "\n 1950-2017 trend =", round(mod1$coefficients[2]*10*6, 4), "\n p-value =", round(summary(mod1)$coefficients[,4][2], 7)))

readline(prompt="Press [enter] to continue")
dev.off()

plot(cpnas$Year, cpnas$Precip_Annual_Average, pch=16, main = "Corvallis State University Station \n Annual Precipitation (mm) - 1950-2017", las=2, xlab = "Year", ylab = "Precipitation (mm)")
lines(cpnas)

mod2<-lm(cpnas$Precip_Annual_Average~cpnas$Year)
abline(coefficients(mod2), lwd=2, lty=2, col="red")

text(1961, 2.5, paste("annual trend =", round(mod2$coefficients[2], 4),  "\n decadal trend =", round(mod2$coefficients[2]*10, 4), "\n 1950-2017 trend =", round(mod2$coefficients[2]*10*6, 4), "\n p-value =", round(summary(mod2)$coefficients[,4][2], 7)))

readline(prompt="Press [enter] to continue")
dev.off()

#---june.july.aug and dec.jan.feb

corvallis_temp_normals_annual <- subset(corvallis_temp, Year >= 1950 & Year <= 2017)
corvallis_temp_normals_annual <- cbind(corvallis_temp_normals_annual[,1],data.frame(fahrenheit.to.celsius(corvallis_temp_normals_annual[,2:13])))

ctna_jja <- cbind(corvallis_temp_normals_annual$Jun, corvallis_temp_normals_annual$Jul, corvallis_temp_normals_annual$Aug)
ctna_djf <- cbind(corvallis_temp_normals_annual$Dec, corvallis_temp_normals_annual$Jan, corvallis_temp_normals_annual$Feb)


corvallis_precip_normals_annual <- subset(corvallis_precip, Year >= 1950 & Year <= 2017)
cpna_jja <- cbind(corvallis_precip_normals_annual$Jun, corvallis_precip_normals_annual$Jul, corvallis_precip_normals_annual$Aug)
cpna_djf <- cbind(corvallis_precip_normals_annual$Dec, corvallis_precip_normals_annual$Jan, corvallis_precip_normals_annual$Feb)

ctna_jja_sum <- as.data.frame(rowMeans(ctna_jja))
ctna_jja_sum <- cbind(c(1950:2017), ctna_jja_sum)
colnames(ctna_jja_sum) <- c("Year", "Temp_Annual_Average_JJA")
plot(ctna_jja_sum$Year, ctna_jja_sum$Temp_Annual_Average_JJA, main = "Corvallis State University Station \n Annual Temperature (C) - 1950-2017 - June/July/August", pch=16, las=2, xlab = "Year", ylab = "Temperature (C)")
lines(ctna_jja_sum)
mod3<-lm(ctna_jja_sum$Temp_Annual_Average_JJA~ctna_jja_sum$Year)
abline(coefficients(mod3), lwd=2, lty=2, col="red")

text(2008, 17, paste("annual trend =", round(mod3$coefficients[2], 4),  "\n decadal trend =", round(mod3$coefficients[2]*10, 4), "\n 1950-2017 trend =", round(mod3$coefficients[2]*10*6, 4), "\n p-value =", round(summary(mod3)$coefficients[,4][2], 7)))

readline(prompt="Press [enter] to continue")
dev.off()

cpna_jja_sum <- as.data.frame(rowMeans(cpna_jja))
cpna_jja_sum <- cbind(c(1950:2017), cpna_jja_sum)
colnames(cpna_jja_sum) <- c("Year", "Precip_Annual_Average_JJA")
plot(cpna_jja_sum$Year, cpna_jja_sum$Precip_Annual_Average_JJA, main = "Corvallis State University Station \n Annual Precipitation (mm) - 1950-2017 - June/July/August", pch=16, las=2, xlab = "Year", ylab = "Precipitation (mm)")
lines(cpna_jja_sum)

mod4<-lm(cpna_jja_sum$Precip_Annual_Average_JJA~cpna_jja_sum$Year)
abline(coefficients(mod4), lwd=2, lty=2, col="red")

text(2008, 2, paste("annual trend =", round(mod4$coefficients[2], 4),  "\n decadal trend =", round(mod4$coefficients[2]*10, 4), "\n 1950-2017 trend =", round(mod4$coefficients[2]*10*6, 4), "\n p-value =", round(summary(mod4)$coefficients[,4][2], 4)))

readline(prompt="Press [enter] to continue")
dev.off()


ctna_djf_sum <- as.data.frame(rowMeans(ctna_djf))
ctna_djf_sum <- cbind(c(1950:2017), ctna_djf_sum)
colnames(ctna_djf_sum) <- c("Year", "Temp_Annual_Average_DJF")
plot(ctna_djf_sum$Year, ctna_djf_sum$Temp_Annual_Average_DJF, pch=16, las=2, main = "Corvallis State University Station \n Annual Temperature (C) - 1950-2017 - Dec/Jan/Feb", xlab = "Year", ylab = "Temperature (C)")
lines(ctna_djf_sum)

mod5<-lm(ctna_djf_sum$Temp_Annual_Average_DJF~ctna_djf_sum$Year)
abline(coefficients(mod5), lwd=2, lty=2, col="red")

text(2008, 3, paste("annual trend =", round(mod5$coefficients[2], 4),  "\n decadal trend =", round(mod5$coefficients[2]*10, 4), "\n 1950-2017 trend =", round(mod5$coefficients[2]*10*6, 4), "\n p-value =", round(summary(mod5)$coefficients[,4][2], 4)))

readline(prompt="Press [enter] to continue")
dev.off()


cpna_djf_sum <- as.data.frame(rowMeans(cpna_djf))
cpna_djf_sum <- cbind(c(1950:2017), cpna_djf_sum)
colnames(cpna_djf_sum) <- c("Year", "Precip_Annual_Average_DJF")
plot(cpna_djf_sum$Year, cpna_djf_sum$Precip_Annual_Average_DJF, pch=16, las=2, main = "Corvallis State University Station \n Annual Precipitation (mm) - 1950-2017 - Dec/Jan/Feb", xlab = "Year", ylab = "Precipitation (mm)")
lines(cpna_djf_sum)

mod6<-lm(cpna_djf_sum$Precip_Annual_Average_DJF~cpna_djf_sum$Year)
abline(coefficients(mod6), lwd=2, lty=2, col="red")

text(2008, 12, paste("annual trend =", round(mod6$coefficients[2], 4),  "\n decadal trend =", round(mod6$coefficients[2]*10, 4), "\n 1950-2017 trend =", round(mod6$coefficients[2]*10*6, 4), "\n p-value =", round(summary(mod6)$coefficients[,4][2], 4)))

readline(prompt="Press [enter] to continue")
dev.off()

#--table summarizing trends
C1 <- data.frame(c("Temperature (C/Decade)", "Precipitation (mm/Decade)"))
C2 <- data.frame(c(round(mod1$coefficients[2]*10, 4), round(mod2$coefficients[2]*10, 4)))
C3 <- data.frame(c(round(mod5$coefficients[2]*10, 4), round(mod6$coefficients[2]*10, 4)))
C4 <- data.frame(c(round(mod3$coefficients[2]*10, 4), round(mod4$coefficients[2]*10, 4)))

PC1 <- data.frame(c("P-value Temperature (C/Decade)", "P-value Precipitation (mm/Decade)"))
PC2 <- data.frame(c(round(summary(mod1)$coefficients[,4][2], 7), round(summary(mod2)$coefficients[,4][2], 7)))
PC3 <- data.frame(c(round(summary(mod5)$coefficients[,4][2], 7), round(summary(mod6)$coefficients[,4][2], 7)))
PC4 <- data.frame(c(round(summary(mod3)$coefficients[,4][2], 7), round(summary(mod4)$coefficients[,4][2], 7)))

#Description <- data.frame(c("Description", "Annual", "Winter", "Summer"))

Desc2 <- cbind(C1, C2, C3, C4)
Desc3 <- cbind(PC1, PC2, PC3, PC4)
colnames(Desc3) <- c("Description", "P-Values Annual", "P-Values Winter", "P-Values Summer")

colnames(Desc2) <- c("Description", "Annual", "Winter", "Summer")
#rownames(Desc2) <- c("Description", "Temperature (C/Decade)", "Precipitation (mm/Decade)")

Desc2[1,2] <- paste(Desc2[1,2], "*", sep="")
Desc2[1,4] <- paste(Desc2[1,4], "*", sep="")
#plotting description table
grid.table(Desc2, rows = NULL)

readline(prompt="Press [enter] to continue")
dev.off()

grid.table(Desc3, rows = NULL)


#--climate detective aacs

#subset precip and temp for 1948-2017

corvallis_temp_detective <- subset(corvallis_temp, Year >= 1948 & Year <= 2017)
corvallis_precip_detective <- subset(corvallis_precip, Year >= 1948 & Year <= 2017)

#--precip conversion to mm

cor1_precip_d <- data.frame(corvallis_precip_detective[,2:13])
#cor1_precip_d2 <- colMeans(cor1_precip_d)
cor1_precip_d2_mm <- cor1_precip_d * 25.4

cor1_precip_d2_mm_final <- cbind(corvallis_precip_detective[,1], cor1_precip_d2_mm)
colnames(cor1_precip_d2_mm_final) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

#--temp conversion to C


cor1_temp_d <- data.frame(corvallis_temp_detective[,2:13])
#cor1_temp_d2 <- colMeans(cor1_temp_d)
cor1_temp_d2_celsius <- data.frame(fahrenheit.to.celsius(cor1_temp_d))

cor1_temp_d2_celsius_final <- cbind(corvallis_temp_detective[,1], cor1_temp_d2_celsius)
colnames(cor1_temp_d2_celsius_final) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


#--assembly of jfma data for precip and temp

cor1_temp_jfma <- cbind(cor1_temp_d2_celsius_final$Jan, cor1_temp_d2_mm_final$Feb, cor1_temp_d2_mm_final$Mar, cor1_temp_d2_mm_final$Apr)
colnames(cor1_temp_jfma) <- c("Jan", "Feb", "Mar", "Apr")
cor1_temp_jfma_final <- cbind(corvallis_temp_detective[,1], cor1_temp_jfma)
average_temp <- rowMeans(cor1_temp_jfma_final[,2:5])
average_temp2 <- cbind(cor1_temp_jfma_final[,1], average_temp)

cor1_precip_jfma <- cbind(cor1_precip_d2_mm_final$Jan, cor1_precip_d2_mm_final$Feb, cor1_precip_d2_mm_final$Mar, cor1_precip_d2_mm_final$Apr)
colnames(cor1_precip_jfma) <- c("Jan", "Feb", "Mar", "Apr")
cor1_precip_jfma_final <- cbind(corvallis_temp_detective[,1], cor1_precip_jfma)
average_precip <- rowMeans(cor1_precip_jfma_final[,2:5])
average_precip2 <- cbind(cor1_precip_jfma_final[,1], average_precip)

#determine top three years, warmest, coldest, wettest, driest

sorted_average_temp <- average_temp2[order(average_temp),] 

sorted_average_precip <- average_precip2[order(average_precip),] 

wettest <- sorted_average_precip[68:70,]
wettest <- wettest[ nrow(wettest):1, ]

driest <- sorted_average_precip[1:3,]
coldest <- sorted_average_temp[1:3,]

warmest <- sorted_average_temp[68:70,]
warmest <- warmest[ nrow(warmest):1, ]



normals_temp_jfma <- mean(cor1_temp_normals_celsius[1:4])

normals_precip_jfma <- mean(cor1_precip_normals_mm[1:4])

wettest_anomaly <- cbind(wettest[,1], data.frame(wettest[,2] / normals_precip_jfma)*100)
colnames(wettest_anomaly) <- c("Year", "Anomaly")

driest_anomaly <- cbind(driest[,1], data.frame(driest[,2] / normals_precip_jfma)*100)
colnames(driest_anomaly) <- c("Year", "Anomaly")

coldest_anomaly <- cbind(coldest[,1], data.frame(coldest[,2] - normals_temp_jfma))
colnames(coldest_anomaly) <- c("Year", "Anomaly")


warmest_anomaly <- cbind(warmest[,1], data.frame(warmest[,2] - normals_temp_jfma))
colnames(warmest_anomaly) <- c("Year", "Anomaly")

jfma_table <- cbind(warmest_anomaly, coldest_anomaly, wettest_anomaly, driest_anomaly)



#---aacs 4



solarflux <- read.csv("/dmine/code/git/clim401/solarflux.csv", header = TRUE)
sp500 <- read.csv("/dmine/code/git/clim401/s_and_p_500.csv", header = TRUE)
elnino3_4 <- read.csv("/dmine/code/git/clim401/elnino3_4.csv", header = TRUE)

#remove commas from sp500
sp500$jan <- as.numeric(gsub(",","",sp500$jan))

colnames(solarflux) <- c("year", "average")
colnames(elnino3_4) <- c("year", "average")
colnames(sp500) <- c("year", "average")

threepredictors <- as.data.frame(cbind(solarflux$average, elnino3_4$average, sp500$average))

#average_temp2 is jfma average temp for 1948-2017
#average_precip2 is jfma average precip for 1948-2017

average_temp3 <- as.data.frame(average_temp2)
colnames(average_temp3) <- c("year", "average")

average_precip3 <- as.data.frame(average_precip2)
colnames(average_precip3) <- c("year", "average")

#average_temp3_80_17 is average jfma temp for 1980 - 2017
#average_precip3_80_17 is average jfma precip for 1980-2017
average_temp3_80_17 <- subset(average_temp3, year >= 1980)
average_precip3_80_17 <- subset(average_precip3, year >= 1980)

#temp vs solarflux

temp_solarflux <- as.data.frame(cbind(average_temp3_80_17$year, threepredictors$V1, average_temp3_80_17$average))
colnames(temp_solarflux) <- c("year", "solarflux", "temp")


plot(temp_solarflux$solarflux, temp_solarflux$temp, col = "blue", main = "Corvallis State University Station, OR \n JFMA Temperature vs Solar Flux - 1980 - 2017", pch=16, las=2, xlab = "Solar Flux (w/m2)", ylab = "Temperature (Celsius)")

text(temp_solarflux$solarflux, temp_solarflux$temp,labels=temp_solarflux$year, pos = 1)


mod4<-lm(temp_solarflux$temp~temp_solarflux$solarflux)
abline(coefficients(mod4), lwd=2, lty=2, col="red")
correlation <- cor(temp_solarflux$temp, temp_solarflux$solarflux,method="pearson")

text(1361.9, 8.5, paste("pearson's correlation =", round(correlation, 4),  "\n p-value =", round(summary(mod4)$coefficients[,4][2], 4)))

#--temp vs sp500

temp_solarflux <- as.data.frame(cbind(average_temp3_80_17$year, threepredictors$V3, average_temp3_80_17$average))
colnames(temp_solarflux) <- c("year", "sp500", "temp")


plot(temp_solarflux$sp500, temp_solarflux$temp, col = "blue", main = "Corvallis State University Station, OR \n JFMA Temperature vs S & P 500 - 1980 - 2017", pch=16, las=2, xlab = "S & P 500 Index", ylab = "Temperature (Celsius)")

text(temp_solarflux$sp500, temp_solarflux$temp,labels=temp_solarflux$year, pos = 1)


mod4<-lm(temp_solarflux$temp~temp_solarflux$sp500)
abline(coefficients(mod4), lwd=2, lty=2, col="red")
correlation <- cor(temp_solarflux$temp, temp_solarflux$sp500,method="pearson")

text(2000, 8.5, paste("pearson's correlation =", round(correlation, 4),  "\n p-value =", round(summary(mod4)$coefficients[,4][2], 4)))


#--temp vs elnino

temp_solarflux <- as.data.frame(cbind(average_temp3_80_17$year, threepredictors$V2, average_temp3_80_17$average))
colnames(temp_solarflux) <- c("year", "elnino", "temp")


plot(temp_solarflux$elnino, temp_solarflux$temp, col = "blue", main = "Corvallis State University Station, OR \n JFMA Temperature vs El Nino 3.4 Index - 1980 - 2017", pch=16, las=2, xlab = "El Nino 3.4 Index", ylab = "Temperature (Celsius)")

text(temp_solarflux$elnino, temp_solarflux$temp,labels=temp_solarflux$year, pos = 1)


mod4<-lm(temp_solarflux$temp~temp_solarflux$elnino)
abline(coefficients(mod4), lwd=2, lty=2, col="red")
correlation <- cor(temp_solarflux$temp, temp_solarflux$elnino,method="pearson")

text(2, 7, paste("pearson's correlation =", round(correlation, 4),  "\n p-value =", round(summary(mod4)$coefficients[,4][2], 6)))

#precip vs solarflux

temp_solarflux <- as.data.frame(cbind(average_precip3_80_17$year, threepredictors$V1, average_precip3_80_17$average))
colnames(temp_solarflux) <- c("year", "solarflux", "precip")


plot(temp_solarflux$solarflux, temp_solarflux$precip, col = "blue", main = "Corvallis State University Station, OR \n JFMA Precipitation vs Solar Flux - 1980 - 2017", pch=16, las=2, xlab = "Solar Flux (w/m2)", ylab = "Precipitation (mm)")

text(temp_solarflux$solarflux, temp_solarflux$precip,labels=temp_solarflux$year, pos = 1)


mod4<-lm(temp_solarflux$precip~temp_solarflux$solarflux)
abline(coefficients(mod4), lwd=2, lty=2, col="red")
correlation <- cor(temp_solarflux$precip, temp_solarflux$solarflux,method="pearson")

text(1362, 200, paste("pearson's correlation =", round(correlation, 4),  "\n p-value =", round(summary(mod4)$coefficients[,4][2], 4)))

#--precip vs sp500

temp_solarflux <- as.data.frame(cbind(average_precip3_80_17$year, threepredictors$V3, average_precip3_80_17$average))
colnames(temp_solarflux) <- c("year", "sp500", "precip")


plot(temp_solarflux$sp500, temp_solarflux$precip, col = "blue", main = "Corvallis State University Station, OR \n JFMA Precipitation vs S & P 500 - 1980 - 2017", pch=16, las=2, xlab = "S & P 500 Index", ylab = "Precipitation (mm)")

text(temp_solarflux$sp500, temp_solarflux$precip,labels=temp_solarflux$year, pos = 1)


mod4<-lm(temp_solarflux$precip~temp_solarflux$sp500)
abline(coefficients(mod4), lwd=2, lty=2, col="red")
correlation <- cor(temp_solarflux$precip, temp_solarflux$sp500,method="pearson")

text(2000, 210, paste("pearson's correlation =", round(correlation, 4),  "\n p-value =", round(summary(mod4)$coefficients[,4][2], 4)))


#--precip vs elnino

temp_solarflux <- as.data.frame(cbind(average_precip3_80_17$year, threepredictors$V2, average_precip3_80_17$average))
colnames(temp_solarflux) <- c("year", "elnino", "precip")


plot(temp_solarflux$elnino, temp_solarflux$precip, col = "blue", main = "Corvallis State University Station, OR \n JFMA Precipitation vs El Nino 3.4 - 1980 - 2017", pch=16, las=2, xlab = "El Nino 3.4 Index", ylab = "Precipitation (mm)")

text(temp_solarflux$elnino, temp_solarflux$precip,labels=temp_solarflux$year, pos = 1)


mod4<-lm(temp_solarflux$precip~temp_solarflux$elnino)
abline(coefficients(mod4), lwd=2, lty=2, col="red")
correlation <- cor(temp_solarflux$precip, temp_solarflux$elnino,method="pearson")

text(1.5, 200, paste("pearson's correlation =", round(correlation, 4),  "\n p-value =", round(summary(mod4)$coefficients[,4][2], 6)))
