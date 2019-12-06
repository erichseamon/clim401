##microclimates.R
#--final homework for geog401 

tmax <- read.csv("/dmine/code/git/clim401/microclimates_data/GEOG401_FieldData_tmax.csv", header = TRUE)
tmin <- read.csv("/dmine/code/git/clim401/microclimates_data/GEOG401_FieldData_tmin.csv", header = TRUE)
meta <- read.csv("/dmine/code/git/clim401/microclimates_data/GEOG401_FieldData_metadata.csv", header = TRUE)
hourly <- read.csv("/dmine/code/git/clim401/microclimates_data/GEOG401_FieldData_hourly.csv", header = TRUE)

meta <- data.frame(meta)

#1. daily average high over all days

tmax_mean <- colMeans(tmax[,2:9])
tmin_mean <- colMeans(tmin[2:9])


colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMin <- function(data) sapply(data, min, na.rm = TRUE)
stationmax <- data.frame(colMax(tmax[2:9]))
colnames(stationmax) <- "max"
stationmin <- data.frame(colMin(tmin[2:9]))
colnames(stationmin) <- "min"



table1 <- cbind(meta, tmax_mean, tmin_mean, stationmax, stationmin)
table1 <- table1[,2:10]
colnames(table1)[3] <- "Elevation"
colnames(table1)[4] <- "Radiation Loading"
colnames(table1)[5] <- "TMI"

table1 <- t(table1)
table2 <- cbind(meta, tmax_mean, tmin_mean)

write.csv(table1, "/tmp/table1.csv")

grid.table(table1)

colnames(table2) <- c("site", "lat", "lon", "elevation", "radiation_loading", "TMI", "max_mean", "min_mean" )
table2_mystery <- data.frame(table2)
table2 <- table2[1:7,]

#scatterplots

plot(table2$max_mean, table2$radiation_loading, ylab = "April Radiation Loading (W/m2)", xlab = "Average Max Temperature per site (42 days)", ylim = c(180, 220), pch = c(1:7), main = "Average Max Temperature per site vs \n April Radiation Loading")

mod2<-lm(table2$radiation_loading~table2$max_mean)
abline(coefficients(mod2), lwd=2, lty=2, col="red")
correlation <- cor(table2$max_mean, table2$radiation_loading, method="pearson")
text(5, 210, paste("pearson's correlation =", round(correlation, 4),  "\n p-value =", round(summary(mod2)$coefficients[,4][2], 17)))
legend("bottomright", inset=.05, title="Sites",
       legend = table2$site, pch=c(1:7), horiz=FALSE)



plot(table2$min_mean, table2$TMI,  ylab = "Topo Convergence Index (25m Composite)", xlab = "Average Min Temperature per site (42 days)", ylim = c(1.0, 4.5), pch = c(1:7), main = "Average Min Temperature per site vs TMI")

mod2<-lm(table2$TMI~table2$min_mean)
abline(coefficients(mod2), lwd=2, lty=2, col="red")
correlation <- cor(table2$min_mean, table2$TMI, method="pearson")
text(-1,4, paste("pearson's correlation =", round(correlation, 4),  "\n p-value =", round(summary(mod2)$coefficients[,4][2], 17)))
legend("left", inset=.05, title="Sites",
       legend = table2$site, pch=c(1:7), horiz=FALSE)


plot(table2$max_mean, table2$elevation, ylab = "Elevation (m)", xlab = "Average Max Temperature per site (42 days)", ylim = c(900, 1500), pch = c(1:7), main = "Average Max Temperature per site vs Elevation (m)")

mod2<-lm(table2$elevation~table2$max_mean)
abline(coefficients(mod2), lwd=2, lty=2, col="red")
correlation <- cor(table2$max_mean, table2$elevation, method="pearson")
text(5, 1000, paste("pearson's correlation =", round(correlation, 4),  "\n p-value =", round(summary(mod2)$coefficients[,4][2], 17)))
legend("top", inset=.05, title="Sites",
       legend = table2$site, pch=c(1:7), horiz=FALSE)


plot(table2$min_mean, table2$elevation, ylab = "Elevation (m)", xlab = "Average Min Temperature per site (42 days)", ylim = c(900, 1500), pch = c(1:7), main = "Average Min Temperature per site vs Elevation (m)")

mod2<-lm(table2$elevation~table2$min_mean)
abline(coefficients(mod2), lwd=2, lty=2, col="red")
correlation <- cor(table2$min_mean, table2$elevation, method="pearson")
text(-0.1, 1400, paste("pearson's correlation =", round(correlation, 4),  "\n p-value =", round(summary(mod2)$coefficients[,4][2], 17)))
legend("bottomleft", inset=.05, title="Sites",
       legend = table2$site, pch=c(1:7), horiz=FALSE)

par(mfrow=c(2,1)) 

plot(tmax$big.kahuna, pch = 1, las = 3, ylim = c(-3, 16), ylab = "Daily Max T (C)", xlab = "Daily Timespan - March 12th to April 23rd, 2018")

lines(tmax$big.kahuna, col = "blue")
lines(tmax$mud, col = "red")
lines(tmax$wet.foot, col = "green")
lines(tmax$little.kahuna, col = "orange")
lines(tmax$vodka, col = "magenta")
lines(tmax$david, col = "black")
lines(tmax$SNOTEL, col = "brown")


plot(tmin$big.kahuna, pch = 1, las = 3, ylim = c(-7, 6), ylab = "Daily Min T (C)", xlab = "Daily Timespan - March 12th to April 23rd, 2018")

lines(tmin$big.kahuna, col = "blue")
lines(tmin$mud, col = "red")
lines(tmin$wet.foot, col = "green")
lines(tmin$little.kahuna, col = "orange")
lines(tmin$vodka, col = "magenta")
lines(tmin$david, col = "black")
lines(tmin$SNOTEL, col = "brown")

#legend("bottomleft", inset=.05, title="Sites",
#       legend = table2$site, horiz=FALSE, lty=c(1,1,1,1,1,1,1), lwd=c(2.5,2.5, 2.5, 2.5, 2.5, 2.5, 2.5),col=c("blue", "red", "green", "orange", "magenta", "black", "brown"))





