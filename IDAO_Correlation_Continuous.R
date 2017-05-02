## Correlations between continuous time series

# This is an analysis to examine whether the number of steps taken on a given day has
# an association with the minutes asleep

rm(list=ls()) # Clear memory
graphics.off() # Clears graphics
library(forecast) # Needed to run forecast and auto.arima functions
library(astsa) # To run acf
library(psych) # To perform summary stats by group (needed for describeBy function)
library(stringr) # Load file to match strings for device type (install.packages("stringr") to use)
#install.packages("orcutt")
library(orcutt)
#install.packages("tseries")
library(tseries)
#install.packages("vars")
library(vars)
#install.packages("fracdiff")
library(fracdiff)

# Load data
full_data <- read.csv("~/Syncplicity/Science_files/Ongoing_active_projects/Individualized_Data_Analysis_Organization/IDAO_Correlation_Continuous/Mike_Fitbit_data.csv", as.is = TRUE, header = TRUE)
full_data$Date <- as.Date(full_data$Date)
attach(full_data)

# Basic plot of minutes asleep
plot(Date, Minutes.Asleep, type="l", main="Minutes Asleep per Day")
segments(min(Date), mean(Minutes.Asleep), max(Date), mean(Minutes.Asleep), col="red", lwd=2)
abline((linear=lm(Minutes.Asleep~Date)), col="blue", lwd=2)
lines(lowess(Date, Minutes.Asleep, f=1/6), col="green", lwd=2)
legend(min(Date), mean(Minutes.Asleep), #Location
       c("Lowess Smooth", paste("Mean (=", round(mean(Minutes.Asleep),2)," min/day)"), paste("Linear Fit (Slope=", round(linear$coefficients[2], 4), "min/day)")), #Text
       col=c("green", "red", "blue"), #Line colors
       lty=c(1, 1, 1), #Line types
       lwd=c(2.5, 2.5, 2.5), #Line thickness
       bty="n", #No border ("o" if border)
       cex=1.3, #Text size
       y.intersp=0.85 #Spacing between text/lines
) 

# Time Series Diagnostics
# ACF plots
acf2(ts(Minutes.Asleep))

# Frequency plot
x = diff(Minutes.Asleep)
FF = abs(fft(x)/sqrt(length(x)))^2  # Need to square and normalize raw fft output
P = (4/(length(FF)))*FF[1:(((length(FF))/2)+1)] # Periodogram values (can scale by multiplying times 4/n)
f = (0:(length(FF)/2))/length(FF) # Harmonic frequencies (from 0 to 0.5)
P.filter = filter(P, c(rep(0.2, 5)), side=2) # Linear filter for plot
#P.filter = lowess(f, P, f=1/15)
plot(f, P, type="l", main="Spectral Plot", xlab="Frequency", ylab="Periodogram")
lines(f, P.filter$y, col="red")
# Quantitatively identify period/cycle
Spect <- as.data.frame(cbind(f, P))
Spect <- Spect[order(Spect$P, decreasing=TRUE)]  # Sort Spectrum from highest to lowest
predcycle <- Spect[Spect$P==max(Spect$P),] # Find frequency where scaled Periodogram is maximal
(Period = 1/predcycle$f) # Identify cycle/period
abline(v=predcycle$f, col="blue")
abline(v=1/7, col="green")
abline(v=1/14, col="orange")
abline(v=1/30, col="purple")
legend(0, max(P), #Location
       c(paste("Dominant frequency (Period=", round(Period, 2), ")"), "Weekly", "Bi-weekly", "Monthly"), #Text
       col=c("blue", "green", "orange", "purple"), #Line colors
       lty=c(1,1,1,1), #Line types
       lwd=c(2.0, 2.0, 2.0, 2.0), #Line thickness
       bty="o", #No border ("o" if border)
       cex=0.9, #Text size
       y.intersp=0.9 #Spacing between text/lines
) 

# Basic model with seasonality based on dominant peak
(model1 <- auto.arima(ts(Minutes.Asleep, freq = 2)))

