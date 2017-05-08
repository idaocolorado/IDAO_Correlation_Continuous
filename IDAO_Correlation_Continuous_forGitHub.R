## Correlations between continuous time series
# This is an analysis to examine the relationship between sleep and activity from a Fitbit

# Automation of continuous correlation analysis
# Takes two files from Fitbit Dashboard: "Activity.csv" and "Sleep.csv"
# Input the output file location as outFileLocation, or defaults to current working directory

auto.continuous.correl <- function(activityData, sleepData, fileNameRoot = getwd()) {

# Clear memory and graphics
rm(list=ls()) # Clear memory
graphics.off() # Clears graphics

# Error handling: Unable to read file data or wrong file data
tryCatch(
  { # Attempt to read file entered
    fitbit_activity <- read.csv(activityData, as.is = TRUE, header = TRUE)
    print("Successfully loaded Activity data")
  }, 
  warning = function(w) 
    {
    print() # Dummy variable to suppress output of warning
  },
  error = function(err)
  {
    stop("Unable to read Activity data file")
  })

tryCatch(
  { # Attempt to read file entered
    fitbit_sleep <- read.csv(sleepData, as.is = TRUE, header = TRUE)
    print("Successfully loaded Sleep data")
  }, 
  warning = function(w) 
  {
    print() # Dummy variable to suppress output of warning
  },
  error = function(err)
  {
    stop("Unable to read sleep data file")
  })
  
tryCatch(
  { # Attempt to merge files into one main file
    fitbit <- merge(fitbit_sleep, fitbit_activity, by="Date")
    print("Successfully merged files")
  },
  warning = function(w)
  {
    print()
  },
  error = function(err)
  {stop("Unable to merge files")
  })

# Load requisite packages
tryCatch(
  {
    library(forecast) # Needed to run forecast and auto.arima functions
    print("Successfully loaded forecast package")
  },
  warning = function(w)
  {
    print()
  }, 
  error = function(err)
  { 
    print("Forecast package not installed, attempting to install from internet")
    tryCatch(
      {
        install.packages("forecast")
      },
      warning = function(w)
      {print()
      },
      error = function(err)
      {stop("Unable to install forecast package from internet")
      })
  })

tryCatch(
  {
    library(astsa) # To run acf
    print("Successfully loaded astsa package")
  },
  warning = function(w)
  {
    print()
  }, 
  error = function(err)
  { 
    print("Astsa package not installed, attempting to install from internet")
    tryCatch(
      {
        install.packages("astsa")
      },
      warning = function(w)
      {print()
      },
      error = function(err)
      {stop("Unable to install astsa package from internet")
      })
  })



############################################################



# Remove technical outliers where not wearing (either make = mean or zero)
# Create indicator based on time
fitbit$timetotal <- fitbit$Minutes.Sedentary + fitbit$Minutes.Lightly.Active + fitbit$Minutes.Fairly.Active + fitbit$Minutes.Very.Active + fitbit$Time.in.Bed
fitbit$perc.recorded <- fitbit$timetotal/(max(fitbit$timetotal, na.rm=TRUE))

#Remove fitbit recordings with less than 70% of time captured
fitbit <- subset(fitbit, fitbit$perc.recorded >= 0.7)

# Create variable for quality of sleep
fitbit$sleepquality <- fitbit$Minutes.Asleep/fitbit$Time.in.Bed
attach(fitbit)

#############################################################
# Examine relationships of multiple variables
data <- as.data.frame(cbind(fitbit$Minutes.Asleep, fitbit$Steps, fitbit$Minutes.Very.Active))
colnames(data) <- c("Sleep", "Steps", "Very Active")

# Create graph of time series
fileName1 = paste0(fileNameRoot, "/ThreeTSPlot.jpeg")
jpeg(filename=fileName1, width = 800, height = 600, quality=90)
plot.ts(data, main="Plot of all three time series") # TS plots of three variables
dev.off()

# Auto-ARIMA for Random-Fixed models
(model1 <- auto.arima(Minutes.Asleep, xreg = Minutes.Very.Active))
(model2 <- auto.arima(Minutes.Asleep, xreg = Steps))
(model3 <- auto.arima(Steps, xreg = Minutes.Asleep))
(model4 <- auto.arima(Minutes.Very.Active, xreg = Minutes.Asleep))
(model5 <- auto.arima(Steps, xreg = Minutes.Very.Active))

# Obtain statistics
getStats <- function(model1) {
model_coef <- round(tail(model1$coef, n=1), 4) # Name coefficient variable 
model1$se <- sqrt(diag(model1$var.coef)) # Calculate standard error as diag of cov matrix
model_se <-round(tail(model1$se, n=1), 4)
model1$tstat <- round(abs(model1$coef/(model1$se)),2) # Calculate the t statistic
model_tstat <- round(tail(model1$tstat, n=1), 4)
model1$signif <- tail(model1$tstat >= 2, n=1)
model_signif <- model1$signif
return(data.frame(model_coef, model_se, model_tstat, model_signif))
}

getStats(model1)
getStats(model2)
getStats(model3)
getStats(model4)
getStats(model5)


## Cross correlation graph
fileName2 = paste0(fileNameRoot, "/ccfPlot.jpeg")
jpeg(filename=fileName2, width = 800, height = 600, quality=90)
par(mfrow=c(3,1))
ccf(Steps, Minutes.Asleep, 7)
ccf(Minutes.Very.Active, Minutes.Asleep, 7)
ccf(Minutes.Very.Active, Steps, 7)
dev.off()

SleepTS = ts(Minutes.Asleep)
StepsTS = ts(Steps)
ActiveTS = ts(Minutes.Very.Active)

Steps1 = lag(StepsTS, -1)
data1 <- ts.intersect(SleepTS, Steps1)
(model6 <- lm(SleepTS~Steps1, data = data1))
summary(model6)

fileName3 = paste0(fileNameRoot, "/lag2Plot.jpeg")
jpeg(filename=fileName3, width = 800, height = 600, quality=90)
lag2.plot(Steps, Minutes.Asleep, 3)
dev.off()

# Check nonlinearity-- Have to lag manually to check quadratics
StepsMan = head(StepsTS, (length(StepsTS) - 1))
SleepMan = SleepTS[-1]
StepsMansq = StepsMan^2

model7 <- lm(SleepMan~StepsMan+StepsMansq)
summary(model7)


## Plot predicted based on steps day before
priorSteps = seq(6000, 20000, 500)
priorStepsSq = priorSteps^2
predictedFit <- predict(model7, data.frame(StepsMan = priorSteps, StepsMansq = priorStepsSq), 
                          se.fit = TRUE,
                          interval = "confidence",
                          level = 0.95)

par(mfrow=c(1,1))
fileName4 = paste0(fileNameRoot, "/predictedSleep.jpeg")
jpeg(filename=fileName4, width = 800, height = 600, quality=90)
plot(priorSteps, predictedFit$fit[,1], type="l", 
     main = "Amount of Sleep Expected after Daily Steps",
     xlab = "Steps Day Before",
     ylab = "Minutes of Sleep", 
     ylim = c(360, 520))
lines(priorSteps, predictedFit$fit[,2], col = "blue", lty="dashed")
lines(priorSteps, predictedFit$fit[,3], col = "blue", lty="dashed")
dev.off()

################################################################
# Find Seasonality
# Frequency plot

fileName5 = paste0(fileNameRoot, "/frequencySearch.jpeg")
jpeg(filename=fileName5, width = 800, height = 600, quality=90)
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
dev.off()


## Forecast model with seasonality
model8 <- auto.arima(ts(SleepMan, freq = 7), xreg=StepsMan)
summary(model8)

# Plot forecast based on different numbers of steps beforehand
auto.fore.8k <- forecast(model8, xreg=rep(8000, 30), h=30)
auto.fore.10k <- forecast(model8, xreg=rep(10000, 30), h=30)
auto.fore.12k <- forecast(model8, xreg=rep(12000, 30), h=30)
auto.fore.14k <- forecast(model8, xreg=rep(14000, 30), h=30)
auto.fore.16k <- forecast(model8, xreg=rep(16000, 30), h=30)
auto.fore.18k <- forecast(model8, xreg=rep(18000, 30), h=30)
auto.fore.20k <- forecast(model8, xreg=rep(20000, 30), h=30)

Date <- as.Date(Date)
Date.fore <- c(Date, max(Date)+c(1:29))
model.fore.mean <- c(auto.fore.12k$x, auto.fore.12k$mean)
model.fore.8k <- c(rep("NA", length(auto.fore.8k$x)), auto.fore.8k$mean)
model.fore.10k <- c(rep("NA", length(auto.fore.10k$x)), auto.fore.10k$mean)
model.fore.14k <- c(rep("NA", length(auto.fore.14k$x)), auto.fore.14k$mean)
model.fore.16k <- c(rep("NA", length(auto.fore.16k$x)), auto.fore.16k$mean)
model.fore.18k <- c(rep("NA", length(auto.fore.18k$x)), auto.fore.18k$mean)
model.fore.20k <- c(rep("NA", length(auto.fore.20k$x)), auto.fore.20k$mean)


#Graph
fileName6 = paste0(fileNameRoot, "/forecastSleep.jpeg")
jpeg(filename=fileName6, width = 800, height = 600, quality=90)
plot(Date.fore, model.fore.mean, type="l", main="Predicted Minutes of Sleep after Steps Day Before", 
     xlab="Date", 
     ylab="Minutes Asleep", 
     ylim = c(350, 550))
lines(Date.fore, model.fore.8k, col="blue")
lines(Date.fore, model.fore.10k, col="red")
lines(Date.fore, model.fore.14k, col="brown")
lines(Date.fore, model.fore.16k, col="pink")
lines(Date.fore, model.fore.18k, col="orange")
lines(Date.fore, model.fore.20k, col="purple")

legend(min(Date.fore), 550, xjust=0, yjust=1,#Location
       c("12,000 Steps", "8,000 Steps", "10,000 Steps", "14,000 Steps", "16,000 Steps", "18,000 Steps", "20,000 Steps"),  #Text
       col=c("black", "blue", "red", "brown", "pink", "orange", "purple"), #Line colors
       lty=c(1, 1, 1, 1, 1, 1, 1), #Line types
       lwd=c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5), #Line thickness
       bty="n", #No border ("o" if border)
       cex=1.1, #Text size
       y.intersp=0.65 #Spacing between text/lines
) 
text(max(Date.fore), 550,  #Location
     labels=paste0("ARIMA(", model8$arma[1],",", 
                   model8$arma[6],",", 
                   model8$arma[2],") (", 
                   model8$arma[3], ",",
                   model8$arma[7], ",",
                   model8$arma[4], ") Period =", 
                   model8$arma[5] ),  #Text
     pos=2,
     bty="n", #No border ("o" if border)
     cex=1.1, #Text size
     offset=0.85
)
dev.off()

}


