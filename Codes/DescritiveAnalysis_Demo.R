# Data 
data("Seatbelts")

# Exposure and outcome 
outcomeDeath <- log(Seatbelts[,1])
exposurePrice <- Seatbelts[,6]

# plot 
plot(outcomeDeath)
plot(exposurePrice)

# Fit and diagnose, ACF is valid for linear model (not quite for Poisson model)
fit <- lm(outcomeDeath ~ exposurePrice)
acf(fit$residuals, 40 )





driveDistance <- Seatbelts[,5]
plot(driveDistance)
fit <- lm(driveDistance ~ exposurePrice)
fit
acf(fit$residuals, 40)





driveDistance <- Seatbelts[,5]
time <- seq(1:length(driveDistance))
freqVariable <- tsModel::harmonic(time, nfreq = 4, period = 12)
fit <- lm(driveDistance ~ exposurePrice + freqVariable)
summary(fit)
acf(fit$residuals, 40)






driveDistance <- Seatbelts[,5]
time <- seq(1:length(driveDistance))
freqVariable <- tsModel::harmonic(time, nfreq = 4, period = 12)
fit <- lm(driveDistance ~ exposurePrice + freqVariable + time)
summary(fit)
acf(fit$residuals, 40)
