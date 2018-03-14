#### Statistical Regression Tests ####


## First, different tests for a given regression model
# Hausman test and Breusch Pagan Test
# Plot Residulals and Stuff

# Use regression did_1.1.2, fixed effects model
# did_1.1.2 = plm(Log.Full.Employment.Rate ~ DiD.estimator.Kaitz + Log.Population + year13.dummy + year12.dummy, data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
a = did_1.1.2
# Use b, random effects model
b = plm(Log.Full.Employment.Rate ~ DiD.estimator.Kaitz + Log.Population + year13.dummy + year12.dummy, data = estimation, index=c("State.of.Residence", "Wave"), model = "random")
summary(b)

# a: fixed, b: random, Hausmann Test
phtest(a, b)
## p-value < 0.05 therefore use fixed effects model



## Check for time fixed effects
plmtest(a, c("time"), type=("bp"))
# p-value < 0.05 use time fixed effects

### Homo. vs Heteroskedasticity
#Plot Residuals
estimation$predicted = predict(a)   # Save the predicted values
estimation$residuals = residuals(a)

# Check predicted vs Employment Rate
estimation %>% select(Log.Full.Employment.Rate, predicted, residuals) %>% head()

plot(estimation$residuals)

par(mfrow = c(2, 2))
plot(estimation)

## Breusch-Pagan LM test of independence: testing for cross-sectional dependence
pcdtest(a, test = c("lm"))
# p-value < 0.05, implies heteroskedasticity -> cross.sectional dependece
#Use robust estimation
coeftest(a)








## Second, write function that applies tests to a model and dependent on them adjusts the regression




