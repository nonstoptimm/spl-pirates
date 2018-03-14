stargazer(did_1.1.1, did_1.1.2, did_1.1.3, did_1.1.4, title="Results", type="text", align=TRUE, 
          no.space=TRUE, 
          keep.stat= c("n","adj.rsq","rsq"), 
          dep.var.labels = ("Panel A: Regular Employment"), 
          covariate.labels=c("Bite", "D2015", "Population(log,t)", "Bite x D2013", "Bite x D2012", "Bite x D2015"))


stargazer(did_1.1.1, did_1.1.2, did_1.1.3, did_1.1.4, title="Results", type="text", align=TRUE, 
          no.space=TRUE, 
          keep.stat= c("n","adj.rsq","rsq"), 
          dep.var.labels = ("Panel A: Full Time Employment"), 
          covariate.labels=c("Bite.K", "D2015", "Population(log,t)", "Bite x D2013", "Bite x D2012", "Bite x D2015"))






## Test whether Fixed Effects or Random Effects: Hausman test
d = plm(Log.Full.Employment ~ (DiD.estimator) + Log.Population , data = DiD_DIW, index=c("State.of.Residence", "Wave"), model = "within")
summary(d)
e = plm(Log.Full.Employment ~ (DiD.estimator) + Log.Population , data = DiD_DIW, index=c("State.of.Residence", "Wave"), model = "random")
summary(e)
phtest(d, e)

### Homo. vs Heteroskedasticity
#Plot Residuals
DiD_DIW$predicted = predict(d)   # Save the predicted values
DiD_DIW$residuals = residuals(d)

DiD_DIW %>% select(Log.Full.Employment, predicted, residuals) %>% head()



plot(d$residuals)

par(mfrow = c(2, 2))
plot(d)

## Breusch-Pagan LM test of independence: testing for cross-sectional dependence
pcdtest(d, test = c("lm"))
#Imply Heteroskedasticity
#Use robust estimation
coeftest(d)
