# Quantlet 8
# Load Packages used in Q8
library(ggplot2)
library(sp)
library(plm)
library(lmtest)

# Create regressions fee and ree
fee = plm(Log.Full.Employment.Rate ~ DiD.estimator.Kaitz + Log.Population + year13.dummy + year12.dummy, data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
ree = plm(Log.Full.Employment.Rate ~ DiD.estimator.Kaitz + Log.Population + year13.dummy + year12.dummy, data = estimation, index=c("State.of.Residence", "Wave"), model = "random")

# Create function "estimate_appropriate_model" 
estimate_appropriate_model = function(fe, re) {
  testvalues = c()
  output = vector("list", length = 2)
  # HausmannTest
  HausmanTest = phtest(fee, ree)
    hvalue = ifelse(HausmanTest$p.value < 0.05, 1, 0)
    testvalues = c(testvalues, hvalue)
  if(hvalue == 1) {
    TimeFixedEffectsTest = plmtest(fee, effect = c("time"), type = c("bp"))
      currvalue = ifelse(TimeFixedEffectsTest$p.value < 0.05, 1, 0)
      testvalues = c(testvalues, currvalue)
  } else {
    TimeFixedEffectsTest = plmtest(ree, effect = c("time"), type = c("bp"))
      currvalue = ifelse(TimeFixedEffectsTest$p.value < 0.05, 1, 0)
      testvalues = c(testvalues, currvalue)
  } 
  # RandomEffectsTest
  if(hvalue == 1) {
    currvalue = 0
    testvalues = c(testvalues, currvalue)
  } else {
    RandomEffectsTest = plmtest(ree, type = c("bp"))
    currvalue = ifelse(RandomEffectsTest$p.value < 0.05, 1, 0)
    testvalues = c(testvalues, currvalue)
  } 
  # CrossSectionalDependenceTest
    if(hvalue == 1) {
      CrossSectionalDependence = pcdtest(fee, test = c("lm"))
      currvalue = ifelse(CrossSectionalDependence$p.value < 0.05, 1, 0)
      testvalues = c(testvalues, currvalue)
    } else {
      currvalue = 0
      testvalues = c(testvalues, currvalue)
    } 
  # SerialCorrelationTest
    if(hvalue == 1) {
      SerialCorrelationTest = pbgtest(fee)
      currvalue = ifelse(SerialCorrelationTest$p.value < 0.05, 1, 0)
      testvalues = c(testvalues, currvalue)
    } else {
      currvalue = 0
      testvalues = c(testvalues, currvalue)
    } 
  # HeteroSkedasticityTest
    if(hvalue == 1) {
      HeteroSkedasticityTest = bptest(fee)
      currvalue = ifelse(HeteroSkedasticityTest$p.value < 0.05, 1, 0)
      testvalues = c(testvalues, currvalue)
    } else {
      HeteroSkedasticityTest = bptest(ree)
      currvalue = ifelse(HeteroSkedasticityTest$p.value < 0.05, 1, 0)
      testvalues = c(testvalues, currvalue)
    } 
  # Estimate correct coefficients
    if(testvalues[1] == 0) {
      if(testvalues[6] == 0) {
        output[1] = "Random effect model without heteroskedasticity"
        output[[2]] = coeftest(ree)
      } else {
        if(testvalues[5] == 0) {
          output[1] = "Random effect model with heteroskedasticity but without serial correlation"
          output[[2]] = coeftest(ree, vcovHC(ree, method = "white1"))
        } else {
          output[1] = "Random effect model with heteroskedasticity and serial correlation"
          output[[2]] = coeftest(ree, vcovHC(ree, method = "arellano"))
        }
      }
    } else {
      if(testvalues[6] == 0) {
        output[1] = "Fixed effect model without heteroskedasticity"
        output[[2]] = coeftest(fee)
      } else {
        if(testvalues[5] == 0) {
          output[1] = "Fixed effect model with heteroskedasticity but without serial correlation"
          output[[2]] = coeftest(fee, vcovHC(fee, method = "white1"))
        } else {
          output[1] = "Fixed effect model with heteroskedasticity and serial correlation"
          output[[2]] = coeftest(fee, vcovHC(fee, method = "arellano"))
        }
      }
    }
    return(output)
}

# Apply estimate_appropriate_model-function to fee and ree
estimate_appropriate_model(fee, ree)
# Compare with original estimation
summary(fee)

