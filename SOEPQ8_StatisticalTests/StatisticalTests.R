## Quantlet 8 - StatisticalTests Load Packages used in Q8
library(plm)
library(lmtest)
# Execution of Q1, Q4, Q5, Q6 and Q7 is necessary beforehand!

# Create regressions fee and ree
fee = plm(Log.Full.Employment.Rate ~ DiD.estimator.Kaitz + Log.Population + year13.dummy + year12.dummy,
    data = estimation, index = c("State.of.Residence", "Wave"), model = "within")
ree = plm(Log.Full.Employment.Rate ~ DiD.estimator.Kaitz + Log.Population + year13.dummy + year12.dummy,
    data = estimation, index = c("State.of.Residence", "Wave"), model = "random")

# Create function 'estimate_appropriate_model'
estimate_appropriate_model = function(fe, re) {
    testvalues = c()
    output = vector("list", length = 2)
    # HausmannTest
    HausmanTest = phtest(fe, re)
    hvalue = ifelse(HausmanTest$p.value < 0.05, 1, 0)
    testvalues = c(testvalues, hvalue)
    if (hvalue == 1) {
        TimeFixedEffectsTest = plmtest(fe, effect = c("time"), type = c("bp"))
        currvalue = ifelse(TimeFixedEffectsTest$p.value < 0.05, 1, 0)
        testvalues = c(testvalues, currvalue)
    } else {
        TimeFixedEffectsTest = plmtest(re, effect = c("time"), type = c("bp"))
        currvalue = ifelse(TimeFixedEffectsTest$p.value < 0.05, 1, 0)
        testvalues = c(testvalues, currvalue)
    }
    # RandomEffectsTest
    if (hvalue == 1) {
        currvalue = 0
        testvalues = c(testvalues, currvalue)
    } else {
        RandomEffectsTest = plmtest(re, type = c("bp"))
        currvalue = ifelse(RandomEffectsTest$p.value < 0.05, 1, 0)
        testvalues = c(testvalues, currvalue)
    }
    # CrossSectionalDependenceTest
    if (hvalue == 1) {
        CrossSectionalDependence = pcdtest(fe, test = c("lm"))
        currvalue = ifelse(CrossSectionalDependence$p.value < 0.05, 1, 0)
        testvalues = c(testvalues, currvalue)
    } else {
        currvalue = 0
        testvalues = c(testvalues, currvalue)
    }
    # SerialCorrelationTest
    if (hvalue == 1) {
        SerialCorrelationTest = pbgtest(fe)
        currvalue = ifelse(SerialCorrelationTest$p.value < 0.05, 1, 0)
        testvalues = c(testvalues, currvalue)
    } else {
        currvalue = 0
        testvalues = c(testvalues, currvalue)
    }
    # HeteroSkedasticityTest
    if (hvalue == 1) {
        HeteroSkedasticityTest = bptest(fe)
        currvalue = ifelse(HeteroSkedasticityTest$p.value < 0.05, 1, 0)
        testvalues = c(testvalues, currvalue)
    } else {
        HeteroSkedasticityTest = bptest(re)
        currvalue = ifelse(HeteroSkedasticityTest$p.value < 0.05, 1, 0)
        testvalues = c(testvalues, currvalue)
    }
    # Estimate correct coefficients
    if (testvalues[1] == 0) {
        if (testvalues[6] == 0) {
            output[1] = "Random effect model without heteroskedasticity"
            output[[2]] = coeftest(re)
        } else {
            if (testvalues[5] == 0) {
                output[1] = "Random effect model with heteroskedasticity but without serial correlation"
                output[[2]] = coeftest(re, vcovHC(re, method = "white1"))
            } else {
                output[1] = "Random effect model with heteroskedasticity and serial correlation"
                output[[2]] = coeftest(re, vcovHC(re, method = "arellano"))
            }
        }
    } else {
        if (testvalues[6] == 0) {
            output[1] = "Fixed effect model without heteroskedasticity"
            output[[2]] = coeftest(fe)
        } else {
            if (testvalues[5] == 0) {
                output[1] = "Fixed effect model with heteroskedasticity but without serial correlation"
                output[[2]] = coeftest(fe, vcovHC(fe, method = "white1"))
            } else {
                output[1] = "Fixed effect model with heteroskedasticity and serial correlation"
                output[[2]] = coeftest(fe, vcovHC(fe, method = "arellano"))
            }
        }
    }
    return(output)
}

# Apply estimate_appropriate_model-function to the regressions in Q7
# Use did_2.2.2 and specify counterpart random model
fixed.1 = did_2.2.2
random.1 = plm(Log.Part.Employment.Rate ~ DiD.estimator.Fraction + Log.Population + Interaction_Fraction_y13 +
    Interaction_Fraction_y12, data = estimation, index = c("State.of.Residence", "Wave"), model = "random")
# Estimate the appropriate model
estimate_appropriate_model(fixed.1, random.1)
# Compare with original estimation
summary(fixed.1)

# Use did.4.2 and specify counterpart random model
fixed.2 = did_4.2
random.2 = plm(Delta.Log.Full.Employment ~ binary1, data = estimation, index = c("State.of.Residence",
    "Wave"), model = "random")
# Estimate the appropriate model
estimate_appropriate_model(fixed.2, random.2)
# Compare with original estimation
summary(fixed.2)
