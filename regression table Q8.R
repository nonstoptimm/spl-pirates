### regression tables ### 

# Regression 1.1.1 (baseline): Using Kaitz Index
stargazer(did_1.1.1, did_1.1.2, did_1.1.3, did_1.1.4, title="Results", type="text", align=TRUE, 
          no.space=TRUE, keep.stat= c("n","adj.rsq","rsq"), dep.var.labels = ("Panel A: Full Time Employment"), 
          covariate.labels=c("Bite.K", "D2015", "Population(log,t)", "Bite x D2013", "Bite x D2012", "Bite x D2015"))

# Regression 1.2.1 (baseline): Using Fraction Index
stargazer(did_1.2.1, did_1.2.2, did_1.2.3, did_1.2.4, title="Results", type="text", align=TRUE, 
          no.space=TRUE, keep.stat= c("n","adj.rsq","rsq"), dep.var.labels = ("Panel A2: Full Time Employment"), 
          covariate.labels=c("Bite.F", "D2015", "Population(log,t)", "Bite x D2013", "Bite x D2012", "Bite x D2015"))



# Regression 2.1.1 (baseline): Using Kaitz Index
stargazer(did_2.1.1, did_2.1.2, did_2.1.3, did_2.1.4, title="Results", type="text", align=TRUE, 
          no.space=TRUE, keep.stat= c("n","adj.rsq","rsq"), dep.var.labels = ("Panel B: Part Time Employment"), 
          covariate.labels=c("Bite.K", "D2015", "Population(log,t)", "Bite x D2013","Bite x D2012", "Bite x D2015"))

# Regression 2.2.1 (baseline): Using Fraction Index
stargazer(did_2.2.1, did_2.2.2, did_2.2.3, did_2.2.4, title="Results", type="text", align=TRUE, 
          no.space=TRUE, keep.stat= c("n","adj.rsq","rsq"), dep.var.labels = ("Panel B2: Part Time Employment"), 
          covariate.labels=c("Bite.F", "D2015", "Population(log,t)", "Bite x D2013","Bite x D2012", "Bite x D2015"))



## Regression 3: We regress on Marginal Employment
stargazer(did_3.1.1, did_3.1.2, did_3.1.3, did_3.1.4, title="Results", type="text", align=TRUE, 
          no.space=TRUE, keep.stat= c("n","adj.rsq","rsq"), dep.var.labels = ("Panel C: Marginal Employment"), 
          covariate.labels=c("Bite.K", "D2015", "Population(log,t)", "Bite x D2013","Bite x D2012", "Bite x D2015"))

# Regression 3.2.1 (baseline): Using Fraction Index
stargazer(did_3.2.1, did_3.2.2, did_3.2.3, did_3.2.4, title="Results", type="text", align=TRUE, 
          no.space=TRUE, keep.stat= c("n","adj.rsq","rsq"), dep.var.labels = ("Panel C2: Marginal Employment"), 
          covariate.labels=c("Bite.F", "D2015", "Population(log,t)", "Bite x D2013","Bite x D2012", "Bite x D2015"))