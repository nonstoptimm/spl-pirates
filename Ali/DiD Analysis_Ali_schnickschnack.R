# Diff in Diff Gleichung
# log(y_it) = bite_i + D_t^MW * beta_l + Sum_t D_t^year * gamma_t + alpha + e_i,t
#Control for State: Average labor income in different periods

#install.packages("plm")
#library(plm)

## D&D1 with treatment.analysis1
str(Treatment.analysis1)
#Need a numeric variable for years, thus convert the variable wave
Treatment.analysis1$year = as.numeric(as.character(Treatment.analysis1$Wave))
# Creat dummy for year for indicator when minimum wage was introduced
Treatment.analysis1$year.dummy = ifelse(Treatment.analysis1$year >= 2015, 1, 0)

Treatment.analysis1$did = Treatment.analysis1$year.dummy * Treatment.analysis1$binary_treatment1


did1 = lm(Employment.Rate ~ binary_treatment1 + year.dummy + did, data = Treatment.analysis1)
summary(did1)

#phtest()


##Don't use the aggreagated values, therefore use of analyze_tc
analyze_tc$year = as.numeric(as.character(analyze_tc$Wave))
# Creat dummy for year for indicator when minimum wage was introduced
analyze_tc$year.dummy = ifelse(analyze_tc$year >= 2015, 1, 0)

analyze_tc$did = analyze_tc$year.dummy * analyze_tc$binary_treatment1

did2 = lm(Employment.Rate ~ binary_treatment1 + year.dummy + did, data = analyze_tc)
summary(did2)
