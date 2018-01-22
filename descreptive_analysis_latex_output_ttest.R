
# T-Test Statistic that we might can use later using loop to create Statistic for every year
# from 2010 to 2015
y = 1
for (years in c(datalist)) {
  current_year = datalist[y]
  current_data = get(current_year)
  print(current_year)
  
  # Ttest for gender
  print(t.test(current_data$Sexnum[current_data$Treatment == 1], current_data$Sexnum[(current_data$Control_1 == 
                                                                                        1 | current_data$Control_2 == 1 | current_data$Control_3 == 1)]))
  # Ttest for Educ
  print(t.test(current_data$`Amount of Education Or Training in Years`[current_data$Treatment == 
                                                                         1], current_data$`Amount of Education Or Training in Years`[(current_data$Control_1 == 
                                                                                                                                        1 | current_data$Control_2 == 1 | current_data$Control_3 == 1)]))
  # Ttest for German
  print(t.test(current_data$German[current_data$Treatment == 1], current_data$Sexnum[(current_data$Control_1 == 
                                                                                        1 | current_data$Control_2 == 1 | current_data$Control_3 == 1)]))
  # Ttest for Income
  print(t.test(current_data$`Current Gross Labor Income in Euro`[current_data$Treatment == 1], 
               current_data$`Current Gross Labor Income in Euro`[(current_data$Control_1 == 1 | current_data$Control_2 == 
                                                                    1 | current_data$Control_3 == 1)]))
  # Ttest for Worktime
  print(t.test(current_data$`Actual Work Time Per Week`[current_data$Treatment == 1], current_data$`Actual Work Time Per Week`[(current_data$Control_1 == 
                                                                                                                                  1 | current_data$Control_2 == 1 | current_data$Control_3 == 1)]))
  # Ttest for Workexp
  print(t.test(current_data$working_exp_with_NA[current_data$Treatment == 1], current_data$working_exp_with_NA[(current_data$Control_1 == 
                                                                                                                  1 | current_data$Control_2 == 1 | current_data$Control_3 == 1)]))
  # Ttest for Birthyear
  print(t.test(current_data$`Year of Birth`[current_data$`Year of Birth` > 0 & current_data$Treatment == 
                                              1], current_data$`Year of Birth`[current_data$`Year of Birth` > 0 & (current_data$Control_1 == 
                                                                                                                     1 | current_data$Control_2 == 1 | current_data$Control_3 == 1)]))
  y = y + 1
}

# Installing Stargazer for a nice Overview of the Descreptive Statistic
# install.packages("stargazer")
library("stargazer")

# first we need to set the negative numbers to NA negative numbers exist, because the People
# maybe didnt want to say it
merged2015$`Birth Year` = NA
merged2015$`Birth Year`[merged2015$`Year of Birth` > 0] = merged2015$`Year of Birth`[merged2015$`Year of Birth` > 
                                                                                       0]


merged2010$`Birth Year` = NA
merged2010$`Birth Year`[merged2010$`Year of Birth` > 0] = merged2010$`Year of Birth`[merged2010$`Year of Birth` > 
                                                                                       0]


# Generating a descreptive statistical overview code for LateX

# Use the Dataframes of 2010 and 2015 for Treatment and Controlgroup for some important
# Variables (Gender, Year of Birth, Education, Workexperience, Income, Worktime and in 2015
# for Treatmentgroup also the Minwage) to show differents and equals of the different groups
# the code design a code for LateX that let the overview looks nicer


# Treatmentgroup 2010
stargazer(merged2010[merged2010$Treatment == 1, ], keep = c("Sexnum", "Birth Year", "German", 
                                                            "Amount of Education Or Training in Years", "working_exp_with_NA", "Current Gross Labor Income in Euro", 
                                                            "Actual Work Time Per Week"), title = "Descriptive Statistic of Treatment Group 2010")
# Treatmentgroup 2010
stargazer(merged2010[(merged2010$Control_1 == 1 | merged2010$Control_2 == 1 | merged2010$Control_3 == 
                        1), ], keep = c("Sexnum", "Birth Year", "German", "Amount of Education Or Training in Years", 
                                        "working_exp_with_NA", "Current Gross Labor Income in Euro", "Actual Work Time Per Week"), 
          title = "Descriptive Statistic of Control Group 2010")


# Treatmentgroup 2015
stargazer(merged2015[merged2015$Treatment == 1, ], keep = c("Sexnum", "Birth Year", "German", 
                                                            "Amount of Education Or Training in Years", "working_exp_with_NA", "Current Gross Labor Income in Euro", 
                                                            "Actual Work Time Per Week", "Minwagenona"), title = "Descriptive Statistic of Treatment Group 2015")
# Controlgroup 2015
stargazer(merged2015[(merged2015$Control_1 == 1 | merged2015$Control_2 == 1 | merged2015$Control_3 == 
                        1), ], keep = c("Sexnum", "Birth Year", "German", "Amount of Education Or Training in Years", 
                                        "working_exp_with_NA", "Current Gross Labor Income in Euro", "Actual Work Time Per Week"), 
          title = "Descriptive Statistic of Control Group 2015")



'
########################
#Output of all Tables::#
########################

\begin{table}[!htbp] \centering 
\caption{Descriptive Statistic of Treatment Group 2010} 
\label{} 
\resizebox{\linewidth}{!}{ %% "
\begin{tabular}{@{\extracolsep{5pt}}lccccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
Statistic & \multicolumn{1}{c}{N} & \multicolumn{1}{c}{Mean} & \multicolumn{1}{c}{St. Dev.} & \multicolumn{1}{c}{Min} & \multicolumn{1}{c}{Max} \\ 
\hline \\[-1.8ex] 
Amount of Education Or Training in Years & 3,076 & 11.649 & 2.197 & 7.000 & 18.000 \\ 
Actual Work Time Per Week & 3,286 & 32.821 & 16.817 & 1.000 & 80.000 \\ 
Current Gross Labor Income in Euro & 3,286 & 703.556 & 493.470 & 0 & 2,500 \\ 
Sexnum & 2,172 & 0.584 & 0.493 & 0 & 1 \\ 
German & 3,286 & 0.928 & 0.259 & 0 & 1 \\ 
working\_exp\_with\_NA & 3,049 & 9.754 & 11.259 & 0.000 & 59.300 \\ 
Birth Year & 2,172 & 1,971.259 & 14.895 & 1,922 & 1,992 \\ 
\hline \\[-1.8ex] 
\end{tabular} 
} %resizebox
\end{table} 



\begin{table}[!htbp] \centering 
\caption{Descriptive Statistic of Control Group 2010} 
\label{} 
\resizebox{\linewidth}{!}{ %% "
\begin{tabular}{@{\extracolsep{5pt}}lccccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
Statistic & \multicolumn{1}{c}{N} & \multicolumn{1}{c}{Mean} & \multicolumn{1}{c}{St. Dev.} & \multicolumn{1}{c}{Min} & \multicolumn{1}{c}{Max} \\ 
\hline \\[-1.8ex] 
Amount of Education Or Training in Years & 987 & 11.727 & 2.038 & 7.000 & 18.000 \\ 
Actual Work Time Per Week & 1,013 & 36.256 & 15.756 & 2.000 & 80.000 \\ 
Current Gross Labor Income in Euro & 1,013 & 1,334.210 & 583.430 & 70 & 3,116 \\ 
Sexnum & 681 & 0.589 & 0.492 & 0 & 1 \\ 
German & 1,013 & 0.926 & 0.262 & 0 & 1 \\ 
working\_exp\_with\_NA & 939 & 13.005 & 11.000 & 0.000 & 48.000 \\ 
Birth Year & 681 & 1,968.326 & 12.412 & 1,932 & 1,991 \\ 
\hline \\[-1.8ex] 
\end{tabular} 
} %resizebox
\end{table} 



\begin{table}[!htbp] \centering 
\caption{Descriptive Statistic of Treatment Group 2015} 
\label{} 
\resizebox{\linewidth}{!}{ %% "
\begin{tabular}{@{\extracolsep{5pt}}lccccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
Statistic & \multicolumn{1}{c}{N} & \multicolumn{1}{c}{Mean} & \multicolumn{1}{c}{St. Dev.} & \multicolumn{1}{c}{Min} & \multicolumn{1}{c}{Max} \\ 
\hline \\[-1.8ex] 
Amount of Education Or Training in Years & 3,111 & 11.651 & 2.371 & 7.000 & 18.000 \\ 
Actual Work Time Per Week & 3,425 & 32.838 & 15.091 & 1.000 & 80.000 \\ 
Current Gross Labor Income in Euro & 3,441 & 980.551 & 983.931 & 0 & 18,159 \\ 
Sexnum & 3,148 & 0.589 & 0.492 & 0 & 1 \\ 
German & 3,441 & 0.807 & 0.395 & 0 & 1 \\ 
working\_exp\_with\_NA & 3,394 & 9.215 & 11.496 & 0.000 & 54.000 \\ 
Minwagenona & 912 & 8.508 & 0.587 & 3.000 & 13.950 \\ 
Birth Year & 3,148 & 1,976.929 & 14.645 & 1,928 & 1,997 \\ 
\hline \\[-1.8ex] 
\end{tabular}  
} %resizebox
\end{table} 




\begin{table}[!htbp] \centering 
\caption{Descriptive Statistic of Control Group 2015} 
\label{} 
\resizebox{\linewidth}{!}{ %% "
\begin{tabular}{@{\extracolsep{5pt}}lccccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
Statistic & \multicolumn{1}{c}{N} & \multicolumn{1}{c}{Mean} & \multicolumn{1}{c}{St. Dev.} & \multicolumn{1}{c}{Min} & \multicolumn{1}{c}{Max} \\ 
\hline \\[-1.8ex] 
Amount of Education Or Training in Years & 1,136 & 11.539 & 2.187 & 7.000 & 18.000 \\ 
Actual Work Time Per Week & 1,190 & 32.064 & 15.384 & 1.000 & 80.000 \\ 
Current Gross Labor Income in Euro & 1,190 & 1,177.287 & 568.195 & 34 & 3,000 \\ 
Sexnum & 1,052 & 0.663 & 0.473 & 0 & 1 \\ 
German & 1,190 & 0.768 & 0.422 & 0 & 1 \\ 
working\_exp\_with\_NA & 1,179 & 11.483 & 11.366 & 0.000 & 55.700 \\ 
Birth Year & 1,052 & 1,972.520 & 12.466 & 1,937 & 1,997 \\ 
\hline \\[-1.8ex] 
\end{tabular} 
} %resizebox
\end{table} 

