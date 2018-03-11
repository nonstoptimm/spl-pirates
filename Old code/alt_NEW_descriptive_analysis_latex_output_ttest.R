# Treatment group 2010
stargazer(merged2010[merged2010$Treatment == 1, ], keep = c("Sexnum", "Birth Year", "German", 
                                                            "Amount of Education Or Training in Years", "working_exp_with_NA", "Current Gross Labor Income in Euro", 
                                                            "Actual Work Time Per Week"), title = "Descriptive Statistic of Treatment Group 2010")
# Control group 2010
stargazer(merged2010[(merged2010$Control_3 == 1), ], keep = c("Sexnum", "Birth Year", "German", "Amount of Education Or Training in Years", 
                                        "working_exp_with_NA", "Current Gross Labor Income in Euro", "Actual Work Time Per Week"), 
          title = "Descriptive Statistic of Control Group 2010")


# Treatment group 2015
stargazer(merged2015[merged2015$Treatment == 1, ], keep = c("Sexnum", "Birth Year", "German", 
                                                            "Amount of Education Or Training in Years", "working_exp_with_NA", "Current Gross Labor Income in Euro", 
                                                            "Actual Work Time Per Week", "Minwagenona"), title = "Descriptive Statistic of Treatment Group 2015")
# Control group 2015
stargazer(merged2015[(merged2015$Control_3 == 1), ], keep = c("Sexnum", "Birth Year", "German", "Amount of Education Or Training in Years", 
                                        "working_exp_with_NA", "Current Gross Labor Income in Euro", "Actual Work Time Per Week"), 
          title = "Descriptive Statistic of Control Group 2015")


\begin{table}[!htbp] \centering 
\caption{Descriptive Statistic of Treatment Group 2010} 
\label{} 
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
\hline \\[-1.8ex] 
\end{tabular} 
\end{table} 


\begin{table}[!htbp] \centering 
\caption{Descriptive Statistic of Control Group 2010} 
\label{} 
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
\hline \\[-1.8ex] 
\end{tabular} 
\end{table} 


\begin{table}[!htbp] \centering 
\caption{Descriptive Statistic of Treatment Group 2015} 
\label{} 
\begin{tabular}{@{\extracolsep{5pt}}lccccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
Statistic & \multicolumn{1}{c}{N} & \multicolumn{1}{c}{Mean} & \multicolumn{1}{c}{St. Dev.} & \multicolumn{1}{c}{Min} & \multicolumn{1}{c}{Max} \\ 
\hline \\[-1.8ex] 
Amount of Education Or Training in Years & 804 & 11.871 & 2.289 & 7.000 & 18.000 \\ 
Actual Work Time Per Week & 826 & 33.268 & 13.697 & 2.000 & 77.000 \\ 
Current Gross Labor Income in Euro & 842 & 1,845.799 & 1,516.348 & 0 & 18,159 \\ 
Sexnum & 795 & 0.584 & 0.493 & 0 & 1 \\ 
German & 842 & 0.833 & 0.374 & 0 & 1 \\ 
working\_exp\_with\_NA & 831 & 13.126 & 11.759 & 0.000 & 45.000 \\ 
Minwagenona & 842 & 8.413 & 0.366 & 3.000 & 8.500 \\ 
\hline \\[-1.8ex] 
\end{tabular} 
\end{table} 

\begin{table}[!htbp] \centering 
\caption{Descriptive Statistic of Control Group 2015} 
\label{} 
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
\hline \\[-1.8ex] 
\end{tabular} 
\end{table} 

