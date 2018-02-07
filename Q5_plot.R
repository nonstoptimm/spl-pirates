### Create Plot Load the library 'ggplot2'
library(ggplot2)
# Create Vector for every variable
p_control1 = c()
p_control2 = c()
p_control3 = c()
p_controlsum = c()
p_treatm = c()
p_type = c(rep("p_control1", length(list_years)), rep("p_control2", length(list_years)), rep("p_control3", length(list_years)), rep("p_controlsum", length(list_years)), 
           rep("p_treatm", length(list_years)))

# Set Control Variable
y = 1

# Loop through every year, sum the variable and add it to the vector
for (years_plot in c(datalist)) {
  current_year = datalist[y]
  current_data = get(current_year)
  p_control1 = c(p_control1, sum(current_data$Control_1 == 1))
  p_control2 = c(p_control2, sum(current_data$Control_2 == 1))
  p_control3 = c(p_control3, sum(current_data$Control_3 == 1))
  p_controlsum = c(p_controlsum, sum(current_data$Control_1, current_data$Control_2, current_data$Control_3))
  p_treatm = c(p_treatm, sum(current_data$Treatment == 1))
  y = y + 1
}

# Integrate all vectors into one vector
p_values = c(p_control1, p_control2, p_control3, p_controlsum, p_treatm)
p_condition = rep(list_years, 5)
# Combine data, conditions and variable names
p_data = data.frame(p_type, p_condition, p_values)

# Plot the bar chart
p_plot = ggplot(p_data, aes(y = p_values, x = p_type, color = p_type, fill = p_type)) + geom_bar(stat = "identity") + facet_wrap(~p_condition) + labs(x = "Variables", 
                                                                                                                                                      y = "No. of Observations") + ggtitle("Number of Observations in Treatment and Control Groups")

# Store plot as png
ggsave(filename = "plots/p_plot.png", plot = p_plot)

