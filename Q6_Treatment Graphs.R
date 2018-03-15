# Quantlet 6
###Graphical Analysis for Employment Rates of Treatment and Control Groups###
library(ggplot2)
library(sp)

### Combine Information of Q4 and Q5
# We use the averaged data from Q4 for the employment rates and add the Fraction and Keitz Index from Q5
combine_data = function(x,y) {
  combined = merge(x, y)
  combined = combined %>% group_by(Wave, State.of.Residence) %>% arrange(Wave)
  return(combined)
}

# Apply combine_data to merge, filter, group and arrange Employment.yearly.state and dbys
analyze_tc = combine_data(Employment.yearly.state, dbys)

###Treatment Identification with Keitz Index
## Use Median of Keitz Index in 2013 for identification
#1.Standart treatment: If Kaitz higher than Median the State will be treatmant, otherwise control
#2.Robust treatment: If Kaitz higher than 60% - Percetil will be treatment, if under 40% 
data_selector = function(analyze_tc, wave) {
  select(filter(analyze_tc, Wave == wave), c(Wave, State.of.Residence, Kaitz, Fraction))
}

# Create subset for 2013
analyze_2013 = data_selector(analyze_tc, 2013)

add_treatment = function(x) {
  x$binary_treatment1 = NA
  x$binary_treatment1[x$Kaitz > median(x$Kaitz)] = 1
  x$binary_treatment1[is.na(x$binary_treatment1)] = 0
  x$binary_treatment2 = NA
  x$binary_treatment2[x$Kaitz > quantile(x$Kaitz, c(0.6))] = 1
  x$binary_treatment2[x$Kaitz < quantile(x$Kaitz, c(0.4))] = 0
  return(x)
}

# Apply add_treatment with analyze_2013 dataset
analyze_2013 = add_treatment(analyze_2013)


###### AB HIER PRÜFEN OB MAN LÖSCHEN KANN WEIL ES DURCH add_treatment FUNKTION ERSETZT WURDE

  #Add Treatmentvariable 1 - Schmitz
  #analyze_2013$binary_treatment1 = NA
  #analyze_2013$binary_treatment1[analyze_2013$Kaitz > median(analyze_2013$Kaitz)] = 1
  #analyze_2013$binary_treatment1[is.na(analyze_2013$binary_treatment1)] = 0 
  
  ## Better code for it
  #if(analyze_2013$Kaitz > median(analyze_2013$Kaitz)){
  # analyze_2013$binary_treatment1 == 1
  #}else{ analyze_2013$binary_treatment1 == 0
  #}
  
  #Add Treatmentvariable 2 - Schmitz
  #analyze_tc$binary_treatment2 = NA
  #analyze_2013$binary_treatment2[analyze_2013$Kaitz > quantile(analyze_2013$Kaitz, c(0.6))] = 1
  #analyze_2013$binary_treatment2[analyze_2013$Kaitz < quantile(analyze_2013$Kaitz, c(0.4))] = 0 

###### BIS HIER PRÜFEN OB MAN LÖSCHEN KANN WEIL ES DURCH add_treatment FUNKTION ERSETZT WURDE
# Warum arbeiten wir zuerst mit analyze_tc$binary_treatment2 = NA und bei den unteren mit analyze 2013
# bei Add Treatmentvariable 2 - Schmitz ? Soll beides eigentlich analyze_2013 sein?

#Warum setzen wir diese Variable jetzt und nicht gleich oben drüber in analyze_tc :)?
#Merge Treatment Identifiers back to main data  
analyze_tc$binary_treatment1 = analyze_2013$binary_treatment1
analyze_tc$binary_treatment2 = analyze_2013$binary_treatment2

## Function to aggregate data into standard Treatment and Control group using binary_treatment1 or binary_treatment2, depending on the input
aggregate_treatment_control = function(x, y) {
  if(y == 1) {
    x$y = x$binary_treatment1
  } else if (y == 2) {
    x$y = x$binary_treatment2
  }
  x = x %>%
  group_by(Wave, y) %>%
  summarise(Observation =  n(),
    Avg.Log.Full.Employment = mean(Log.Full.Employment, na.rm=TRUE),
    Avg.Log.Part.Employment = mean(Log.Part.Employment, na.rm=TRUE),
    Avg.Log.Marginal.Employment = mean(Log.Marginal.Employment, na.rm=TRUE),
    Avg.Delta.Log.Full.Employment = mean(Delta.Log.Full.Employment, na.rm=TRUE),
    Avg.Delta.Log.Part.Employment = mean(Delta.Log.Part.Employment, na.rm=TRUE),
    Avg.Delta.Log.Marginal.Employment = mean(Delta.Log.Marginal.Employment, na.rm=TRUE),
    Avg.Full.Employment.Rate = mean(Full.Employment.Rate, na.rm=TRUE),
    Avg.Part.Employment.Rate = mean(Part.Employment.Rate, na.rm=TRUE),
    Avg.Marginal.Employment.Rate = mean(Marginal.Employment.Rate, na.rm=TRUE))
  if(y == 1) {
    x$binary_treatment1 = x$y
  } else if (y == 2) {
    x$binary_treatment2 = x$y
  } else {
    print("Only 1 and 2 are valid as values for y!")
  }
  x$y <- NULL
  return(x)
}

# Apply aggregate_standard_treatment_control to analyze_tc either with binary_treatment11 or binary_treatment12
Treatment.analysis1 = aggregate_treatment_control(analyze_tc, 1)
Treatment.analysis2 = aggregate_treatment_control(analyze_tc, 2)

# Define function to delete all rows with missing values, so that only complete datasets remain
drop_sub_na = function(x) { x[complete.cases(x), ] }
# Apply drop_sub_na to Treatment.analysis2 to drop lines with missing values
Treatment.analysis2_noNA = drop_sub_na(Treatment.analysis2)

### GRAPHS ###
# Function to plot log employment of binary treatmentgroup for different employment status
plot_treatment = function(x, yaxis, treatment, title) {
  if(yaxis == "Full") {
    x$yaxis = x$Avg.Log.Full.Employment
  } else if(yaxis == "Part") {
    x$yaxis = x$Avg.Log.Part.Employment
  } else if(yaxis == "Marginal") {
    x$yaxis = x$Avg.Log.Marginal.Employment
  } else {
    print("Input must be Full, Part or Marginal!")
  }
  if(treatment == 1) {
    x$treatment= x$binary_treatment1
  } else if(treatment == 2) {
    x$treatment = x$binary_treatment2
  } else {
    print("Input must be 1 or 2!")
  }
  ggplot(data = x, aes(x=Wave, y = yaxis, group = factor(treatment), colour = factor(treatment))) +
  geom_line() +
  geom_point() +
  labs(title = title,
       y = "Log Employment level",
       x = "Years") +
  geom_vline(xintercept = 5, color = "red") +
  theme_classic() +
  scale_colour_discrete(name = "Treatment",
                        labels = c("lower Kaitz than median", "higher Kaitz than median")) +
  coord_cartesian(xlim = c(1.6,7))
}

# Apply the function to different treatment analysis datasets
# For Full Time Employment, T1
plot_treatment(Treatment.analysis1, "Full", 1, "Log Employment of binary Treatmentgroups for Full Employment")
# For Part Time Employment, T1
plot_treatment(Treatment.analysis1, "Part", 1, "Log Employment of binary Treatmentgroups for Part Employment")
# For Marginal Time Employment, T1
plot_treatment(Treatment.analysis1, "Marginal", 1,"Log Employment of binary Treatmentgroups for Marginal Employment")
# For Full Time Employment, T2
plot_treatment(Treatment.analysis2, "Full", 2, "Log Employment of binary Treatmentgroups for Full Employment")
# For Part Time Employment, T2
plot_treatment(Treatment.analysis2, "Part", 2, "Log Employment of binary Treatmentgroups for Part Employment")
# For Marginal Time Employment, T2
plot_treatment(Treatment.analysis2, "Marginal", 2, "Log Employment of binary Treatmentgroups for Marginal Employment")


# MAP PLOTS to illustrate Kaitz, Fraction and Treatment for the German states
# Read Map File for Germany
map <- readRDS("geodata/DEU_adm1.rds")

# Unify the States
substituteState <- function(x) {
  x <- as.character(x)
  x <- gsub("16", "Thüringen", x)
  x <- gsub("15", "Sachsen-Anhalt", x)
  x <- gsub("14", "Sachsen", x)
  x <- gsub("13", "Mecklenburg-Vorpommern", x)
  x <- gsub("12", "Brandenburg", x)
  x <- gsub("11", "Berlin", x)
  x <- gsub("10", "Saarland", x)
  x <- gsub("9", "Bayern", x)
  x <- gsub("8", "Baden-Württemberg", x)
  x <- gsub("7", "Rheinland-Pfalz", x)
  x <- gsub("6", "Hessen", x)
  x <- gsub("5", "Nordrhein-Westfalen", x)
  x <- gsub("4", "Bremen", x)
  x <- gsub("3", "Niedersachsen", x)
  x <- gsub("2", "Hamburg", x)
  x <- gsub("1", "Schleswig-Holstein", x)
}

# Apply the substituteState-Function to plot it properly
analyze_2013$State.of.Residence <- substituteState(analyze_2013$State.of.Residence)

# Correct and Order the State Names, so that the names are distinct
correctState = function(x) {
  # Order it alphabetically, ascending
  x = x[order(x$State.of.Residence), ]
  # Define the Treatment as Factor
  x$binary_treatment1 <- as.factor(x$binary_treatment1)
  # List with all State Names
  x$State.of.Residence <- c("Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", "Bremen",  "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen-Anhalt", "Sachsen", "Schleswig-Holstein", "Thüringen")  
  return(x)
}

# Apply the correctState to the analyze_2013 dataset
analyze_2013 = correctState(analyze_2013)

# Function to create map data which can be proceeded with ggplot
createMapdata = function(input_map, input) {
  data_combined <- data_frame(id=rownames(input_map@data), State.of.Residence=input_map@data$NAME_1) %>% 
    left_join(input, by = "State.of.Residence")
  map2 <- fortify(input_map)
  final_map <- left_join(map2, data_combined, by = "id")
}

# Apply the imported polygon map and the dataset createMapdata to create a valid plot dataset
final_map = createMapdata(map, analyze_2013)

# PLOT Functions
# Function to plot the Kaitz or Fraction Index. The dataset created with createMapdata has to be used for this
plot_result_index = function(input, mode, highcolor) {
  if(mode == "Fraction") {
    input$mode = input$Fraction
  } else if(mode == "Kaitz") {
    input$mode = input$Kaitz
  } else {
    print("Input must be Fraction or Kaitz!")
  }
  ggplot(input, aes(x=long, y = lat, group = group)) + 
    geom_polygon(data = input, aes(fill=mode), alpha=0.8, color = "black") + 
    scale_fill_gradient(low = "white", high = highcolor) + 
    coord_map() +
    theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.title = element_blank()) +
    labs(title = paste(mode, "Index", sep="-"))
}

# Function to plot the Factors. The dataset created with createMapdata has to be used for this
plot_result_factor = function(x) {
  Treatment = factor(x$binary_treatment1)
  ggplot(x) + 
    aes(x=long, y = lat, group = group, fill=Treatment) + 
    geom_polygon(color = "black") +
    coord_map() +
    scale_fill_manual(values = c("white", "blue")) + 
    geom_path(color="black") +
    theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.title = element_blank()) +
    labs(title = "Binary")
}

# APPLY Plot Functions
plot_result_binary <- plot_result_factor(final_map)
plot_result_kaitz <- plot_result_index(final_map, "Kaitz", "blue")
plot_result_fraction <- plot_result_index(final_map, "Fraction", "red")

# SAVE the Plots
ggsave("plots/plot-kaitz.png", plot_result_kaitz)
ggsave("plots/plot-fraction.png", plot_result_fraction)
ggsave("plots/plot-factor.png", plot_result_kaitz)
