library(sp)
library(ggplot2)
# Read Map File for Germany
map <- readRDS("geodata/DEU_adm1.rds")
#q6
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
# Function to plot the Kaitz Index. The dataset created with createMapdata has to be used for this
plot_result_kaitz = function(input) {
  ggplot(input, aes(x=long, y = lat, group = group)) + 
    geom_polygon(data = final_map, aes(fill=Kaitz), alpha=0.8, color = "black") + 
    scale_fill_gradient(low = "white", high = "blue") + 
    coord_map() +
    theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(hjust = 0.5)) +
  labs(title = "Kaitz-Index")
}

# Function to plot the Fraction Index. The dataset created with createMapdata has to be used for this
plot_result_fraction = function(input) {
  ggplot(input, aes(x=long, y = lat, group = group)) + 
    geom_polygon(data = final_map, aes(fill=Fraction), alpha=0.8, color = "black") + 
    scale_fill_gradient(low = "white", high = "red") + 
    coord_map() +
    theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(hjust = 0.5)) +
  labs(title = "Fraction-Index")
}

# Function to plot the Factors. The dataset created with createMapdata has to be used for this
plot_result_factor = function(x) {
  Treatment = factor(x$binary_treatment1)
  ggplot(x) + 
  aes(x=long, y = lat, group = group, fill=Treatment) + 
  geom_polygon(color = "black") +
  #geom_polygon(data = x, aes(fill=binary_treatment1, alpha=0.8, color = "black")) + 
  coord_map() +
  #coord_equal() +
  scale_fill_manual(values = c("white", "blue")) + 
  geom_path(color="black") +
  #scale_fill_manual("legend", values = c(0 = "black", 1 = "orange")) +
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Binary")
  }

# APPLY Plot Functions
plot_result_kaitz <- plot_result_kaitz(final_map)
plot_result_fraction <- plot_result_fraction(final_map)
plot_result_binary <- plot_result_factor(final_map)

# Save the Plots
ggsave("plots/plot-kaitz.png", plot_result_kaitz)
ggsave("plots/plot-fraction.png", plot_result_fraction)
ggsave("plots/plot-factor.png", plot_result_binary)