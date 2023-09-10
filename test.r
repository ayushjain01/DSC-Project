# Load the required libraries
library(ggplot2)
library(dplyr)

# Read dataset and convert it into a DataFrame
data <- read.csv("ias-profile.csv")
df <- data.frame(data)

# Count occurrences of each place of domicile
place_counts <- df %>% 
  group_by(Place_of_Domicile) %>% 
  summarise(count = n())

# Open a new graphics device
plot.new()

# Create a bar plot using ggplot
ggplot(place_counts, aes(x = Place_of_Domicile, y = count, fill = Place_of_Domicile)) +
  geom_bar(stat = "identity") +
  labs(title = "Occurrences of Place of Domicile", x = "Place of Domicile", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
