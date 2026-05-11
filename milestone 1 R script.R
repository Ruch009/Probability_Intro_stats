#Name :Ruchika Patidar
#CLass: ALY6010 Probability theory and Intro Statistics
#Date: 03/07/24

## Data Analysis of Abalone data set
#Read Data
abalone <- read.csv("abalone.data")
abalone
install.packages("dplyr")
library(dplyr)

#Renaming Column 
abalone <- abalone %>% 
  rename(Sex =  M,
         Length = X0.455,
         Diameter = X0.365, 
         Height = X0.095, 
         Whole_weight = X0.514, 
         Shucked_weight = X0.2245,
         Viscera_weight = X0.101, 
         Shell_weight = X0.15, 
         Rings = X15)
print(abalone)


#Distribution of Length
ggplot(abalone, aes(x = Length)) +
  geom_histogram(fill = "pink", color = "black", bins = 10) +
  labs(title = "Distribution of Length", x = "Length (mm)", y = "Count") +
  theme_minimal()
#Max Length
longest_abalone <- max(abalone$Length)
print(longest_abalone)

# Distribution of Diameter
ggplot(abalone, aes(x = Diameter)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 10) +
  labs(title = "Distribution of Diameter", x = "Diameter (mm)", y = "Count") +
  theme_minimal()
#Max Diameter
max_diameter <- max(abalone$Diameter)
print(max_diameter)

#Distribution of Height
ggplot(abalone, aes(x = Height)) +
  geom_histogram(fill = "violet", color = "black", bins = 10) +
  labs(title = "Distribution of Height", x = "Height (mm)", y = "Count") +
  theme_minimal()
#Max Height
max_height <- max(abalone$Height)
print(max_height)

#Distribution of Whole Weight
ggplot(abalone, aes(x = Whole_weight)) +
  geom_histogram(fill = "darkturquoise", color = "black", bins = 10) +
  labs(title = "Distribution of Whole Weight", x = "Whole Weight (grams)", y = "Count") +
  theme_minimal()
#max Whole Weight
max_whole_weight <- max(abalone$Whole_weight)
print(max_whole_weight)

# Distribution of Shucked Weight
ggplot(abalone, aes(x = Shucked_weight)) +
  geom_histogram(fill = "cyan", color = "black", bins = 10) +
  labs(title = "Distribution of Shucked Weight", x = "Shucked Weight (grams)", y = "Count") +
  theme_minimal()
#max shell weight 
max_shell_weight <- max(abalone$Shell_weight)
print(max_shell_weight)
