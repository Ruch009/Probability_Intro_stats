#Name :Ruchika Patidar
#CLass: ALY6010 Probability theory and Intro Statistics
#Date: 03/07/24

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


#Sex data table
sex_counts <- table(abalone$Sex)
sex_counts
sex_data <- data.frame(Sex = names(sex_counts), Count = as.numeric(sex_counts))
sex_data

#Sex Distribution Pie chart
library(ggplot2)
sex_colors <- c("F" = "lightcoral", "M" = "lightblue", "I" = "green")
pie_chart <- ggplot(sex_data, aes(x = "", y = Count, fill = Sex)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5), color = "black", size = 4) + # Add count labels
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Sex in Abalone Dataset",
       fill = "Sex",
       x = NULL,
       y = NULL) +
  scale_fill_manual(values = sex_colors) +
  theme_void()
pie_chart

#Descriptive Statistics for the 'Diameter' Variable
library(psych)
describe(abalone$Diameter)
diameter_descriptive <- data.frame(
  Statistics = c("Number of Observations (n)", "Mean", "Standard Deviation (SD)", 
                 "Median", "Trimmed Mean", "Median Absolute Deviation (MAD)", 
                 "Minimum", "Maximum", "Range", "Skewness", "Kurtosis", "Standard Error (SE)"),
  Values = c(4176, 0.41, 0.1, 0.42, 0.41, 0.1, 0.06, 0.65, 0.6, -0.61, -0.05, 0)
)
knitr::kable(diameter_descriptive, align = "l", caption = "Descriptive Statistics for the 'Diameter' Variable")

#Descriptive Statistics for the 'Height' Variable
describe(abalone$Height)
height_descriptive <- data.frame(
  Statistics = c("Number of Observations (n)", "Mean", "Standard Deviation (SD)", 
                 "Median", "Trimmed Mean", "Median Absolute Deviation (MAD)", 
                 "Minimum", "Maximum", "Range", "Skewness", "Kurtosis", "Standard Error (SE)"),
  Values = c(4176, 0.14, 0.04, 0.14, 0.14, 0.04, 0, 1.13, 1.13, 3.13, 75.92, 0)
)
knitr::kable(height_descriptive, align = "l", caption = "Descriptive Statistics for the 'Height' Variable")

#Descriptive Statistics for 'Whole_weight' Variable
describe(abalone$Whole_weight)
whole_weight_descriptive <- data.frame(
  Statistics = c("Number of Observations (n)", "Mean", "Standard Deviation (SD)", 
                 "Median", "Trimmed Mean", "Median Absolute Deviation (MAD)", 
                 "Minimum", "Maximum", "Range", "Skewness", "Kurtosis", "Standard Error (SE)"),
  Values = c(4176, 0.83, 0.49, 0.8, 0.8, 0.53, 0, 2.83, 2.82, 0.53, -0.03, 0.01)
)
knitr::kable(whole_weight_descriptive, align = c("l", "c"), caption = "Descriptive Statistics for 'Whole_weight' Variable")

##Descriptive Statistics for 'Shucked_weight' Variable
describe(abalone$Shucked_weight)
shucked_weight_descriptive <- data.frame(
  Statistics = c("Number of Observations (n)", "Mean", "Standard Deviation (SD)", 
                 "Median", "Trimmed Mean", "Median Absolute Deviation (MAD)", 
                 "Minimum", "Maximum", "Range", "Skewness", "Kurtosis", "Standard Error (SE)"),
  Values = c(4176, 0.36, 0.22, 0.34, 0.34, 0.23, 0, 1.49, 1.49, 0.72, 0.59, 0)
)
knitr::kable(shucked_weight_descriptive, align = c("l", "c"), caption = "Descriptive Statistics for 'Shucked_weight' Variable")

##Descriptive Statistics for 'Viscera_weight' Variable
describe(abalone$Viscera_weight)
viscera_weight_descriptive <- data.frame(
  Statistics = c("Number of Observations (n)", "Mean", "Standard Deviation (SD)", 
                 "Median", "Trimmed Mean", "Median Absolute Deviation (MAD)", 
                 "Minimum", "Maximum", "Range", "Skewness", "Kurtosis", "Standard Error (SE)"),
  Values = c(4176, 0.18, 0.11, 0.17, 0.17, 0.12, 0, 0.76, 0.76, 0.59, 0.08, 0)
)
knitr::kable(viscera_weight_descriptive, align = c("l", "c"), caption = "Descriptive Statistics for 'Viscera_weight' Variable")

##Descriptive Statistics for 'Shell_weight' Variable
describe(abalone$Shell_weight)
shell_weight_descriptive <- data.frame(
  Statistics = c("Number of Observations (n)", "Mean", "Standard Deviation (SD)", 
                 "Median", "Trimmed Mean", "Median Absolute Deviation (MAD)", 
                 "Minimum", "Maximum", "Range", "Skewness", "Kurtosis", "Standard Error (SE)"),
  Values = c(4176, 0.24, 0.14, 0.23, 0.23, 0.15, 0, 1, 1, 0.62, 0.53, 0)
)
knitr::kable(shell_weight_descriptive, align = c("l", "c"), caption = "Descriptive Statistics for 'Shell_weight' Variable")

##Descriptive Statistics for 'Rings' Variable
describe(abalone$Rings)
rings_descriptive <- data.frame(
  Statistics = c("Number of Observations (n)", "Mean", "Standard Deviation (SD)", 
                 "Median", "Trimmed Mean", "Median Absolute Deviation (MAD)", 
                 "Minimum", "Maximum", "Range", "Skewness", "Kurtosis", "Standard Error (SE)"),
  Values = c(4176, 9.93, 3.22, 9, 9.64, 2.97, 1, 29, 28, 1.11, 2.33, 0.05)
)
knitr::kable(rings_descriptive, align = c("l", "c"), caption = "Descriptive Statistics for 'Rings' Variable")

#Length Of Abalone
##table of lenght 
length_summary <- as.data.frame(table(abalone$Length))
length_summary
colnames(length_summary) <- c("Length", "Frequency")
print(length_summary)

#max lenght 
longest_abalone <- max(abalone$Length)
print(longest_abalone)

# Descriptive Statistic table of length  
describe(abalone$Length)
length_descriptive <- data.frame(
  Statistics = c("Number of Observations (n)", "Mean", "Standard Deviation (SD)", 
                 "Median", "Trimmed Mean", "Median Absolute Deviation (MAD)", 
                 "Minimum", "Maximum", "Range", "Skewness", "Kurtosis", "Standard Error (SE)"),
  Values = c(4176, 0.52, 0.12, 0.54, 0.53, 0.12, 0.07, 0.81, 0.74, -0.64, 0.06, 0)
)
knitr::kable(length_descriptive, align = "l", caption = "Descriptive Statistics for the 'Length' Variable")


#Distribution of Length 
ggplot(abalone, aes(x = Length)) +
  geom_histogram(binwidth = 0.1, fill = "pink", color = "black") +
  labs(title = "Length Distribution of Abalone",
       x = "Length",
       y = "Frequency") +
  theme_minimal()

#mean Length
mean_Length <- mean(abalone$Length, na.rm = TRUE)
print(mean_Length)
ggplot(abalone, aes(x = Length)) +
  geom_histogram(binwidth = 0.1, fill = "pink", color = "black") +
  geom_vline(xintercept = mean_Length, color = "red", linetype = "dashed", size = 1) +
  geom_text(aes(x = mean_Length, label = paste("Mean =", round(mean_Length, 2))), 
            y = 50, vjust = -1, color = "red", size = 4) +
  labs(title = "Length Distribution of Abalone",
       x = "Length",
       y = "Frequency") +
  theme_minimal()

#mode Length
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_Length <- Mode (abalone$Length)
print(mode_Length)
ggplot(abalone, aes(x = Length)) +
  geom_histogram(binwidth = 0.1, fill = "pink", color = "black") +
  geom_vline(xintercept = mode_Length, color = "red", linetype = "dashed", size = 1) +
  geom_text(aes(x = mode_Length, label = paste("Mode =", mode_Length)), 
            y = 50, vjust = -1, color = "red", size = 4) +
  labs(title = "Length Distribution of Abalone",
       x = "Length",
       y = "Frequency") +
  theme_minimal()

#standadrd deviation
sd_Length <- sd(abalone$Length)
print(sd_Length)

ggplot(abalone, aes(x = Length)) +
  geom_histogram(binwidth = 0.1, fill = "pink", color = "black") +
  geom_vline(xintercept = mean(abalone$Length) + sd_Length, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean(abalone$Length) - sd_Length, color = "red", linetype = "dashed", size = 1) +
  geom_text(aes(x = mean(abalone$Length) + sd_Length, label = paste("Mean + SD =", round(mean(abalone$Length) + sd_Length, 2))), 
            y = 50, vjust = -1, color = "red", size = 4) +
  geom_text(aes(x = mean(abalone$Length) - sd_Length, label = paste("Mean - SD =", round(mean(abalone$Length) - sd_Length, 2))), 
            y = 50, vjust = -1, color = "red", size = 4) +
  labs(title = "Length Distribution of Abalone",
       x = "Length",
       y = "Frequency") +
  theme_minimal()

#Scatter Plot for Length vs Diameter
scatter_plot <- ggplot(abalone, aes(x = Length, y = Diameter, color = Sex)) +
  geom_point() +
  labs(title = "Scatter Plot: Length vs Diameter",
       x = "Length (mm)",
       y = "Diameter (mm)")
scatter_plot

#Histogram: Whole Weight
histogram_plot <- ggplot(abalone, aes(x = Whole_weight, fill = Sex)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(title = "Histogram: Whole Weight",
       x = "Whole Weight (grams)",
       y = "Frequency")
histogram_plot

#Box plot Rings by Sex
box_plot <- ggplot(abalone, aes(x = Sex, y = Rings, fill = Sex)) +
  geom_boxplot() +
  labs(title = "Box Plot: Rings by Sex",
       x = "Sex",
       y = "Rings") +
  stat_summary(fun.data = "count", geom = "text", aes(label = after_stat(count)), vjust = -0.5, size = 3, color = "black")
box_plot

#Jitter Plot for Length vs Diameter
jitter_plot_length_diameter <- ggplot(abalone, aes(x = Length, y = Diameter, color = Sex)) +
  geom_jitter(width = 0.2, height = 0.2) +
  labs(title = "Jitter Plot: Length vs Diameter",
       x = "Length (mm)",
       y = "Diameter (mm)")
jitter_plot_length_diameter

#Jitter Plot for Whole Weight vs Shucked Weight
jitter_plot_weight <- ggplot(abalone, aes(x = Whole_weight, y = Shucked_weight, color = Sex)) +
  geom_jitter(width = 0.2, height = 0.2) +
  labs(title = "Jitter Plot: Whole Weight vs Shucked Weight",
       x = "Whole Weight (grams)",
       y = "Shucked Weight (grams)")
jitter_plot_weight

#Jitter Plot for Viscera Weight vs Shell Weight
jitter_plot_weight_viscera <- ggplot(abalone, aes(x = Viscera_weight, y = Shell_weight, color = Sex)) +
  geom_jitter(width = 0.2, height = 0.2) +
  labs(title = "Jitter Plot: Viscera Weight vs Shell Weight",
       x = "Viscera Weight (grams)",
       y = "Shell Weight (grams)")
jitter_plot_weight_viscera