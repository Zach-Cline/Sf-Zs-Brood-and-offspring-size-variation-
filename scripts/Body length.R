# Load  libraries
library(ggplot2)
library(dplyr)
library(effsize)
library(here)  

# Define relative file path for the dataset
data_file <- here("data", "adult_length.csv")  

# Load the dataset for body length
body_length_data <- read.csv(data_file)

# Preview the dataset 
cat("Preview of the Body Length dataset:\n")
print(head(body_length_data))

# Check column names for uniformity
cat("\nColumn names in the dataset:\n")
print(colnames(body_length_data))

# Rename columns if necessary
colnames(body_length_data) <- c("species", "length_mm")

# Check unique species to confirm data structure
cat("\nUnique species in the dataset:\n")
print(unique(body_length_data$species))

# Subset data by species
segestria_lengths <- subset(body_length_data, species == "Segestria")$length_mm
zoropsis_lengths <- subset(body_length_data, species == "Zoropsis")$length_mm

# Check normality of length for each species
shapiro_test_segestria <- shapiro.test(segestria_lengths)
shapiro_test_zoropsis <- shapiro.test(zoropsis_lengths)

cat("\nShapiro-Wilk Test for Segestria Lengths:\n")
print(shapiro_test_segestria)

cat("\nShapiro-Wilk Test for Zoropsis Lengths:\n")
print(shapiro_test_zoropsis)

# Test for variance homogeneity
var_test_result <- var.test(length_mm ~ species, data = body_length_data)
cat("\nF-test for equality of variances:\n")
print(var_test_result)

# Perform t-test or non-parametric test based on normality and variance results
if (shapiro_test_segestria$p.value > 0.05 && shapiro_test_zoropsis$p.value > 0.05 && var_test_result$p.value > 0.05) {
  # Normal data and equal variances: Perform t-test
  t_test_result <- t.test(length_mm ~ species, data = body_length_data, var.equal = TRUE)
  cat("\nT-test Result:\n")
  print(t_test_result)
} else if (shapiro_test_segestria$p.value > 0.05 && shapiro_test_zoropsis$p.value > 0.05) {
  # Normal data but unequal variances: Perform Welch's t-test
  t_test_result <- t.test(length_mm ~ species, data = body_length_data, var.equal = FALSE)
  cat("\nWelch's T-test Result:\n")
  print(t_test_result)
} else {
  # Non-normal data: Perform Mann-Whitney U test
  mann_whitney_result <- wilcox.test(length_mm ~ species, data = body_length_data)
  cat("\nMann-Whitney U Test Result:\n")
  print(mann_whitney_result)
}

# Calculate Cohen's d for effect size
cohen_d_result <- cohen.d(length_mm ~ species, data = body_length_data)
cat("\nCohen's d for Effect Size:\n")
print(cohen_d_result)


# Define output file paths 
plot_path <- here("figures", "Body_Length_Comparison_by_Species.png")

# Create boxplot 
body_length_plot <- ggplot(body_length_data, aes(x = species, y = length_mm, fill = species)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Body Length Comparison by Species", 
    x = "Species", 
    y = "Body Length (mm)"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, max(body_length_data$length_mm, na.rm = TRUE)), expand = c(0, 0)) +
  scale_fill_manual(values = c("Segestria" = "red", "Zoropsis" = "blue"))  # Custom colors

# Save the plot
ggsave(
  filename = plot_path,
  plot = body_length_plot,
  width = 6,   # Width in inches
  height = 4,  # Height in inches
  dpi = 300,   # High resolution
  bg = "white" # Explicitly set background to white
)

# Print message to confirm file saving
message("\nBody length comparison plot saved to: ", plot_path)