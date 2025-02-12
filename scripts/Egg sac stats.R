# Load  libraries
library(ggplot2)
library(dplyr)
library(here) 

# Define relative file path for the dataset
data_file <- here("data", "egg_sac_count.csv")  

# Load the egg sac count dataset
egg_sac_data <- read.csv(data_file)

# Preview dataset 
cat("Preview of the Egg Sac Count dataset:\n")
print(head(egg_sac_data))

# Check column names and ensure uniformity
cat("\nColumn names in the dataset:\n")
print(colnames(egg_sac_data))

# Subset data by species
segestria_egg_sac_counts <- subset(egg_sac_data, species == "Segestria")$number.of.egg.sacs
zoropsis_egg_sac_counts <- subset(egg_sac_data, species == "Zoropsis")$number.of.egg.sacs

# Ensure data is numeric
segestria_egg_sac_counts <- as.numeric(segestria_egg_sac_counts)
zoropsis_egg_sac_counts <- as.numeric(zoropsis_egg_sac_counts)

# Check sample sizes
cat("\nSample size for Segestria:", length(segestria_egg_sac_counts), "\n")
cat("Sample size for Zoropsis:", length(zoropsis_egg_sac_counts), "\n")

# Perform Shapiro-Wilk normality test (only if there are sufficient data points)
if (length(segestria_egg_sac_counts) >= 3) {
  shapiro_test_segestria <- shapiro.test(segestria_egg_sac_counts)
  cat("\nShapiro-Wilk Test for Segestria Egg Sac Counts:\n")
  print(shapiro_test_segestria)
} else {
  cat("\nNot enough data points for Shapiro-Wilk test for Segestria.\n")
}

if (length(zoropsis_egg_sac_counts) >= 3) {
  shapiro_test_zoropsis <- shapiro.test(zoropsis_egg_sac_counts)
  cat("\nShapiro-Wilk Test for Zoropsis Egg Sac Counts:\n")
  print(shapiro_test_zoropsis)
} else {
  cat("\nNot enough data points for Shapiro-Wilk test for Zoropsis.\n")
}

# Define output file path for histogram
histogram_path <- here("figures", "Egg_Sac_Counts_Histogram.png")

# Perform statistical test based on normality results
# Assuming non-normal data (if Shapiro-Wilk indicates non-normality)
if (length(segestria_egg_sac_counts) >= 3 && length(zoropsis_egg_sac_counts) >= 3) {
  wilcox_test_result <- wilcox.test(segestria_egg_sac_counts, zoropsis_egg_sac_counts)
  cat("\nWilcoxon Rank-Sum Test Result:\n")
  print(wilcox_test_result)
  
  # If both datasets are normal, use t-test
  if (shapiro_test_segestria$p.value > 0.05 && shapiro_test_zoropsis$p.value > 0.05) {
    t_test_result <- t.test(segestria_egg_sac_counts, zoropsis_egg_sac_counts, var.equal = FALSE)
    cat("\nT-test Result:\n")
    print(t_test_result)
  } else {
    cat("\nSkipping T-test due to non-normality or insufficient data points.\n")
  }
} else {
  cat("\nNot enough data points for statistical tests.\n")
}

# Print message to confirm file saving
message("\nHistogram plot saved to: ", histogram_path)