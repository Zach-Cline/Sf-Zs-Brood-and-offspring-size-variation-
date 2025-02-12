# Load  libraries
library(ggplot2)
library(dplyr)
library(effsize)
library(here)

# Define relative file path for the dataset
data_file <- here("data", "neonate_log_transform_data.csv")

# Read the neonates dataset
neonates_data <- read.csv(data_file)

# Subset data by species
segestria_data <- subset(neonates_data, species == "Segestria")
zoropsis_data <- subset(neonates_data, species == "Zoropsis")

# Check normality of Log_Weight and Weight_mg for each species
shapiro_test_segestria_log <- shapiro.test(segestria_data$Log_Weight)
shapiro_test_zoropsis_log <- shapiro.test(zoropsis_data$Log_Weight)

shapiro_test_segestria_raw <- shapiro.test(segestria_data$Weight_mg)
shapiro_test_zoropsis_raw <- shapiro.test(zoropsis_data$Weight_mg)

cat("Shapiro-Wilk Test for Segestria Species (Log-transformed):\n")
print(shapiro_test_segestria_log)

cat("\nShapiro-Wilk Test for Zoropsis Species (Log-transformed):\n")
print(shapiro_test_zoropsis_log)

cat("\nShapiro-Wilk Test for Segestria Species (Raw Weight):\n")
print(shapiro_test_segestria_raw)

cat("\nShapiro-Wilk Test for Zoropsis Species (Raw Weight):\n")
print(shapiro_test_zoropsis_raw)

# Test for variance homogeneity (F-test for Log_Weight and Weight_mg)
var_test_log <- var.test(Log_Weight ~ species, data = neonates_data)
var_test_raw <- var.test(Weight_mg ~ species, data = neonates_data)

cat("\nF-test for equality of variances (Log-transformed Weight):\n")
print(var_test_log)

cat("\nF-test for equality of variances (Raw Weight):\n")
print(var_test_raw)

# Perform Welch's t-test for Log_Weight and Weight_mg
t_test_log <- t.test(Log_Weight ~ species, data = neonates_data, var.equal = FALSE)
t_test_raw <- t.test(Weight_mg ~ species, data = neonates_data, var.equal = FALSE)

cat("\nT-test / Welch's T-test result (Log-transformed Weight):\n")
print(t_test_log)

cat("\nT-test / Welch's T-test result (Raw Weight):\n")
print(t_test_raw)

# Calculate Cohen's d for Log_Weight and Weight_mg
cohen_d_log <- cohen.d(Log_Weight ~ species, data = neonates_data)
cohen_d_raw <- cohen.d(Weight_mg ~ species, data = neonates_data)

cat("\nCohen's d for effect size (Log-transformed Weight):\n")
print(cohen_d_log)

cat("\nCohen's d for effect size (Raw Weight):\n")
print(cohen_d_raw)

# Summary statistics for both raw and log-transformed weights
combined_summary <- neonates_data %>%
  group_by(species) %>%
  summarise(
    raw_mean_weight = mean(Weight_mg, na.rm = TRUE),
    raw_sd_weight = sd(Weight_mg, na.rm = TRUE),
    log_mean_weight = mean(Log_Weight, na.rm = TRUE),
    log_sd_weight = sd(Log_Weight, na.rm = TRUE)
  )

cat("\nSummary Statistics for Raw and Log-Transformed Weights:\n")
print(combined_summary)

# Visualizations
# Raw Weight Boxplot
raw_weight_plot <- ggplot(neonates_data, aes(x = species, y = Weight_mg, fill = species)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Comparison of Raw Offspring Weight Between Species",
    x = "Species",
    y = "Weight (mg)"
  ) +
  theme_minimal()

cat("\nRaw Weight Boxplot Created.\n")
print(raw_weight_plot)

# Log-Transformed Weight Boxplot
log_weight_plot <- ggplot(neonates_data, aes(x = species, y = Log_Weight, fill = species)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Comparison of Log-Transformed Offspring Weight Between Species",
    x = "Species",
    y = "Log-Transformed Weight"
  ) +
  theme_minimal()

cat("\nLog-Transformed Weight Boxplot Created.\n")
print(log_weight_plot)

# Define output file paths
summary_path <- here("data", "offspring_weight_summary.csv")
raw_plot_path <- here("figures", "raw_weight_boxplot.png")
log_plot_path <- here("figures", "log_weight_boxplot.png")

# Export summary statistics and plots
write.csv(combined_summary, summary_path, row.names = FALSE)

ggsave(filename = raw_plot_path, plot = raw_weight_plot, width = 8, height = 6, dpi = 300)
ggsave(filename = log_plot_path, plot = log_weight_plot, width = 8, height = 6, dpi = 300)

# Print message to confirm file saving
message("\nSummary statistics saved to: ", summary_path)
message("Raw weight plot saved to: ", raw_plot_path)
message("Log-transformed weight plot saved to: ", log_plot_path)