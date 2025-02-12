# Load  libraries
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)
library(here)  

# Define relative file path for the dataset
data_file <- here("data", "neonate_log_transform_data.csv")  

# Load the neonate dataset
neonate_data <- read.csv(data_file)

# Linear Mixed-Effects Model: Spiderling Weight by Species (with Mother as a Random Effect)
lme_model <- lmer(Log_Weight ~ species + (1 | mother), data = neonate_data)

# Summarize the LME model
cat("\nLME Results:\n")
print(summary(lme_model))

# Pairwise comparisons for species
pairwise_comparisons <- emmeans(lme_model, "species")
cat("\nPairwise Comparisons:\n")
print(pairwise_comparisons)

# Define output file path
plot_path <- here("figures", "Spiderling_Weight_Comparison.png")

# Visualize the data
spiderling_weight_plot <- ggplot(neonate_data, aes(x = species, y = Log_Weight, fill = species)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(color = "black", width = 0.2, alpha = 0.5) +  # Add points for individual spiderlings
  labs(
    title = "Spiderling Weight Comparison by Species (with Random Effect of Mother)",
    x = "Species",
    y = "Log-Transformed Spiderling Weight"
  ) +
  scale_fill_manual(values = c("Segestria" = "blue", "Zoropsis" = "red")) +
  theme_minimal()

# Save the plot
ggsave(
  filename = plot_path,
  plot = spiderling_weight_plot,
  width = 6,   
  height = 4,  
  dpi = 300,   
  bg = "white"
)

# Print message to confirm file saving
message("\nSpiderling weight comparison plot saved to: ", plot_path)