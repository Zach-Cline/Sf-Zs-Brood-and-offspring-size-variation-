# Load libraries
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)
library(here) 

# Define relative file path for the dataset
data_file <- here("data", "neonate_log_transform_data.csv") 

# Load the neonate dataset
neonate_data <- read.csv(data_file)

# Update species names to full scientific names
neonate_data$species <- factor(ifelse(neonate_data$species == "Segestria", 
                                      "Segestria florentina", 
                                      "Zoropsis spinimana"),
                               levels = c("Segestria florentina", "Zoropsis spinimana")) # Maintain order

# Linear Mixed-Effects Model Spiderling Weight by Species (with Mother as a Random Effect)
lme_model <- lmer(Log_Weight ~ species + (1 | mother), data = neonate_data)

# Summarize the LME model
cat("\nLME Results:\n")
print(summary(lme_model))

# Pairwise comparisons for species
pairwise_comparisons <- emmeans(lme_model, "species")
cat("\nPairwise Comparisons:\n")
print(pairwise_comparisons)

# Spiderling Weight Comparison
plot <- ggplot(neonate_data, aes(x = species, y = Log_Weight, fill = species)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(color = "black", width = 0.2, alpha = 0.5) +  # Add points for individual spiderlings
  labs(
    title = " ",
    x = "Species",
    y = "Log-Transformed Spiderling Weight (mg)"
  ) +
  scale_fill_manual(values = c("Segestria florentina" = "blue", "Zoropsis spinimana" = "red")) +
  scale_x_discrete(labels = c(
    "Segestria florentina" = expression(italic("Segestria florentina")),
    "Zoropsis spinimana" = expression(italic("Zoropsis spinimana"))
  )) + # Italicize species names
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    panel.background = element_rect(fill = "white", color = NA),  # White background
    plot.background = element_rect(fill = "white", color = NA),   # White background
    legend.position = "none"  # Remove legend
  )

# Define output file path 
plot_path <- here("figures", "Spiderling_Weight_Comparison_by_Species.png")

# Save the plot
ggsave(
  filename = plot_path,
  plot = plot,
  width = 6,   # Width in inches
  height = 4,  # Height in inches
  dpi = 300,   # High resolution
  bg = "white" # White background
)

# Print message to confirm file saving
message("Plot saved to: ", plot_path)