# Load  libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(here)  # File paths are relative to the project

# Define relative file path for dataset
data_file <- here("data", "reproduction_data.csv")

# Load the reproduction dataset
reproduction_data <- read.csv(data_file)

# Rename columns for uniformity
colnames(reproduction_data) <- c(
  "species", "mother", 
  "live_offspring_egg_sac_1", "dead_eggs_egg_sac_1",
  "live_offspring_egg_sac_2", "dead_eggs_egg_sac_2",
  "live_offspring_egg_sac_3", "dead_eggs_egg_sac_3",
  "live_offspring_egg_sac_4", "dead_eggs_egg_sac_4",
  "total_live_offspring_all_egg_sacs", "total_all_spiderlings_and_eggs"
)

# Update species names to full names
reproduction_data$species <- ifelse(reproduction_data$species == "Segestria", 
                                    "Segestria florentina", 
                                    "Zoropsis spinimana")

# Extract egg sac columns and reshape data
egg_sac_data <- reproduction_data %>%
  select(
    species, 
    starts_with("live_offspring_egg_sac"), 
    starts_with("dead_eggs_egg_sac")
  ) %>%
  pivot_longer(
    cols = -species, 
    names_to = "egg_sac", 
    values_to = "count"
  ) %>%
  mutate(
    type = ifelse(grepl("live", egg_sac), "Live Offspring", "Dead Offspring"),
    egg_sac_number = as.numeric(gsub("\\D", "", egg_sac))  # Extract egg sac number
  ) %>%
  filter(!is.na(count))  # Remove rows with missing values

# Calculate means for each egg sac, type, and species
mean_egg_sac_data <- egg_sac_data %>%
  group_by(species, egg_sac_number, type) %>%
  summarise(mean_count = mean(count, na.rm = TRUE)) %>%
  ungroup()

# Stacked Bar Plot by Species
stacked_bar_plot <- ggplot(mean_egg_sac_data, aes(x = as.factor(egg_sac_number), y = mean_count, fill = type)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ species) +  # Facet by species
  labs(
    title = "Mean Number of Live and Dead Offspring by Egg Sac (Separated by Species)",
    x = "Egg Sac Number",
    y = "Mean Offspring Count",
    fill = "Offspring Type"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 14),  # Increase facet label size
    panel.background = element_rect(fill = "white", color = NA),  # White background
    plot.background = element_rect(fill = "white", color = NA)    # White background
  )

# Side-by-Side Boxplots by Species
boxplot_plot <- ggplot(egg_sac_data, aes(x = as.factor(egg_sac_number), y = count, fill = type)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ species) +  # Facet by species
  labs(
    title = " ",
    x = "Egg Sac Number",
    y = "Offspring Count",
    fill = "Offspring Type"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 14),  # Increase facet label size
    panel.background = element_rect(fill = "white", color = NA),  # White background
    plot.background = element_rect(fill = "white", color = NA)    # White background
  )

# Define output file paths
stacked_bar_path <- here("figures", "Mean_Live_Dead_Offspring_Stacked_Bar_Plot.png")
boxplot_path <- here("figures", "Distribution_Live_Dead_Offspring_Boxplot.png")

# Save the plots
ggsave(
  filename = stacked_bar_path,
  plot = stacked_bar_plot,
  width = 6,
  height = 4,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = boxplot_path,
  plot = boxplot_plot,
  width = 6,
  height = 4,
  dpi = 300,
  bg = "white"
)

# Print message to confirm file saving
message("Stacked bar plot saved to: ", stacked_bar_path)
message("Boxplot saved to: ", boxplot_path)