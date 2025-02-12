# Load  libraries
library(ggplot2)
library(dplyr)
library(here)

# Define relative file path
data_file <- here("data", "adult_length.csv")  # Points to the "data" folder in the repo

# Load the dataset for body length
body_length_data <- read.csv(data_file)

# Rename columns for uniformity
colnames(body_length_data) <- c("species", "length_mm")

# Update species names to full scientific names
body_length_data$species <- recode(body_length_data$species, 
                                   "Segestria" = "Segestria florentina", 
                                   "Zoropsis" = "Zoropsis spinimana")

# Convert species names to factors 
body_length_data$species <- factor(body_length_data$species, 
                                   levels = c("Segestria florentina", "Zoropsis spinimana"))

# Create the plot
body_length_plot <- ggplot(body_length_data, aes(x = species, y = length_mm, fill = species)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = " ",
    x = "Species", 
    y = "Body Length (mm)"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, max(body_length_data$length_mm, na.rm = TRUE)), expand = c(0, 0)) +
  scale_fill_manual(values = c("Segestria florentina" = "red", "Zoropsis spinimana" = "blue")) +
  scale_x_discrete(labels = c(
    "Segestria florentina" = expression(italic("Segestria florentina")),
    "Zoropsis spinimana" = expression(italic("Zoropsis spinimana"))
  )) + # Italicize species names
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none" # Remove legend
  )

# Save the plot using a relative path
plot_file <- here("figures", "Body_Length_Comparison_by_Species.png")
ggsave(
  filename = plot_file,
  plot = body_length_plot,
  width = 6,   # Width in inches
  height = 4,  # Height in inches
  dpi = 300,   # High resolution
  bg = "white" # Explicitly set background to white
)

# Print message for confirmation
message("Plot successfully saved to: ", plot_file)