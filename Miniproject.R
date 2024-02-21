

# Loading the dataframe
data <- data.frame(
  park = c("Parque de la Salud", "Parque Guazu", "Botanico", "Yacht", "Jockey", "Parque Carlos Antonio López"),
  richness = c(96, 270, 235, 215, 22, 60),
  area = c(19, 150, 110, 54, 45, 5.9),
  coverage = c(97, 42, 83, 48, 11, 71))

# Establishing the order of parks
desired_park_order <- c("carlos", "salud", "jockey", "yacht", "botanico", "guazu")
data_organized <- mutate(parkdata, park = factor(park, levels = desired_park_order))

# Plotting Area and Coverage of Parks
measuresplot <- ggplot(data_organized, aes(x = park)) +
  geom_bar(aes(y = area, fill = "Area"), stat = "identity", alpha = 0.7) +
  geom_bar(aes(y = coverage, fill = "Coverage"), stat = "identity", alpha = 0.7) +
  geom_text(aes(x = park, y = area, label = sprintf("%.0f", area)), vjust = 0.2, hjust = -0.5, color = "darkred", size = 5) +
  geom_text(aes(x = park, y = coverage, label = sprintf("%.0f", coverage)), vjust = -0.0, hjust = 1.5, color = "darkgreen", size = 5) +
  labs(title = "Area and Coverage of Parks",
       x = "Parks", y = "Area (hectares)") +
  scale_fill_manual(values = c("Area" = "#fc8d62", "Coverage" = "#66c2a5" ), name = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 16),
        legend.position = "bottom",
        legend.title = element_text(face = "italic", size = 16),
        legend.text = element_text(size = 16),
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold", size = 14))

measuresplot

# Plotting Species Richness
richnessplot <- ggplot(data_organized, aes(x = park, y = richness, fill = park)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_text(aes(label = richness), vjust = 0.1, color = "black", size = 5) +
  labs(title = "Species Richness across Parks",
       x = "Parks", y = "Species Richness (units)") +
  scale_fill_manual(values = rep("#ffd92f", nrow(data_organized)), name = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 16),
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold", size = 14),
        legend.box.background = element_rect(color = "black", size = 2))

richnessplot

# Combining plots to seek for trends in the data
combined_plot <- plot_grid(measuresplot, richnessplot, ncol = 1, align = 'v', labels = c("A", "B", "C"))

combined_plot

# Testing for normality in the variables

hist(data_organized$richness)
hist(data_organized$area)
hist(data_organized$coverage)

Neither of the variables follow a normal distribution

# Generating log colums for area and coverage
data_organized$log_area <- log(data_organized$area)
data_organized$log_coverage <- log(data_organized$coverage)

# Performing Poisson GLM
poisson_model <- glm(richness ~ log_area + log_coverage, family = poisson(), data = data_organized)
summary (poisson_model)

# Generating predicted values
predicted_values2 <- predict(poisson, type = "response", data=data_organized)

# Plotting of predicted values on y and coverage on x
ggplot(data_organized, aes(x = coverage, y = richness)) +
  geom_point(aes(color = "Count"), size = 4, alpha = 0.7) +  # Darker blue dots
  geom_line(aes(y = predicted_values2, color = "Predicted value"), size = 2) +  # Red line
  labs(title = "Poisson General Linear Model",
       x = "Coverage",
       y = "Richness") +
  scale_color_manual(values = c("Count" = "#08519c", "Predicted value" = "#e41a1c")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(face = "bold", size = 16),
        plot.title = element_text(face = "bold", size = 18),
        legend.position = "top",
        legend.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 16))


