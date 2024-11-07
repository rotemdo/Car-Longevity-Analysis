# install.packages("survival")
# install.packages("scales")

# Load necessary libraries
library(survival)
library(broom)
library(dplyr)
library(ggplot2)
library(data.table)
library(scales)

# Load the data
file_path <- "C:\\Users\\rotem\\OneDrive\\מסמכים\\תואר\\סטטיסטיקה\\שנה ג\\מחקר מודרך\\DATA\\Combined_2018_Final.csv"
df <- fread(file_path, select = c("car_age", "event", "colour", "hierarchy", "fuel_type", "postcode_area", "test_mileage"))

# Sample the data
dfSample <- df %>%
  group_by(colour) %>%
  group_modify(~ slice_sample(.x, n = min(5000, nrow(.x)))) %>%
  ungroup()


# Factorize the columns
dfSample$colour <- factor(dfSample$colour)
dfSample$colour <- relevel(dfSample$colour, ref = "WHITE")
dfSample$hierarchy <- factor(dfSample$hierarchy)
dfSample$fuel_type <- factor(dfSample$fuel_type)
dfSample$postcode_area <- factor(dfSample$postcode_area)

# Creating the survival object
surv_obj <- Surv(time = dfSample$car_age, event = dfSample$event)

# Fitting a Cox proportional hazards model
cox_model <- coxph(surv_obj ~ colour + hierarchy + fuel_type + postcode_area, data = dfSample)
# cox_model <- coxph(surv_obj ~ colour, data = dfSample)

# Viewing the summary of the model
summary(cox_model)

# Get a tidy summary including confidence intervals and exponentiate for hazard ratios
tidy_cox <- tidy(cox_model, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(grepl("^colour", term)) %>%
  mutate(`Effect Direction` = case_when(
    estimate < 1 ~ "Positive",
    estimate > 1 ~ "Negative",
    TRUE ~ "Neutral"
  )) %>%
  mutate(term = sub("^colour", "", term))

# Adjusting estimate for visualization to reflect the effect on longevity in percentages
tidy_cox <- tidy_cox %>%
  mutate(estimate_adj = (1 - estimate))  # Adjust estimate for visualization

# Define custom colors for the plot
custom_colors <- c("BEIGE" = "#F5F5DC", "BLACK" = "#000000", "BLUE" = "#0000FF", "BRONZE" = "#CD7F32", "BROWN" = "#A52A2A", "CREAM" = "#FFFDD0", "GOLD" = "#FFD700", "GREEN" = "#008000", "GREY" = "#808080", "MAROON" = "#800000", "ORANGE" = "#FFA500", "PINK" = "#FFC0CB", "PURPLE" = "#800080", "RED" = "#FF0000", "SILVER" = "#C0C0C0", "TURQUOISE" = "#40E0D0", "WHITE" = "#FFFFFF", "YELLOW" = "#FFFF00")

# Plotting with significance levels and percentage signs on y-axis
plot <- ggplot(tidy_cox, aes(x = reorder(term, estimate_adj), y = estimate_adj, fill = term, color = `Effect Direction`)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors, guide = "none") +  # Remove legend
  scale_color_manual(values = c("Positive" = "green", "Negative" = "red")) +  # Green outline for positive, red for negative
  labs(x = "Car Color", y = "Effect on Car Longevity [%]",
       title = "Effect of Car Color on Car Longevity (Inverse of Hazard)") +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +  # Add percentage signs and scale properly
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels

plot

# ggsave("C:\\Users\\rotem\\OneDrive\\מסמכים\\תואר\\סטטיסטיקה\\שנה ג\\מחקר מודרך\\DATA\\car_color_effect_on_longevity_sample_5000_with_colors_3.pdf", plot = plot, width = 10, height = 6, dpi = 300)