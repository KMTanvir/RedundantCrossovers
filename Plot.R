# Installing required package (If required)
# devtools::install_github("matherealize/looplot")

# Required packages
library(looplot)
library(ggpubr)
library(tidyr)
library(dplyr)

simulation_results <- read.csv("simulation_results.csv")

simulation_results$ICC <- as.factor(simulation_results$ICC)
simulation_results$Design <- factor(simulation_results$Design, labels = c("Midpoint", "Multiple"))
simulation_results$CAC <- as.factor(simulation_results$CAC)
simulation_results$Design <- as.factor(simulation_results$Design)

# Create a dummy staircase variable using Time
simulation_results$Period <- as.numeric(as.character(simulation_results$Time))

# Pivot wider to split Power column by Design
simulation_wide <- simulation_results %>%
  pivot_wider(
    names_from = Design,
    values_from = Power,
    names_prefix = "Power_"
  )

# Theoretical power according to the combination in simulation results, calculated using https://clusterrcts.shinyapps.io/rshinyapp/
Theoretical_Power <- c(
  0.773, 0.913, 0.969, 0.651, 0.82, 0.914, 0.537, 0.71, 0.827, 0.796, 0.927, 0.976,
  0.75, 0.898, 0.962,  0.699, 0.86, 0.94, 0.808, 0.933, 0.979, 0.808, 0.933, 0.979,
  0.808, 0.933, 0.979, 0.811, 0.936, 0.98, 0.827, 0.945, 0.984, 0.846, 0.955, 0.988
)

simulation_wide <- cbind(simulation_wide, Theoretical_Power)


nested_loop_plot(
  resdf = simulation_wide,
  x = "ICC",
  steps = c("CAC"),
  grid_rows = "Period",
  steps_y_base = 0.4,
  steps_y_height = 0.05,
  steps_y_shift = 0.05,
  
  # Include all methods to plot
  methods = c("Power_Midpoint", "Power_Multiple", "Theoretical_Power"),
  
  # Define colors for each method
  colors = c(
    "Power_Midpoint" = "#fe704a",         # dark red
    "Power_Multiple" = "#4acdfe",         # light blue
    "Theoretical_Power" = "black"         # black for theoretical
  ),
  
  # Define line types for each method
  line_linetypes = c(
    "Power_Midpoint" = "solid",
    "Power_Multiple" = "dashed",
    "Theoretical_Power" = "dotted"        # dotted line for theory
  ),
  
  # Define point shapes (no point for theoretical)
  point_shapes = c(
    "Power_Midpoint" = 16,
    "Power_Multiple" = 16,
    "Theoretical_Power" = NA              # no point for theoretical line
  ),
  
  point_size = 2.5,
  line_size = 1,
  
  # Step labels
  steps_values_annotate = TRUE,
  steps_annotation_size = 2.5,
  steps_annotation_nudge = 0.1,
  
  # Axis and legend labels
  y_name = "Power",
  x_name = "ICC",
  y_expand_add = c(0.05, 0.05),
  
  legend_name = "Design",
  legend_labels = c(
    "Power_Midpoint" = "Single Crossover",
    "Power_Multiple" = "Multiple Crossover",
    "Theoretical_Power" = "Theoretical Power"
  ),
  
  # Theme adjustments
  post_processing = list(
    add_custom_theme = list(
      legend.position = "bottom",
      legend.key.width = grid::unit(2.5, "cm"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      strip.text = ggplot2::element_text(size = 12, face = "bold"),
      plot.margin = ggplot2::margin(10, 10, 25, 10),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 12)),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 5))
    )
  )
)

