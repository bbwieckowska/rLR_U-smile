# Check if required packages are installed, install if missing
required_packages <- c("rstatix", "writexl", "dplyr", "ggplot2", "knitr", "readxl")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)

# ..................................................................................................................
# U-smile plot of rLR (weighted or not)
plot.single.usmile <- function(ref_model, new_model, y_coef, circle_sizes, testing, test_dataset = NULL, y_lim = NULL, net = FALSE, crit = 0) {
  Y <- y_coef
  
  # Prepare training or testing data
  if (testing == FALSE) {
    y <- ref_model$y
    p_ref <- ref_model$fitted.values
    p <- new_model$fitted.values
  } else if (testing == TRUE) {
    dependent_variable_name <- colnames(ref_model$model)[1]  # Get dependent variable name from ref_model
    y <- test_dataset[[dependent_variable_name]]            # Get dependent variable values from test_dataset
    p_ref <- predict(ref_model, newdata = test_dataset, type = "response")
    p <- predict(new_model, newdata = test_dataset, type = "response")
  }
  
  # LR test
  lrt_stat <- as.vector(2 * abs(logLik(new_model) - logLik(ref_model))) 
  df <- abs(attr(logLik(new_model), "df") - attr(logLik(ref_model), "df"))
  p_value <- as.vector(pchisq(lrt_stat, df = df, lower.tail = FALSE)) 
  
  # Calculate classes
  delta <- p - p_ref
  flag <- ifelse(delta > 0, 'up',
                 ifelse(delta < 0, 'dw', 'c'))
  subclass <- ifelse(y == 0 & flag == 'dw', 'nonev_be',
                     ifelse(y == 0 & flag == 'up', 'nonev_wo',
                            ifelse(y == 1 & flag == 'dw', 'event_wo',
                                   ifelse(y == 1 & flag == 'up', 'event_be', 'unknown')))) 
  
  probs <- data.frame(y, p_ref, p, subclass)
  
  # Count events and their weights
  n0 <- sum(probs$y == 0)
  n1 <- sum(probs$y == 1)
  n <- n0 + n1
  
  if (circle_sizes) {
    I_nonev_be <- sum(probs$subclass == 'nonev_be') / n0
    I_nonev_wo <- sum(probs$subclass == 'nonev_wo') / n0
    I_event_wo <- sum(probs$subclass == 'event_wo') / n1
    I_event_be <- sum(probs$subclass == 'event_be') / n1
    point_sizes <- c(I_nonev_be, I_nonev_wo, I_event_wo, I_event_be)
  } else {
    I_nonev_be = NA
    I_nonev_wo = NA
    I_event_wo = NA
    I_event_be = NA
    point_sizes <- c(3, 3, 3, 3)
  }
  
  # Net and overall weights
  netto_I_nonev = I_nonev_be - I_nonev_wo
  netto_I_event = I_event_be - I_event_wo
  I_overall = (netto_I_nonev + netto_I_event)
  
  # Calculate rLR based on y_coef
  res_ref <- (y*log(p_ref)+(1-y)*log(1-p_ref))
  res <- (y*log(p)+(1-y)*log(1-p))
  probs <- data.frame(y, p_ref, p, subclass, res_ref, res)
  
  # Calculate LR residuals sums
  SS_nonev_ref <- sum(probs$res_ref[probs$y == 0])
  SS_event_ref <- sum(probs$res_ref[probs$y == 1])
  SS_nonev <- sum(probs$res[probs$y == 0])
  SS_event <- sum(probs$res[probs$y == 1])
  SS_nonev_dw_ref <- sum(probs$res_ref[probs$subclass == 'nonev_be']) 
  SS_nonev_up_ref <- sum(probs$res_ref[probs$subclass == 'nonev_wo']) 
  SS_event_dw_ref <- sum(probs$res_ref[probs$subclass == 'event_wo']) 
  SS_event_up_ref <- sum(probs$res_ref[probs$subclass == 'event_be']) 
  SS_nonev_dw     <- sum(probs$res[probs$subclass == 'nonev_be']) 
  SS_nonev_up     <- sum(probs$res[probs$subclass == 'nonev_wo']) 
  SS_event_dw     <- sum(probs$res[probs$subclass == 'event_wo']) 
  SS_event_up     <- sum(probs$res[probs$subclass == 'event_be']) 
  delta_SS_nonev_dw <- SS_nonev_dw_ref - SS_nonev_dw
  delta_SS_nonev_up <- SS_nonev_up_ref - SS_nonev_up
  delta_SS_event_dw <- SS_event_dw_ref - SS_event_dw
  delta_SS_event_up <- SS_event_up_ref - SS_event_up
  
  # Calculate LR
  nonev_be <- -2*delta_SS_nonev_dw
  nonev_wo <- 2*delta_SS_nonev_up
  event_wo <- 2*delta_SS_event_dw
  event_be <- -2*delta_SS_event_up
  # Net values
  netto_nonev = nonev_be - nonev_wo
  netto_event = event_be - event_wo
  # Overall
  overall = (netto_nonev + netto_event) 
  
  # Check statistical significance based on LR
  # Cutoff line
  Y_cut_nonev_be <- ifelse(nonev_be >= nonev_wo, nonev_wo + qchisq(1 - 0.05, df), nonev_wo - qchisq(1 - 0.05, df))
  Y_cut_nonev_wo <- ifelse(nonev_be >= nonev_wo, nonev_wo + qchisq(1 - 0.05, df), nonev_wo - qchisq(1 - 0.05, df))
  Y_cut_event_wo <- ifelse(event_be >= event_wo, event_wo + qchisq(1 - 0.05, df), event_wo - qchisq(1 - 0.05, df))
  Y_cut_event_be <- ifelse(event_be >= event_wo, event_wo + qchisq(1 - 0.05, df), event_wo - qchisq(1 - 0.05, df))
  # p-values for LR0 and LR1
  p_Y_nonev <- 1 - pchisq(2*netto_nonev, df = 1)
  p_Y_event <- 1 - pchisq(2*netto_event, df = 1)
  p_Y_overall <- 1 - pchisq(overall, df = 1)
  # Mark statistically significant results in each class
  if (crit == 2 && y_coef == "rLR") {
    nonev_solid <- (p_Y_nonev < 0.05)
    event_solid <- (p_Y_event < 0.05)
  } else {
    nonev_solid <- FALSE
    event_solid <- FALSE
  }
  
  # Calculate maximum LR values for given Y values
  max_netto_Y_nonev <- ifelse(nonev_be >= nonev_wo, -2*SS_nonev_ref, -2*SS_nonev)
  max_netto_Y_event <- ifelse(event_be >= event_wo, -2*SS_event_ref, -2*SS_event)
  max_overall_Y = -2*(SS_nonev_dw_ref + SS_nonev_up_ref + SS_event_dw_ref + SS_event_up_ref) 
  max_netto_Y_ratio = max_netto_Y_event/max_netto_Y_nonev
  
  # Calculate rLR 
  Y_nonev_be <- nonev_be / max_netto_Y_nonev
  Y_nonev_wo <- nonev_wo / max_netto_Y_nonev
  Y_event_wo <- event_wo / max_netto_Y_event
  Y_event_be <- event_be / max_netto_Y_event
  netto_Y_nonev = netto_nonev / max_netto_Y_nonev 
  netto_Y_event = netto_event / max_netto_Y_event
  Y_overall <- overall / max_overall_Y
  
  # Data for plot
  data.Y <- data.frame(subclass = c('nonev_be',
                                    'nonev_wo',
                                    'event_wo',
                                    'event_be'),
                       value = c(Y_nonev_be,
                                 Y_nonev_wo,
                                 Y_event_wo,
                                 Y_event_be),
                       coefficient = rep('Y', 4),
                       segment = c('nonev', 'nonev', 'event', 'event'),
                       wo_be = c(0,1,1,0))
  
  
  
  # Plot settings
  subclass_order <- c('nonev_be',
                      'nonev_wo',
                      'event_wo',
                      'event_be')
  usmile_colors <- c('nonev_be' = '#0F3C78',
                     'nonev_wo' = '#0F3C78',
                     'event_wo' = '#D51424',
                     'event_be' = '#D51424')
  usmile_fills  <- c('nonev_be' = '#0F3C78',
                     'nonev_wo' = '#BED2FA',
                     'event_wo' = '#FBCDB9',
                     'event_be' = '#D51424')
  usmile_labels <- c('non-events with better prediction',
                     'non-events with worse prediction',
                     'events with worse prediction',
                     'events with better prediction')
  fixed_sizes <- c(0, 0.25, 0.5, 0.75, 1) # Fixed point sizes for legend
  
  # Create U-smile plot
  single_usmile <- ggplot(data.Y, aes(x = subclass, y = value, group = coefficient))
  
  # Add lines depending on crit == 2
  if (crit == 2 && y_coef == "rLR") {
    # Lines for non-events segment
    nonev_data <- subset(data.Y, segment == 'nonev')
    single_usmile <- single_usmile + 
      geom_line(data = nonev_data, 
                linetype = ifelse(nonev_solid, "solid", "dotted"),
                size = 0.5, 
                show.legend = FALSE)
    # Lines for events segment
    event_data <- subset(data.Y, segment == 'event')
    single_usmile <- single_usmile + 
      geom_line(data = event_data, 
                linetype = ifelse(event_solid, "solid", "dotted"),
                size = 0.5, 
                show.legend = FALSE)
    # ADDED LINE: Connection between nonev_wo and event_wo
    wo_be_data <- subset(data.Y, wo_be == 1) 
    single_usmile <- single_usmile +
      geom_line(data = wo_be_data,
                linetype = "solid",
                color = "gray",
                size = 0.5,
                show.legend = FALSE)
  } else {
    # Standard dashed line for all points
    single_usmile <- single_usmile +
      geom_line(data = data.Y, 
                linetype = "solid", 
                color = "gray",
                size = 0.5, 
                show.legend = FALSE)
  }
  
  # Add weights (point sizes) and other plot elements
  single_usmile <- single_usmile +
    geom_point(data = data.Y, 
               aes(col = subclass, fill = subclass, size = point_sizes), 
               shape = 21, 
               stroke = 1, 
               show.legend = TRUE) +
    scale_size_continuous(
      breaks = if (circle_sizes) fixed_sizes else NULL, 
      range = if (circle_sizes) c(0.1, 10) else 1, 
      labels = if (circle_sizes) fixed_sizes else NULL, 
      limits = if (circle_sizes) c(min(fixed_sizes), max(fixed_sizes)) else NULL,
      guide = if (circle_sizes) guide_legend(direction = "horizontal") else NULL
    ) +
    scale_x_discrete(limits = subclass_order) +
    scale_color_manual(values = usmile_colors,
                       breaks = subclass_order,
                       labels = usmile_labels) +
    scale_fill_manual(values = usmile_fills,
                      breaks = subclass_order,
                      labels = usmile_labels) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank()) +
    ggtitle(paste(Y, sprintf(" (p = %.4f)", p_value))) +
    coord_cartesian(clip = "off")
  
  if (circle_sizes) {
    single_usmile <- single_usmile +
      guides(size = guide_legend(title = "point size", override.aes = list(shape = 21)))
  }
  
  # Set Y-axis range with 2% margin
  if (!is.null(y_lim)) {
    y_range <- y_lim
  } else if (y_coef == "rLR") {
    y_min <- 0
    y_max <- 1
    y_range <- c(y_min, y_max)
  } else {
    y_min <- min(Y_nonev_be, Y_nonev_wo, Y_event_wo, Y_event_be)
    y_max <- max(Y_nonev_be, Y_nonev_wo, Y_event_wo, Y_event_be)
    y_range <- c(y_min, y_max)
  }
  y_margin <- (y_range[2] - y_range[1]) * 0.02
  y_range[2] <- y_range[2] + y_margin
  single_usmile <- single_usmile + ylim(y_range[1], y_range[2])
  
  # Add Y-axis scaling in percentages with forced range
  my_percent_labels <- function(x) {paste0(x*100, "%")} # Custom function for label formatting
  single_usmile <- single_usmile + 
    scale_y_continuous(
      limits = y_range,  
      labels = my_percent_labels 
    )
  
  # Add vertical lines and labels for net coefficients  
  if (net) {
    # Calculate offset for labels (e.g., 5% of original range)
    y_offset <- (y_range[2] - y_range[1]) * 0.05
    single_usmile <- single_usmile +
      # First vertical line (netto_Y_nonev)
      geom_segment(aes(x = 0.2, xend = 0.2, 
                       y = ifelse(netto_Y_nonev < 0, Y_nonev_wo, Y_nonev_be), 
                       yend = ifelse(netto_Y_nonev < 0, Y_nonev_be, Y_nonev_wo)), 
                   color = ifelse(netto_Y_nonev < 0, '#BED2FA', '#0F3C78'), size = 2) +
      # Label for first line
      geom_label(
        x = 0.5,
        y = max(Y_nonev_be, Y_nonev_wo) + y_offset,
        label = sprintf("%.1f%%", netto_Y_nonev *100),
        color = '#0F3C78',
        label.size = 0, 
        alpha = 0, 
        size=4
      ) +
      # Second vertical line (netto_Y_event)
      geom_segment(aes(x = 4.8, xend = 4.8, 
                       y = ifelse(netto_Y_event < 0, Y_event_wo, Y_event_be), 
                       yend = ifelse(netto_Y_event < 0, Y_event_be, Y_event_wo)), 
                   color = ifelse(netto_Y_event < 0, '#FBCDB9', '#D51424'), size = 2) +
      # Label for second line
      geom_label(
        x = 4.5,
        y = max(Y_event_be, Y_event_wo) + y_offset,
        label = sprintf("%.1f%%", netto_Y_event *100),
        color = '#D51424',
        label.size = 0, 
        alpha = 0, 
        size=4
      )
  }
  
  # Coefficients to return to user
  results <- data.frame(
    label = c(
      "non-events with better prediction",
      "non-events with worse prediction",
      "events with worse prediction",
      "events with better prediction",
      "netto non-events",
      "netto events",
      "overall",
      "max netto non-events",
      "maxnetto events",
      "max overall", 
      "max events non-events ratio",
      "LR non-events p-value",
      "LR events p-value",
      "LR overall p-value",
      "I non-events with better prediction",
      "I non-events with worse prediction",
      "I events with worse prediction",
      "I events with better prediction",
      "I netto netto non-events",
      "I netto netto events",
      "I overall"
    ),
    Y = c(
      data.Y$value,
      netto_Y_nonev,
      netto_Y_event,
      Y_overall,
      max_netto_Y_nonev,
      max_netto_Y_event,
      max_overall_Y, 
      max_netto_Y_ratio,
      p_Y_nonev,
      p_Y_event,
      p_Y_overall,
      I_nonev_be,
      I_nonev_wo,
      I_event_wo,
      I_event_be,
      netto_I_nonev,
      netto_I_event,
      I_overall
    )
  )
  # Change column name to y_coef
  colnames(results)[2] <- "coefficient" 
  
  return(list(plot = single_usmile, data = results))
}


# ..................................................................................................................
# Example code demonstrating how to use the plot.single.usmile() function
# This creates a U-smile plot comparing two logistic regression models

# 1. Load the training dataset
train_dataset <- read_excel('train_dataset_final.xlsx')

# 2. Build the reference model (intercept-only model)
ref_model <- glm(disease ~ 1, data = train_dataset, family = binomial)

# 3. Build the new model (reference model + one predictor)
new_model <- glm(disease ~ 1 + hlt_high_asym, data = train_dataset, family = binomial)

# 4. Generate the U-smile plot with different configurations:

# Option 1: Basic plot without weights
basic_plot <- plot.single.usmile(
  ref_model = ref_model,
  new_model = new_model,
  y_coef = "rLR",          # Plot relative likelihood ratio
  circle_sizes = FALSE,    # Don't show weighted points
  testing = FALSE,         # Use training data
  net = FALSE,             # Don't show net effect lines
  crit = 0                 # No significance testing
)

# Option 2: Enhanced plot with weights and net effects
enhanced_plot <- plot.single.usmile(
  ref_model = ref_model,
  new_model = new_model,
  y_coef = "rLR",          # Plot relative likelihood ratio
  circle_sizes = TRUE,     # Show weighted points (size indicates proportion)
  y_lim = c(0, 0.5),       # Custom Y-axis limits
  testing = FALSE,         # Use training data
  net = TRUE,              # Show net effect lines
  crit = 2                 # Add significance testing (solid/dotted lines)
)

# 5. Display the plot and results
print(enhanced_plot$plot)  # Show the ggplot object
View(enhanced_plot$data)   # View the numerical results

# 6. Save the plot to file
ggsave("u_smile_plot.png", plot = enhanced_plot$plot, width = 8, height = 6, dpi = 300)
