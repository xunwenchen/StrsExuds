plot_plant <- function(data, x_var, y_var, fill_var, ...) {
  ggplot(data, aes_string(x = x_var, y = y_var, fill = fill_var)) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75), 
                alpha = 0.3) +
    scale_fill_manual(values = c("white", "lightgray", "darkgray"))
}

plot_bar <- function(df, var, group, xlab=x, ylab=y) {
    # Calculate mean and standard deviation for each group
    df_summary <- df %>%
      dplyr::group_by(!!rlang::sym(group)) %>%
      dplyr::summarise(mean = mean(!!rlang::sym(var)), sd = sd(!!rlang::sym(var)), max_val = max(!!rlang::sym(var)))
    
    # Perform ANOVA
    aov_res <- stats::aov(stats::as.formula(paste(var, "~", group)), data = df)
    
    # Perform Tukey's HSD test
    tukey_res <- stats::TukeyHSD(aov_res)
    
    # Generate grouping letters
    tukey_letters <- multcompView::multcompLetters4(aov_res, tukey_res)
    df_summary$letters <- tukey_letters[[group]]$Letters
    
    plot <- ggplot2::ggplot(data = df_summary, ggplot2::aes_string(x = group, y = "mean", fill = group)) +
      ggplot2::geom_bar(stat = "identity") +
      # Add jitter points
      ggplot2::geom_jitter(data = df, ggplot2::aes_string(x = group, y = var), width = 0.1, alpha = 0.3, size = 2) +
      # Add error bars using mean and standard deviation
      ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
      # Annotate bars with grouping letters above the max value in proportion to the max value
      ggplot2::geom_text(ggplot2::aes(label = letters, y = max_val + 0.05 * max_val), size = 4) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::scale_fill_brewer(palette = "Dark2") +
      ggplot2::theme_bw()+
      ggplot2::theme(legend.position = "none")
    
    return(plot)
  }

plot_box <- function(df, response, factor) {
  # Perform ANOVA
  aov_res <- aov(as.formula(paste(response, "~", factor)), data = df)
  
  # Perform Tukey's HSD test using agricolae package
  library(agricolae)
  tukey.rs <- HSD.test(aov_res, factor, group = TRUE)
  
  # Plot boxplot with jitter points
  library(ggplot2)
  p <- ggplot(df, aes_string(x = factor, y = response, fill = factor)) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75), 
                alpha = 0.3) +
    scale_fill_manual(values = c("white", "lightgray", "darkgray"))
  
  # Add Tukey group letters above the boxplot
  tk.rs <- tukey.rs$groups
  tk.rs[[factor]] <- rownames(tk.rs)
  max_values <- aggregate(df[[response]], by = list(df[[factor]]), FUN = max)
  colnames(max_values) <- c(factor, "max_value")
  tk.rs <- merge(tk.rs, max_values, by = factor)
  # Set y-axis limits to be proportionally longer
  y_max <- max(df[[response]]) * 1.02
  p <- p + geom_text(data = tk.rs, aes_string(x = factor, y = "max_value", label = "groups"), vjust = -1, size = 4)+
    ylim(NA, y_max)
  
  return(p)
}


adonis2_pairwise <- function(physeq_obj, group_var, method = "wunifrac") {
  # Calculate distance matrix using the specified method
  distance_matrix <- phyloseq::distance(physeq_obj, method = method)
  
  # Extract sample data
  sample_data <- data.frame(sample_data(physeq_obj))
  
  # Construct the formula dynamically
  formula <- as.formula(paste("distance_matrix ~", group_var))
  
  # Conduct adonis2 test
  adonis2_result <- adonis2(formula, data = sample_data)
  print(adonis2_result)
  

  print(adonis2_result)
  
  # Perform pairwise comparisons
  pairwise_result <- pairwise.adonis(distance_matrix, sample_data[[group_var]])
  print(pairwise_result)
}


plot_pcoa <- function(physeq_obj, distance_method = "unifrac", weighted = TRUE, color_var, shape_var, title) {
  # Perform PCoA ordination
  if (distance_method %in% c("unifrac", "wunifrac")) {
    pcoa_res <- ordinate(physeq_obj, method = "PCoA", distance = distance_method, weighted = weighted)
  } else {
    pcoa_res <- ordinate(physeq_obj, method = "PCoA", distance = distance_method)
  }
  
  # Create the PCoA plot
  pcoa_plot <- plot_ordination(physeq_obj, pcoa_res, color = color_var, shape = shape_var) + 
    geom_point(size = 3) + 
    stat_ellipse(aes_string(group = color_var), type = "norm", level = 0.95) + 
    ggtitle(title) +
    theme_bw()
  
  return(pcoa_plot)
}


