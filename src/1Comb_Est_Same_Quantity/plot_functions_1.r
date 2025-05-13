# Function to visualize the best weight (only for 2 estimators)
# optimal_weight --> the optimal weight found

visualize_opt <- function(optimal_weight){
  
  # Plot the function var_to_opt
  curve(var_to_opt(x), from = 0, to = 1, xlab = "Weights", ylab = "Values")
  
  # Add a vertical line at the optimal weight
  abline(v = optimal_weight, col = "red", lwd = 2, lty = 2)
  
  # Add a point at the minimum value
  points(optimal_weight, var_to_opt(optimal_weight), col = "blue", pch = 19)
  
  # Add a text label to indicate the minimum value
  text(optimal_weight, var_to_opt(optimal_weight), labels = round(optimal_weight, 2), pos = 2, col = "blue")
  
  # Display the plot
  title(main = "Optimization of Weights", sub = "Red line indicates the optimal weight")
  
}

# Function that take that plot the density distribution of the estimators passed
# estimators_list     --> the list of estimators for which plot the density
# conf_intervals_list --> the list of each estimators confidence interval
# colors              --> the list of colors to use

display_distributions <- function(estimators_list,
                                  conf_intervals_list, 
                                  colors = NULL) {
  
  # if the colors are not passed use the colors mapped from 1 to the length of the estimators list
  if(is.null(colors)){
    colors = seq(1:length(estimators_list))
  }
  
  name <- names(estimators_list)
  
  # Determine the range for x and y axes
  all_values <- unlist(estimators_list)
  xlim_range <- range(all_values)
  ylim_range <- range(sapply(estimators_list, function(est) density(est)$y))
  
  # Plot the first estimator's density to set up the plot
  plot(density(estimators_list[[1]]), 
       xlab = "Value", ylab = "Density", 
       col = colors[1], 
       xlim = xlim_range, ylim = ylim_range, 
       main = "Distributions")
  
  # Add densities for the rest of the estimators
  for (i in 2:length(estimators_list)) {
    lines(density(estimators_list[[i]]), col = colors[i])
  }
  
  # Add lines to visualize the confidence intervals
  for (i in 1:length(conf_intervals_list)) {
    abline(v = conf_intervals_list[[i]], col = colors[i], lty = 2)
  }
  
  # Create labels for the legend
  labels <- sapply(1:length(conf_intervals_list), function(i) {
    paste("Estimators", name[i], "\nC.I.:",
           round(conf_intervals_list[[i]][1]), "-", round(conf_intervals_list[[i]][2]), 
           "\nW:", round(conf_intervals_list[[i]][2]) - round(conf_intervals_list[[i]][1]))
  })
  
  # Plot the legend
  legend("topleft", 
         legend = labels, 
         col = colors, 
         lwd = 2, 
         cex = 0.70,          # Adjust the text size
         inset = 0.02,        # Adjust the position to be slightly inside the plot area
         bty = "n",           # Remove the box around the legend
         bg = "transparent",  # Set the background to be transparent
         box.lty = 0)         # Remove the box line type
}



# Function to visualize the best intervals for a given estimator w.r.t. to some 
# hypothetical values of the true parameters in a specific range
# range          --> hypothetical (lower and upper) values of the true hypothetical parameters
# distances      --> a list containing the distances for each estimators from the true parameters
# best_estimator --> the names of the estimators for which we want to find where it is the best

visualize_best_intervals <- function(range,
                                     distances,
                                     best_estimator) {
  
  # Ensure the specified estimator is in the distances list
  if (!(best_estimator %in% names(distances))) {
    stop("The specified estimator is not in the distances list.")
  }
  
  # Call get_best_intervals to get the lower and upper values
  best_values <- get_best_intervals(range, distances, best_estimator)
  
  lower <- min(range)
  upper <- max(range)
  
  true_params <- seq(lower, upper, .5)
  
  # Plot the distances for each estimator
  colors <- rainbow(length(distances))
  plot(true_params, distances[[1]], type = "l", col = colors[1],
       xlab = "True Parameter Value", ylab = "Distance from Mean",
       ylim = range(unlist(distances)), lwd = 2)
  
  for (i in 2:length(distances)) {
    lines(true_params, distances[[i]], col = colors[i], lwd = 2)
  }
  
  # Add a legend to the plot
  legend("topleft", legend = names(distances), col = colors, lwd = 2,
          cex = 0.8, inset = 0.02, bty = "n", bg = "transparent", box.lty = 0)
  
  # Show the best region with vertical lines
  abline(v = best_values[1], col = "black", lty = 2)
  abline(v = best_values[2], col = "black", lty = 2)
  
  # Annotate the plot with the intervals where the specified estimator is the best
  text_x_position <- min(true_params) + (max(true_params) - min(true_params)) * 0.50
  text_y_position <- min(unlist(distances)) + (max(unlist(distances)) - min(unlist(distances))) * 0.85
  text(text_x_position, 
       text_y_position, 
       labels = paste(best_estimator, "is best in interval:\n", 
                      paste(round(best_values)[1], "-", round(best_values)[2]), collapse = "\n"), 
                      pos = 4, col = "black")
}