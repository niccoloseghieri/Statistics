# This script emulate section 4. An Example
# from the paper by: Thomas Struppeck, "Combining Estimates".

# We created an application that replicates the Excel spreadsheet using a custom Shiny application.

# Purpose:
# The purpose of this script is to provide a dynamic tool for exploring and 
# adjusting statistical estimates based on percentile ranges, correlation 
# coefficients among different variables, and the ability to add new lines 
# dynamically. This flexibility allows users to explore various scenarios 
# and obtain combined estimates using three main approaches outlined in the paper:
# - Naïve approach
# - No correlation
# - With covariance adjustment

# In addition the user can visually see the difference among results through box plot to visually compares 
# results across different approaches, providing insights into the variability of estimates.

# Sets the R working directory to the directory where the currently active document in RStudio
# is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the necessary libraries
library(shiny)
library(DT) # For interactive tables
library(ggplot2)
library(shinythemes) # For Shiny app themes

# Load the global variables and functions to make computations 
source("global_variables_2.R")
source("functions_2.R")

# Function to constrain correlation matrix values:
# matrix --> take as input the correlation matrix

# return --> the constrained correlation matrix 

constrained_correlation_matrix <- function(matrix) {
  # Sets diagonal to 1
  diag(matrix) <- 1
  
  # Constrains values between -1 and 1
  matrix <- pmin(pmax(matrix, -1), 1)
  
  # Ensures matrix remains symmetric
  matrix[lower.tri(matrix)] <- t(matrix)[lower.tri(matrix)]
  
  return(matrix)
}

# Define the user interface (UI) of the application
ui <- fluidPage(
  titlePanel("Lines Loss Simulation"),
  theme = shinytheme("cosmo"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("dynamicLines"),
      actionButton("addLine", "Add line"), # Button to add new input line
      hr(),
      
      h4("Correlations"),
      DTOutput("correlationMatrix"), # Output for interactive correlation matrix
      br(),
      
      actionButton("generate", "Simulate Losses") # Button to simulate the results
    ),
    
    mainPanel( 
      DTOutput("summaryTable"), # Result table section
      plotOutput("boxPlot")  # Box plot section
    )
  )
)

# Define the server logic for the Shiny app
server <- function(input, output) {
  
  # Reactive values to store input lines, correlation matrix, and line counter
  values <- reactiveValues(
    
    # Default lines data
    lines = list(A = list(lower_percentile = lower_percentile, upper_percentile = upper_percentile, 
                          lower_estimate = lower_estimate_A, upper_estimate = upper_estimate_A),
                 B = list(lower_percentile = lower_percentile, upper_percentile = upper_percentile,
                          lower_estimate = lower_estimate_B, upper_estimate = upper_estimate_B),
                 C = list(lower_percentile = lower_percentile, upper_percentile = upper_percentile,
                          lower_estimate = lower_estimate_C, upper_estimate = upper_estimate_C)),
    
    # Default correlation matrix
    correlations = matrix(c(1, corr_AB, corr_AC, corr_AB, 1, corr_BC, corr_AC, corr_BC, 1), nrow = 3, ncol = 3),
    
    # Default line count
    lineCounter = 3
  )
  
  # Add a new input line when "Add line" button is clicked
  observeEvent(input$addLine, {
    newLine <- paste0(LETTERS[values$lineCounter + 1]) # Increase the letter
    
    # Assign the default percentiles and set the default values to 0 
    values$lines[[newLine]] <- list(lower_percentile = lower_percentile, upper_percentile = upper_percentile,
                                    lower_estimate = 0, upper_estimate = 0)
    
    values$lineCounter <- values$lineCounter + 1 # Increase the line counter
    
    # Expand the correlation matrix 
    new_corr <- matrix(0, nrow = values$lineCounter, ncol = values$lineCounter) # Create a new correlation matrix with zeros
    # Copy existing correlations into the new matrix, preserving current values (the default ones)
    new_corr[1:(values$lineCounter-1), 1:(values$lineCounter-1)] <- values$correlations
    # Set the diagonal elements to 1
    diag(new_corr) <- 1
    # Update the reactive value 'values$correlations' with the expanded correlation matrix
    values$correlations <- new_corr
  })
  
  # Render dynamic input lines for estimates
  output$dynamicLines <- renderUI({
    # Generate UI elements for each line in 'values$lines'
    linesUI <- lapply(names(values$lines), function(line) {
      lineData <- values$lines[[line]] # Retrieve data for the current line
      # Create a fluid row for each line with three columns: line name, lower inputs, and upper inputs
      fluidRow(
        column(4, h4(line)),
        # Numeric input for lower and upper percentile constrained into an interval 0-1 with buttons to increment it by 0.05 (step)
        column(4,
               numericInput(paste0(line, "_lower_percentile"), "Lower percentile:", value = lineData$lower_percentile, min = 0, max = 1, step = 0.05),
               numericInput(paste0(line, "_lower_estimate"), "Lower estimate:", value = lineData$lower_estimate, min = 0)
        ),
        column(4,
               numericInput(paste0(line, "_upper_percentile"), "Upper percentile:", value = lineData$upper_percentile, min = 0, max = 1, step = 0.05),
               numericInput(paste0(line, "_upper_estimate"), "Upper estimate:", value = lineData$upper_estimate, min = 0)
        )
      )
    })
    # Combine all generated UI elements into a single list and render them
    do.call(tagList, linesUI)
  })
  
  # Render correlation matrix in an interactive datatable
  output$correlationMatrix <- renderDT({
    lineNames <- names(values$lines) # Extract line names for column and row labels
    corr_matrix <- values$correlations # Retrieve the correlation matrix from reactive values
    
    # Set column and row labels of the correlation matrix
    colnames(corr_matrix) <- lineNames
    rownames(corr_matrix) <- lineNames
    
    # Render the correlation matrix as an editable datatable with specific options
    datatable(corr_matrix, editable = TRUE, options = list(
    dom = 't' # Display table without search box and other controls
    ))
  })
  
  # Handle edits made to the correlation matrix by the user
  observeEvent(input$correlationMatrix_cell_edit, { 
    info <- input$correlationMatrix_cell_edit # Capture information about the edited cell
    # Identify the row and column index of the edited cell
    i <- info$row
    j <- info$col
    k <- as.numeric(info$value)
    
    # Update the correlation matrix with the edited value
    values$correlations[i, j] <- k
    values$correlations[j, i] <- k
    
    # Apply the constrained function to ensure the correlation matrix remains valid
    values$correlations <- constrained_correlation_matrix(values$correlations)
  })

# Generate simulation when "Simulate Losses" button is clicked
observeEvent(input$generate, {
  # Extract input data for each line from the user interface
  linesData <- lapply(names(values$lines), function(line) {
    list(
      lwr_percentile = input[[paste0(line, "_lower_percentile")]],
      uppr_percentile = input[[paste0(line, "_upper_percentile")]],
      lower_estimate = input[[paste0(line, "_lower_estimate")]],
      upper_estimate = input[[paste0(line, "_upper_estimate")]]
    )
  })
  # Assign names to the data corresponding to each line
  names(linesData) <- names(values$lines)
  
  # Compute mean and standard deviation using the percentile_matching function
  means_sds <- lapply(linesData, function(line) {
    percentile_matching(line$lwr_percentile, line$uppr_percentile, line$lower_estimate, line$upper_estimate)
  })
  
  # Extract means and standard deviations from the computed results
  means <- sapply(means_sds, function(x) x$mean)
  sds <- sapply(means_sds, function(x) x$sd)
  
  # Build the covariance matrix 
  corrMatrix <- values$correlations
  cov_matrix <- diag(sds) %*% corrMatrix %*% diag(sds)
  
  # Utilize get_perc to obtain the exact 25th and 75th percentiles
  perc_vector <- get_perc(means, sds)
  
  # Unpack lower and upper percentile values for use in subsequent calculations (get_total)
  lower_vect <- perc_vector$perc_25 
  upper_vect <- perc_vector$perc_75
  
  # Calculate statistics using the get_total function for all options (Naïve total, With covariance adjustment and No correlation)
  naive_total <- get_total(means, sds, lower_vect, upper_vect, cov_matrix, 1)
  cov_adj_total <- get_total(means, sds, lower_vect, upper_vect, cov_matrix, 2)
  no_corr_total <- get_total(means, sds, lower_vect, upper_vect, cov_matrix, 3)
  
  # Render the summary table with computed results rounding the calculated statistics
  summary_data <- data.frame(
    "Line.of.Business" = c(names(linesData), "Naïve total", "With Covariance Adjustment", "No Correlation"),
    "Estimated.Mean" = c(round(means, 1), round(naive_total$mean, 1), round(cov_adj_total$mean, 1), round(no_corr_total$mean, 1)),
    "Lower.Estimate" = c(round(lower_vect, 1), round(naive_total$lower_perc_loss, 1), round(cov_adj_total$lower_perc_loss, 1), round(no_corr_total$lower_perc_loss, 1)),
    "Upper.Estimate" = c(round(upper_vect, 1), round(naive_total$upper_perc_loss, 1), round(cov_adj_total$upper_perc_loss, 1), round(no_corr_total$upper_perc_loss, 1)),
    "Standard.Deviation" = c(round(sds, 1), round(naive_total$sd, 1), round(cov_adj_total$sd, 1), round(no_corr_total$sd, 1)),
    "Coefficient.of.Variation" = c(round(sds / means, 3), round(naive_total$cv, 3), round(cov_adj_total$cv, 3), round(no_corr_total$cv, 3))
  )
  
  # Render the summary table using the renderDT function
  output$summaryTable <- renderDT({
    
    # Create a datatable object using summary_data with specific options
    datatable(summary_data, options = list(
      pageLength = 20, 
      autoWidth = TRUE,
      dom = 't'  # Display table without search box and other controls
    ), rownames = FALSE) %>% # Do not display row names in the table
      
      # Format specified columns as currency with a dollar sign and 1 decimal
      formatCurrency(columns = c("Estimated.Mean", "Lower.Estimate", "Upper.Estimate"), currency = "$", digits = 1) %>%
      
      # Apply background color to rows based on values in 'Line.of.Business' column
      formatStyle(
        'Line.of.Business',
        target = 'row',
        backgroundColor = styleEqual(c("Naïve total", "With Covariance Adjustment", "No Correlation"), c('#D8E2FC', '#E4FCD9', '#F8D8D7'))
      )
  })

 # Prepare the data frame for the box plot with the specific statistics
box_data <- data.frame(
  Loss_values = c("Naïve total", "With Covariance Adjustment", "No Correlation"),
  Mean = c(naive_total$mean, cov_adj_total$mean, no_corr_total$mean),
  Lower = c(naive_total$lower_perc_loss, cov_adj_total$lower_perc_loss, no_corr_total$lower_perc_loss),
  Upper = c(naive_total$upper_perc_loss, cov_adj_total$upper_perc_loss, no_corr_total$upper_perc_loss),
  sds = c(naive_total$sd, cov_adj_total$sd, no_corr_total$sd)
)

# Generate the box plot
output$boxPlot <- renderPlot({
  ggplot(box_data, aes(x = Loss_values, fill = Loss_values)) + # Specify x-axis and fill color based on Loss_values
    geom_boxplot(aes(
      ymin = Lower, lower = Lower, middle = Mean, upper = Upper, ymax = Upper, width = 0.4
    ), stat = "identity") +
    geom_point(aes(y = Mean), color = "red", size = 3, show.legend = FALSE) + # Add red points for mean values
    geom_errorbar(aes(ymin = Mean - sds, ymax = Mean + sds), width = 0.1) + # Error bars for mean ± standard deviation
    labs(x = "Loss", y = "Values") + # Axis labels
    scale_fill_manual(values = c("Naïve total" = "#D8E2FC", "With Covariance Adjustment" = "#E4FCD9", "No Correlation" = "#F8D8D7")) +
    theme_minimal()
})
})
}

# Run the application
shinyApp(ui = ui, server = server)