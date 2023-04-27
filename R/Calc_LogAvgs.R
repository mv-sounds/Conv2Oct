#' Takes filtered results from TimePeriods.R and logaverages all decibel levels to a single figure
#'
#' Creates a new dataframe and csv file
#'
#' @param results The filtered results from TimePeriods.R
#' @param num_periods The number of periods for analsysis in TimePeriods.R
#' @return A new single row dataframe of the log-averaged decibel levels
#' @export
#' @examples
#' Calc_LogAvgs(results,num_periods = 2)

Calc_LogAvgs <- function(results, num_periods) {
  avg_levels <- data.frame()  # Initialize empty data frame to store results
  for (i in 1:num_periods) {
    # Get the period data frame
    period <- results[[paste0("period_", i)]]
    # Calculate the logarithmic average level
    period_avg <- period %>%
      # Select columns starting from the third column
      select(3:length(period)) %>%
      # Convert to decibels and calculate the column-wise mean for each column
      summarise(across(everything(), ~ 10 * log10(mean(10^(. / 10))))) %>%
      round(digits = 1)
    # Add the period and average level to the results data frame
    avg_levels <- rbind(avg_levels, period_avg)
  }
  # Add a column to indicate the period number
  avg_levels$period <- paste0("period_", 1:num_periods)
  # Set the period column as the row names
  rownames(avg_levels) <- avg_levels$period
  avg_levels <- avg_levels %>%
    select(period, everything())
  # Export the new data frame as octave band data
  write.csv(avg_levels,  file = "CSVs/Log_Average_levels.csv", row.names = FALSE)
  return(avg_levels)
}
