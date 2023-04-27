#' Filters octave data by period and target level
#'
#' Choose number of periods for analysis, enter time periods
#' and enter target levels - levels within 0.5 dB of target level will be filtered for further analysus
#' @param num_periods number of periods for analysis
#' @param data is the octave band data as an output dataframe from Conv2Oct
#' @return New filtered dataframes (periods and target levels) in octave bands and exports a CSV to a folder
#' @export
#' @examples
#' TimePeriods(oct_df,num_periods = 2)


TimePeriods<- function(data, num_periods) {
  # Create a new folder called "CSVs" if it does not already exist
  if (!dir.exists("CSVs")) {
    dir.create("CSVs")
  }
  periods <- list()
  num_periods <- as.numeric(readline(paste0("Enter number of periods for analysis: ")))

  for(i in 1:num_periods) {
    start_time <- as.numeric(readline(paste0("Enter start time for period ", i, ": ")))
    end_time <- as.numeric(readline(paste0("Enter end time for period ",i , ": ")))

    if (end_time < start_time) {
      # Period crosses midnight, so create two subsets
      start_time1 <- sprintf("%02d:00:00", start_time)
      end_time1 <- "23:59:59"
      start_time2 <- "00:00:00"
      end_time2 <- sprintf("%02d:00:00", end_time)

      period_subset <- data %>%
        filter(time >= as.POSIXct(start_time1, format = "%H:%M:%S") &
                 time <= as.POSIXct(end_time1, format = "%H:%M:%S"))

      period_subset2 <- data %>%
        filter(time >= as.POSIXct(start_time2, format = "%H:%M:%S") &
                 time <= as.POSIXct(end_time2, format = "%H:%M:%S"))

      # Combine the two subsets into one
      period_subset <- bind_rows(period_subset, period_subset2)

    } else {
      start_time <- sprintf("%02d:00:15", start_time)
      end_time <- sprintf("%02d:00:00", end_time)

      period_subset <- data %>%
        filter(time >= as.POSIXct(start_time, format = "%H:%M:%S") &
                 time <= as.POSIXct(end_time, format = "%H:%M:%S"))
    }
    # Write unfiltered period subset to a CSV file
    write.csv(period_subset, file = paste0("CSVs/period_", i, ".csv"), row.names = FALSE)

    # Ask for the target level for this period
    target_level <- as.numeric(readline(paste0("Enter target level for period ", i, ": ")))

    # Filter the period subset based on the target level
    filtered_results <- period_subset %>%
      filter(`Leq` >= target_level - 0.5 & `Leq` <= target_level + 0.4)

    # Write filtered period subset to a CSV file
    write.csv(filtered_results, file = paste0("CSVs/period_filt_", i, ".csv"), row.names = FALSE)


    # Assign each period subset to a new data frame
    assign(paste0("period_", i), filtered_results)

    # Add the filtered results to the periods list
    periods[[i]] <- filtered_results
  }

  # Return a list of the period data frames
  period_list <- mget(paste0("period_", 1:num_periods))
 }
