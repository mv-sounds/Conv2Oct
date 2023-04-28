#' Converts Svan 1/3 octave data to octave data
#'
#' This function takes a csv data input, skips the first 14 cols
#' and log sums the third octave data to octave data
#' @param center_freqs The octave band centre freqs
#' @param df the svan data in third octaves
#' @return A new dataframe in octave bands and exports a CSV to a folder
#' @export
#' @examples
#' Conv2Oct(file_path)

Conv2Oct <- function(df,sheet_name) {
  if (!require("dplyr")) {
    install.packages("dplyr")
  }
    library("dplyr")

  if (!require("readxl")) {
    install.packages("readxl")
  }
  library("readxl")

  if (!require("processx")) {
    install.packages("processx")
  }
  library("processx")


    # Create a new folder called "CSVs" if it does not already exist
  if (!dir.exists("CSVs")) {
    dir.create("CSVs")
  }

    # read the sheet into a data frame
  df <- read_excel(file_path,sheet = sheet_name)

    # define the center frequencies for each octave band
  center_freqs <- c(31.5, 63, 125, 250, 500, 1000, 2000, 4000, 8000, 16000)

  # define the corresponding column names for each octave band
  col_names <- paste0(center_freqs, " Hz")

  # create a new data frame to store the octave band levels
  octave_df <- data.frame(matrix(ncol = length(col_names), nrow = nrow(df)))
  colnames(octave_df) <- col_names

  # iterate over each row of the input data frame
  for (i in 1:nrow(df)) {
    # iterate over each octave band
    for (j in 1:length(center_freqs)) {
      # calculate the lower and upper indices of the three columns
      lower_index <- 4 + (j - 1) * 3
      upper_index <- lower_index + 2

      # extract the three decibel levels and convert them to numeric values
      decibel_levels <- as.numeric(df[i, lower_index:upper_index])

      # filter out any NA values
      decibel_levels <- decibel_levels[!is.na(decibel_levels)]

      # calculate the logarithmic sum of the decibel levels and insert the result into the octave_df
      octave_df[i, j] <- 10 * log10(sum(10^(decibel_levels/10)))
      octave_df <- round(octave_df, digits = 1)
          }
  }
  #Subset time, Ln and overall weighted values from original data
  subset_time <- df[, c(1:2)]
  subset_weight <- round(df[, c(34:36)], digits = 1)
  # Combine subsets with original dataframe
  data_df <- cbind(subset_time, octave_df,subset_weight)
  data_df$time <- as.POSIXct(data_df$time, format = "%H:%M:%S")
  # export the new dataframe as octave band data
  write.csv(data_df,  file = "CSVs/Octave_bands.csv", row.names = FALSE)

  return(data_df)
}
