#' Creates an xlsx from all the csvs in the CSV folder
#'
#' Creates an .xlsx file
#'
#' @param csv_files Directory to folder holding all csv files
#' @return Output excel file in XLSX folder
#' @export
#' @examples
#' Create_xlsx()

Create_xlsx <- function() {
  if (!require("readr")) {
    install.packages("readr")
  }

  if (!require("openxlsx")) {
    install.packages("openxlsx")
  }

  library("readr")
  library("openxlsx")
  loc <- getwd()


  # Get a list of all CSV files in the directory
  csv_files <- list.files(path = "CSVs/", pattern = "\\.csv$", full.names = TRUE)

  # Create a new workbook
  wb <- createWorkbook()

  # Loop through each CSV file and add it as a new sheet in the workbook
  for (i in seq_along(csv_files)) {
    sheet_name <- gsub("\\.csv$", "", basename(csv_files[i]))
    sheet_data <- read_csv(csv_files[i])
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, sheet_data)
  }

  # create the XLSX directory if it doesn't exist
  if (!file.exists("XLSX")) {
    dir.create("XLSX")
  }


  # Save the workbook as an XLSX file
  xlsx_file <- file.path("XLSX", "Conv2Oct_data.xlsx")
  saveWorkbook(wb, xlsx_file)
}
