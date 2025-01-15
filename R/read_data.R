# Script where we provide functions to read in the data file(s).

# IMPORTANT: Data files pushed to GitHub repositories are immediately public.
# You should not be pushing unpublished data to the repository prior to your
# publication date. You should use dummy data or already-published data during
# development of your dashboard.

# In order to help prevent unpublished data being accidentally published, the
# template will not let you make a commit if there are unidentified csv, xlsx,
# tex or pdf files contained in your repository. To make a commit, you will need
# to either add the file to .gitignore or add an entry for the file into
# datafiles_log.csv.


# read_ind_data <- function(file = "data/FEO_industry_202122.zip") {
#   dfInd <- read.csv(file)
#   return(dfInd)
# }


read_ind_data <- function(file = "data/FEO_industry_202122.zip") {
  # Create a temporary directory to extract the file
  temp_dir <- tempdir()
  # Unzip the file into the temporary directory
  unzip(file, exdir = temp_dir)

  # Find the extracted file (assuming only one CSV file in the zip)
  csv_file <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)

  # Read the extracted CSV file
  dfInd <- read.csv(csv_file)

  # Return the data frame
  return(dfInd)
}
