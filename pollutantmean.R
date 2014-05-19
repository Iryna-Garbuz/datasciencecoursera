pollutantmean <- function(directory, pollutant, id = 1:10) {
  filenames <- sprintf("%03d.csv", id)
  ## paste() Concatenate vectors after converting to character.
  filenames <- paste(directory, filenames, sep="/")
  
  ldf <- lapply(filenames, read.csv)
  ##ldply() for each element of a list, apply function then combine results into a data frame.
  df=ldply(ldf)
  # df is your list of data.frames
  round(mean(df[, pollutant], na.rm = TRUE), 4)
}