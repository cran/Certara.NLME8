
# Concatenate progress.txt files and generates ConvergenceData.csv
generateConvergenceFile <- function(jobList, unique_sorted_values, outputFileNames, scenarioNames, localWorkingDir) {
  GlobalSummaryLine2 <- paste("Generating ConvergenceData.csv")
  assign("GlobalSummaryLine2", GlobalSummaryLine2, envir = nlmeEnv)
  UpdateProgressMessages()

  num_sort_columns <- get("num_sort_columns", envir = nlmeEnv)
  sort_column_names <- get("sort_column_names", envir = nlmeEnv)

  nxtScenario <- 1
  nxtSortKey <- 1

  HeaderLine <- c("Scenario", "Iter", "Parameter", "Value")
  if (num_sort_columns > 0) {
    HeaderLine <- c(sort_column_names, HeaderLine)
  }

  convergenceDataOverall <- data.frame()
  for (job in jobList) {
    fileToRead <- outputFileNames[job]
    pData <- readProgressDotTxt(fileToRead)
    if (nrow(pData) > 0) {
      scenarioNamesDF <- data.frame(rep(scenarioNames[nxtScenario], nrow(pData)))
      convergenceData <- cbind.data.frame(scenarioNamesDF, pData, row.names = NULL)
      if (num_sort_columns > 0) {
        sortValuesVec <- character(0)
        for (sortColumnIndex in 1:num_sort_columns) {
          val <- unlist(unique_sorted_values[[sortColumnIndex]])[nxtSortKey]
          if (is.na(val)) {
            val <- unlist(unique_sorted_values[[sortColumnIndex]])[nxtSortKey + 1]
          }
          val <- sub("^\\s+", "", val)
          sortValuesVec <- c(sortValuesVec, val)
        }

        sortValuesDF <- t(data.frame(sortValuesVec))
        convergenceData <- cbind.data.frame(sortValuesDF, convergenceData, row.names = NULL)
      }

      convergenceDataOverall <- rbind.data.frame(
        convergenceDataOverall,
        convergenceData
      )
    }

    nxtScenario <- nxtScenario + 1
    if (nxtScenario > length(scenarioNames)) {
      nxtScenario <- 1
      nxtSortKey <- nxtSortKey + 1
    }
  }

  if (nrow(convergenceDataOverall) == 0) {
    if (length(HeaderLine) > 1) {
      convergenceDataOverall <- data.frame(matrix(ncol = length(HeaderLine), nrow = 0))
    } else {
      return()
    }
  }

  colnames(convergenceDataOverall) <- HeaderLine
  OutputFile <- file.path(localWorkingDir, "ConvergenceData.csv")
  write.csv(convergenceDataOverall, file = OutputFile, row.names = FALSE)
}
