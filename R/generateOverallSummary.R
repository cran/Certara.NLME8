
# Reads outnnnnn.txt from multiple runs and generates Overall.csv
generateOverallSummary <- function(outputFileNames, scenarioNames, sorting = FALSE) {
  localWorkingDir <- get("localWorkingDir", envir = nlmeEnv)
  GlobalSummaryLine2 <- sprintf("Generating Overall.csv")
  assign("GlobalSummaryLine2", GlobalSummaryLine2, envir = nlmeEnv)
  UpdateProgressMessages()


  if (sorting == TRUE) {
    SortColumns <- .check_numSortColumns()
    num_sort_columns <- SortColumns$num_sort_columns
    unique_sorted_values <- SortColumns$unique_sorted_values
    sort_column_names <- SortColumns$sort_column_names
  } else {
    unique_sorted_values <- NULL

    num_sort_columns <- 0
    sort_column_names <- NULL
  }

  nxtScenario <- 1
  nxtSortKey <- 1
  OverallFilename <- file.path(localWorkingDir, "Overall.csv")

  if (num_sort_columns != 0) {
    sortNames <- paste0(sort_column_names[1:num_sort_columns], ",", collapse = "")
  } else {
    sortNames <- ""
  }

  cat(paste0(sortNames, "Scenario,RetCode,LogLik,-2LL,AIC,BIC,nParm,nObs,nSub,EpsShrinkage,Condition"),
    file = OverallFilename, sep = "\n", append = FALSE
  )

  for (fileName in outputFileNames) {
    if (file.exists(fileName)) {
      sortValues <- ""
      if (num_sort_columns != 0) {
        for (Col in 1:num_sort_columns) {
          nam <- sort_column_names[Col]
          val <- unlist(unique_sorted_values[[Col]])[nxtSortKey]
          if (is.na(val)) {
            val <- unlist(unique_sorted_values[[Col]])[nxtSortKey + 1]
          }
          val <- sub("^\\s+", "", val)
          sortValues <- sprintf("%s%s,", sortValues, val)
        }
      }

      cat(.parse_outputFile(fileName, sortValues, scenarioNames, nxtScenario),
        file = OverallFilename, sep = "\n", append = TRUE
      )
    } else {
      cat(sprintf("%s,,,,,,,,,,", scenarioNames[nxtScenario]), file = OverallFilename, sep = "\n", append = TRUE)
    }

    nxtScenario <- nxtScenario + 1
    if (nxtScenario > length(scenarioNames)) {
      nxtScenario <- 1
      nxtSortKey <- nxtSortKey + 1
    }
  }
}
