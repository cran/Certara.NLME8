concatenateInitEstimates <- function(jobList, unique_sorted_values, scenarioNames, localWorkingDir) {
  GlobalSummaryLine2 <- sprintf("Generating initest.csv")
  assign("GlobalSummaryLine2", GlobalSummaryLine2, envir = nlmeEnv)
  UpdateProgressMessages()

  num_sort_columns <- get("num_sort_columns", envir = nlmeEnv)
  sort_column_names <- get("sort_column_names", envir = nlmeEnv)
  jobsDirectoryRoot <- get("jobsDirectoryRoot", envir = nlmeEnv)
  jobsBaseDirectory <- getBatchDirectoryLocation(jobsDirectoryRoot)

  resultsDataframe <- NULL
  first <- TRUE
  nxtScenario <- 1
  nxtSortKey <- 1
  missingFiles <- TRUE
  for (job in jobList) {
    files <- c()
    jobBaseIndx <- job %% 100
    fileToRead <- sprintf("%s/jobs/%02d/%d/%s", jobsBaseDirectory, jobBaseIndx, job, "iniest.csv")

    if (file.exists(fileToRead) && file.info(fileToRead)$size != 0) {
      missingFiles <- FALSE
      dataf <- read.csv(fileToRead, header = TRUE)
      dataf[, c(1:5)] <- NULL

      colnames(dataf) <- c("parameter", "init", "low", "high")
      dataf <- cbind("Scenario" = scenarioNames[nxtScenario], dataf)
      if (num_sort_columns > 0) {
        for (c in num_sort_columns:1) {
          nam <- sort_column_names[c]
          val <- unlist(unique_sorted_values[[c]])[nxtSortKey]
          if (is.na(val)) {
            val <- unlist(unique_sorted_values[[c]])[nxtSortKey + 1]
          }
          dataf <- cbind(nam = val, dataf)

          cn <- colnames(dataf)
          cn[1] <- nam
          colnames(dataf) <- cn
        }
      }
      if (first == TRUE) {
        resultsDataframe <- dataf
      } else {
        resultsDataframe <- rbind(resultsDataframe, dataf)
      }
      first <- FALSE
    }
    nxtScenario <- nxtScenario + 1
    if (nxtScenario > length(scenarioNames)) {
      nxtScenario <- 1
      nxtSortKey <- nxtSortKey + 1
    }
  }
  if (missingFiles == FALSE) {
    write.csv(resultsDataframe,
      file = file.path(localWorkingDir, "iniest.csv"),
      row.names = FALSE, quote = FALSE, na = ""
    )
  }
}
