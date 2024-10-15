# Summarizes the results from profile estimation runs
# generates Overall.csv and StatusWindows.txt
summarizeProfileEstimation <-
  function(localWorkingDir, jobList, control_lines) {
    workflow_name <- get("workflow_name", envir = nlmeEnv)
    unique_sorted_values <-
      get("unique_sorted_values", envir = nlmeEnv)
    jobsDirectoryRoot <- get("jobsDirectoryRoot", envir = nlmeEnv)
    jobsBaseDirectory <- getBatchDirectoryLocation(jobsDirectoryRoot)

    GlobalSummaryLine1 <-
      sprintf("Summarizing profile estimation %d runs", length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir = nlmeEnv)
    assign("GlobalSummaryLine2", "", envir = nlmeEnv)
    assign("GlobalSummaryLine3", "", envir = nlmeEnv)
    UpdateProgressMessages()

    if (jobsDirectoryRoot != "") {
      jobsDirectory <-
        file.path(jobsBaseDirectory,
                  "jobs",
                  sprintf("%02d", jobList %% 100),
                  jobList)
      OutFileNames <-
        file.path(jobsDirectory, getRunSuccessFilename(control_lines))
      progressfilenames <- file.path(jobsDirectory, "progress.txt")
      statusFiles <-
        file.path(jobsDirectoryRoot, sprintf("S_%03d.status", as.integer(jobList)))
    } else {
      OutFileNames <- file.path(localWorkingDir,
                                paste0(getRunSuccessFilename(control_lines), ".Job", jobList))
      progressfilenames <- file.path(localWorkingDir,
                                     paste0("progress.txt.Job", jobList))
      statusFiles <- c()
    }

    scenarioNames <- unique(getScenarioName(control_lines))
    generateProfileSummary(OutFileNames, scenarioNames, TRUE)

    generateStatusWindow(statusFiles, progressfilenames, scenarioNames)
  }

# Reads outnnnnn.txt from multiple runs and generates Profile.csv
generateProfileSummary <-
  function(outputFileNames, scenarioNames, sorting = FALSE) {
    workflow_name <- get("workflow_name", envir = nlmeEnv)
    localWorkingDir <- get("localWorkingDir", envir = nlmeEnv)

    profileModels <- getListOfExesNeeded()
    if (sorting == TRUE) {
      unique_sorted_values <- get("unique_sorted_values", envir = nlmeEnv)
      num_sort_columns <- get("num_sort_columns", envir = nlmeEnv)
      sort_column_names <- get("sort_column_names", envir = nlmeEnv)
    } else {
      unique_sorted_values <- NULL

      num_sort_columns <- 0
      sort_column_names <- NULL
    }

    nxtScenario <- 1
    nxtSortKey <- 1
    nxtProfileIndex <- 1
    ProfileFilename <- file.path(localWorkingDir, "Profile.csv")
    sortNames <- ""
    if (num_sort_columns != 0) {
      for (c in 1:num_sort_columns) {
        nam <- sort_column_names[c]
        sortNames <- sprintf("%s%s,", sortNames, nam)
      }
    }
    cat(
      sprintf(
        "%sScenario,Theta,Estimate,LogLik,RetCode,Delta,Percent",
        sortNames
      ),
      file = ProfileFilename,
      sep = "\n",
      append = FALSE
    )
    workflow_name <- get("workflow_name", envir = nlmeEnv)

    UpdateProgressMessages()
    for (fileName in outputFileNames) {
      if (!file.exists(fileName)) {
        cat(
          sprintf("%s,,,,,,", scenarioNames[nxtScenario]),
          file = ProfileFilename,
          sep = "\n",
          append = TRUE
        )
        next
      }

      lines <- .get_outtxt(fileName)
      ReturnCode_line <- grep("^ReturnCode", lines)
      if (length(ReturnCode_line) == 0) {
        cat(
          sprintf("%s,,,,,,", scenarioNames[nxtScenario]),
          file = ProfileFilename,
          sep = "\n",
          append = TRUE
        )
      } else {
        returnCode <- as.integer(gsub("^ReturnCode\\s*=\\s*",
                                      "",
                                      lines[ReturnCode_line]))

        Column <- "LogLikelihood"
        logLik <- as.double(gsub(paste0("^", Column, "\\s*=\\s*"),
                                 "",
                                 lines[grep(paste0("^", Column), lines)][1]))


        sortValues <- ""
        if (num_sort_columns != 0) {
          for (c in 1:num_sort_columns) {
            nam <- sort_column_names[c]
            val <- unlist(unique_sorted_values[[c]])[nxtSortKey]
            if (is.na(val)) {
              val <- unlist(unique_sorted_values[[c]])[nxtSortKey + 1]
            }
            val <- sub("^\\s+", "", val)
            sortValues <- sprintf("%s%s,", sortValues, val)
          }
        }

        cat(
          sprintf(
            "%s%s,%s,%f,%f,%d,%s,%s",
            sortValues,
            scenarioNames[nxtScenario],
            profileModels[[nxtProfileIndex]]$theta,
            profileModels[[nxtProfileIndex]]$initialValue,
            logLik,
            returnCode,
            profileModels[[nxtProfileIndex]]$delta,
            profileModels[[nxtProfileIndex]]$percent
          ),
          file = ProfileFilename,
          sep = "\n",
          append = TRUE
        )
      }

      nxtProfileIndex <- nxtProfileIndex + 1
      if (nxtProfileIndex > length(profileModels)) {
        nxtProfileIndex <- 1
        nxtScenario <- nxtScenario + 1
      }
      if (nxtScenario > length(scenarioNames)) {
        nxtScenario <- 1
        nxtSortKey <- nxtSortKey + 1
      }
    }
  }
