# Summarizes the results from a covariate search run.
# generates Overall.csv and StatusWindows.txt
summarizeCovarSearch <-
  function(localWorkingDir, jobList, control_lines) {
    workflow_name <- get("workflow_name", envir = nlmeEnv)
    jobsDirectoryRoot <- get("jobsDirectoryRoot", envir = nlmeEnv)
    jobsBaseDirectory <-
      getBatchDirectoryLocation(jobsDirectoryRoot)
    GlobalSummaryLine1 <-
      sprintf("Summarizing shotgun covariate search results for %d scenarios",
              length(jobList))
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir = nlmeEnv)
    GlobalSummaryLine2 <- ""
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir = nlmeEnv)
    GlobalSummaryLine3 <- ""
    assign("GlobalSummaryLine3", GlobalSummaryLine3, envir = nlmeEnv)
    UpdateProgressMessages()

    fullOutputFileNames <- c()
    fullProgressFileNames <- c()
    fullStatusFileNames <- c()
    scenarioNames <- c()
    for (job in jobList) {
      jobBaseIndx <- job %% 100

      outputFileName <- getOutputFilenames(control_lines[job])
      fullOutputFileName <-
        sprintf("%s/jobs/%02d/%d/%s",
                jobsBaseDirectory,
                jobBaseIndx,
                job,
                outputFileName)
      fullOutputFileNames <-
        c(fullOutputFileNames, fullOutputFileName)

      progressFileName <- "progress.txt"
      fullProgressFileName <-
        sprintf("%s/jobs/%02d/%d/%s",
                jobsBaseDirectory,
                jobBaseIndx,
                job,
                progressFileName)
      fullProgressFileNames <-
        c(fullProgressFileNames, fullProgressFileName)

      statusFileName <- sprintf("S_%03d.status", job)
      fullStatusFileName <-
        file.path(jobsDirectoryRoot, statusFileName)
      fullStatusFileNames <-
        c(fullStatusFileNames, fullStatusFileName)

      scenarioNames <-
        c(scenarioNames, getScenarioName(control_lines[job]))
    }

    generateOverallSummary(
      outputFileNames = fullOutputFileNames,
      scenarioNames = scenarioNames,
      sorting = FALSE
    )

    generateStatusWindow(
      statusFiles = fullStatusFileNames,
      progressFiles = fullProgressFileNames,
      scenarioNames = scenarioNames
    )
  }
