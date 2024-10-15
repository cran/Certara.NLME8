# This method copies all files from grid/remote directory and summarizes the
# generic run result
generateJobResults <-
  function(localDir,
           jobType,
           jobList,
           control_lines,
           stepwiseSummary = FALSE) {
    if (jobType == "GENERIC" || jobType == "ESTIMATION_RUN") {
      copyFilesFlag <- TRUE
    } else {
      copyFilesFlag <- FALSE
    }

    tryCatch({
      collectJobResultsGeneric(jobList, copyFilesFlag, control_lines)

      assign("jobsDirectoryRoot",
             get("SharedWorkingDir", envir = nlmeEnv),
             envir = nlmeEnv)
      if (copyFilesFlag) {
        copyResults(dirToCopyTo = localDir)
      }

      if (jobType == "GENERIC") {
        jobsList <- c(1)
        summarizeSimpleEstimation(localDir, jobList, control_lines, sorting = FALSE)
      } else if (jobType == "COVAR_SEARCH") {
        summarizeCovarSearch(localDir, jobList, control_lines)
      } else if (jobType == "ESTIMATION_RUN") {
        # grab out.txt and nlme7engine.log file for last job
        # job <- jobList[length(jobList)]
        # collectJobStatusAndCoreFiles(job, c("nlme7engine.log", getRunSuccessFilename(control_lines[job])), localDir)
        summarizeSimpleEstimation(localDir, jobList, control_lines, sorting = TRUE)
      } else if (jobType == "PROFILE_RUN") {
        # grab out.txt and nlme7engine.log file for last job
        job <- jobList[length(jobList)]
        collectJobStatusAndCoreFiles(job,
                                     c(
                                       "nlme7engine.log",
                                       getRunSuccessFilename(control_lines[job])
                                     ),
                                     localDir)
        summarizeProfileEstimation(localDir, jobList, control_lines)
      } else if (jobType == "STEPWISE_SEARCH") {
        if (stepwiseSummary == TRUE) {
          scenarios <- get("scenarios", envir = nlmeEnv)
          summarizeStepwiseCovarSearch(localWorkingDir, scenarios)
        }
        # since summarizeStepwiseCovarSearch is called elsewhere, we don't need to print the message below
        return(stepwiseSummary)
      }
      GlobalSummaryLine1 <-
        "\nFinished summarizing results. Transferring data and loading the results...\n"
      assign("GlobalSummaryLine1", GlobalSummaryLine1, envir = nlmeEnv)
      UpdateProgressMessages()
    },
    error = function(ex) {
      warning("\nFailed to generate ",
              jobType,
              " Results. Error is:\n",
              ex,
              "\n")
    })
  }
