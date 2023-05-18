

# This method copies all error files from grid/remote directory
collectJobErrors <-
  function(localDir,
           jobType,
           jobList,
           control_lines,
           stepwiseSummary = FALSE) {
    copyFilesFlag <- TRUE
    tryCatch({
      collectJobErrorLogs(localDir, jobList, copyFilesFlag)
    },
    error = function(ex) {
      warning(
        "Failed to collectJobErrors(). Error is: ",
        ex,
        call. = FALSE,
        immediate. = TRUE
      )
    })
  }

# Look for err2.txt in all jobs directories and list the errors
# so they are captured in the log file.
collectJobErrorLogs <-
  function(localDir, done, copyFilesFlag = FALSE) {
    SharedWorkingDir <- get("SharedWorkingDir", envir = nlmeEnv)
    baseDirectory <- getBatchDirectoryLocation(SharedWorkingDir)

    # Return files from individual runs.
    # If Failed with no jobs, this is the initial estimates run
    if (is.null(done)) {
      f <- "err2.txt"
      if (copyFilesFlag &&
          file.exists(file.path(SharedWorkingDir, f))) {
        copy_filesWarnLong(file.path(SharedWorkingDir, f),
                           file.path(localDir, f),
                           overwrite = TRUE)
        lines <- readLines(file.path(localDir, f))
        message("\nerr2.txt output from ",
                file.path(SharedWorkingDir, f),
                lines,
                sep = "\n")
      }
    } else {
      hangAroundABitForTheStatusFile(SharedWorkingDir, done)
      listJobErrors(localDir, num_samples = NULL, done = done)
    }
  }

#
# Looks for the presence of err2.txt and print it if it exist
listJobErrors <- function(localDir, num_samples, done = NULL) {
  if (is.null(done)) {
    jobList <- 1:num_samples
  } else {
    jobList <- done
  }

  SharedWorkingDir <- get("SharedWorkingDir", envir = nlmeEnv)
  baseDirectory <- getBatchDirectoryLocation(SharedWorkingDir)
  integration_errorsFilePath <-
    file.path(SharedWorkingDir, "integration_errors.txt")
  err2FilePath <- file.path(SharedWorkingDir, "err2.txt")
  FilesToTransfer <- c(integration_errorsFilePath, err2FilePath)
  FilesToDelete <- file.exists(FilesToTransfer)
  if (any(FilesToDelete)) {
    file.remove(FilesToDelete)
  }

  err2linesAll <- c()

  for (job in jobList) {
    baseIndx <- job %% 100
    wd <- sprintf("%s/jobs/%02d/%d/", baseDirectory, baseIndx, job)
    err2ToCheck <- file.path(wd, "err2.txt")
    if (file.exists(err2ToCheck)) {
      err2lines <- readLines(err2ToCheck)
      if (length(err2lines) != 0) {
        err2lines <- paste0(unique(err2lines),
                            sep = "\n",
                            collapse = "\n")
        names(err2lines) <- job
        err2linesAll <- c(err2linesAll, err2lines)
        cat(paste0("Errors in job ", job),
            file = err2FilePath,
            sep = "\n")
        file.append(err2FilePath, err2ToCheck)
      }
    }

    interrToCheck <- file.path(wd, "integration_errors.txt")
    if (file.exists(interrToCheck)) {
      cat(paste0("Integration errors in job ", job),
          file = integration_errorsFilePath,
          sep = "\n")
      file.append(integration_errorsFilePath, interrToCheck)
    }
  }

  err2linesAll <- na.omit(err2linesAll)
  for (errMsg in unique(err2linesAll)) {
    jobsWithErrs <- names(err2linesAll[err2linesAll == errMsg])
    warning(
      "\nFor the job(s) ",
      paste(jobsWithErrs, sep = ", ", collapse = ", "),
      "\nthe following error message reported:\n",
      errMsg,
      call. = FALSE, immediate. = TRUE
    )
  }

  if (file.exists(err2FilePath)) {
    copy_filesWarnLong(err2FilePath, localDir)
    warning(
      "\nNLME errors reported are appended in\n",
      file.path(localDir, "err2.txt"),
      call. = FALSE, immediate. = TRUE
    )
  }

  if (file.exists(integration_errorsFilePath)) {
    copy_filesWarnLong(integration_errorsFilePath, localDir)
    warning(
      "\nIntegration errors reported for the final solutions are appended in\n",
      file.path(localDir, "integration_errors.txt"),
      call. = FALSE, immediate. = TRUE
    )
  }
}
