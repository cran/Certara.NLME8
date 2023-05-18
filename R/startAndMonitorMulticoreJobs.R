startAndMonitorMulticoreJobs <- function(numCoresToUse,
                                         num_samples,
                                         seeds,
                                         allowIntermediateResults,
                                         jobType,
                                         progressStage = "") {
  reportProgress <- get("reportProgress", envir = nlmeEnv)
  statusArray <- vector(mode = "character", num_samples)
  SharedWorkingDir <- get("SharedWorkingDir", envir = nlmeEnv)
  NextJobToRun <- 1
  NumCoresToUse <- min(c(numCoresToUse, num_samples))
  for (job in (1:NumCoresToUse)) {
    if (jobType == "BOOTSTRAP") {
      runNLMEBootstrapSample(job, seeds[job], SharedWorkingDir)
    } else {
      runNLMEGenericSample(job, SharedWorkingDir)
    }
  }
  NextJobToRun <- NextJobToRun + NumCoresToUse


  done <- 0
  numSuccessful <- 0
  numFailed <- 0
  while (done != 1) {
    if (IsJobCanceled()) {
      CancelProgress(num_samples, numSuccessful, numFailed)
      done <- 1
      break
    }

    if (IsEarlyTerminationRequested()) {
      done <- 1
      break
    }

    jobId <- 0
    files <- file.path(
      SharedWorkingDir,
      list.files(
        path = SharedWorkingDir,
        pattern = "^S.*\\.status$",
        all.files = TRUE
      )
    )
    currentRunningJobDir <- ""

    for (f in files) {
      jobId <-
        as.integer(tail(unlist(strsplit(
          sub(".status$", "", f), "_"
        )), 1))

      if (!is.na(jobId) && (jobId != 0)) {
        baseIndx <- jobId %% 100
        if (nchar(statusArray[jobId]) == 0) {
          for (t in 1:10)
          {
            withCallingHandlers(
              {
                stat <- ""
                try(
                  {
                    nchars <- file.info(f)$size - 1
                    if (!is.null(nchars) &&
                      !is.na(nchars) && nchars > 5) {
                      stat <- substr(readChar(f, nchars, useBytes = TRUE), 1, 6)
                    }
                  },
                  silent = TRUE
                )

                if (length(stat) == 0) {
                  stat <- ""
                }

                if (stat %in% c("SUCCES", "FAILED")) {
                  if (tolower(get("parallelMethod", envir = nlmeEnv)) %in% c("none", "local_mpi", "mpi") &&
                    NumCoresToUse == 1) {
                    numSuccessful <- sum(statusArray == "SUCCES")
                    numFailed <- sum(statusArray == "FAILED")

                    UpdateProgress(num_samples, numSuccessful, numFailed, 0)
                    UpdateProgressMessages(
                      currentJobDirectory = sprintf(
                        "%s/jobs/%02d/%d/",
                        SharedWorkingDir,
                        baseIndx,
                        jobId
                      ),
                      progressStage = progressStage,
                      FinalCall = TRUE
                    )
                  }

                  statusArray[jobId] <- stat
                  if (NextJobToRun <= num_samples) {
                    if (jobType == "BOOTSTRAP") {
                      runNLMEBootstrapSample(NextJobToRun, seeds[NextJobToRun], SharedWorkingDir)
                    } else {
                      runNLMEGenericSample(NextJobToRun, SharedWorkingDir)
                    }
                    NextJobToRun <- NextJobToRun + 1
                  }
                } else if (stat == "RUNNIN") {
                  currentRunningJobDir <-
                    sprintf(
                      "%s/jobs/%02d/%d/",
                      SharedWorkingDir,
                      baseIndx,
                      jobId
                    )
                }

                t <- 99
                break
              },
              warning = function(w) {
                message(
                  paste(
                    "Warning in startAndMonitorMulticoreJobs():",
                    conditionMessage(w)
                  )
                )
                invokeRestart("muffleWarning")
              },
              error = function(ex) {
                statusArray[jobId] <- ""
                Sys.sleep(1)
              }
            )
          }
        }
      }
    }

    numSuccessful <- 0
    numFailed <- 0
    jobList <- c()
    for (jobId in seq_along(statusArray)) {
      if (statusArray[jobId] == "SUCCES") {
        numSuccessful <- numSuccessful + 1
        jobList <- c(jobList, jobId)
      } else if (statusArray[jobId] == "FAILED") {
        numFailed <- numFailed + 1
      }
    }

    # Check to see if we are to return interim results
    if (InterimResultsRequested()) {
      RemoveUserCommands()
      if (numSuccessful > 0) {
        if (allowIntermediateResults == TRUE) {
          if (jobType == "BOOTSTRAP") {
            localDir <- get("localWorkingDir", envir = nlmeEnv)
            WriteResultsStatusFile("Generating Results...")
            generateBootstrapResults(localDir, jobList)
            WriteResultsStatusFile("Results Generated.")
          } else {
            localDir <- get("localWorkingDir", envir = nlmeEnv)
            control_lines <- get("control_lines", envir = nlmeEnv)
            WriteResultsStatusFile("Generating Results...")
            generateJobResults(localDir, jobType, jobList, control_lines, TRUE)
            WriteResultsStatusFile("Results Generated.")
          }
        } else {
          WriteResultsStatusFile("Results Generated.")
        }
      } else {
        WriteResultsStatusFile("Results Generated.")
      }
    }

    if (reportProgress == TRUE) {
      if (get("num_samples", envir = nlmeEnv) != 1 &&
        !tolower(get("parallelMethod", envir = nlmeEnv)) %in% c("none", "local_mpi", "mpi")) {
        # report current status for multicore
        reportCurrentStatus(num_samples, numSuccessful, numFailed)
      }
    }

    UpdateProgress(num_samples, numSuccessful, numFailed, 0)
    UpdateProgressMessages(
      currentJobDirectory = currentRunningJobDir,
      progressStage = progressStage
    )

    if (num_samples == (numSuccessful + numFailed)) {
      break
    }
    Sys.sleep(1)
  }

  if (get("num_samples", envir = nlmeEnv) != 1 &&
    tolower(get("parallelMethod", envir = nlmeEnv)) %in% c("none", "local_mpi", "mpi")) {
    # report final status for non-parallelized runs
    reportCurrentStatus(num_samples, numSuccessful, numFailed)
  }

  Sys.sleep(1)

  if (IsJobCanceled()) {
    CancelProgress(num_samples, numSuccessful, numFailed)
    retStatus <- FALSE
    done <- c()
  } else {
    retStatus <- TRUE
    done <- grabDoneReplicates(statusArray)
  }

  return(list(stat = retStatus, done = done))
}
