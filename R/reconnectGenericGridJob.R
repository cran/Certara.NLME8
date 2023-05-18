# This method reconnects to a job that is already been submitted to grid
reconnectGenericGridJob <- function(jobType) {
  num_processes <- as.integer(get("num_processes", envir = nlmeEnv))
  SharedWorkingDir <- get("SharedWorkingDir", envir = nlmeEnv)
  num_samples <- as.integer(get("num_samples", envir = nlmeEnv))
  gridRegistry <- get("gridRegistry", envir = nlmeEnv)
  assign("jobType", jobType, envir = nlmeEnv)

  jobOrder <- 1:num_samples
  status <- vector(mode = "character", num_samples)

  numberOfChunks <- num_processes

  done <- batchtools::findDone(gridRegistry)

  numSuccessful <- 0
  numFailed <- 0
  while (TRUE) {
    if (IsJobCanceled()) {
      CancelProgress(num_samples, numSuccessful, numFailed)
      break
    }

    tryCatch({
      expired <- batchtools::findExpired(gridRegistry)
    },
    error = function(ex) {
      expired <- c()
    })

    for (e in expired) {
      status[e] <- "EXPIRED"
    }

    for (d in 1:length(status)) {
      baseIndx <- d %% 100
      if (nchar(status[d]) == 0) {
        statusFile <-
          sprintf("%s/jobs/%02d/%d/.status",
                  SharedWorkingDir,
                  baseIndx,
                  d)

        if (file.exists(statusFile)) {
          try({
            stat <-
              readChar(statusFile,
                       file.info(statusFile)$size - 1,
                       useBytes = TRUE)
            stat <- gsub("\r", "", stat, fixed = TRUE)
            if (substr(stat, 1, 7) != "RUNNING") {
              status[d] <- stat
            }
          },
          silent = TRUE)
        }
      }
    }
    numSuccessful <- 0
    numFailed <- 0
    numExpired <- 0
    jobList <- c()

    for (jobId in 1:length(status)) {
      s <- status[jobId]
      if (s == "SUCCESS") {
        numSuccessful <- numSuccessful + 1
        jobList <- c(jobList, jobId)
      }

      if (s == "FAILED") {
        numFailed <- numFailed + 1
      }
      if (s == "EXPIRED") {
        numExpired <- numExpired + 1
      }
    }

    # Check to see if we are to return interim results
    #
    if (InterimResultsRequested()) {
      RemoveUserCommands()
      if (numSuccessful > 0) {
        if (jobType == "BOOTSTRAP") {
          localDir <- get("localWorkingDir", envir = nlmeEnv)
          WriteResultsStatusFile("Generating Bootstrap Results...")
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
    }

    cat(
      paste(
        "num_samples",
        num_samples,
        "numSuccessful:",
        numSuccessful,
        "numFailed:",
        numFailed,
        "numExpired:",
        numExpired
      ),
      file = "status.log",
      sep = "\n",
      append = FALSE
    )
    UpdateProgress(num_samples, numSuccessful, numFailed, numExpired)
    UpdateProgressMessages()
    if (num_samples == (numSuccessful + numFailed + numExpired))
      break

    # TODO
    # Lets check  on the running time of the jobs still out there
    flag <- Sys.getenv("NLME_JOB_TIME_LIMIT")

    if (flag != "") {
      cat(
        paste("NLME_JOB_TIME_LIMIT:", flag),
        file = "status.log",
        sep = "\n",
        append = TRUE
      )

      for (jobId in 1:length(status)) {
        s <- status[jobId]
        if (nchar(status[jobId]) == 0) {
          statusFile <- sprintf("%s/S_%03d.status", SharedWorkingDir, jobId)
          if (file.exists(statusFile)) {
            try(lines <- readLines(statusFile),
                silent = TRUE)

            if (length(lines) == 2) {
              if (lines[1] == "RUNNING") {
                startTime <- as.numeric(as.POSIXlt(lines[2]))
                now <- as.numeric(Sys.time())
                lapse <- now - startTime
                cat(
                  paste(
                    "startTime",
                    startTime,
                    "now",
                    now,
                    "lapse",
                    lapse
                  ),
                  file = "status.log",
                  sep = "\n",
                  append = TRUE
                )

                if (lapse > (60 * as.numeric(flag))) {
                  baseIndx <- jobId %% 100
                  stopFile <-
                    sprintf("%s/jobs/%02d/%d/stop.txt",
                            SharedWorkingDir,
                            baseIndx,
                            jobId)

                }
              }
            }
          }
        }
      }
    }

    Sys.sleep(1)
    tryCatch({
      done <- batchtools::findDone(gridRegistry)
    },
    error = function(ex) {
      Sys.sleep(1)
      done <- batchtools::findDone(gridRegistry)
    })
    if (IsJobCanceled()) {
      break
    }
  }
  return(TRUE)
}
