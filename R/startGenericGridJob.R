startGenericGridJob <-
  function(jobType,
           allowIntermediateResults,
           progressStage = "") {
    # Figure out working env
    reportProgress <- get("reportProgress", envir = nlmeEnv)
    num_processes <- as.integer(get("num_processes", envir = nlmeEnv))
    MpiNumCores <- get("MpiNumCores", envir = nlmeEnv)
    parallelMethod <- get("parallelMethod", envir = nlmeEnv)
    SharedWorkingDir <- get("SharedWorkingDir", envir = nlmeEnv)
    num_samples <- as.integer(get("num_samples", envir = nlmeEnv))
    MpiNumCores <- get("MpiNumCores", envir = nlmeEnv)
    localWorkingDir <- get("localWorkingDir", envir = nlmeEnv)

    # create a registry
    gridRegistry <-
      prepare_ClusterRegistry(SharedWorkingDir, parallelMethod, localWorkingDir)

    assign("gridRegistry", gridRegistry, envir = nlmeEnv)

    # Job information
    assign("jobType", jobType, envir = nlmeEnv)
    jobOrder <- 1:num_samples
    status <- vector(mode = "character", num_samples)

    # Copy files to the remote directory
    copy_InputFiles(SharedWorkingDir)

    # Figure out initial estimates run
    if (jobType == "BOOTSTRAP") {
      if (Sys.getenv("NLME_SKIP_INITIAL_BOOTSTRAP_RUN") == "TRUE") {
        DoInitialNlmeRun <- FALSE
      } else {
        DoInitialNlmeRun <- TRUE
      }
      start_seed <- as.integer(get("start_seed", envir = nlmeEnv))
      seeds <- 1:num_samples
      for (i in 1:num_samples) {
        seeds[i] <- start_seed + (i - 1) * 100
      }
    } else {
      DoInitialNlmeRun <- FALSE
    }

    ResetMPIFlags(SharedWorkingDir, MpiNumCores)

    if (DoInitialNlmeRun) {
      MpiArgument <- get("MpiArgument", envir = nlmeEnv)
      stat <-
        runNLMEInitialRun(MpiNumCores,
                          MpiArgument,
                          parallelMethod,
                          localWorkingDir,
                          SharedWorkingDir)
      if (stat == FALSE) {
        return(list(stat = FALSE, done = c()))
      }
      writeOutGlobals(file.path(SharedWorkingDir, "myglobaldefs.r"))
    } else {
      ret <- compileAndLinkNLME()
      if (ret == FALSE) {
        return(list(stat = FALSE, done = c()))
      }
    }

    if (jobType == "BOOTSTRAP") {
      GlobalSummaryLine1 <- "Processing replicates"
      UpdateProgress(num_samples, 0, 0, 0)
      functionToRun <- "runNLMEBootstrapSample"
      ids <-
        batchtools::batchMap(
          reg = gridRegistry,
          fun = runNLMEBootstrapSample,
          indx = jobOrder,
          seed = seeds,
          SharedWorkingDir = SharedWorkingDir
        )
    } else {
      GlobalSummaryLine1 <- "Processing jobs"
      functionToRun <- "runNLMEGenericSample"
      ids <-
        batchtools::batchMap(
          reg = gridRegistry,
          fun = runNLMEGenericSample,
          jobIndx = jobOrder,
          SharedWorkingDir = SharedWorkingDir
        )
    }

    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir = nlmeEnv)
    UpdateProgressMessages()

    num_processes <- as.integer(num_processes / MpiNumCores)

    numberOfChunks <- num_processes
    if (num_samples <= num_processes) {
      done <- batchtools::submitJobs(reg = gridRegistry,
                                     resources = list(nodes = MpiNumCores,
                                                      walltime = 86400))
    } else {
      jobs <- batchtools::findJobs(reg = gridRegistry)
      chunked <-
        batchtools::chunk(jobs$job.id, n.chunks = numberOfChunks, shuffle = TRUE)
      jobs["chunk"] <- chunked
      done <- batchtools::submitJobs(reg = gridRegistry,
                                     jobs,
                                     resources = list(nodes = MpiNumCores,
                                                      walltime = 86400))
    }

    done <- batchtools::findDone(ids = NULL, gridRegistry)
    errors <- batchtools::findErrors(ids = NULL, gridRegistry)
    if (nrow(done) > 0) {
      done <- done$job.id
    } else {
      done <- c()
    }
    if (nrow(errors) > 0) {
      errors <- errors$job.id
    } else {
      errors <- c()
    }

    numSuccessful <- 0
    numFailed <- 0
    numErrors <- 0

    allJobsAreDone <- FALSE
    while (allJobsAreDone == FALSE) {
      jobInProgressPath <- ""
      if (IsJobCanceled()) {
        CancelProgress(num_samples, numSuccessful, numFailed)
        killAGridJob(gridRegistry)
        allJobsAreDone <- TRUE
        break
      }
      if (IsEarlyTerminationRequested()) {
        if (num_samples == 1) {
          # drop a file to force NLME to quit
          jobId <- 1
          requestStopEarly(sharedWorkingDir, jobId)
        } else {
          done <- grabDoneReplicates(status)
          break
        }
      }
      tryCatch({
        # baseJobsDirectory <- getBatchDirectoryLocation(SharedWorkingDir)
        running <-
          batchtools::findRunning(ids = NULL, reg = gridRegistry)
        if (nrow(running) > 0) {
          running <- running$job.id
        } else {
          running <- c()
        }
        errors <-
          batchtools::findErrors(ids = NULL, reg = gridRegistry)
        if (nrow(errors) > 0) {
          errors <- errors$job.id
        } else {
          errors <- c()
        }

        if (length(running) >= 1) {
          d <- running[1]
          baseIndx <- d %% 100
          jobInProgressPath <-
            sprintf("%s/registry/jobs/%02d/%d/",
                    SharedWorkingDir,
                    baseIndx,
                    d)
        }
      },
      error = function(ex) {
        jobInProgressPath <- ""
      })

      for (d in 1:length(status)) {
        baseIndx <- d %% 100
        if (nchar(status[d]) == 0) {
          statusFile <-
            sprintf(
              "%s/jobs/%02d/%d/.status",
              getBatchDirectoryLocation(SharedWorkingDir),
              baseIndx,
              d
            )
          if (file.exists(statusFile) &&
              file.info(statusFile)$size > 0) {
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

      numSuccessful <- sum(status == "SUCCESS")
      jobList <- which(status == "SUCCESS", arr.ind = TRUE)
      numFailed <- sum(status == "FAILED")
      numExpired <- sum(status == "EXPIRED")
      numErrors <- sum(status == "ERROR")

      # Check to see if we are to return interim results
      if (InterimResultsRequested()) {
        RemoveUserCommands()
        if (numSuccessful > 0 || jobType == "STEPWISE_SEARCH") {
          if (allowIntermediateResults == TRUE) {
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
          numExpired,
          "numErrors:",
          numErrors
        ),
        file = file.path(localWorkingDir, "status.log"),
        sep = "\n",
        append = FALSE
      )

      UpdateProgress(num_samples,
                     numSuccessful,
                     numFailed,
                     numExpired,
                     numErrors = numErrors)


      if (num_samples == (numSuccessful + numFailed + numExpired + numErrors)) {
        GlobalSummaryLine1 <- "Grid jobs completed. Summarizing results."
        assign("GlobalSummaryLine1", GlobalSummaryLine1, envir = nlmeEnv)
        UpdateProgressMessages(
          currentJobDirectory = jobInProgressPath,
          progressStage = progressStage,
          FinalCall = TRUE
        )
        allJobsAreDone <- TRUE
        if (reportProgress == TRUE) {
          reportCurrentStatus(num_samples, numSuccessful, numFailed)
        }
        break
      }

      UpdateProgressMessages(currentJobDirectory = "",
                             progressStage = progressStage)
      # TODO
      # Lets check  on the running time of the jobs still out there
      flag <- Sys.getenv("NLME_JOB_TIME_LIMIT")

      if (flag != "") {
        cat(
          paste("NLME_JOB_TIME_LIMIT:", flag),
          file = file.path(localWorkingDir, "status.log"),
          sep = "\n",
          append = TRUE
        )

        for (jobId in 1:length(status)) {
          s <- status[jobId]
          if (nchar(status[jobId]) == 0) {
            statusFile <- sprintf("%s/S_%03d.status", SharedWorkingDir, jobId)
            if (file.exists(statusFile)) {
              tryCatch({
                lines <- readLines(statusFile)
              })

              if (length(lines) == 2 && lines[1] == "RUNNING") {
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
                  file = file.path(localWorkingDir, "status.log"),
                  sep = "\n",
                  append = TRUE
                )
                if (lapse > (60 * as.numeric(flag))) {
                  cat(
                    paste("STOP job", jobId),
                    file = file.path(localWorkingDir, "status.log"),
                    sep = "\n",
                    append = TRUE
                  )

                  requestStopEarly(sharedWorkingDir, jobId)
                }
              }
            }
          }
        }
      }

      Sys.sleep(1)
      tryCatch({
        done <- batchtools::findDone(ids = NULL, reg = gridRegistry)
        if (nrow(done) > 0) {
          done <- done$job.id
        } else {
          done <- c()
        }
        errors <- batchtools::findErrors(ids = NULL, gridRegistry)
        if (nrow(errors) > 0) {
          errors <- errors$job.id
          for (e in errors) {
            status[e] <- "ERROR"
          }
        } else {
          errors <- c()
        }
      },
      error = function(ex) {
        Sys.sleep(1)
        done <- batchtools::findDone(ids = NULL, reg = gridRegistry)
        if (nrow(done) > 0) {
          done <- done$job.id
        } else {
          done <- c()
        }
        errors <- batchtools::findErrors(ids = NULL, gridRegistry)
        if (nrow(errors) > 0) {
          errors <- errors$job.id
          for (e in errors) {
            status[e] <- "ERROR"
          }
        } else {
          errors <- c()
        }
      })

      if (reportProgress == TRUE) {
        reportCurrentStatus(num_samples, numSuccessful, numFailed)
      }
    }

    jobs <- batchtools::getJobTable(reg = gridRegistry)
    assign("gridJobs", jobs$batch.id, envir = nlmeEnv)
    if (IsJobCanceled()) {
      return(list(stat = FALSE, done = c()))
    }
    if ((numErrors + numFailed) == num_samples) {
      return(list(stat = FALSE, done = jobList))
    } else {
      return(list(stat = TRUE, done = jobList))
    }
  }
