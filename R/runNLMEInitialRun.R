# Run an NLME job to get initial estimates used in bootstrap
runNLMEInitialRun <- function(MpiNumCores, MpiArgument, parallelMethod, localWorkingDir, SharedWorkingDir) {
  OldMpiNumCores <- MpiNumCores
  if (MpiArgument == "MPINO") {
    NewMpiNumCores <- 1
  } else {
    NewMpiNumCores <- figureOutMpiNumCores(1)
  }

  # We need to account for the case where MPI is being used and run's
  # number of cores is one.  We do not want to use MPI for this case
  # if we used MPI for initial estimates run in the above scenario, we need
  # to regenerate the NLME7.exe and turn off MPI
  if (OldMpiNumCores == 1 && MpiArgument != "MPINO" && NewMpiNumCores > 1) {
    OldMpiExecutable <- MpiExecutable
    OldMpiArgument <- MpiArgument
    MpiExecutable <- "NLME7.exe"
    MpiArgument <- "MPINO"
    assign("MpiExecutable", MpiExecutable, envir = nlmeEnv)
    assign("MpiArgument", MpiArgument, envir = nlmeEnv)
  }

  assign("MpiNumCores", NewMpiNumCores, envir = nlmeEnv)

  # Switch and create a new registry
  OldSharedWorkingDir <- SharedWorkingDir

  jobHome <- get("jobHome", envir = nlmeEnv)
  SharedWorkingDir <- gsub("\\", "/", tempfile(pattern = "NLME", tmpdir = jobHome), fixed = TRUE)
  dir.create(SharedWorkingDir, recursive = TRUE, showWarnings = FALSE)
  assign("SharedWorkingDir", SharedWorkingDir, envir = nlmeEnv)
  # setwd(SharedWorkingDir)

  ResetMPIFlags(SharedWorkingDir, NewMpiNumCores)

  gridRegistry2 <- prepare_ClusterRegistry(SharedWorkingDir, parallelMethod, localWorkingDir)


  progress <- get("ProgressStatus", envir = nlmeEnv)
  progress$Status <- "Running"
  GlobalSummaryLine1 <- "Running bootstrap initial estimates"
  assign("GlobalSummaryLine1", GlobalSummaryLine1, envir = nlmeEnv)

  assign("ProgressStatus", progress, envir = nlmeEnv)

  UpdateProgress(1, 0, 0, 0)
  UpdateProgressMessages(progressStage = "Initial Estimates ")

  # Copy files and generate executable
  copy_InputFiles(SharedWorkingDir)
  ret <- generateNLMEScriptAndRun("COMPILE")
  if (ret == FALSE) {
    return(FALSE)
  }

  jobIds <- rep(1, 1)
  bids <- batchtools::batchMap(reg = gridRegistry2, fun = runNLMEInitialSample, indx = jobIds, SharedWorkingDir = SharedWorkingDir)
  done <- batchtools::submitJobs(reg = gridRegistry2,
                                 resources = list(nodes = NewMpiNumCores,
                                                  walltime = 86400,
                                                  ncpus = NewMpiNumCores),
                                 ids = bids)

  jobs <- batchtools::getJobTable(reg = gridRegistry2)
  assign("gridJobs2", jobs$batch.id, envir = nlmeEnv)

  numSuccessful <- 0
  numFailed <- 0
  done <- FALSE
  num_samples <- 1
  numSuccessuful <- 0
  numFailed <- 0
  numExpired <- 0
  wd <- file.path(getBatchDirectoryLocation(SharedWorkingDir), "jobs", "01", "1")
  while (!done) {
    if (IsJobCanceled()) {
      killAGridJob(gridRegistry2)
      CancelProgress(num_samples, numSuccessful, numFailed)
      done <- TRUE
      break
    }
    if (IsEarlyTerminationRequested()) {
      requestStopEarly(sharedWorkingDir, 1)
    }
    tryCatch(
      {
        running <- batchtools::findRunning(ids = NULL, reg = gridRegistry2)
        if (nrow(running) > 0) {
          running <- running$job.id
        } else {
          running <- c()
        }

        doneJobs <- batchtools::findDone(ids = NULL, reg = gridRegistry2)
        if (nrow(doneJobs) > 0) {
          doneJobs <- doneJobs$job.id
        } else {
          doneJobs <- c()
        }

        if (length(doneJobs) > 0) {
          statusFile <- file.path(wd, ".status")
          if (file.exists(statusFile)) {
            done <- TRUE
            break
          }
        }
      },
      error = function(ex) {
        wd <- ""
        warning(ex)
      }
    )
    UpdateProgress(num_samples, numSuccessful, numFailed, numExpired)
    UpdateProgressMessages(
      currentJobDirectory = wd,
      progressStage = "Initial Estimates "
    )
  }

  # Copy results back to original registry
  MpiExecutable <- get("MpiExecutable", envir = nlmeEnv)
  model_file <- get("model_file", envir = nlmeEnv)
  if (file.exists(file.path(wd, "out.txt"))) {
    files <- c(
      "VarCoVar.csv",
      "doses.csv",
      "err2.txt",
      "err1.txt",
      "IniCovr.txt",
      "MultCovr.txt",
      "progress.txt",
      "IdEta.txt",
      "dmp.txt",
      "EtaEta.txt",
      "EtaCov.txt",
      "StrCov.txt",
      "EtaShrinkageBySubject.txt",
      "bluptable.dat",
      "etameansnp.asc",
      "nparsupport.asc"
    )

    files <- list.files(wd, pattern = paste0("(", files, ")", collapse = "|"), all.files = TRUE)

    copy_filesWarnLong(file.path(wd, files), dirname(SharedWorkingDir), overwrite = TRUE)

    # copy only dmp.txt to create new mdl
    copy_filesWarnLong(file.path(wd, "dmp.txt"), SharedWorkingDir, overwrite = TRUE)

    UpdateMDLfrom_dmptxt(dmpfile = "dmp.txt", SharedWorkingDir, model_file = model_file)

    # copy all files in shared dir
    copy_filesWarnLong(file.path(wd, "out.txt"), file.path(dirname(SharedWorkingDir), "out_initialEstimates.txt"))
    copy_filesWarnLong(file.path(wd, "nlme7engine.log"), dirname(SharedWorkingDir))

    FilesToOldSharedWorkingDir <- c("dmp.txt", model_file, MpiExecutable)
    copy_filesWarnLong(file.path(wd, FilesToOldSharedWorkingDir), OldSharedWorkingDir)

    UpdateProgress(1, 1, 0, 0)
    UpdateProgressMessages()
    status <- TRUE
  } else {
    if (file.exists(file.path(wd, "err2.txt"))) {
      copy_filesWarnLong(file.path(wd, "err2.txt"), file.path(dirname(SharedWorkingDir), "err2.txt"), overwrite = TRUE)
      lines <- readLines(file.path(wd, "err2.txt"))
      warning(lines)
    }
    UpdateProgress(1, 0, 1, 0)
    UpdateProgressMessages()
    FailProgress()
    status <- FALSE
  }

  assign("gridRegistry2", gridRegistry2, envir = nlmeEnv)
  assign("SharedWorkingDir", OldSharedWorkingDir, envir = nlmeEnv)

  ResetMPIFlags(OldSharedWorkingDir, OldMpiNumCores)

  assign("MpiNumCores", OldMpiNumCores, envir = nlmeEnv)
  return(status)
}
