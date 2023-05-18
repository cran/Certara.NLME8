reconnectToGenericNLMERun <- function(args) {
  requiredargs <-
    c(
      "jobType",
      "parallelMethod",
      "install_dir",
      "shared_directory",
      "localWorkingDir",
      "controlFile",
      "NumProc",
      "workflow_name",
      "gridRegistryDir"
    )

  if (length(args) != 9) {
    check_Arguments(requiredargs, args, "performParallelNLMERun")
  }

  reg.finalizer(nlmeEnv, nlmeEnvIsDone, onexit = TRUE)

  if (.Platform$OS.type == "windows") {
    exeFileExtension <- ".ps1"
  } else {
    exeFileExtension <- ".sh"
  }

  assign("exeFileExtension", exeFileExtension, envir = nlmeEnv)
  jobType <- args[1]
  parallelMethod <- args[2]
  assign("parallelMethod", parallelMethod, envir = nlmeEnv)
  gridRegistryDir <- args[9]
  jobHome <- dirname(gridRegistryDir)
  jobHome <- gsub("\\", "/", jobHome, fixed = TRUE)

  assign("jobHome", jobHome, envir = nlmeEnv)

  SharedWorkingDir <- gridRegistryDir

  assign("SharedWorkingDir", SharedWorkingDir, envir = nlmeEnv)

  localWorkingDir <- gsub("\\", "/", args[5], fixed = TRUE)
  assign("localWorkingDir", localWorkingDir, envir = nlmeEnv)

  control_file <- args[6]
  assign("control_file", control_file, envir = nlmeEnv)
  lines <- readLines(control_file)
  control_lines <- lines[5:length(lines)]
  assign("control_lines", control_lines, envir = nlmeEnv)
  model_file <- lines[1]
  assign("model_file", model_file, envir = nlmeEnv)
  files_to_copy <- lines[2]
  assign("files_to_copy", files_to_copy, envir = nlmeEnv)
  files_to_return <- lines[3]
  assign("files_to_return", files_to_return, envir = nlmeEnv)
  num_samples <- as.integer(lines[4])
  assign("num_samples", num_samples, envir = nlmeEnv)

  extra_args_file <- getExtraArgumentFilename(control_lines[1])
  assign("extra_args_file",
         gsub("\\", "/", extra_args_file, fixed = TRUE),
         envir = nlmeEnv)

  num_processes <- as.integer(args[7])
  assign("num_processes", num_processes, envir = nlmeEnv)
  assign("workflow_name", args[8], envir = nlmeEnv)
  progress <-
    list(
      MachineName = "LocalHost",
      ParallelProtocol = "None",
      ModelName = "",
      StartTime = "",
      EndTime = "",
      Status = "InProgress",
      NumOfSamples = 0,
      NumOfSamplesCompleted = 0,
      NumOfSamplesFailed = 0,
      NumOfSamplesExpired = 0,
      NumOfSamplesErrored = 0
    )
  progress$NumOfSamples <- num_samples
  progress$Status <- "InProgress"
  progress$ParallelProtocol <- "GRID"
  progress$ParallelProtocol <- parallelMethod
  progress$StartTime <- getLocalTimeUTC()

  assign("ProgressStatus", progress, envir = nlmeEnv)
  ReportProgress(progress)

  RunningOnGrid <- TRUE
  gridRegistry <- batchtools::loadRegistry(gridRegistryDir)
  assign("gridRegistry", gridRegistry, envir = nlmeEnv)
  message("-------------------------reconnectGenericGridJob()--------")
  stat <- reconnectGenericGridJob(jobType)
  message("----------------- After reconnectGenericGridJob()--------")
  done <- batchtools::findDone(gridRegistry)
  expired <- batchtools::findExpired(gridRegistry)
  done <- c(done, expired)

  if (stat == TRUE && !IsJobCanceled()) {
    if (length(done) == 0) {
      done <- 1:num_samples
    }
    generateJobResults(localWorkingDir, jobType, done, control_lines)
    listJobErrors(localWorkingDir, num_samples)
  }

  if (partialJob == FALSE && !IsJobCanceled()) {
    CompleteProgress()
  }

  if (IsJobCanceled() || IsEarlyTerminationRequested()) {
    RemoveUserCommands()
    if (RunningOnGrid == TRUE) {
      killAGridJob(gridRegistry)
    }
  }
  progress <- get("ProgressStatus", envir = nlmeEnv)
  flag <- Sys.getenv("NLME_KEEP_GRID_RESULTS")

  if (RunningOnGrid) {
    if (progress$NumOfSamplesFailed == 0 &&
        progress$NumOfSamplesExpired == 0 && (flag != "TRUE")) {
      tryCatch({
        if (flag != TRUE) {
          message("removeRegistry()")
          removeRegistry(reg = gridRegistry)
        }
      },
      error = function(ex) {
        Sys.sleep(1)
      })
    }
  }

  if (progress$NumOfSamplesFailed == 0 &&
      progress$NumOfSamplesExpired == 0 &&
      flag != "TRUE") {
    jobHome <- get("jobHome", envir = nlmeEnv)
    removeTempWorkingDirectory(jobHome)
  }

}
