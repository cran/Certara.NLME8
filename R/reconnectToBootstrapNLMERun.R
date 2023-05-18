#' Use to reconnect to a grid job
#'
#' Use to reconnect to a grid job
#' @param args Arguments for reconnecting to bootstrap grid run
#' @return Directory path where NLME job was executed
#' @keywords NLME Bootstrap
#' @export
reconnectToBootstrapNLMERun <- function(args) {
  if (length(args) != 17) {
    stop("arguments expected parallelMethod install_dir shared_directory localWorkingDir engine num_iterations num_samples max_tries model_file column_def_file data_file start_seed extra_args_file files_to_copy NumProc ConfidenceLevel tempdir")
  }

  jobType <- "BOOTSTRAP"

  if (.Platform$OS.type == "windows") {
    exeFileExtension <- ".ps1"
  } else {
    exeFileExtension <- ".sh"
  }
  assign("exeFileExtension", exeFileExtension, envir = nlmeEnv)
  parallelMethod <- args[1]
  assign("parallelMethod", parallelMethod, envir = nlmeEnv)

  gridRegistryDir <- args[17]
  jobHome <- dirname(gridRegistryDir)
  jobHome <- gsub("\\", "/", jobHome, fixed = TRUE)
  assign("jobHome", jobHome, envir = nlmeEnv)
  SharedWorkingDir <- gridRegistryDir

  assign("SharedWorkingDir", SharedWorkingDir, envir = nlmeEnv)

  localWorkingDir <- gsub("\\", "/", args[4], fixed = TRUE)
  assign("localWorkingDir", localWorkingDir, envir = nlmeEnv)
  assign("engine", as.integer(args[5]), envir = nlmeEnv)
  assign("num_iterations", as.integer(args[6]), envir = nlmeEnv)
  assign("num_samples", as.integer(args[7]), envir = nlmeEnv)
  assign("max_tries", as.integer(args[8]), envir = nlmeEnv)
  assign("model_file", args[9], envir = nlmeEnv)
  assign("column_def_file", args[10], envir = nlmeEnv)
  assign("data_file", args[11], envir = nlmeEnv)
  assign("start_seed", as.integer(args[12]), envir = nlmeEnv)
  assign("extra_args_file", gsub("\\", "/", args[13], fixed = TRUE), envir = nlmeEnv)
  assign("files_to_copy", gsub("\\", "/", args[14], fixed = TRUE), envir = nlmeEnv)
  num_processes <- as.integer(args[15])
  assign("num_processes", num_processes, envir = nlmeEnv)
  assign("confidence_level", as.double(args[16]), envir = nlmeEnv)
  progress <- list(MachineName = "LocalHost", ParallelProtocol = "None", ModelName = "", StartTime = "", EndTime = "", Status = "InProgress", NumOfSamples = 0, NumOfSamplesCompleted = 0, NumOfSamplesFailed = 0, NumOfSamplesExpired = 0, NumOfSamplesErrored = 0)
  progress$NumOfSamples <- as.integer(args[7])
  progress$Status <- "InProgress"
  progress$ParallelProtocol <- parallelMethod
  progress$StartTime <- getLocalTimeUTC()

  assign("ProgressStatus", progress, envir = nlmeEnv)

  ReportProgress(progress)

  RunningOnGrid <- TRUE
  gridRegistry <- loadRegistry(gridRegistryDir)

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
  if (!IsJobCanceled()) {
    CompleteProgress()
  }

  if (IsJobCanceled()) {
    batchtools::killJobs(gridRegistry, findNotDone(gridRegistry))
  }
  progress <- get("ProgressStatus", envir = nlmeEnv)

  if (RunningOnGrid) {
    if (Sys.getenv("NLME_KEEP_GRID_RESULTS") != "TRUE") {
      if (progress$NumOfSamplesFailed == 0 && progress$NumOfSamplesExpired == 0) {
        tryCatch(
          {
            message("removeRegistry()")
            batchtools::removeRegistry(reg = gridRegistry)
          },
          error = function(ex) {
            Sys.sleep(1)
          }
        )
      }
    }
  }

  if (progress$NumOfSamplesFailed == 0 &&
    progress$NumOfSamplesExpired == 0 &&
    flag != "TRUE") {
    jobHome <- get("jobHome", envir = nlmeEnv)
    removeTempWorkingDirectory(jobHome)
  }
}
