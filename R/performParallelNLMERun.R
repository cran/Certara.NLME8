#' Runs a set of NLME jobs in parallel
#'
#' Runs a set of NLME jobs in parallel
#'
#' @param args a vector of arguments provided as the following:
#' c(jobType, parallelMethod, install_dir, shared_directory,
#' localWorkingDir, controlFile, NumProc, workflow_name, fixefUnits)
#'
#' @param partialJob is \code{TRUE} if it is not required to stop the job
#' as for covariate stepwise search
#' @param allowIntermediateResults is \code{TRUE} if intermediate results
#' are possible like for sorting
#' @param progressStage stage of analysis to be reported
#' @param func function to be executed after NLME job
#' @param func_arg arguments to be provided to the function by name provided above
#' @param reportProgress whether it is required to report the progress
#' (for local jobs usually)
#' @return Directory path where NLME job was executed
#' @export
performParallelNLMERun <- function(args,
                                   partialJob = FALSE,
                                   allowIntermediateResults = TRUE,
                                   progressStage = "",
                                   func = "",
                                   func_arg = NULL,
                                   reportProgress = FALSE) {
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
      "fixefUnits"
    )

  if (length(args) < 8) {
    check_Arguments(requiredargs, args, "performParallelNLMERun")
  }

  jobType <- args[1]
  if (jobType == "GENERIC") {
    rm(list = ls(envir = nlmeEnv), envir = nlmeEnv)
  }

  assign("reportProgress", reportProgress, envir = nlmeEnv)

  parallelMethod <- args[2]
  Sys.setenv(INSTALLDIR = args[3])
  shared_directory <- args[4]
  localWorkingDir <- gsub("\\", "/", args[5], fixed = TRUE)
  control_file <- gsub("\\", "/", args[6], fixed = TRUE)
  arglist <- unlist(strsplit(args[7], split = ","))
  workflow_name <- args[8]
  if (length(args) > 8) {
    assign("fixefUnits", eval(parse(text = args[9])), envir = nlmeEnv)
  }

  generateProfileModels(jobType)
  reg.finalizer(nlmeEnv, nlmeEnvIsDone, onexit = TRUE)

  if (.Platform$OS.type == "windows") {
    exeFileExtension <- ".ps1"
  } else {
    exeFileExtension <- ".sh"
  }

  assign("exeFileExtension", exeFileExtension, envir = nlmeEnv)
  assign("parallelMethod", parallelMethod, envir = nlmeEnv)
  assign("localWorkingDir", localWorkingDir, envir = nlmeEnv)
  assign("workflow_name", workflow_name, envir = nlmeEnv)

  if (!create_jobdirs(shared_directory = shared_directory, jobtype = jobType)) {
    SharedWorkingDir <- get("SharedWorkingDir", envir = nlmeEnv)
    errMsg <- "Cannot create directories for execution."
    if (.Platform$OS.type == "windows" &&
        nchar(SharedWorkingDir) > 254) {
      errMsg <-
        paste(
          errMsg,
          "\nPossible reason: Current directory path is too long:\n",
          SharedWorkingDir,
          "\nConsider to shrink the path for execution."
        )
    }

    stop(errMsg)
  }

  jobHome <- get("jobHome", envir = nlmeEnv)

  assign("control_file", control_file, envir = nlmeEnv)
  if (dirname(control_file) == ".") {
    lines <- readLines(file.path(localWorkingDir, control_file))
  } else {
    lines <- readLines(control_file)
  }
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


  num_processes <- as.integer(arglist[1])
  assign("num_processes", num_processes, envir = nlmeEnv)
  if (length(arglist) == 2) {
    num_cores <- as.integer(arglist[2])
  } else {
    if (tolower(parallelMethod) %in% c("local_mpi", "mpi")) {
      num_cores <- num_processes
    } else if (grepl("(torque_mpi)|(sge_mpi)|(lsf_mpi)|(slurm_mpi)",
                     parallelMethod,
                     ignore.case = TRUE)) {
      num_cores <-
        figureOutMpiNumCoresForPop(num_samples, control_file, localWorkingDir)
    } else {
      num_cores <- 1
    }
  }

  progress <- list(
    MachineName = "LocalHost",
    ParallelProtocol = parallelMethod,
    ModelName = workflow_name,
    StartTime = getLocalTimeUTC(),
    EndTime = "",
    Status = "InProgress",
    NumOfSamples = num_samples,
    NumOfSamplesCompleted = 0,
    NumOfSamplesFailed = 0,
    NumOfSamplesExpired = 0,
    NumOfSamplesErrored = 0
  )

  assign("ProgressStatus", progress, envir = nlmeEnv)
  ReportProgress(progress)

  if (jobType == "STEPWISE_SEARCH") {
    # need for reporting
    assign("scenarioIndexes", func_arg, envir = nlmeEnv)
  }

  RunningOnGrid <- FALSE
  stat <- FALSE
  MpiExecutable <- "NLME7.exe"
  MpiArgument <- "MPINO"
  MpiNumCores <- 1
  MpiLocal <- "NO"
  assign("MpiExecutable", MpiExecutable, envir = nlmeEnv)
  assign("MpiArgument", MpiArgument, envir = nlmeEnv)
  assign("MpiLocal", MpiLocal, envir = nlmeEnv)
  assign("MpiNumCores", MpiNumCores, envir = nlmeEnv)

  if (tolower(parallelMethod) == "none") {
    done <- 1:num_samples
    ret <- multiCoreGeneric(
      parallelMethod,
      jobType,
      num_processes,
      allowIntermediateResults,
      progressStage = progressStage
    )
    stat <- ret$stat
    done <- ret$done
  } else if (tolower(parallelMethod) == "multicore") {
    ret <- multiCoreGeneric(
      parallelMethod,
      jobType,
      num_processes,
      allowIntermediateResults,
      progressStage = progressStage
    )
    done <- 1:num_samples
    stat <- ret$stat
    done <- ret$done
  } else if (tolower(parallelMethod) %in% c("local_mpi", "mpi")) {
    MpiExecutable <- "mpiNLME7.exe"
    MpiArgument <- "MPIYES"
    MpiLocal <- "YES"
    MpiNumCores <- num_cores
    assign("MpiExecutable", MpiExecutable, envir = nlmeEnv)
    assign("MpiArgument", MpiArgument, envir = nlmeEnv)
    assign("MpiLocal", MpiLocal, envir = nlmeEnv)
    assign("MpiNumCores", MpiNumCores, envir = nlmeEnv)
    ret <- multiCoreGeneric(
      parallelMethod,
      jobType,
      num_processes,
      allowIntermediateResults,
      progressStage = progressStage
    )
    done <- 1:num_samples
    stat <- ret$stat
    done <- ret$done
  } else {
    if (grepl("(torque_mpi)|(sge_mpi)|(lsf_mpi)|(slurm_mpi)",
              parallelMethod,
              ignore.case = TRUE)) {
      MpiExecutable <- "mpiNLME7.exe"
      MpiArgument <- "MPIYES"
      MpiLocal <- "NO"
      MpiNumCores <- num_cores
    } else {
      MpiExecutable <- "NLME7.exe"
      MpiArgument <- "MPINO"
      MpiNumCores <- 1
      MpiLocal <- "NO"
    }

    assign("MpiExecutable", MpiExecutable, envir = nlmeEnv)
    assign("MpiArgument", MpiArgument, envir = nlmeEnv)
    assign("MpiLocal", MpiLocal, envir = nlmeEnv)
    assign("MpiNumCores", MpiNumCores, envir = nlmeEnv)
    RunningOnGrid <- TRUE
    ret <- startGenericGridJob(
      jobType,
      allowIntermediateResults,
      progressStage = progressStage
    )
    stat <- ret$stat
    done <- ret$done
    waitTillAllJobsAreFinished()
  }

  message("\nTrying to generate job results...")
  if (stat == TRUE && !IsJobCanceled()) {
    if (length(done) == 0) {
      done <- 1:num_samples
    }

    generateJobResults(localWorkingDir, jobType, done, control_lines)
    listJobErrors(localWorkingDir, num_samples)
  } else if (stat == FALSE) {
    collectJobErrors(localWorkingDir, jobType, done, control_lines)
  }

  message("Done generating job results.")

  if (partialJob == FALSE && !IsJobCanceled()) {
    if (stat == FALSE) {
      FailProgress()
    } else {
      CompleteProgress()
    }
  }

  if (IsJobCanceled() || IsEarlyTerminationRequested()) {
    if (RunningOnGrid == TRUE) {
      killAGridJob(gridRegistry)
    }
  }

  progress <- get("ProgressStatus", envir = nlmeEnv)
  if (func != "") {
    do.call(func, list(func_arg))
  }

  if (RunningOnGrid) {
    if (Sys.getenv("NLME_KEEP_GRID_RESULTS") != "TRUE") {
      if (progress$NumOfSamplesFailed == 0 &&
          progress$NumOfSamplesExpired == 0) {
        for (t in 1:1) {
          tryCatch({
            removeRegistry(reg = gridRegistry)
            t <- 999
            break
          },
          error = function(ex) {
            Sys.sleep(1)
          })
        }
      }
    }
  }

  if (partialJob == FALSE) {
    if (progress$NumOfSamplesFailed == 0 &&
        progress$NumOfSamplesExpired == 0 &&
        Sys.getenv("NLME_KEEP_GRID_RESULTS") != "TRUE") {
      removeTempWorkingDirectory(jobHome)
    }
  }

  return(jobHome)
}
