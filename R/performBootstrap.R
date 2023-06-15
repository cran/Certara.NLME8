#' NLME Bootstrap Function
#'
#' Runs an NLME bootstrap job in parallel and produces summaries
#'
#' @param args Arguments for bootstrap execution
#' @param allowIntermediateResults Set to \code{TRUE} to return intermediate results
#' @param reportProgress Set to \code{TRUE} to report progress
#' @return Directory path where NLME job was executed
#' @keywords NLME Bootstrap
#' @export
performBootstrap <-
  function(args,
           allowIntermediateResults = TRUE,
           reportProgress = FALSE) {
    requiredargs <-
      c(
        "parallel_mechanism",
        "install_dir",
        "shared_directory",
        "localWorkingDir",
        "engine",
        "num_iterations",
        "num_samples",
        "max_tries",
        "model_file",
        "column_def_file",
        "data_file",
        "start_seed",
        "extra_args_file",
        "files_to_copy",
        "NumProc",
        "ConfidenceLevel",
        "[gridDirectory]"
      )

    if (length(args) != 16 && length(args) != 17) {
      check_Arguments(requiredargs, args, "performBootstrap")
    }

    rm(list = ls(envir = nlmeEnv), envir = nlmeEnv)
    assign("reportProgress", reportProgress, envir = nlmeEnv)

    tryCatch({
      # preparing the method
      parallelMethod <- args[1]
      assign("parallelMethod", parallelMethod, envir = nlmeEnv)

      # preparing the NLME executable dir
      if (!dir.exists(args[2])) {
        if (!dir.exists(Sys.getenv("INSTALLDIR"))) {
          stop(paste0(
            "Cannot find a valid path to NLME executable directory given as ",
            args[2]
          ))
        }

        warning("Using NLME executable directory as ",
          Sys.getenv("INSTALLDIR"), "since the directory ",
          args[2],
          " does not exist or couldn't be accessed"
        )

        args[2] <- Sys.getenv("INSTALLDIR")
      } else {
        Sys.setenv(INSTALLDIR = args[2])
      }

      # preparing temporary shared folders
      if (!dir.exists(args[3])) {
        stop(paste0("Cannot find a valid path to shared directory given as ", args[3]))
      }

      shared_directory <- args[3]
      create_jobdirs(shared_directory = shared_directory,
                     jobtype = "BOOTSTRAP")

      # preparing local working dir
      if (!dir.exists(args[4])) {
        stop(paste0(
          "Cannot find a valid path to local working directory given as ",
          args[4]
        ))
      }

      localWorkingDir <- gsub("\\", "/", args[4], fixed = TRUE)
      assign("localWorkingDir", localWorkingDir, envir = nlmeEnv)

      # preparing model arguments (engine, num_iterations, num_samples, max_tries, start_seed)
      for (i in c(5:8, 12)) {
        int_param <- as.integer(args[i])
        if (is.na(int_param)) {
          stop(paste0(requiredargs[i]), " is not an integer")
        }

        assign(requiredargs[i], int_param, envir = nlmeEnv)
      }
      # we will need that soon:
      num_samples <- as.integer(args[7])

      # preparing model files
      assign("model_file", args[9], envir = nlmeEnv)
      assign("column_def_file", args[10], envir = nlmeEnv)
      assign("data_file", args[11], envir = nlmeEnv)
      assign("extra_args_file",
             gsub("\\", "/", args[13], fixed = TRUE),
             envir = nlmeEnv)
      assign("files_to_copy", gsub("\\", "/", args[14], fixed = TRUE), envir = nlmeEnv)

      # figure out parallel method
      arglist <- unlist(strsplit(args[15], split = ","))
      num_processes <- as.integer(arglist[1])

      if (length(arglist) == 2) {
        num_cores <- as.integer(arglist[2])
      } else {
        if (tolower(parallelMethod) %in% c("local_mpi", "mpi", "none")) {
          num_cores <- num_processes
        } else {
          assign("num_processes", num_processes, envir = nlmeEnv)
          num_cores <- figureOutMpiNumCores(num_samples)
        }
      }

      if (grepl("none", parallelMethod, ignore.case = TRUE) &&
          num_processes > 1) {
        warning(
          "no parallel methods were chosen but number of cores is ",
          num_processes,
          "\nReseting to 1.",
          call. = FALSE,
          immediate. = TRUE
        )
        num_cores <- num_processes <- 1
      }

      assign("num_processes", num_processes, envir = nlmeEnv)

      # figure out conf level
      conf_level <- as.double(args[16])
      if (is.na(conf_level)) {
        stop(paste0("conf level is not a double: ", args[16]))
      }
      assign("confidence_level", as.double(args[16]), envir = nlmeEnv)

      # assign workflow name
      if (is.na(args[17])) {
        workflow_name <- ""
      } else {
        workflow_name <- args[17]
      }
      assign("workflow_name", workflow_name, envir = nlmeEnv)

      updateInitialStatus("Bootstrap", parallelMethod, localWorkingDir)

      if (.Platform$OS.type == "windows") {
        exeFileExtension <- ".ps1"
      } else {
        exeFileExtension <- ".sh"
      }
      assign("exeFileExtension", exeFileExtension, envir = nlmeEnv)

      progress <- list(
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
      progress$ParallelProtocol <- parallelMethod
      progress$StartTime <- getLocalTimeUTC()

      assign("ProgressStatus", progress, envir = nlmeEnv)
      ReportProgress(progress)

      MpiExecutable <- "NLME7.exe"
      MpiArgument <- "MPINO"
      MpiNumCores <- 1
      MpiLocal <- "NO"
      assign("MpiExecutable", MpiExecutable, envir = nlmeEnv)
      assign("MpiArgument", MpiArgument, envir = nlmeEnv)
      assign("MpiLocal", MpiLocal, envir = nlmeEnv)
      assign("MpiNumCores", MpiNumCores, envir = nlmeEnv)

      RunningOnGrid <- FALSE

      if (tolower(parallelMethod) == "none") {
        done <- 1:num_samples
        ret <- multiCoreGeneric(
          parallelMethod = parallelMethod,
          jobType = "BOOTSTRAP",
          numCoresToUse = num_processes
        )
        stat <- ret$stat
        done <- ret$done
      } else if (tolower(parallelMethod) == "multicore") {
        ret <- multiCoreGeneric(
          parallelMethod = parallelMethod,
          jobType = "BOOTSTRAP",
          numCoresToUse = num_processes
        )
        stat <- ret$stat
        done <- ret$done
        if (!IsEarlyTerminationRequested() && !is.null(done)) {
          done <- 1:num_samples
        }
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
          parallelMethod = parallelMethod,
          jobType = "BOOTSTRAP",
          numCoresToUse = num_processes
        )
        done <- 1:num_samples
        stat <- ret$stat
        done <- ret$done
      } else {
        RunningOnGrid <- TRUE
        if (grepl(
          "(torque_mpi)|(sge_mpi)|(lsf_mpi)|(slurm_mpi)",
          parallelMethod,
          ignore.case = TRUE
        )) {
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
        GlobalSummaryLine1 <- sprintf("")
        assign("GlobalSummaryLine1", GlobalSummaryLine1, envir = nlmeEnv)
        ret <- startGenericGridJob(
          "BOOTSTRAP",
          allowIntermediateResults,
          progressStage = "Processing Replicates"
        )
        gridRegistry <- get("gridRegistry", envir = nlmeEnv)
        done <- ret$done
        stat <- ret$stat
      }
      if (stat == TRUE && !IsJobCanceled()) {
        generateBootstrapResults(localWorkingDir, done)
        CompleteProgress()
      } else if (stat == FALSE) {
        if (!is.null(done)) {
          done <- 1:num_samples
        }
        collectJobErrors(localWorkingDir, "BOOTSTRAP", done, control_lines)

        if (!IsJobCanceled()) {
          FailProgress()
        }
      }

      if ((IsJobCanceled() ||
           IsEarlyTerminationRequested()) && RunningOnGrid == TRUE) {
        killAGridJob(gridRegistry)
      }

      progress <- get("ProgressStatus", envir = nlmeEnv)
      if (RunningOnGrid) {
        waitTillAllJobsAreFinished()
        if (Sys.getenv("NLME_KEEP_GRID_RESULTS") != "TRUE") {
          if (progress$NumOfSamplesFailed == 0 &&
              progress$NumOfSamplesExpired == 0) {
            for (t in 1:2) {
              tryCatch({
                removeRegistry(gridRegistry)
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

      if (progress$NumOfSamplesFailed == 0 &&
          progress$NumOfSamplesExpired == 0 &&
          Sys.getenv("NLME_KEEP_GRID_RESULTS") != "TRUE") {
        jobHome <- get("jobHome", envir = nlmeEnv)
        removeTempWorkingDirectory(jobHome)
      }
    },
    error = function(ex) {
      warning("Failed to performBootstrap. Error is: ", ex, call. = FALSE, immediate. = TRUE)
      FailProgress()
    })

  }
