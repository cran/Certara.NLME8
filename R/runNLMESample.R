runNLMESample <-
  function(indx,
           eArgsFile,
           ofn,
           extraArgs = "",
           seed = -1,
           max_tries = 1,
           exePostfix = "",
           SharedWorkingDir = "") {
    if (SharedWorkingDir == "") {
      SharedWorkingDir <- get("SharedWorkingDir", envir = nlmeEnv)
    }

    source(file.path(SharedWorkingDir, "myglobaldefs.r"), local = TRUE)

    if (missing(eArgsFile)) {
      eArgsFile <- extra_args_file
    }

    if (dirname(eArgsFile) == ".") {
      eArgsFile <- file.path(SharedWorkingDir, eArgsFile)
    }

    if (jobType == "BOOTSTRAP" && seed != -1) {
      if (.Platform$OS.type == "unix") {
        extraArgs <-
          sprintf(
            "/boot ${bootSeed} /bootsamp %d -boottry ${numTries}",
            indx
          )
      } else {
        extraArgs <-
          sprintf("/boot $bootSeed /bootsamp %d -boottry $numTries", indx)
      }
    }

    baseDirectory <- SharedWorkingDir
    if (tolower(parallelMethod) %in% c("multicore", "none", "local_mpi", "mpi")) {
      baseJobDirectory <- baseDirectory
    } else {
      baseJobDirectory <- file.path(baseDirectory, "registry")
    }

    indx02d <- sprintf("%02d", indx %% 100)
    indx03d <- sprintf("%03d", indx)
    if (indx == 0) {
      workingDir <- baseJobDirectory
      newFilePath <-
        file.path(baseJobDirectory, paste0("exNLME", exeFileExtension))
    } else {
      # Create working directory to run individual jobs in
      workingDir <- file.path(baseJobDirectory, "jobs", indx02d, indx)
      dir.create(workingDir,
        recursive = TRUE,
        showWarnings = FALSE
      )
      newFilePath <-
        file.path(workingDir, paste0("exNLME", exeFileExtension))
      files_to_copy <- unlist(strsplit(files_to_copy, split = " "))
      # Copy all the input files to the individual job directory
      copy_filesWarnLong(file.path(SharedWorkingDir, files_to_copy),
        workingDir,
        overwrite = TRUE
      )
      # that file already has full path:
      copy_filesWarnLong(eArgsFile, workingDir, overwrite = TRUE)
    }

    statusFile <-
      file.path(baseDirectory, paste0("S_", indx03d, ".status"))
    statusBackupFile <-
      file.path(baseDirectory, paste0("S_", indx03d, ".status.bak"))
    logFile <- file.path(workingDir, ".status")
    outFile <- file.path(workingDir, ofn)

    # nlmeargs.txt needs @ for commandline
    if (nchar(eArgsFile) > 0) {
      extraArgsFile <- paste0("@", basename(eArgsFile))
    }

    MpiExecutable <- get("MpiExecutable", envir = nlmeEnv)
    MpiArgument <- get("MpiArgument", envir = nlmeEnv)
    MpiNumCores <- get("MpiNumCores", envir = nlmeEnv)
    MpiLocal <- get("MpiLocal", envir = nlmeEnv)
    MpiExecutable <-
      gsub(".exe", paste0(exePostfix, ".exe"), MpiExecutable, fixed = TRUE)

    if (.Platform$OS.type == "unix") {
      # There are two ways to specify the input files
      # As part of the commandline or inside of nlmeargs.txt
      # Args :
      #        %1 RUN_MODE   = COMPILE_AND_RUN COMPILE RUN
      #        %2 MODELFILE  = PML file to use for the model
      #        %3 LCWD       = Full path Working directory to start on local host
      #        %4 MPIFLAG    = MPIYES | MPINO
      #        %5 LOCAL_HOST =  YES | NO
      #        %6 NUM_NODES  = Number of mpi nodes
      #        %7 SHARED_DRIVE = Location of shared drive for #ote MPI
      #        %8 RCWD         = Name of the working directory on #ote/shared drive
      #        %9 FILES_TO_COPY= List of files to copy to #ote node or shared directory
      #        %10 NLME_ARGS    = Arguments passed on to xxx.exe
      #           CMD_HASHCODE=${11}
      #           NLME_EXE_POSTFIX=${12}
      argsToPaste <-
        shQuote(paste(extraArgs, extraArgsFile), # "/d1", column_def_file, data_file),
          type = "cmd"
        )

      commandString <-
        paste(
          shQuote(file.path(
            baseDirectory, paste0("execNLMECmd", exeFileExtension)
          )),
          "RUN",
          shQuote(file.path(baseDirectory, model_file), type = "cmd"),
          shQuote(workingDir, type = "cmd"),
          MpiArgument,
          MpiLocal,
          MpiNumCores,
          "\"\"",
          "NLME_DIR",
          "\"\"",
          argsToPaste,
          "\"\"",
          exePostfix
        )


      NLMEexeLinking <- ifelse(baseDirectory != workingDir,
        paste0(
          "\nln -s ",
          shQuote(file.path(baseDirectory, MpiExecutable), type = "cmd"),
          " ",
          shQuote(workingDir, type = "cmd")
        ),
        ""
      )
      # Write out the shellscript that we will run
      cat(
        paste0(
          "#!/bin/bash",
          "\nset -x",
          "\ndeclare -i numTries",
          "\ndeclare -i bootSeed",
          "\ncd ",
          shQuote(workingDir),
          "\nrm -f ",
          shQuote(outFile),
          NLMEexeLinking,
          "\nchmod 777 ",
          MpiExecutable,
          "\necho 'RUNNING' >",
          shQuote(logFile, type = "cmd"),
          "\necho 'RUNNING' >",
          shQuote(statusFile, type = "cmd"),
          "\necho '",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
          "' >>",
          shQuote(statusFile, type = "cmd"),
          "\nmaxTries=",
          max_tries,
          "\nnumTries=1",
          "\nbootSeed=",
          seed,
          "\nstartTime=`date +\"%x %T\"`",
          "\nwhile [ ${numTries} -le ${maxTries} ]; do",
          "\n  ",
          commandString,
          "\n  stopTime=`date +\"%x %T\"`",
          "\n  ((numTries++))",
          "\n  bootSeed=${bootSeed}+1",
          "\n  sleep 1",
          "\n  if [ -s ",
          shQuote(outFile, type = "cmd"),
          " ]",
          "\n  then",
          "\n    echo 'SUCCESS' >",
          shQuote(logFile, type = "cmd"),
          "\n    echo 'SUCCESS' >",
          shQuote(statusFile, type = "cmd"),
          "\n    echo ${startTime} >>",
          shQuote(statusFile, type = "cmd"),
          "\n    echo ${stopTime} >>",
          shQuote(statusFile, type = "cmd"),
          "\n    exit",
          "\n  fi",
          "\ndone",
          "\necho 'FAILED' >",
          shQuote(logFile, type = "cmd"),
          "\necho 'FAILED' >",
          shQuote(statusFile, type = "cmd")
        ),
        file = newFilePath,
        sep = "\n",
        append = FALSE
      )

      Sys.chmod(newFilePath, mode = "0777", use_umask = TRUE)
      Sys.chmod(list.files(
        path = Sys.getenv("INSTALLDIR"),
        pattern = "*.sh|TDL5$",
        full.names = TRUE,
        recursive = TRUE
      ),
      mode = "777"
      )

      if (indx != 0 &&
        (tolower(parallelMethod) %in% c("multicore", "none", "local_mpi", "mpi"))) {
        system(paste(
          "nohup",
          shQuote(newFilePath, type = "cmd"),
          " > ",
          shQuote(file.path(workingDir, "log"), type = "cmd"),
          " 2>&1  &"
        ))
      } else {
        system(paste0(
          shQuote(newFilePath, type = "cmd"),
          " > ",
          shQuote(file.path(workingDir, "log"), type = "cmd"),
          " 2>&1 ",
          indx,
          " "
        ))
      }
    } else {
      argsToPaste <- shQuote(paste(
        extraArgs,
        extraArgsFile
      ))


      argsToPaste <- paste0("\"\"\"", argsToPaste, "\"\"\"")
      commandString <- paste(
        "RUN",
        shQuote(file.path(baseDirectory, model_file)),
        shQuote(workingDir),
        MpiArgument,
        MpiLocal,
        MpiNumCores,
        "\"\"\"\"\"\"",
        "NLME_DIR",
        "\"\"\"\"\"\"",
        argsToPaste,
        "\"\"\"\"\"\"",
        exePostfix
      )

      copy_filesWarnLong(file.path(Sys.getenv("INSTALLDIR"), "NLMESample.ps1"),
        newFilePath,
        overwrite = TRUE
      )

      if (indx != 0) {
        copy_filesWarnLong(file.path(baseDirectory, MpiExecutable),
          workingDir,
          overwrite = TRUE
        )
      }

      ps_args <- c(
        "-noninteractive",
        "-executionpolicy",
        "remotesigned",
        "-File",
        shQuote(newFilePath),
        shQuote(gsub("/", "\\", logFile, fixed = TRUE)),
        shQuote(gsub("/", "\\", statusFile, fixed = TRUE)),
        shQuote(gsub("/", "\\", statusBackupFile, fixed = TRUE)),
        shQuote(gsub("/", "\\", outFile, fixed = TRUE)),
        paste("-bootseed ", seed),
        max_tries,
        shQuote(baseDirectory),
        shQuote(commandString)
      )

      ps_wait <-
        (indx == 0) ||
          !(tolower(parallelMethod) %in% c("multicore", "none", "local_mpi", "mpi"))

      cat("powershell ",
        paste0(ps_args, " ", collapse = " "),
        file = file.path(workingDir, "mpilog.txt")
      )

      system2(
        "powershell",
        args = ps_args,
        stdout = file.path(workingDir, "ps1log.txt"),
        stderr = file.path(workingDir, "ps1err.txt"),
        wait = ps_wait
      )
    }
  }
