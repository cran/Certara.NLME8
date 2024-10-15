# Compile the model and create NLME.exe
compileAndLinkNLME <- function() {
  listOfExesToBuild <- getListOfExesNeeded()
  listOfExesToBuildLength <- length(listOfExesToBuild)
  if (listOfExesToBuildLength == 0) {
    if (!generateNLMEScriptAndRun("COMPILE")) {
      return(FALSE)
    }
  } else {
    current <- 0
    for (l in listOfExesToBuild) {
      modelFilename <- l$modelName
      exePostfix <- l$exePostfix
      current <- current + 1
      updateCompileStatus(current, listOfExesToBuildLength)
      if (!generateNLMEScriptAndRun("COMPILE", modelFilename, exePostfix)) {
        return(FALSE)
      }
    }
  }

  assign("GlobalSummaryLine1", "", envir = nlmeEnv)

  TRUE
}

getListOfExesNeeded <- function() {
  if (exists("profileModels", envir = nlmeEnv)) {
    profileModels <- get("profileModels", envir = nlmeEnv)
  } else {
    profileModels <- NULL
  }
  return(profileModels)
}

generateNLMEScriptAndRun <-
  function(runType,
           customModel = "",
           exePostfix = "") {
    SharedWorkingDir <-
      path.expand(get("SharedWorkingDir", envir = nlmeEnv))
    source(file.path(SharedWorkingDir, "myglobaldefs.r"), local = TRUE)
    if (customModel != "") {
      model_file <- customModel
    }

    MpiArgument <- get("MpiArgument", envir = nlmeEnv)
    MpiLocal <- get("MpiLocal", envir = nlmeEnv)
    MpiNumCores <- get("MpiNumCores", envir = nlmeEnv)

    newFilePath <-
      file.path(SharedWorkingDir, paste0("exNLME", exeFileExtension))

    if (nchar(extra_args_file) > 0) {
      extra_args_file <- sprintf("@%s", extra_args_file)
    }

    INSTALLDIR <- path.expand(Sys.getenv("INSTALLDIR"))
    checkGCCResult <- checkGCC()

    checkLicenseFileResult <- checkLicenseFile(INSTALLDIR)

    if (as.numeric(MpiNumCores) > 1) {
      if (.Platform$OS.type == "windows") {
        checkMPI <- checkMPIWindows()
      } else {
        checkMPI <- checkMPILinux()
      }
    } else {
      checkMPI <- TRUE
    }

    if (!checkGCCResult |
        !checkLicenseFileResult | !checkMPI) {
      num_samples <- as.integer(get("num_samples", envir = nlmeEnv))
      CancelProgress(num_samples, 0, num_samples)
      return(FALSE)
    }
    compileLogFile <- file.path(SharedWorkingDir, "compilelog.txt")
    if (.Platform$OS.type == "unix") {
      commandString <- paste(
        shQuote(file.path(SharedWorkingDir, "execNLMECmd.sh"), type = "cmd"),
        runType,
        model_file,
        shQuote(SharedWorkingDir, type = "cmd"),
        MpiArgument,
        MpiLocal,
        MpiNumCores,
        '\"\"',
        "NLME_DIR",
        '\"\"',
        shQuote(extra_args_file, type = "cmd"),
        '\"\"',
        exePostfix,
        ">",
        shQuote(compileLogFile, type = "cmd"),
        "2>&1"
      )

      cat(
        paste0("#!/bin/bash"),
        paste0("##set -x "),
        paste0("export INSTALLDIR=", shQuote(INSTALLDIR, type = "cmd")),
        paste0(commandString),
        file = newFilePath,
        sep = "\n",
        append = FALSE
      )

      Sys.chmod(newFilePath, mode = "0777", use_umask = TRUE)
      ret <- system(shQuote(newFilePath, type = "cmd"))
      if (ret != 0) {
        num_samples <- as.integer(get("num_samples", envir = nlmeEnv))
        CancelProgress(num_samples, 0, num_samples)
        WarningMessage <- "Compile/Link Failed"
        compileLogFile <- file.path(SharedWorkingDir, "compilelog.txt")
        if (file.exists(compileLogFile)) {
          WarningMessage <- paste0(
            WarningMessage,
            "\nOutput from compilelog.txt:\n",
            readChar(compileLogFile, file.info(compileLogFile)$size, useBytes = TRUE)
          )
        }

        TDL5LogFile <- file.path(SharedWorkingDir, "log.txt")
        if (file.exists(TDL5LogFile)) {
          WarningMessage <- paste0(
            WarningMessage,
            "\nOutput from TDL5 run:\n",
            readChar(TDL5LogFile, file.info(TDL5LogFile)$size, useBytes = TRUE)
          )
        }

        warning(WarningMessage, call. = FALSE)
        return(FALSE)
      }
    } else {
      # that args are passed to ps, see execNLMECmd.ps1 for details
      # shortShared <- gsub("/", "\\", shortPathName(SharedWorkingDir), fixed = TRUE)
      ps_args <- c(
        "-noninteractive",
        "-executionpolicy",
        "remotesigned",
        "-File",
        shQuote(
          file.path(SharedWorkingDir, "execNLMECmd.ps1", fsep = "\\")
        ),
        runType,
        model_file,
        shQuote(SharedWorkingDir),
        MpiArgument,
        MpiLocal,
        MpiNumCores,
        "\"\"",
        "NLME_DIR",
        "\"\"",
        shQuote(extra_args_file),
        "\"\"",
        exePostfix
      )

      installDirShort <-
        gsub("\\", "/", utils::shortPathName(INSTALLDIR), fixed = TRUE)

      # compile and/or run NLME executable
      system2(
        "powershell",
        args = ps_args,
        wait = TRUE,
        stdout = compileLogFile,
        stderr = compileLogFile
      )

      # exit if file is not created
      if (MpiArgument == "MPIYES") {
        NLME7exe <-
          file.path(
            gsub("\\", "/", SharedWorkingDir, fixed = TRUE),
            paste0("mpiNLME7", exePostfix, ".exe")
          )
      } else {
        NLME7exe <-
          file.path(
            gsub("\\", "/", SharedWorkingDir, fixed = TRUE),
            paste0("NLME7", exePostfix, ".exe")
          )
      }

      if (!file.exists(NLME7exe)) {
        num_samples <- as.integer(get("num_samples", envir = nlmeEnv))
        CancelProgress(num_samples, 0, num_samples)

        WarningMessage <- "Compile/Link Failed"
        if (file.exists("log.txt")) {
          WarningMessage <- paste0(
            WarningMessage,
            "\nOutput from TDL5 run:\n",
            readChar("log.txt", file.info("log.txt")$size, useBytes = TRUE)
          )
        }

        if (file.exists("mpilog.txt")) {
          WarningMessage <- paste0(
            WarningMessage,
            "\nOutput from powershell run:\n",
            readChar(
              "mpilog.txt",
              file.info("mpilog.txt")$size,
              useBytes = TRUE
            )
          )
        }

        warning(WarningMessage,
                call. = FALSE)
        return(FALSE)
      } else {
        message("The model compiled")
      }
    }

    return(TRUE)
  }

updateCompileStatus <- function(indx, tot) {
  GlobalSummaryLine1 <-
    sprintf("Compiling %d of %d NLME models\n", indx, tot)
  assign("GlobalSummaryLine1", GlobalSummaryLine1, envir = nlmeEnv)
  GlobalSummaryLine2 <- ""
  assign("GlobalSummaryLine2", GlobalSummaryLine2, envir = nlmeEnv)
  GlobalSummaryLine3 <- ""
  assign("GlobalSummaryLine3", GlobalSummaryLine3, envir = nlmeEnv)
  UpdateProgressMessages()
}
