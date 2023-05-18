
OLDrunNLMEInitialRun <- function() {
  ProgressStatus <- get("ProgressStatus", envir = nlmeEnv)
  progress <- ProgressStatus
  progress$Status <- "Running"
  GlobalSummaryLine1 <- "Running bootstrap initial estimates"
  assign("GlobalSummaryLine1", GlobalSummaryLine1, envir = nlmeEnv)
  assign("ProgressStatus", progress, envir = nlmeEnv)

  UpdateProgress(1, 0, 0, 0)
  UpdateProgressMessages(
    currentJobDirectory = "",
    progressStage = "Initial Estimates "
  )

  ret <- generateNLMEScriptAndRun("COMPILE")
  if (ret == FALSE) {
    return(FALSE)
  }

  SharedWorkingDir <- get("SharedWorkingDir", envir = nlmeEnv)
  runNLMEInitialSample(indx = 0, SharedWorkingDir)

  progress <- get("ProgressStatus", envir = nlmeEnv)
  SharedWorkingDir <- get("SharedWorkingDir", envir = nlmeEnv)
  if (file.exists(file.path(SharedWorkingDir, "out.txt"))) {
    UpdateMDLfrom_dmptxt(dmpfile = "dmp.txt", SharedWorkingDir = SharedWorkingDir, model_file = get("model_file", envir = nlmeEnv))
    files <- file.path(
      SharedWorkingDir,
      c(
        "VarCoVar.csv",
        "doses.csv",
        "err2.txt",
        "err1.txt",
        "progress.txt",
        "dmp.txt",
        "EtaEta.txt",
        "EtaCov.txt",
        "StrCov.txt",
        "EtaShrinkageBySubject.txt",
        "nlme7engine.log"
      )
    )
    filesToCopy <- files[file.exists(files)]
    copy_filesWarnLong(filesToCopy, dirname(SharedWorkingDir), overwrite = TRUE)

    copy_filesWarnLong(
      file.path(SharedWorkingDir, "out.txt"),
      file.path(dirname(SharedWorkingDir), "out_initialEstimates.txt")
    )

    progress$Status <- "Done"
    GlobalSummaryLine1 <- "Finished bootstrap initial estimates run"
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir = nlmeEnv)
    assign("ProgressStatus", progress, envir = nlmeEnv)
    UpdateProgress(1, 1, 0, 0)
    UpdateProgressMessages(
      currentJobDirectory = "",
      progressStage = "Initial Estimates "
    )
    return(TRUE)
  } else {
    GlobalSummaryLine1 <- "Failed running initial estimates"
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir = nlmeEnv)
    assign("ProgressStatus", progress, envir = nlmeEnv)

    UpdateProgress(1, 0, 1, 0)
    UpdateProgressMessages(
      currentJobDirectory = "",
      progressStage = "Initial Estimates "
    )
    FailProgress()
    return(FALSE)
  }
}
