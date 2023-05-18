FailProgress <- function() {
  progress <- get("ProgressStatus", envir = nlmeEnv)
  progress$Status <- "Failed"
  progress$NumOfSamplesFailed <- progress$NumOfSamples
  progress$EndTime <- getLocalTimeUTC()
  assign("ProgressStatus", progress, envir = nlmeEnv)
  ReportProgress(progress)
  warning(
    "\nStatus: ", progress$Status,
    "\nStage: ",
    paste(get("GlobalSummaryLine1", envir = nlmeEnv),
      get("GlobalSummaryLine2", envir = nlmeEnv),
      get("GlobalSummaryLine3", envir = nlmeEnv),
      collapse = " "
    ), call. = FALSE, immediate. = TRUE
  )
}
