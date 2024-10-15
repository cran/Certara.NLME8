updateInitialStatus <- function(runMode = "", parallelMethod = "", localWorkingDir = "") {
  progress <- list(MachineName = "LocalHost", ParallelProtocol = "None", ModelName = "", StartTime = "", EndTime = "", Status = "InProgress", NumOfSamples = 1, NumOfSamplesCompleted = 0, NumOfSamplesFailed = 0, NumOfSamplesExpired = 0, NumOfSamplesErrored = 0)
  progress$NumOfSamples <- 1
  progress$Status <- "InProgress"
  progress$ParallelProtocol <- parallelMethod
  progress$StartTime <- getLocalTimeUTC()
  assign("ProgressStatus", progress, envir = nlmeEnv)

  GlobalSummaryLine1 <- sprintf("Preparing files for %s run\n", runMode)
  assign("GlobalSummaryLine1", GlobalSummaryLine1, envir = nlmeEnv)
  GlobalSummaryLine2 <- ""
  assign("GlobalSummaryLine2", GlobalSummaryLine2, envir = nlmeEnv)
  GlobalSummaryLine3 <- ""
  assign("GlobalSummaryLine3", GlobalSummaryLine3, envir = nlmeEnv)
  assign("localWorkingDir", localWorkingDir, envir = nlmeEnv)
  UpdateProgressMessages()
}
