CancelProgress <- function(numSamples, numSuccessful, numFailed) {
  ProgressStatus <- get("ProgressStatus", envir = nlmeEnv)
  progress <- ProgressStatus
  progress$NumOfSamples <- numSamples
  progress$NumOfSamplesCompleted <- numSuccessful
  progress$NumOfSamplesFailed <- numFailed
  progress$Status <- "Canceled"
  progress$EndTime <- getLocalTimeUTC()
  assign("ProgressStatus", progress, envir = nlmeEnv)
  ReportProgress(progress)
}
