UpdateProgress <- function(numSamples, numSuccessful, numFailed, numExpired, numErrors = 0) {
  progress <- get("ProgressStatus", envir = nlmeEnv)
  progress$NumOfSamples <- numSamples
  progress$NumOfSamplesCompleted <- numSuccessful
  progress$NumOfSamplesFailed <- numFailed
  progress$NumOfSamplesExpired <- numExpired
  progress$NumOfSamplesErrored <- numErrors

  assign("ProgressStatus", progress, envir = nlmeEnv)
  ReportProgress(progress)
}
