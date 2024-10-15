multiCoreGeneric <- function(parallelMethod, jobType, numCoresToUse, allowIntermediateResults = TRUE, progressStage = "") {
  SharedWorkingDir <- get("SharedWorkingDir", envir = nlmeEnv)
  num_samples <- as.integer(get("num_samples", envir = nlmeEnv))
  assign("jobType", jobType, envir = nlmeEnv)
  seeds <- 1:num_samples

  copy_InputFiles(SharedWorkingDir)

  if (jobType == "BOOTSTRAP") {
    start_seed <- as.integer(get("start_seed", envir = nlmeEnv))
    for (i in 1:num_samples) {
      seeds[i] <- start_seed + (i - 1) * 100
    }

    if (Sys.getenv("NLME_SKIP_INITIAL_BOOTSTRAP_RUN") == "FALSE") {
      stat <- OLDrunNLMEInitialRun()
    } else {
      stat <- compileAndLinkNLME()
    }
  } else {
    stat <- compileAndLinkNLME()
  }

  if (stat == FALSE) {
    return(list(stat = FALSE, done = c()))
  }

  createJobsDirectory(SharedWorkingDir, num_samples)

  if (jobType == "BOOTSTRAP") {
    assign("GlobalSummaryLine1", "Processing replicates", envir = nlmeEnv)
    UpdateProgressMessages()
    UpdateProgress(num_samples, 0, 0, 0)
    assign("GlobalSummaryLine1", "", envir = nlmeEnv)
  }

  if (tolower(parallelMethod) %in% c("local_mpi", "mpi")) {
    ret <- startAndMonitorMulticoreJobs(1, num_samples, seeds, allowIntermediateResults, jobType, progressStage)
  } else {
    ret <- startAndMonitorMulticoreJobs(numCoresToUse, num_samples, seeds, allowIntermediateResults, jobType, progressStage)
  }
  return(ret)
}
