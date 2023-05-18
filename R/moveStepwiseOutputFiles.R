# Make copies of the file we need later, tag then with scnario ID
# These are basically : progress.txt S_00N.status
# outnnnn.txt just gets copied from run directory into current directory.
#
moveStepwiseOutputFiles <- function(scenarioIndexes) {
  if (exists("jobsDirectoryRoot", envir = nlmeEnv)) {
    jobsDirectoryRoot <- get("jobsDirectoryRoot", envir = nlmeEnv)
  } else {
    jobsDirectoryRoot <- ""
  }
  localWorkingDir <- get("localWorkingDir", envir = nlmeEnv)
  if (jobsDirectoryRoot != "") {
    jobsBaseDirectory <- getBatchDirectoryLocation(jobsDirectoryRoot)
    scenarios <- get("scenarios", envir = nlmeEnv)
    indx <- 1
    for (s in scenarioIndexes) {
      for (t in 1:10) {
        tryCatch({
          if (scenarios[[s]]$status == "Completed") {
            t <- 99
            break
          }
          if (jobsDirectoryRoot != "") {
            jobBaseIndx <- indx %% 100
            progressFile <-
              file.path(
                jobsBaseDirectory,
                "jobs",
                sprintf("%02d", jobBaseIndx),
                indx,
                "progress.txt"
              )
            statusFile <-
              file.path(jobsDirectoryRoot,
                        sprintf("S_%03d.status", indx))
            newProgressFile <- paste0("progress.txt.", s)
            newStatusFile <- paste0("status.txt.", s)
            outputFile <-
              file.path(
                jobsBaseDirectory,
                "jobs",
                sprintf("%02d", jobBaseIndx),
                indx,
                scenarios[[s]]$outputFilename
              )
            copy_filesWarnLong(
              outputFile,
              file.path(localWorkingDir, scenarios[[s]]$outputFilename),
              overwrite = TRUE
            )
          } else {
            progressFile <- paste0("progress.txt.Job", indx)
            statusFile <- sprintf("S_%03d.status", indx)
            newProgressFile <- paste0("progress.txt.", s)
            newStatusFile <- paste0("status.txt.", s)
          }
          copy_filesWarnLong(progressFile,
                             file.path(localWorkingDir, newProgressFile),
                             overwrite = TRUE)
          copy_filesWarnLong(statusFile,
                             file.path(localWorkingDir, newStatusFile),
                             overwrite = TRUE)
          indx <- indx + 1
          break
        },
        error = function(ex) {
          warning(ex)
          Sys.sleep(1)
        })
      }
    }
  }
}
