reportCurrentStatus <- function(num_samples, numSuccessful, numFailed) {
  .output_progress(paste0("Num Jobs/Completed/Failed:",
                          paste(num_samples, numSuccessful, numFailed, sep = "/"),
                          "\n"))
}


CompleteProgress <- function() {
  ProgressStatus <- get("ProgressStatus", envir = nlmeEnv)
  progress <- ProgressStatus
  progress$Status <- "Finished"
  progress$EndTime <- getLocalTimeUTC()
  GlobalSummaryLine1 <- "Finished, uploading results"
  assign("GlobalSummaryLine1", GlobalSummaryLine1, envir = nlmeEnv)
  GlobalSummaryLine2 <- ""
  assign("GlobalSummaryLine2", GlobalSummaryLine2, envir = nlmeEnv)
  GlobalSummaryLine3 <- ""
  assign("GlobalSummaryLine3", GlobalSummaryLine3, envir = nlmeEnv)
  assign("ProgressStatus", progress, envir = nlmeEnv)
  ReportProgress(progress)
}

ReportProgress <- function(ProgressStatus) {
  localWorkingDir <- get("localWorkingDir", envir = nlmeEnv)
  ProgressStatus$ModelName <- shortModelName()

  root <- xml2::xml_new_root("progress")
  for (i in seq_along(ProgressStatus)) {
    xml2::xml_add_child(root, names(ProgressStatus[i]), as.character(ProgressStatus[[i]]))
  }

  numTries <- 0
  numTimesToTry <- 5
  done <- 0
  xmlToSave <- file.path(localWorkingDir, "progress.xml")
  while (!done) {
    tryCatch(
      {
        xml2::write_xml(root, file = xmlToSave)
        done <- 1
      },
      error = function(ex) {
        numTries <- numTries + 1
        if (numTries < numTimesToTry) {
          Sys.sleep(1)
        } else {
          done <- 1
          warning(paste("Tried", numTries, "times to write", xmlToSave))
        }
      }
    )
  }
  xml2::xml_remove(root, free = TRUE)
}
