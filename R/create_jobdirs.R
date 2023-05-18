create_jobdirs <- function(shared_directory, jobtype) {
  if (jobtype == "BOOTSTRAP") {
    jobShort <- "BOOT"
  } else if (jobtype == "ESTIMATION_RUN") {
    jobShort <- "EST"
  } else if (jobtype == "STEPWISE_SEARCH") {
    jobShort <- "STEP"
  } else if (jobtype == "COVAR_SEARCH") {
    jobShort <- "SHOT"
  } else if (jobtype == "PROFILE_RUN") {
    jobShort <- "PROF"
  } else if (jobtype == "GENERIC") {
    jobShort <- "GEN"
  } else {
    jobShort <- jobtype
  }

  jobHome <-
    file.path(
      shared_directory,
      paste0(jobShort, format(Sys.time(), "%y%m%d%H%M%OS2"))
    )
  assign("jobHome", gsub("\\", "/", jobHome, fixed = TRUE),
    envir = nlmeEnv
  )
  listWDs <- list.dirs(jobHome, recursive = FALSE, full.names = FALSE)
  if (length(listWDs) > 0) {
    SharedWorkingDir <- file.path(
      jobHome,
      paste0("Shared_", length(listWDs))
    )
  } else {
    SharedWorkingDir <- file.path(
      jobHome,
      "Shared"
    )
  }

  SharedWorkingDir <- normalizePath(SharedWorkingDir, winslash = "/", mustWork = FALSE)
  assign("SharedWorkingDir", SharedWorkingDir, envir = nlmeEnv)
  all(dir.create(SharedWorkingDir, showWarnings = FALSE, recursive = TRUE))
}
