# Copy NLME input files into remote-shared-directory
copy_InputFiles <- function(dirToCopyTo) {
  localWorkingDir <- get("localWorkingDir", envir = nlmeEnv)
  exeFileExtension <- get("exeFileExtension", envir = nlmeEnv)

  files_to_copy <- get("files_to_copy", envir = nlmeEnv)
  jobType <- get("jobType", envir = nlmeEnv)
  num_samples <- get("num_samples", envir = nlmeEnv)

  if (jobType != "BOOTSTRAP") {
    control_lines <- get("control_lines", envir = nlmeEnv)
    if (localWorkingDir != dirToCopyTo) {
      for (indx in 1:num_samples) {
        fileToCopy <- getExtraArgumentFilename(control_lines[indx])
        if (dirname(fileToCopy) == ".") {
          fileToCopy <- file.path(localWorkingDir, fileToCopy)
        }

        fileIndex <-
          getExtraArgumentFilenameIndex(control_lines[indx])
        if (is.na(fileIndex) ||
            fileIndex == "1" || num_samples == 1) {
          copy_filesWarnLong(fileToCopy, dirToCopyTo, overwrite = TRUE)
        }
      }
    }
  }

  if (localWorkingDir != dirToCopyTo) {
    copy_filesWarnLong(file.path(localWorkingDir, unlist(strsplit(
      files_to_copy, split = " "
    ))),
    dirToCopyTo,
    overwrite = TRUE)
  }
  writeOutGlobals(file.path(dirToCopyTo, "myglobaldefs.r"))

  if (.Platform$OS.type != "windows") {
    Sys.chmod(list.files(
      path = Sys.getenv("INSTALLDIR"),
      pattern = "*.sh|TDL5$",
      full.names = TRUE,
      recursive = TRUE
    ),
    mode = "777")
  }

  copy_filesWarnLong(file.path(
    Sys.getenv("INSTALLDIR"),
    paste0("execNLMECmd", exeFileExtension)
  ), dirToCopyTo, overwrite = TRUE)
  if (.Platform$OS.type == "windows") {
    copy_filesWarnLong(file.path(Sys.getenv("INSTALLDIR"), "common.ps1"),
                       dirToCopyTo,
                       overwrite = TRUE)
  }
}
