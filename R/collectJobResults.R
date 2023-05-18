# copy/concatenate/coallate Bootstrap NLME results file from all runs into one
collectJobResults <- function(done) {
  SharedWorkingDir <- get("SharedWorkingDir", envir = nlmeEnv)
  num_samples <- get("num_samples", envir = nlmeEnv)
  baseDirectory <- getBatchDirectoryLocation(SharedWorkingDir)

  # copying text files
  out.bin.txt.short <- "out.bin.txt"
  out.bin.txt.output <-
    file.path(dirname(SharedWorkingDir), out.bin.txt.short)
  file.create(out.bin.txt.output)
  progress.txt.short <- c("progress.txt")
  progress.txt.output <-
    file.path(dirname(SharedWorkingDir), progress.txt.short)
  out.txt.short <- "out.txt"
  out.txt.output <-
    file.path(dirname(SharedWorkingDir), out.txt.short)

  wd <-
    file.path(baseDirectory, "jobs", sprintf("%02d", done %% 100), done)
  out.bin.txt.input <- file.path(wd, out.bin.txt.short)
  file.append(out.bin.txt.output, out.bin.txt.input)

  progress.txt.input <- file.path(wd, progress.txt.short)
  file.append(progress.txt.output, progress.txt.input)

  out.txt.input <- file.path(wd, out.txt.short)
  for (jobSeq in seq_along(done)) {
    cat(
      sprintf("#  %03d / %03d", done[jobSeq], num_samples),
      file = out.txt.output,
      sep = "\n",
      append = TRUE
    )
    file.append(out.txt.output, out.txt.input[jobSeq])
  }

  csvfiles <- c("BootSubj.csv", "out.csv")
  for (csvfile in csvfiles) {
    first <- TRUE
    outFileName <- file.path(dirname(SharedWorkingDir), csvfile)
    inFileName <- file.path(wd, csvfile)
    for (jobSeq in seq_along(done)) {
      if (file.exists(inFileName[jobSeq])) {
        if (jobSeq == 1) {
          copy_filesWarnLong(inFileName[jobSeq], outFileName, overwrite = TRUE)
        } else {
          cat(
            scan(
              inFileName[jobSeq],
              what = "character",
              skip = 1,
              quiet = TRUE
            ),
            file = outFileName,
            sep = "\n",
            append = TRUE
          )
        }
      }
    }
  }

  copy_filesWarnLong(file.path(SharedWorkingDir, "test.mdl"),
                     file.path(dirname(SharedWorkingDir), "test_new.mdl"))
  if (file.exists(file.path(SharedWorkingDir, "progress.xml"))) {
    copy_filesWarnLong(file.path(SharedWorkingDir, "progress.xml"),
                       dirname(SharedWorkingDir))
  }

  # CLSC-72
  # Copy out.txt from the first job to the file we are returning to the GUI
  files <- "(iniest.csv$)|(dmp.txt$)"
  lastFiles <-
    list.files(wd[jobSeq], files, all.files = TRUE, full.names = TRUE)
  copy_filesWarnLong(lastFiles, file.path(dirname(SharedWorkingDir)), overwrite = TRUE)
}
