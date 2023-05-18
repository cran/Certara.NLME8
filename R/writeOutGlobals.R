#
# Write out globals and routines that are needed on the remote node
writeOutGlobals <- function(name) {
  exeFileExtension <- get("exeFileExtension", envir = nlmeEnv)
  parallelMethod <- get("parallelMethod", envir = nlmeEnv)
  jobType <- get("jobType", envir = nlmeEnv)
  num_samples <- as.integer(get("num_samples", envir = nlmeEnv))
  num_processes <- as.integer(get("num_processes", envir = nlmeEnv))
  localWorkingDir <- get("localWorkingDir", envir = nlmeEnv)
  SharedWorkingDir <- get("SharedWorkingDir", envir = nlmeEnv)
  files_to_copy <- get("files_to_copy", envir = nlmeEnv)
  model_file <- get("model_file", envir = nlmeEnv)
  extra_args_file <- get("extra_args_file", envir = nlmeEnv)
  MpiExecutable <- get("MpiExecutable", envir = nlmeEnv)
  MpiArgument <- get("MpiArgument", envir = nlmeEnv)
  MpiNumCores <- get("MpiNumCores", envir = nlmeEnv)
  MpiLocal <- get("MpiLocal", envir = nlmeEnv)


  cat_filesWarnLong(paste0(
    " jobType <-", shQuote(jobType),
    "\n num_samples <- ", num_samples,
    "\n exeFileExtension <- ", shQuote(exeFileExtension),
    "\n parallelMethod <- ", shQuote(parallelMethod),
    "\n num_processes <- ", num_processes,
    "\n localWorkingDir <- ", shQuote(path.expand(localWorkingDir)),
    "\n SharedWorkingDir <- ", shQuote(path.expand(SharedWorkingDir)),
    "\n files_to_copy <- ", shQuote(files_to_copy),
    "\n model_file <- ", shQuote(model_file),
    "\n extra_args_file <- ", shQuote(extra_args_file),
    "\n MpiExecutable <- ", shQuote(MpiExecutable),
    "\n MpiArgument <- ", shQuote(MpiArgument),
    "\n MpiLocal <- ", shQuote(MpiLocal),
    "\n MpiNumCores <- ", MpiNumCores,
    "\n assign(\"MpiExecutable\", MpiExecutable, envir = nlmeEnv)",
    "\n assign(\"MpiArgument\", MpiArgument, envir = nlmeEnv)",
    "\n assign(\"MpiLocal\", MpiLocal, envir = nlmeEnv)",
    "\n assign(\"MpiNumCores\", MpiNumCores, envir = nlmeEnv)"
  ),
  file = name, sep = "\n", append = FALSE
  )

  if (jobType == "BOOTSTRAP") {
    column_def_file <- get("column_def_file", envir = nlmeEnv)
    data_file <- get("data_file", envir = nlmeEnv)
    num_iterations <- as.integer(get("num_iterations", envir = nlmeEnv))
    max_tries <- as.integer(get("max_tries", envir = nlmeEnv))
    start_seed <- as.integer(get("start_seed", envir = nlmeEnv))
    confidence_level <- get("confidence_level", envir = nlmeEnv)

    cat(paste0(
      "\n column_def_file <- ", shQuote(column_def_file),
      "\n data_file <- ", shQuote(data_file),
      "\n num_iterations <- ", num_iterations,
      "\n max_tries <- ", max_tries,
      "\n start_seed <- ", start_seed,
      "\n confidence_level <- ", confidence_level
    ),
    file = name, sep = "\n", append = TRUE
    )
  } else {
    control_lines <- get("control_lines", envir = nlmeEnv)

    cat(paste0(
      "\n control_lines <- c('",
      paste(control_lines, collapse = "' ,'"), "')"
    ),
    file = name, sep = "\n", append = TRUE
    )
  }

  # insert runNLMESample
  write_function_to_file(
    quote(runNLMESample),
    paste0(
      "runNLMESample <- ",
      "function(indx, eArgsFile, ofn, extraArgs=\"\", seed=-1, max_tries=1, exePostfix = \"\", SharedWorkingDir = SharedWorkingDir)"
    ),
    name
  )

  # insert getExtraArgumentFilename
  write_function_to_file(
    quote(getExtraArgumentFilename),
    paste0("getExtraArgumentFilename<- function(line)"),
    name
  )

  # insert getRunSuccessFilename
  write_function_to_file(
    quote(getRunSuccessFilename),
    paste0("getRunSuccessFilename<- function(control_lines_given)"),
    name
  )

  # insert getExtraArgumentFilenameIndex
  write_function_to_file(
    quote(getExtraArgumentFilenameIndex),
    paste0("getExtraArgumentFilenameIndex<- function(line)"),
    name
  )


  # insert getExePostfix
  write_function_to_file(
    quote(getExePostfix),
    paste0("getExePostfix<- function(line)"),
    name
  )
}
