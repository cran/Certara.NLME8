nlmeEnv <- new.env()


getBatchDirectoryLocation <- function(SharedWorkingDir) {
  parallelMethod <- get("parallelMethod", envir = nlmeEnv)

  if (tolower(parallelMethod) %in% c("multicore", "none", "local_mpi", "mpi")) {
    return(SharedWorkingDir)
  } else {
    return(file.path(SharedWorkingDir, "registry"))
  }
}

shortModelName <- function() {
  if (!exists("workflow_name", envir = nlmeEnv)) {
    return("")
  }

  workflow_name <-
    gsub("[^A-Za-z0-9_]", "", get("workflow_name", envir = nlmeEnv))

  return(strtrim(workflow_name, 30))
}



CheckUserCommands <- function() {
  try({
    localWorkingDir <- get("localWorkingDir", envir = nlmeEnv)
    fileName <- file.path(localWorkingDir, "nlme.cmd")
    if (file.exists(fileName)) {
      lines <-
        scan(fileName,
             what = character(),
             sep = "\n",
             quiet = TRUE)
      a <- grep("STOP", lines)
      if (length(a) != 0) {
        message("------------------- We are Canceled------------------")
        return("Canceled")
      }
      a <- grep("RESULTS", lines)
      if (length(a) != 0) {
        message("------------------- Generate Interim Results ------------------")
        return("ReturnResults")
      }
      a <- grep("QUIT", lines)
      if (length(a) != 0) {
        message("------------------- Early Termination ------------------")
        return("EarlyTermination")
      }
    }
  },
  silent = TRUE)

  return("")
}



WriteResultsStatusFile <- function(msg) {
  localWorkingDir <- get("localWorkingDir", envir = nlmeEnv)
  fileName <- file.path(localWorkingDir, "nlme_cmd.status")
  try(
    cat(msg,
        file = fileName,
        sep = "\n",
        append = FALSE),
  silent = TRUE)
}


IsJobCanceled <- function() {
  stat <- CheckUserCommands()
  if (stat == "Canceled") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

InterimResultsRequested <- function() {
  stat <- CheckUserCommands()
  if (stat == "ReturnResults") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

IsEarlyTerminationRequested <- function() {
  stat <- CheckUserCommands()
  if (stat == "EarlyTermination") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

RemoveUserCommands <- function() {
  localWorkingDir <- get("localWorkingDir", envir = nlmeEnv)
  fileName <- file.path(localWorkingDir, "nlme.cmd")
  if (file.exists(fileName)) {
    file.remove(fileName)
  }
}

runNLMEBootstrapSample <-
  function(indx, seed, SharedWorkingDir = "") {
    runNLMESample(
      indx = indx,
      ofn = "out.txt",
      seed = seed,
      SharedWorkingDir = SharedWorkingDir
    )
  }

runNLMEInitialSample <- function(indx, SharedWorkingDir = "") {
  runNLMESample(indx, ofn = "out.txt", SharedWorkingDir = SharedWorkingDir)
}

generateFrozenModelFile <- function(origModelFilename,
                                    frozenModelFilename,
                                    fixEffName,
                                    frozenValue) {
  localWorkingDir <- get("localWorkingDir", envir = nlmeEnv)
  lines <-
    readLines(file.path(localWorkingDir, origModelFilename), warn = FALSE)
  frozenModelFilename <-
    file.path(localWorkingDir, frozenModelFilename)
  cat_filesWarnLong(lines,
                    file = frozenModelFilename,
                    sep = "\n",
                    append = FALSE)
  modelName <-
    unlist(strsplit(lines[1], split = "(", fixed = TRUE))[1]
  overrrideText <-
    paste(
      paste0("override ", modelName, "(){"),
      paste0("fixef(", fixEffName, "(freeze) = c(,", frozenValue, ",))"),
      paste0("}"),
      sep = "\n"
    )

  cat(overrrideText,
      file = frozenModelFilename,
      sep = "\n",
      append = TRUE)
}

# Return the factor used to determine mpi num cores
getNlmePopulationFactor <- function() {
  NLME_POPULATION_FACTOR <- 3
  userValue <- Sys.getenv("NLME_POPULATION_FACTOR")
  if (userValue != "") {
    NLME_POPULATION_FACTOR <- as.integer(userValue)
  }
  if (NLME_POPULATION_FACTOR < 1) {
    NLME_POPULATION_FACTOR <- 3
  }
  return(NLME_POPULATION_FACTOR)
}

mpiAutoNumCores <- function(num_samples, smallestPopulation) {
  NLME_POPULATION_FACTOR <- getNlmePopulationFactor()

  num_processes <- get("num_processes", envir = nlmeEnv)
  extraCores <- as.integer(num_processes / num_samples)
  if (extraCores < 1) {
    extraCores <- 1
  }

  userValue <- Sys.getenv("NLME_POP_MPI_FACTOR")
  if (userValue != "") {
    NLME_POP_MPI_FACTOR <- as.integer(userValue)
    extraCoresAllowed <- num_processes / NLME_POP_MPI_FACTOR
    extraCores <- as.integer(num_processes / num_samples)
    if (extraCores < 1) {
      extraCores <- 1
    }
    if (extraCoresAllowed > extraCores) {
      extraCores <- extraCoresAllowed
    }
  }

  if (extraCores > 1) {
    guessAtBestNo <- smallestPopulation / NLME_POPULATION_FACTOR
    if (extraCores > guessAtBestNo) {
      extraCores <- guessAtBestNo
    }
    extraCores <- as.integer(extraCores)
    if (extraCores <= 0) {
      extraCores <- 1
    }
  }
  return(extraCores)
}


figureOutMpiNumCores <- function(num_samples) {
  column_def_file <- get("column_def_file", envir = nlmeEnv)
  data_file <- get("data_file", envir = nlmeEnv)
  localWorkingDir <- get("localWorkingDir", envir = nlmeEnv)

  smallestPopulation <-
    getNumSubjects(
      file.path(localWorkingDir, column_def_file),
      file.path(localWorkingDir, data_file)
    )

  num_cores <- mpiAutoNumCores(num_samples, smallestPopulation)
  return(num_cores)
}

figureOutMpiNumCoresForPop <-
  function(num_samples, control_file, localDir) {
    smallestPopulation <- getMinimumNumSubjects(control_file, localDir)

    num_cores <- mpiAutoNumCores(num_samples, smallestPopulation)

    return(num_cores)
  }

requestStopEarly <- function(sharedWorkingDir, jobId) {
  baseJobDirectory <- getBatchDirectoryLocation(SharedWorkingDir)
  jobId <- 1
  baseIndx <- jobId
  stopFile <-
    sprintf("%s/jobs/%02d/%d/stop.txt",
            baseJobDirectory,
            baseIndx,
            jobId)
  if (file.exists(sprintf("%s/jobs/%02d/%d", baseJobDirectory, baseIndx, jobId))) {
    cat("STOP",
        file = stopFile,
        sep = "\n",
        append = FALSE)
  }
}

ResetMPIFlags <- function(dir, numCores) {
  if (numCores == 1) {
    MpiNumCores <- 1
    MpiArgument <- "MPINO"
    MpiExecutable <- "NLME7.exe"
  } else {
    MpiNumCores <- numCores
    MpiArgument <- "MPIYES"
    MpiExecutable <- "mpiNLME7.exe"
  }
  assign("MpiArgument", MpiArgument, envir = nlmeEnv)
  assign("MpiExecutable", MpiExecutable, envir = nlmeEnv)
  assign("MpiNumCores", MpiNumCores, envir = nlmeEnv)
  writeOutGlobals(file.path(dir, "myglobaldefs.r", fsep = "/"))
}

prepare_ClusterRegistry <-
  function(SharedWorkingDir,
           parallelMethod,
           localWorkingDir) {
    installationDirectory <- Sys.getenv("INSTALLDIR")
    registryDir <- getBatchDirectoryLocation(SharedWorkingDir)
    gridRegistry <-
      batchtools::makeRegistry(file.dir = registryDir, work.dir = SharedWorkingDir)
    if (grepl("torque", parallelMethod, ignore.case = TRUE)) {
      gridRegistry$cluster.functions <-
        batchtools::makeClusterFunctionsTORQUE(template = file.path(installationDirectory, "batchtools.torque.tmpl"))
    } else if (grepl("sge", parallelMethod, ignore.case = TRUE)) {
      gridRegistry$cluster.functions <-
        batchtools::makeClusterFunctionsSGE(template = file.path(installationDirectory, "batchtools.sge.tmpl"))
    } else if (grepl("slurm", parallelMethod, ignore.case = TRUE)) {
      # slurm
      gridRegistry$cluster.functions <-
        batchtools::makeClusterFunctionsSlurm(template = file.path(installationDirectory, "batchtools.slurm.tmpl"))
    } else if (grepl("lsf", parallelMethod, ignore.case = TRUE)) {
      # LSF
      gridRegistry$cluster.functions <-
        batchtools::makeClusterFunctionsLSF(template = file.path(installationDirectory, "batchtools.lsf.tmpl"))
    } else {
      stop("Parallel method ", parallelMethod, " is not  appropriate one.")
    }

    gridRegistry
  }

# Copy results of an NLME run from grid-shared-directory back to user's
# local run directory
copyResults <-
  function(dirToCopyTo) {
    SharedWorkingDir <- get("SharedWorkingDir", envir = nlmeEnv)
    tryCatch({
      expFiles <-
        list.files(dirname(SharedWorkingDir),
                   pattern = "*.*",
                   all.files = TRUE)
      copy_filesWarnLong(file.path(dirname(SharedWorkingDir), expFiles),
                         dirToCopyTo,
                         overwrite = TRUE)

      # Grab the status files so we can report timing
      num_samples <-
        as.integer(get("num_samples", envir = nlmeEnv))
      copy_filesWarnLong(file.path(
        SharedWorkingDir,
        sprintf("S_%03d.status", 1:num_samples)
      ),
      dirToCopyTo,
      overwrite = TRUE)
    },
    error = function(ex) {
      warning("ERROR during results copying: ", ex)
    })
  }

killAGridJob <- function(gridRegistry) {
  tryCatch({
    notDone <- batchtools::findNotDone(reg = gridRegistry)
    if (nrow(notDone) > 0) {
      notDone <- notDone$job.id
      batchtools::killJobs(reg = gridRegistry, ids = notDone)
      notDone <- batchtools::findNotDone(reg = gridRegistry)
    }
  },
  error = function(ex) {
    return("")
  })
}

grabDoneReplicates <- function(status) {
  done <- c()
  for (jobId in seq_along(status)) {
    if (status[jobId] %in% c("SUCCES", "FAILED")) {
      done <- c(done, jobId)
    }
  }
  return(done)
}

# Create the jobs directory
createJobsDirectory <- function(SharedWorkingDir, num_samples) {
  # dir.create(file.path(SharedWorkingDir, "jobs"), recursive = TRUE, showWarnings = FALSE)
  for (indx in 1:num_samples) {
    baseIndx <- indx %% 100
    workingDir <-
      sprintf("%s/jobs/%02d/%d/", SharedWorkingDir, baseIndx, indx)
    if (!dir.create(workingDir,
                    recursive = TRUE,
                    showWarnings = FALSE)) {
      errMsg <-
        paste0("Cannot create directory for execution: ", workingDir)
      if (.Platform$OS.type == "windows" &&
          nchar(workingDir) > 254) {
        errMsg <- paste(
          errMsg,
          "\nPossible reason: Current directory path is too long.",
          "Consider to shrink the path for execution."
        )
      }

      stop(errMsg)
    }
  }
}

# remove residuals from dmp.txt
shrinkDmpDotTxt <- function(fname) {
  dmpLines <- readLines(fname)
  dmpLines <- .remove_dmptxtParts(partName = "residuals", dmpLines)
  dmpLines <- .remove_dmptxtParts(partName = "posthoc", dmpLines)
  dmpLines <- gsub("(=\\W-*nan)|(=\\W-*1.#IND)", "= NA", dmpLines)

  paste(dmpLines, collapse = '\n')
}

.remove_dmptxtParts <- function(partName, dmpLines) {
  partNameQuoted <- paste0("\"", partName, "\"")
  partStart <- grep(paste(partNameQuoted, "="), dmpLines)
  if (length(partStart) != 0) {
    partEnd <- grep(paste("end of", partName), dmpLines)
    namesStart <- grep(" names =", dmpLines)
    dmpLines[namesStart] <-
      gsub(paste(",", partNameQuoted), "", dmpLines[namesStart], fixed = TRUE)
    dmpLines <- dmpLines[-c(partStart:partEnd)]
  }

  dmpLines
}

reformatResidualsFile <- function(outFilename, localWorkingDir) {
  residualFilename <- file.path(localWorkingDir, "res.csv")
  lines <- readLines(outFilename)
  b <- grep("residuals", lines)
  lines[b + 1] <-
    gsub("  ID1  ID2  ID3  ID4  ID5",
         "ID1 \t ID2 \t ID3 \t ID4 \t ID5",
         lines[b + 1],
         fixed = TRUE)
  cat(lines[(b + 1):length(lines)],
      file = residualFilename,
      sep = "\n",
      append = FALSE)
  return(residualFilename)
}

# All jobs will create a S_00n.status file in shared directory.  File will contain any of :
#
# SUCCESS | FAILED | RUNNING
# as the 1st line
hangAroundABitForTheStatusFile <- function(SharedWorkingDir, done) {
  numTimesChecked <- 0
  maximumNumTimesToCheck <- 100
  for (job in done) {
    while (numTimesChecked < maximumNumTimesToCheck) {
      statusFileIsPresent <- FALSE
      tryCatch({
        statusFile <-
          file.path(SharedWorkingDir, sprintf("S_%03d.status", job))
        if (file.exists(statusFile)) {
          lines <- readLines(statusFile)
          if (grepl("FAILED|SUCCESS", lines[1])) {
            numTimesChecked <- 9999
            break
          }
        }
      },
      error = function(ex) {
        Sys.sleep(1)
      })
      numTimesChecked <- numTimesChecked + 1
      Sys.sleep(5)
    }
  }
}

collectJobStatusAndCoreFiles <- function(job, files, localDir) {
  SharedWorkingDir <- get("SharedWorkingDir", envir = nlmeEnv)

  # Return files from individual runs.
  wd <- file.path(
    getBatchDirectoryLocation(SharedWorkingDir),
    "jobs",
    sprintf("%02d", job %% 100),
    job
  )

  # Files that get copied back
  pattern <- paste0("(",
                    unlist(strsplit(files, split = " ")),
                    "$)",
                    collapse = "|")
  expFiles <- list.files(wd, pattern = pattern, all.files = TRUE)
  toFile <- gsub(".txt.*", ".txt", expFiles)
  copy_filesWarnLong(file.path(wd, expFiles), file.path(localDir, toFile))
}


# Get the list of files that is needed to be copied back from remote disk
getGenericResultsList <- function(control_lines) {
  files_to_return <- get("files_to_return", envir = nlmeEnv)
  file_list <- c("progress.xml",
                 unlist(strsplit(files_to_return, " ")))

  control_linesSplitted <- strsplit(control_lines, split = ",")
  sapply_strsplit <- function(i) {
    c(unlist(strsplit(
      control_linesSplitted[[i]][5],
      split = " +",
      perl = TRUE
    )),
    paste0(unlist(
      strsplit(
        control_linesSplitted[[i]][6],
        split = " +",
        perl = TRUE
      )
    ), ".Job", i))
  }

  file_list <- c(file_list,
                 sapply(seq_along(control_lines), sapply_strsplit))

  file_list <- unique(file_list)
  file_list
}

# Grab the extra argument file name ( i.e. nlmeargs.txt ) out of a line
getExtraArgumentFilename <- function(line) {
  fileRec <- unlist(strsplit(line, split = ","))[2]
  # To handle windows full paths, allow for drive specification
  tokens <- unlist(strsplit(fileRec, split = ":"))
  if (length(tokens) > 2) {
    file <- paste0(tokens[1], ":", tokens[2])
  } else {
    file <- tokens[1]
  }

  file
}

getScenarioName <- function(control_lines_given) {
  names <-
    sapply(strsplit(c(control_lines_given), split = ","),
           function(x) {
             if (x[1] == "")
               " "
             else
               x[1]
           })
  names
}

getExtraArgumentFilenameIndex <- function(line) {
  fileRec <- unlist(strsplit(line, split = ","))[2]
  tokens <- unlist(strsplit(fileRec, split = ":"))
  if (length(tokens) > 2) {
    fileIndex <- tokens[3]
  } else {
    fileIndex <- tokens[2]
  }

  fileIndex
}

get_ReturnedFilesPattern <- function(control_lines_given) {
  patternsToEsclude <-
    sapply(strsplit(c(control_lines_given), split = ","), function(x)
      x[3])
  patternsToEsclude
}


getRunSuccessFilename <- function(control_lines_given) {
  files <-
    sapply(strsplit(c(control_lines_given), split = ","), function(x)
      x[4])
  files
}


getOutputFilenames <- function(control_lines_given) {
  files <-
    sapply(strsplit(c(control_lines_given), split = ","), function(x)
      x[5])
  files
}

# # used in shotgun to get progress.txt
# getOutputFilenames2 <- function(control_lines_given) {
#   files <-
#     sapply(strsplit(c(control_lines_given), split = ","), function(x)
#       x[6])
#   files
# }



getExePostfix <- function(line) {
  exePostfix <-
    gsub("^\\s+|\\s+$", "", (unlist(strsplit(line, split = ","))[7]))
  if (is.na(exePostfix)) {
    exePostfix <- ""
  }
  exePostfix
}

checkJobStatus <- function(SharedWorkingDir, Job) {
  statusFile <-
    file.path(SharedWorkingDir, sprintf("S_%03d.status", Job))
  statusFileLines <- readLines(statusFile)

  if (length(grep("FAILED", statusFileLines)) > 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

.StdErrorComputation <- function(array) {
  meanarray <- mean(array)
  return(sqrt(sum(array ^ 2) / length(array) - meanarray * meanarray))
}

getCovariateNames <- function(modelFileName) {
  tempDir <- tempfile("NLME")
  dir.create(tempDir, showWarnings = FALSE, recursive = TRUE)
  installdir <- path.expand(Sys.getenv("INSTALLDIR"))

  NLME_HASH <- as.integer(Sys.getenv("NLME_HASH"))
  if (!is.na(NLME_HASH) && NLME_HASH > 0) {
    NLME_HASH <- paste("-hash", NLME_HASH)
  } else {
    NLME_HASH <- character(0)
  }

  TDL5_run <- system2(
    file.path(installdir, "TDL5"),
    args = c(
      NLME_HASH,
      "-i",
      shQuote(modelFileName, type = "cmd"),
      shQuote(tempDir, type = "cmd")
    ),
    stdout = TRUE,
    stderr = TRUE
  )

  ModelInfo <-
    readLines(file.path(tempDir, "ModelInfo.txt"), warn = FALSE)
  allCovariates <-
    unlist(regmatches(
      ModelInfo,
      gregexpr("(?<=covariates )[ a-zA-Z\\d_()]+(?=\\))",
               ModelInfo,
               perl = TRUE)
    ))

  if (length(allCovariates) > 0) {
    covariatesArray <-
      unlist(strsplit(allCovariates, split = " ", fixed = TRUE))
    covariates <- grepl(")", covariatesArray, fixed = TRUE)
    names(covariates) <- gsub("\\(|\\)", "", covariatesArray)
  } else {
    covariates <- c()
  }

  return(covariates)
}


figureOutDmpFileLocation <-
  function(job,
           jobsBaseDirectory) {
    localWorkingDir <- get("localWorkingDir", envir = nlmeEnv)

    rDumpFile <- "dmp.txt"

    jobBaseIndx <- job %% 100
    rDumpFile <-
      file.path(jobsBaseDirectory,
                "jobs",
                sprintf("%02d", jobBaseIndx),
                job,
                rDumpFile)


    return(rDumpFile)
  }

# Given a filename:recordno
# read the lines
readNlmeArgsFile <- function(filespec, localWorkingDir) {
  tokens <- unlist(strsplit(filespec, ":"))
  if (length(tokens) > 2) {
    filename <- paste0(tokens[1], ":", tokens[2])
    record <- as.integer(tokens[3])
  } else {
    filename <- tokens[1]
    record <- as.integer(tokens[2])
  }

  if (dirname(filename) == ".") {
    allLines <- readLines(paste0(localWorkingDir, "/", filename))
  } else {
    allLines <- readLines(filename)
  }
  numRecords <- length(grep("/o", allLines, fixed = TRUE))
  numArgsLines <- length(allLines) / numRecords
  startRec <- numArgsLines * (record - 1) + 1
  lines <- c()
  for (l in startRec:(startRec + numArgsLines - 1)) {
    lines <- c(lines, allLines[l])
  }

  lines
}


removeTempWorkingDirectory <- function(jobHome) {
  if (Sys.getenv("NLME_KEEP_GRID_RESULTS") != "TRUE") {
    for (t in 1:3) {
      tryCatch({
        existedjobHome <- dir.exists(jobHome)
        if (any(existedjobHome)) {
          ret <-
            unlink(jobHome[existedjobHome], recursive = TRUE, force = TRUE)
        } else {
          break
        }
      },
      error = function(ex) {
        Sys.sleep(5)
        existedjobHome <- dir.exists(jobHome)
        if (any(existedjobHome)) {
          ret <-
            unlink(jobHome[existedjobHome], recursive = TRUE, force = TRUE)
        }
      })
    }
  }
}

waitTillAllJobsAreFinished <- function() {
  gridRegistry <- get("gridRegistry", envir = nlmeEnv)
  stat <- batchtools::waitForJobs(reg = gridRegistry)
  running <- batchtools::findRunning(ids = NULL, reg = gridRegistry)
  expired <- batchtools::findExpired(ids = NULL, reg = gridRegistry)
  errors <- batchtools::findErrors(ids = NULL, gridRegistry)
  if (nrow(running) > 0) {
    running <- running$job.id
  }
  flag <- Sys.getenv("NLME_NO_ZOMBIE_CLEANUP")
  if (flag != "TRUE") {
    jobsStr <- ""
    if (exists("gridJobs", envir = nlmeEnv)) {
      gridJobs <- get("gridJobs", envir = nlmeEnv)

      for (j in gridJobs) {
        jobsStr <- paste(jobsStr, j)
      }
    }
    if (exists("gridRegistry2", envir = nlmeEnv)) {
      gridJobs2 <- get("gridJobs2", envir = nlmeEnv)
      for (j in gridJobs2) {
        jobsStr <- paste(jobsStr, j)
      }
    }
    if (jobsStr != "") {
      fileName <- "clean_zombies.sh"
      clean_zombies <-
        paste0(
          "for j in ",
          jobsStr,
          "\ndo",
          "\n\tret=`qstat -j ${j} 2>/dev/null`",
          "\n\tif [ \"${ret}\" ]",
          "\n\tthen",
          "\n\t\tqdel ${j} 1>/dev/null 2>/dev/null",
          "\n\tfi",
          "\ndone"
        )

      cat(clean_zombies,
          file = fileName,
          sep = "\n",
          append = FALSE)

      system(sprintf("chmod 777 ./%s ", fileName))
      system(sprintf("./%s  ", fileName))
    }
    if (jobsStr != "") {
      command <- sprintf("rm  %s", fileName)
      system(command)
    }
  }
}


#
# Get the number of subjects in a dataset
#
getNumSubjects <- function(colDefFile, dataFile, localDir = "") {
  if (localDir != "") {
    colDefFile <- file.path(localDir, colDefFile)
    dataFile <- file.path(localDir, dataFile)
  }
  lines <- readLines(colDefFile)
  lineNo <- grep("^\\W*id\\W*\\(.*)", lines)
  numSubjects <- 0
  if (length(lineNo) == 1) {
    IDcol <-
      unlist(
        strsplit(
          lines[lineNo],
          "(((^\\W*id\\W*\\(\\W*))*|([\"\']\\W*,\\W*)*)[\"\']\\W*\\)*"
        )
      )
    IDcol <- IDcol[IDcol != ""]
    if (length(IDcol) != 0) {
      data <- read.csv(dataFile, check.names = FALSE)
      colnames(data)[1] <-
        gsub("^##", "", colnames(data)[1], fixed = FALSE)
      cols <- colnames(data)
      IDcolsNotFound <- setdiff(IDcol, cols)
      if (length(IDcolsNotFound) > 0) {
        warning(
          "ID column(s) ",
          paste(IDcolsNotFound, collapse = ", "),
          "\n mapped in cols1 not found in data column names.",
          call. = FALSE
        )
      } else {
        numSubjects <- nrow(unique(data[IDcol]))
      }
    }
  } else if (length(lineNo) > 1) {
    warning("More than one line with id map  found in ", colDefFile, call. = FALSE)
  } else if (length(lineNo) == 0L) {
    warning("No lines with id map found in ", colDefFile, call. = FALSE)
  }
  return(numSubjects)
}

#
# Get the smallest population in a run
#
getMinimumNumSubjects <- function(controlFile, localDir) {
  if (dirname(controlFile) == ".") {
    lines <- readLines(paste0(localDir, "/", controlFile))
  } else {
    lines <- readLines(controlFile)
  }
  numReplicates <- as.integer(lines[4])
  smallestPopulation <- 9999999
  for (n in 1:numReplicates) {
    extraArgsFile <- getExtraArgumentFilename(lines[n + 4])
    extraArgsFileIndx <- getExtraArgumentFilenameIndex(lines[n + 4])
    idx <- as.integer(extraArgsFileIndx)
    if (dirname(extraArgsFile) == ".") {
      extraArgsFile <- file.path(localDir, extraArgsFile)
    }

    extraLines <- readLines(extraArgsFile, warn = FALSE)
    numLines <- length(extraLines)
    numLinesPerRecord <-
      numLines / length(grep("-anagrad", extraLines))
    colnameFile <- "cols1.txt"
    dataFilename <- "data1.txt"
    for (l in 1:numLinesPerRecord) {
      isThere <-
        grep("data1", extraLines[(idx - 1) * numLinesPerRecord + l])
      if (length(isThere) == 1) {
        tokens <-
          unlist(strsplit(extraLines[(idx - 1) * numLinesPerRecord + l], " "))
        for (t in tokens) {
          if (length(grep("data1", t)) != 0) {
            dataFilename <- t
          }
        }
      }
    }
    num <- getNumSubjects(colnameFile, dataFilename, localDir)
    if (num < smallestPopulation) {
      smallestPopulation <- num
    }
  }
  return(smallestPopulation)
}




getScenarioKey <- function(outputFilename) {
  first <- unlist(strsplit(outputFilename, "\\."))[1]
  key <- unlist(strsplit(first, split = "out"))[2]
  return(key)
}




#' Table names from the column definition file
#'
#' Extracts table names from the column definition file
#'
#' @param columnDefinitionFilename path to NLME column definition file
#' @param simtbl logical. \code{TRUE} extracts simulation tables,
#' \code{FALSE} extracts simple tables.
#'
#' @return vector of names of the tables in column definition file if any,
#' empty string otherwise
#'
#' @examples
#' \dontrun{
#' getTableNames("cols1.txt", simtbl = TRUE)
#' }
#'
#' @export
getTableNames <-
  function(columnDefinitionFilename, simtbl = FALSE) {
    if (simtbl) {
      tableName <- "simtbl"
    } else {
      tableName <- "table"
    }

    TableNames <- ""
    cols1Text <- readLines(columnDefinitionFilename)
    indxs <-
      grep(paste0("\\W*", tableName, "\\W*\\(\\W*file\\W*="),
           cols1Text,
           perl = TRUE)
    for (i in indxs) {
      # Parse the filename out
      splittedTableRow <- unlist(strsplit(cols1Text[i], "\"|\'"))
      if (length(splittedTableRow) == 3) {
        TableNames <- paste(TableNames, splittedTableRow[2])
      }
    }
    return(TableNames)
  }



cleanupFromPreviousRun <- function() {
  profileDescriptors <- ""
  assign("profileDescriptors", profileDescriptors, envir = nlmeEnv)
}



nlmeEnvIsDone <- function(e) {
  if (exists("jobHomeDirectories", envir = nlmeEnv)) {
    jobHomeDirectories <- get("jobHomeDirectories", envir = nlmeEnv)
  } else {
    jobHomeDirectories <- NULL
  }
  for (jh in jobHomeDirectories) {
    removeTempWorkingDirectory(jh)
  }
  rm(list = ls(envir = nlmeEnv), envir = nlmeEnv)
}


getLocalTimeUTC <- function() {
  return(c(format(
    as.POSIXlt(Sys.time(), "UTC"), "%b %Y %d %X"
  )))
}

report_filesToGenerate <-
  function(filesToGenerate, ReturnedFilesPattern) {
    filesPresent <-
      filesToGenerate[grepl(ReturnedFilesPattern, filesToGenerate)]
    GlobalSummaryLine2 <-
      paste("Generating", filesPresent, collapse = "\n ")
    assign("GlobalSummaryLine2", GlobalSummaryLine2, envir = nlmeEnv)
    UpdateProgressMessages()
  }

check_Arguments <- function(RequiredArgs, Args, FunctionName) {
  StopMessage <- paste0(
    "\nThere are 14 arguments required for run, but ",
    length(Args),
    " presented. comparing required vs presented args:"
  )

  for (i in 1:length(RequiredArgs)) {
    if (is.null(Args[i])) {
      Args[i] <- "N/A"
    }

    StopMessage <-
      paste0(StopMessage, "\n", RequiredArgs[i], " = ", Args[i])
  }

  stop("Please check the arguments for ", FunctionName, ":\n", StopMessage)
}
