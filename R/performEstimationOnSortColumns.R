#' Sort specification for multiple estimations
#'
#' Runs multiple estimations sorting the input dataset by
#' requested columns and creating multiple data sets
#'
#' @param args a vector of arguments provided as the following:
#' c(method, install_directory, shared_directory, localWorkingDir,
#'  nlmeArgsFile, numColumns, ColumnNames, NumProc, workflowName)
#' @param reportProgress whether it is required to report the progress
#' (for local jobs usually)
#' @return Directory path where NLME job was executed
#' @keywords NLME
#' @export
performEstimationOnSortColumns <-
  function(args, reportProgress = FALSE) {
    requiredargs <-
      c(
        "method",
        "install_directory",
        "shared_directory",
        "localWorkingDir",
        "nlmeArgsFile",
        "numColumns",
        "ColumnNames",
        "NumProc",
        "workflowName"
      )

    if (length(args) < 9) {
      check_Arguments(requiredargs, args, "performEstimationOnSortColumns")
    }

    parallelMethod <- args[1]
    installDir <- args[2]
    sharedDir <- args[3]
    localDir <- args[4]
    nlmeArgsFilename <- args[5]
    numColumns <- args[6]
    columnNamesArray <- args[7]
    numCores <- args[8]
    workflow_name <- args[9]
    if (length(args) > 9) {
      assign("fixefUnits", args[10], envir = nlmeEnv)
    }

    localWorkingDir <- gsub("\\", "/", localDir, fixed = TRUE)
    assign("localWorkingDir", localWorkingDir, envir = nlmeEnv)
    dir.create(localWorkingDir,
               showWarnings = FALSE,
               recursive = TRUE)
    updateInitialStatus("SortByColumn", parallelMethod, localWorkingDir)

    tryCatch({
      controlFilename <-
        file.path(localWorkingDir, "NewnlmeControlFile.txt")
      # Create multiple datasets based on sort column(s)
      assign("profileDescriptors", "", envir = nlmeEnv)
      jobType <- "ESTIMATION_RUN"
      numDatasets <- sortByColumnAndGenerateControlFile(
        "data1.txt",
        numColumns,
        columnNamesArray,
        nlmeArgsFilename,
        controlFilename,
        jobType = jobType
      )
      argList <- c(
        jobType,
        parallelMethod,
        installDir,
        sharedDir,
        localDir,
        controlFilename,
        numCores,
        workflow_name
      )

      performParallelNLMERun(
        argList,
        partialJob = FALSE,
        allowIntermediateResults = TRUE,
        progressStage = "Estimation",
        reportProgress = reportProgress
      )
    },
    error = function(ex) {
      warning(ex)
      FailProgress()
    })
  }
