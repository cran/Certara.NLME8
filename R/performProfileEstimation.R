#' NLME a profile estimation run on list of fixed effects
#'
#' This function runs multiple estimations sorting the input dataset by
#' requested columns and creating multiple data sets
#' Runs are also generated for all profiling variables
#'
#' @param args Arguments for profile estimation
#' @param reportProgress Set to \code{TRUE} to report progress
#' @return Directory path where NLME job was executed
#' @keywords NLME
#' @export
performProfileEstimation <- function(args, reportProgress = FALSE) {
  requiredargs <-
    c(
      "method",
      "install_directory",
      "shared_directory",
      "localWorkingDir",
      "nlmeArgsFile",
      "numColumns",
      "ColumnNames",
      "profileDescriptions",
      "profilePercentFlag",
      "NumProc",
      "workflowName"
    )

  if (length(args) != 11) {
    check_Arguments(requiredargs, args, "performProfileEstimation")
  }

  rm(list = ls(envir = nlmeEnv), envir = nlmeEnv)
  assign("reportProgress", reportProgress, envir = nlmeEnv)

  localWorkingDir <- gsub("\\", "/", args[4], fixed = TRUE)
  assign("localWorkingDir", localWorkingDir, envir = nlmeEnv)
  updateInitialStatus("Profile", args[1], localWorkingDir)

  tryCatch({
    controlFilename <-
      file.path(localWorkingDir, "NewnlmeControlFile.txt")
    parallelMethod <- args[1]
    installDir <- args[2]
    sharedDir <- args[3]
    localDir <- args[4]
    nlmeArgsFilename <- args[5]
    numColumns <- args[6]
    columnNamesArray <- args[7]
    profileArray <- args[8]
    profilePercentFlag <- args[9]
    numCores <- args[10]
    workflow_name <- args[11]
    jobType <- "PROFILE_RUN"
    nxtScenario <- 0
    assign("workflow_name", workflow_name, envir = nlmeEnv)

    profileDescriptors <- profileArray
    assign("profileDescriptors", profileDescriptors, envir = nlmeEnv)
    assign("profilePercentFlag", profilePercentFlag, envir = nlmeEnv)
    #
    # Create multiple datasets based on sort column(s)
    #

    numDatasets <- sortByColumnAndGenerateControlFile(
      numColumns,
      columnNamesArray,
      nlmeArgsFilename,
      controlFilename,
      jobType
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

generateProfileModels <- function(jobType) {
  nxt <- 1
  if (jobType == "PROFILE_RUN" &&
      exists("profileDescriptors", envir = nlmeEnv)) {
    profileDescriptors <- get("profileDescriptors", envir = nlmeEnv)
  } else {
    profileDescriptors <- NULL
  }
  profileModels <-
    array(data = list(modelName = "", exePostfix = ""),
          dim = 1)
  if (length(profileDescriptors) < 1 || profileDescriptors == "") {
    modelSpec <-
      list(
        modelName = "test.mdl",
        exePostfix = "",
        theta = "",
        initialValue = "",
        percentage = "",
        delta = ""
      )
    profileModels[[1]] <- modelSpec
    #        profileModels[[2]]=NULL
  } else {
    profilePercentFlag <- get("profilePercentFlag", envir = nlmeEnv)
    for (prof in unlist(strsplit(profileDescriptors, split = " "))) {
      tokens <- unlist(strsplit(prof, split = ","))
      fixEffName <- tokens[1]
      initialValue <- as.double(tokens[2])
      for (indx in 3:length(tokens)) {
        if (profilePercentFlag == "USE_PERCENTAGE") {
          percent <- as.double(tokens[indx])
          delta <- ""
          frozenValue <- initialValue * (1 + percent / 100)
        } else {
          delta <- as.double(tokens[indx])
          percent <- ""
          frozenValue <- initialValue + delta
        }
        frozenKey <- sprintf("%s_%05.2f", fixEffName, frozenValue)
        newModelFilename <- sprintf("test_%s.mdl", frozenKey)
        generateFrozenModelFile("test.mdl", newModelFilename, fixEffName, frozenValue)
        modelSpec <-
          list(
            modelName = newModelFilename,
            exePostfix = frozenKey,
            theta = fixEffName,
            initialValue = frozenValue,
            percentage = percent,
            delta = delta
          )
        profileModels[[nxt]] <- modelSpec
        nxt <- nxt + 1
      }
    }
  }

  assign("profileModels", profileModels, envir = nlmeEnv)
  return(profileModels)
}
