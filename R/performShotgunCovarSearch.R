#' Shotgun covariate search
#'
#' Runs a set of possible covariate sets in parallel
#'
#' @param args a vector of arguments provided as the following:
#' c(jobType, parallelMethod, install_dir, shared_directory,
#' localWorkingDir, controlFile, NumProc, workflow_name, fixefUnits)
#' @param reportProgress whether it is required to report the progress
#' (for local jobs usually)
#' @return Directory path where NLME job was executed
#' @keywords NLME ShotgunCovariateSearch
#' @export
performShotgunCovarSearch <- function(args, reportProgress = FALSE) {
  if (args[1] != "COVAR_SEARCH") {
    stop(
      "please check the args for performShotgunCovarSearch(),",
      "\n the first one is not valid."
    )
  }

  if (length(args) < 8) {
    check_Arguments(requiredargs, args, "performShotgunCovarSearch")
  }

  performParallelNLMERun(args = args, reportProgress = reportProgress)
}
