#' @importFrom stats aggregate as.formula cov2cor median na.omit qchisq qt quantile setNames
#' @importFrom utils capture.output head read.csv read.delim read.table tail write.csv write.table
#'
NULL


utils::globalVariables(
  c(
    "SharedWorkingDir",
    "control_lines",
    "dmp.txt" ,
    "exeFileExtension",
    "extra_args_file",
    "findNotDone",
    "flag",
    "gridRegistry",
    "jobType",
    "loadRegistry",
    "localWorkingDir",
    "model_file",
    "num_samples",
    "parallelMethod",
    "partialJob",
    "removeRegistry",
    "sharedWorkingDir",
    "requiredargs"
  )
)
