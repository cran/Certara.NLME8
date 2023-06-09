% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/performStepwiseCovarSearch.R
\name{performStepwiseCovarSearch}
\alias{performStepwiseCovarSearch}
\title{NLME stepwise covariate search}
\usage{
performStepwiseCovarSearch(args, reportProgress = FALSE)
}
\arguments{
\item{args}{a vector of arguments provided as the following:
c(method, install_directory, shared_directory, localWorkingDir,
 modelFile, nlmeArgsFile, listOfFilesToCopy, numCovariates,
  CovariateNames, NCriteria, addPValue, removePValue, NumProc, workflowName)}

\item{reportProgress}{whether it is required to report the progress
(for local jobs usually)}
}
\value{
Directory path where NLME job was executed
}
\description{
This function runs a stepwise covariate NLME job in parallel
It is designated to be called in commandline (Rscript)
}
\keyword{NLME}
\keyword{StepwiseCovariateSearch}
