% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkHostParams.R
\name{checkLicenseFile}
\alias{checkLicenseFile}
\title{Checks if NLME run is licensed}
\usage{
checkLicenseFile(installDir, verbose = FALSE, outputGenericInfo = TRUE)
}
\arguments{
\item{installDir}{Directory with NLME executables as specified in
`INSTALLDIR` environment variable.}

\item{verbose}{Flag to output all messages during authorization and
licensing. Default is `FALSE`.}

\item{outputGenericInfo}{Flag to provide TDL5 output when no issues found.
Default is `TRUE`.}
}
\value{
`TRUE` if all checks are successful, otherwise `FALSE`.
}
\description{
Checks if valid license is available for NLME run.
}
\examples{
\dontrun{
checkLicenseFile(Sys.getenv("INSTALLDIR"),
                 verbose = TRUE)
}

}
