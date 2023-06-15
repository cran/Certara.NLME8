checkLinuxGCC <- function() {
  gcc_out <-
    unlist(strsplit(
      system2("gcc", args = "-dumpversion", stdout = TRUE),
      split = ".",
      fixed = TRUE
    ))
  if (as.numeric(gcc_out[1]) > 8) {
    message(
      "gcc version was found on the system: ",
      paste0(gcc_out, collapse = ".")
    )
  } else if (as.numeric(gcc_out[1]) < 8) {
    message(
      "\n gcc version was found on the system: ",
      paste0(gcc_out, collapse = ".")
    )
  }

  return(TRUE)
}

checkWindowsGCC <- function() {
  NLMEGCCDir64 <-
    gsub("\\", "/", Sys.getenv("NLMEGCCDir64"), fixed = TRUE)
  if (!file.exists(file.path(NLMEGCCDir64, "bin", "gcc.exe"))) {
    # common error is to use bin in the path
    # trying to resolve it here
    if (file.exists(file.path(NLMEGCCDir64, "gcc.exe"))) {
      Sys.setenv("NLMEGCCDir64" = dirname(NLMEGCCDir64))
    } else {
      # trying to find gcc
      suppressWarnings(GCCLocation <- system2(
        "where",
        args = "gcc.exe",
        stdout = TRUE,
        stderr = TRUE
      )[1])

      if (!file.exists(GCCLocation)) {
        warning(
          "gcc was not found. ",
          "Please check if NLMEGCCDir64 env.variable is addressing to gcc",
          immediate. = TRUE
        )
        return(FALSE)
      }

      Sys.setenv("NLMEGCCDir64" = dirname(dirname(GCCLocation)))
    }

    NLMEGCCDir64 <-
      gsub("\\", "/", Sys.getenv("NLMEGCCDir64"), fixed = TRUE)
  }

  gccMD5 <-
    tools::md5sum(file.path(NLMEGCCDir64, "bin", "gcc.exe"))
  if (gccMD5 == "4fe3c4f8d4f8ef48c71c38cc6d8beb66") {
    return(TRUE)
  } else {
    warning(
      "The gcc found in\n",
      NLMEGCCDir64,
      "\nis not fully compatible with NLME libraries (built with GCC 8.4.0).",
      "\nPlease set NLMEGCCDir64 env.variable to gcc distributed with the current application,",
      "\nto avoid compile/link/runtime errors",
      immediate. = TRUE
    )
    return(TRUE)
  }
}

#' Checks the local host for GCC version in the path
#'
#' Performs operating system dependent check for availability of GCC.
#'
#' @param OS.type Character specifying operating system type. Defaults to \code{.Platform$OS.type}.
#' @return \code{TRUE} if GCC check is successful, otherwise \code{FALSE}.
#' @examples
#'   checkGCC()
#'
#' @export
checkGCC <- function(OS.type = .Platform$OS.type) {
  if (OS.type == "unix") {
    checkLinuxGCC()
  } else {
    checkWindowsGCC()
  }
}

#' Checks the given directory for the files required for NLME run
#'
#' Checks the given directory for the files required for NLME run
#'
#' @param installDir directory Location of NLME executables as set in \code{INSTALLDIR} environment variable.
#'
#' @return \code{TRUE} if all checks are successful, otherwise \code{FALSE}.
#'
#' @examples
#' \dontrun{
#' checkInstallDir(Sys.getenv("INSTALLDIR"))
#' }
#'
#' @export
checkInstallDir <- function(installDir) {
  if (.Platform$OS.type == "unix") {
    req_files <- c(
      "execNLMECmd.sh",
      "TDL5",
      "libNLME7_linux.a",
      "libNLME7_FORT_linux.a",
      "libMPI_STUB_linux.a",
      "libLAPACK_linux.a",
      "libBLAS_linux.a"
    )
  } else {
    req_files <- c(
      "execNLMECmd.ps1",
      "TDL5.exe",
      "libNLME7.a",
      "libNLME7_FORT.a",
      "libMPI_STUB.a",
      "libLAPACK.a",
      "libBLAS.a"
    )
  }

  res <- file.exists(file.path(installDir, req_files))
  if (any(!res)) {
    warning(
      "File(s) ",
      paste(req_files[!res], collapse = ", "),
      "\n not found",
      " in NLME installation directory provided:\n",
      "INSTALLDIR=",
      installDir,
      "\n",
      call. = TRUE,
      immediate. = TRUE
    )
    return(FALSE)
  }

  if (.Platform$OS.type == "unix") {
    Sys.chmod(file.path(installDir, c("TDL5", "execNLMECmd.sh"), "+x"))
  }

  return(TRUE)
}

#' Check NLME ROOT DIRECTORY for the given local host
#'
#' Checks if NLME ROOT DIRECTORY is provided and ready for writing.
#' That directory is used for temporary folders writing.
#'
#' @param obj NLME Parallel Host to be checked
#'
#' @return \code{TRUE} if NLME ROOT DIRECTORY exists and accessible for writing,
#'  otherwise \code{FALSE}.
#'
#' @examples
#' \dontrun{
#' checkRootDir(host)
#' }
#'
#' @export
checkRootDir <- function(obj) {
  rootDir <- obj@sharedDirectory

  if (rootDir == "") {
    warning("Shared execution directory is not set for this host",
      immediate. = TRUE
    )
    return(FALSE)
  } else if (file.access(rootDir, 2) == -1) {
    warning("Shared execution directory is not accessible for writing",
      immediate. = TRUE
    )
    return(FALSE)
  }
  return(TRUE)
}

#' Checks if NLME run is licensed
#'
#' Checks if valid license file is available for NLME run.
#'
#' @param installDir Directory with NLME executables as specified in \code{INSTALLDIR} environment variable.
#' @param licenseFile Path to the license file. If not given, and Gemalto License
#' server is not active, NLME will try to look for it in \code{installationDirectory},
#' and in Phoenix installation directory.
#' @param verbose Flag to output warnings if issues are found.
#'
#' @return \code{TRUE} if all checks are successful, otherwise \code{FALSE}.
#'
#' @examples
#' \dontrun{
#' checkLicenseFile(Sys.getenv("INSTALLDIR"), FALSE)
#' }
#'
#' @export
checkLicenseFile <-
  function(installDir,
           licenseFile = "",
           verbose = FALSE) {
    stopifnot(checkInstallDir(installDir))
    PhoenixLicenseServerEnvFound <-
      Sys.getenv("PhoenixLicenseServer") != ""
    if (licenseFile != "") {
      if (!file.exists(licenseFile)) {
        warning("Specified license file was not found.")
        return(FALSE)
      } else {
        Sys.setenv(PhoenixLicenseFile = licenseFile)
      }
    }

    PhoenixLicenseFileEnvFound <-
      file.exists(Sys.getenv("PhoenixLicenseFile"))

    InstallDirLicenseFileFound <-
      file.exists(file.path(installDir, "lservrc"))
    PhoenixLicenseEnvFound <-
      file.exists(file.path(
        Sys.getenv("PhoenixDir"),
        "application\\Services\\Licensing\\lservrc"
      ))
    NLME_HASH <- Sys.getenv("NLME_HASH")
    if (!PhoenixLicenseFileEnvFound &&
      !PhoenixLicenseServerEnvFound &&
      !PhoenixLicenseEnvFound &&
      !InstallDirLicenseFileFound &&
      NLME_HASH == "") {
      warning("License server is not specified.\n",
        "lservrc file not found.",
        immediate. = TRUE
      )

      return(FALSE)
    }

    if (verbose) {
      if (NLME_HASH != "") {
        message(
          "NLME_HASH env.variable is not empty and will be used.",
          "\nLicense will be checked only if HASH check fails."
        )
      }
      if (PhoenixLicenseServerEnvFound) {
        message(
          "Phoenix License Server Environment variable PhoenixLicenseServer found and will be checked."
        )
      } else if (PhoenixLicenseFileEnvFound) {
        message(
          "Phoenix License file Environment variable PhoenixLicenseFile found and will be checked."
        )
      } else if (InstallDirLicenseFileFound) {
        message("Phoenix License file found in installDir and will be checked.")
      } else if (PhoenixLicenseEnvFound) {
        message("License file in Phoenix licensing directory found and will be checked.")
      }
    }

    errorMsg <- ""
    TDL5Executable <-
      ifelse(.Platform$OS.type == "unix", "TDL5", "TDL5.exe")

    if (NLME_HASH == "") {
      suppressWarnings(
        errorMsg <-
          system2(
            path.expand(file.path(installDir, TDL5Executable)),
            args = "-license_check",
            stdout = TRUE,
            stderr = TRUE
          )
      )
    } else {
      suppressWarnings(errorMsg <-
        system2(
          file.path(installDir, TDL5Executable),
          args = paste("-hash", NLME_HASH, "-l 1"),
          stdout = TRUE,
          stderr = TRUE
        ))
    }

    if (!any(grepl("(Using license)|(Unable to open file)", errorMsg))) {
      warning(
        "Cannot execute TDL5 using license provided.\n",
        "TDL5 output:\n",
        paste(errorMsg, collapse = "\n"),
        immediate. = TRUE
      )
      return(FALSE)
    } else if (any(grepl("Using license.+_Academic_", errorMsg))) {
      message("\nNLME academic license used.")
    }

    return(TRUE)
  }

checkMPIWindows <- function() {
  PhoenixMPIDir64 <-
    gsub("\\", "/", Sys.getenv("PhoenixMPIDir64"), fixed = TRUE)

  if (PhoenixMPIDir64 == "") {
    warning(
      "MPI directory not set. Cannot execute the job using host with MPI enabled.",
      immediate. = TRUE
    )
    return(FALSE)
  }

  req_files <-
    file.path(PhoenixMPIDir64, "bin", c("mpiexec.exe", "smpd.exe"))

  res <- file.exists(req_files)
  if (any(!res)) {
    warning(
      "Please check if PhoenixMPIDir64 env.variable is addressing to \n",
      paste(req_files[!res], collapse = "\n"),
      immediate. = TRUE
    )
    return(FALSE)
  }

  return(TRUE)
}

checkMPILinux <- function() {
  PhoenixMPIDir64 <- Sys.getenv("PhoenixMPIDir64")

  if (PhoenixMPIDir64 == "") {
    warning(
      "MPI directory not set. Cannot execute the job using host with MPI enabled.",
      immediate. = TRUE
    )
    return(FALSE)
  }

  req_files <-
    file.path(PhoenixMPIDir64, "bin", c("mpiexec", "mpif90"))

  res <- file.exists(req_files)
  if (any(!res)) {
    warning(
      "Please check if PhoenixMPIDir64 env.variable is addressing to \n",
      paste(req_files[!res], collapse = "\n"),
      immediate. = TRUE
    )
    return(FALSE)
  }

  return(TRUE)
}

#' Check MPI settings for the given local host
#'
#' Checks if MPI settings are provided and feasible.
#' Check is done for the hosts where MPI parallel method is used.
#'
#' @param obj NLME Parallel Host to be checked
#'
#' @return \code{TRUE} if MPI executables are ready for running,
#'  otherwise \code{FALSE}. If host does not have MPI in parallel method,
#'  it also returns \code{TRUE}.
#'
#' @examples
#' \dontrun{
#' checkMPISettings(host)
#' }
#'
#' @export
checkMPISettings <- function(obj) {
  if (grepl(".*MPI$", obj@parallelMethod@method, ignore.case = TRUE)) {
    if (.Platform$OS.type == "windows") {
      return(checkMPIWindows())
    } else {
      return(checkMPILinux())
    }
  } else {
    return(TRUE)
  }
}
