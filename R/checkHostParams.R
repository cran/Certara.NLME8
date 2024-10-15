checkLinuxGCC <- function() {
  gcc_out <-
    unlist(strsplit(
      system2("gcc", args = "-dumpversion", stdout = TRUE),
      split = ".",
      fixed = TRUE
    ))
  if (as.numeric(gcc_out[1]) > 8) {
    message("gcc version was found on the system: ",
            paste0(gcc_out, collapse = "."))
  } else if (as.numeric(gcc_out[1]) < 8) {
    message("\n gcc version was found on the system: ",
            paste0(gcc_out, collapse = "."))
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
      PhoenixGCCDir64 <-
        gsub("\\", "/", Sys.getenv("PhoenixGCCDir64"), fixed = TRUE)

      if (file.exists(file.path(PhoenixGCCDir64, "bin", "gcc.exe"))) {
        Sys.setenv("NLMEGCCDir64" = PhoenixGCCDir64)
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
    PML_BIN_DIR <- Sys.getenv("PML_BIN_DIR")
    reqLibsExecs <- c(
      "TDL5",
      "libNLME7_linux.a",
      "libNLME7_FORT_linux.a",
      "libMPI_STUB_linux.a",
      "libLAPACK_linux.a",
      "libBLAS_linux.a",
      "libcrlibm_linux.a",
      "libcadlicensingclient.so.0.1.0",
      "cadlicensingtool"
    )

    reqScripts <- c("execNLMECmd.sh")

    req_files <-
      c(file.path(PML_BIN_DIR, reqLibsExecs),
        reqScripts)
  } else {
    req_files <- c(
      "execNLMECmd.ps1",
      "TDL5.exe",
      "libNLME7.a",
      "libNLME7_FORT.a",
      "libMPI_STUB.a",
      "libLAPACK.a",
      "libBLAS.a",
      "libcrlibm.a",
      "cadlicensingtool.exe",
      "cadlicensingclient.dll"
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
    Sys.chmod(file.path(installDir,
                        c(
                          file.path(PML_BIN_DIR, "TDL5"),
                          "execNLMECmd.sh"
                        )),
              mode = "0777",
              use_umask = TRUE)
  }

  TRUE
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
            immediate. = TRUE)
    return(FALSE)
  } else if (file.access(rootDir, 2) == -1) {
    warning("Shared execution directory is not accessible for writing",
            immediate. = TRUE)
    return(FALSE)
  }
  return(TRUE)
}

#' Checks if NLME run is licensed
#'
#' Checks if valid license is available for NLME run.
#'
#' @param installDir Directory with NLME executables as specified in
#'   `INSTALLDIR` environment variable.
#' @param verbose Flag to output all messages during authorization and
#'   licensing. Default is `FALSE`.
#' @param outputGenericInfo Flag to provide TDL5 output when no issues found.
#'   Default is `TRUE`.
#'
#' @return `TRUE` if all checks are successful, otherwise `FALSE`.
#'
#' @examples
#' \dontrun{
#' checkLicenseFile(Sys.getenv("INSTALLDIR"),
#'                  verbose = TRUE)
#' }
#'
#' @export
checkLicenseFile <-
  function(installDir,
           verbose = FALSE,
           outputGenericInfo = TRUE) {
    stopifnot(checkInstallDir(installDir))
    NLME_HASH <- Sys.getenv("NLME_HASH")

    if (verbose) {
      if (NLME_HASH != "") {
        message(
          "NLME_HASH env.variable is not empty and will be used.",
          "\nLicense will be checked only if HASH check fails."
        )
      }
    }

    errorMsg <- ""
    TDL5Executable <-
      ifelse(.Platform$OS.type == "unix",
             file.path(Sys.getenv("PML_BIN_DIR"), "TDL5"),
             "TDL5.exe")

    if (NLME_HASH != "") {
      # NLME_HASH is provided; we cannot use license_check then
      suppressWarnings(errorMsg <-
                         system2(
                           file.path(installDir, TDL5Executable),
                           args = paste("-v -hash", NLME_HASH, "-l 1"),
                           stdout = TRUE,
                           stderr = TRUE
                         ))

      if (any(grepl("TDL5: Startup error: deltaTime", errorMsg, fixed = TRUE))) {
        warning(
          "Cannot execute TDL5 using NLME_HASH provided: ",
          NLME_HASH,
          "\n",
          "TDL5 output:\n",
          paste(errorMsg, collapse = "\n"),
          immediate. = TRUE
        )
        # license could be still OK

      } else if (any(grepl("unable to open file '1'", errorMsg, fixed = TRUE))) {
        # NLME_HASH is valid since it goes to check fake 1 file
        # Note that in different versions the message is a bit different

        return(TRUE)
      } else {
        warning("NLME_HASH env.variable provided does not point to the valid code.")
        return(FALSE)
      }
    } else {
      if (verbose) {
        message("NLME_HASH not found; performing license authorization check.")
      }

      suppressWarnings(
        errorMsg <-
          system2(
            path.expand(file.path(installDir, TDL5Executable)),
            args = "-v -license_check",
            stdout = TRUE,
            stderr = TRUE
          )
      )
    }

    CADConnection <- any(grepl("Status: OK", errorMsg))
    NoCADConnection <- any(grepl("Status: Disconnected", errorMsg))
    ValidLicense <-
      CADConnection ||
      NoCADConnection ||
      (length(errorMsg) == 1 &&
         grepl("TDL5 version\\:\\W\\d+\\.\\d+\\.\\d+", errorMsg))

    if (verbose ||
        !ValidLicense ||
        (ValidLicense && outputGenericInfo)) {
      message(paste(errorMsg, collapse = "\n"))
    }

    if (!ValidLicense) {
      warning(
        paste(
          "No license found or cannot execute TDL5 using license provided.\n",
          "Please try to reauthenticate. If you are still having issues, contact Certara support."
        ),
        immediate. = TRUE,
        call. = FALSE
      )
    }

    if (NoCADConnection) {
      if (verbose) {
        message("Offline mode.")
        tryCatch({
          RefreshLine <- errorMsg[grepl("Refresh until: ", errorMsg)]
          if (length(RefreshLine) == 0) {
            warning(paste(
              "Cannot obtain information for refresh token in TDL5 output:\n",
              errorMsg
            ))
            return(ValidLicense)
          }

          # the third is the date
          RefreshLineDateString <-
            strsplit(RefreshLine, " ")[[1]][3]
          if (Sys.Date() + 7 > as.Date(RefreshLineDateString, "%Y-%m-%d")) {
            message(
              "Using in offline mode. Number of days before online reconnection is required: ",
              as.Date(RefreshLineDateString, "%Y-%m-%d") - Sys.Date()
            )
          }
        },
        error = function(cond)
          warning(
            paste(
              "Cannot obtain information for refresh token in TDL5 output:\n",
              errorMsg,
              "\nThe error reported:\n",
              conditionMessage(cond)
            )
          ),
        warning = function(cond)
          warning(
            paste(
              "Cannot obtain information for refresh token in TDL5 output:\n",
              errorMsg,
              "\nThe warning reported:\n",
              conditionMessage(cond)
            )
          ))

      }
    }

    ValidLicense
  }


checkMPIWindows <- function() {
  PhoenixMPIDir64 <-
    gsub("\\", "/", Sys.getenv("PhoenixMSMPIDir"), fixed = TRUE)


  if (PhoenixMPIDir64 == "") {
    warning("MPI directory not set. Cannot execute the job using host with MPI enabled.")
    return(FALSE)
  }

  req_files <-
    file.path(PhoenixMPIDir64,
              c("mpiexec.exe", "smpd.exe"))
  res <- file.exists(req_files)
  if (any(!res)) {
    warning(
      paste(
        "Please check if PhoenixMPIDir64 env.variable is addressing to",
        req_files[!res],
        collapse = "\n",
        sep = "\n"
      )
    )

    FALSE
  }

  TRUE
}

checkMPILinux <- function() {
  PhoenixMPIDir64 <- Sys.getenv("PhoenixMPIDir64")

  if (PhoenixMPIDir64 == "") {
    warning("MPI directory not set. Cannot execute the job using host with MPI enabled.")

    return(FALSE)
  }

  req_files <-
    file.path(PhoenixMPIDir64, "bin", c("mpiexec", "mpif90"))

  res <- file.exists(req_files)
  if (any(!res)) {
    warning(
      paste(
        "Please check if PhoenixMPIDir64 env.variable is addressing to",
        req_files[!res],
        collapse = "\n",
        sep = "\n"
      )
    )

    FALSE
  }

  TRUE
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
  if (obj@isLocal &&
      grepl(".*MPI$", obj@parallelMethod@method, ignore.case = TRUE)) {
    if (.Platform$OS.type == "windows") {
      return(checkMPIWindows())
    } else {
      return(checkMPILinux())
    }
  }

  TRUE
}
