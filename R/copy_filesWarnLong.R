copy_filesWarnLong <- function(from, to, overwrite = TRUE) {
  copyRes <- withCallingHandlers(
    file.copy(from, to, overwrite = overwrite),
    error = function(e) {
      if (.Platform$OS.type == "windows") {
        longErrortxt <- paste0(
          "Cannot copy file(s)\n",
          paste0(basename(from), collapse = ", "),
          "\nfrom\n",
          paste0(unique(dirname(from)), collapse = ", "),
          "\nto\n",
          to,
          "\ndue to over-long path. Original error reported:\n",
          e$message
        )
        if (any(nchar(from) > 254 | nchar(to) > 254)) {
          stop(longErrortxt)
        }

        to <- ifelse(dir.exists(to),
                     file.path(to, basename(from)),
                     file.path(dirname(from), to))

        if (any(nchar(to) > 254)) {
          stop(longErrortxt)
        } else {
          stop(e)
        }
      } else {
        stop(e)
      }
    },
    warning = function(w) {
      longErrortxt <- paste0(
        "Cannot copy file(s)\n",
        paste0(basename(from), collapse = ", "),
        "\nfrom\n",
        paste0(unique(dirname(from)), collapse = ", "),
        "\nto\n",
        to,
        "\ndue to over-long path."
      )
      if (any(grepl("over-long path", w$message))) {
        stop(longErrortxt)
      } else if (any(grepl("No such file or directory", w$message)) &&
                 .Platform$OS.type == "windows") {
        if (any(nchar(from) > 254 | nchar(to) > 254)) {
          stop(longErrortxt)
        }

        to <- ifelse(dir.exists(to),
                     file.path(to, basename(from)),
                     file.path(dirname(from), to))

        if (any(nchar(to) > 254)) {
          stop(longErrortxt)
        } else {
          warning(w)
        }
      } else {
        warning(w)
      }
    }
  )
}
