#' @title
#' Set Logger settings
#'
#' @description
#' Updates logger settings, including log level and location of the file.
#'
#' @param logger_file_path A path (including file name) to log the messages.
#' (Default: CRE.log)
#' @param logger_level The log level. When a log level is set, all log levels
#' below it are also activated (if implemented). Available levels include:
#'   - TRACE: Provides verbose detailed logging, including the steps taken to
#'   achieve a result, often used for debugging. Activating TRACE will also
#'   enable DEBUG, INFO, SUCCESS, WARN, ERROR, and FATAL logs.
#'   - DEBUG: Provides detailed logging about the flow of the application, used
#'   mostly by developers to understand potential issues. Activating DEBUG will
#'   also enable INFO, SUCCESS, WARN, ERROR, and FATAL logs.
#'   - INFO (Default): Standard messages that inform the user about the normal
#'   operation of the system. Activating INFO will also enable SUCCESS, WARN,
#'   ERROR, and FATAL logs.
#'   - SUCCESS: Messages indicating successful completion of a particular
#'   operation or task. Activating SUCCESS will also enable WARN, ERROR, and
#'   FATAL logs.
#'   - WARN: Warning messages about events that might cause problems in the
#'   future, but are not yet errors. Activating WARN will also enable ERROR
#'   and FATAL logs.
#'   - ERROR: Reports an error due to which the system may not be able to
#'   achieve its functionality, but the application won't halt. Activating
#'   ERROR will also enable FATAL logs.
#'   - FATAL: Reports very severe error events that will presumably lead the
#'   application to abort.
#'
#' @export
#'
#' @return
#' No return value. This function is called for side effects.
#'
#' @note
#' Log levels are specified by developers during the initial implementation.
#' Future developers or contributors can leverage these log levels to better
#' capture and document the application's processes and events.
#'
#' @examples
#'
#' set_logger("Debug")
#'
set_logger <- function(logger_file_path = "CRE.log",
                       logger_level = "INFO") {

  available_levels <- c("TRACE", "DEBUG", "INFO", "SUCCESS", "WARN",
                        "ERROR", "FATAL")

  if (!is.element(logger_level, available_levels)) {
    stop(paste("logger_level: ", logger_level, " is not valid."))
  }


  logger::log_appender(appender = logger::appender_file(logger_file_path),
                       index = 1)

  set_options("logger_file_path", logger_file_path)
  set_options("logger_level", logger_level)

  if (!is.null(logger_level)) {
    if (is.element(logger_level, available_levels)) {

      logger::log_threshold(logger_level)

    } else {
      stop(paste("Logger level is not valid. Available levels: ",
                 paste(available_levels, collapse = " ")))
    }
  } else {
    logger::log_threshold(logger::INFO, index = 1)
  }
}

#' @title
#' Get Logger settings
#'
#' @description
#' Returns current logger settings.
#'
#' @seealso
#' \code{\link{set_logger}} for information on setting the log level and file path.
#'
#' @return
#' Returns a list that includes **logger_file_path** and **logger_level**.
#'
#' @export
#'
#' @examples
#'
#' set_logger("mylogger.log", "INFO")
#' log_meta <- get_logger()
#'
get_logger <- function() {

  return(list(logger_file_path = get_options("logger_file_path"),
              logger_level = get_options("logger_level")))
}
