#' Print chronological data
#' @param x A `chrono_data` object.
#' @param ... Arguments passed to the data-frame printer.
#' @return `x`, invisibly.
#' @export
print.chrono_data <- function(x, ...) {
  cat("Chronological data:", nrow(x), "rows in", length(unique(stats::na.omit(x$segment))), "segments\n")
  print.data.frame(utils::head(x), ...)
  invisible(x)
}
#' Summarise chronological data
#' @param object A `chrono_data` object.
#' @param ... Reserved for method compatibility.
#' @return A one-row data frame of sample characteristics.
#' @export
summary.chrono_data <- function(object, ...) data.frame(n = nrow(object), complete = sum(is.finite(object$value)), segments = length(unique(stats::na.omit(object$segment))), interval = attr(object, "interval"), mean = mean(object$value, na.rm = TRUE), sd = stats::sd(object$value, na.rm = TRUE))
#' Print a chronological variogram
#' @param x A `chrono_variogram` object.
#' @param ... Arguments passed to the data-frame printer.
#' @return `x`, invisibly.
#' @export
print.chrono_variogram <- function(x, ...) {
  cat("Chronological semivariogram (", sum(x$eligible), " eligible lags)\n", sep = "")
  print.data.frame(x, ...)
  invisible(x)
}
#' Summarise a chronological variogram
#' @param object A `chrono_variogram` object.
#' @param ... Reserved for method compatibility.
#' @return A one-row data frame.
#' @export
summary.chrono_variogram <- function(object, ...) data.frame(lags = nrow(object), eligible = sum(object$eligible), mean = attr(object, "mean"), variance = attr(object, "variance"), max_lag = max(object$lag))
#' Print chronostatistical components
#' @param x A `chrono_components` object.
#' @param ... Arguments passed to the data-frame printer.
#' @return `x`, invisibly.
#' @export
print.chrono_components <- function(x, ...) {
  cat("Chronostatistical components (", x$method, ")\n", sep = "")
  print.data.frame(x, ...)
  invisible(x)
}
#' Summarise chronostatistical components
#' @param object A `chrono_components` object.
#' @param ... Reserved for method compatibility.
#' @return The underlying component data frame.
#' @export
summary.chrono_components <- function(object, ...) unclass(object)
#' Print progressive engineering limits
#' @param x A `chrono_limits` object.
#' @param ... Arguments passed to the data-frame printer.
#' @return `x`, invisibly.
#' @export
print.chrono_limits <- function(x, ...) {
  cat("Progressive engineering control limits\n")
  print.data.frame(x, ...)
  invisible(x)
}
#' Summarise progressive engineering limits
#' @param object A `chrono_limits` object.
#' @param ... Reserved for method compatibility.
#' @return The underlying limits data frame.
#' @export
summary.chrono_limits <- function(object, ...) unclass(object)
