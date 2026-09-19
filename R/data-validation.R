#' Validate chronological process data
#'
#' Data are sorted into increasing time order (with a warning). Missing values
#' start new segments; gaps larger than `gap_tolerance * interval` also start a
#' segment. Smaller departures from the declared interval are rejected because
#' position-based lag pairs would otherwise represent unequal elapsed times.
#' Resample irregular data to a regular grid before calling this function.
#' State-labelled runs are not interpreted or combined.
#' @param data A data frame, including a `tsibble` when that package is
#'   installed.
#' @param time,value Column names or vectors. For a `tsibble`, `time` may be
#'   omitted and the tsibble index is used. If `value` is also omitted, the
#'   only numeric measured variable is used; supply `value` when there is more
#'   than one candidate.
#' @param interval Expected sampling interval; inferred from a unique modal
#'   positive difference when omitted.
#' @param units Optional value units.
#' @param time_units Optional elapsed-time units for numeric time. Date-time
#'   inputs always use seconds and `Date` inputs always use days.
#' @param target,lower_spec,upper_spec Optional finite scalar process metadata.
#' @param gap_tolerance Positive gap multiplier.
#' @return A `chrono_data` data frame with sorted `time`, `value`, `segment`,
#'   and `index` columns. Attributes record interval, separate time/value
#'   units, target/specification values, reordered event metadata, and call.
#' @export
chrono_data <- function(data, time = NULL, value = NULL, interval = NULL, units = NULL,
                        time_units = NULL,
                        target = NULL, lower_spec = NULL, upper_spec = NULL,
                        gap_tolerance = 1.5) {
  if (!is.data.frame(data)) stop("data must be a data frame", call. = FALSE)
  units <- .normalise_unit(units, "units")
  time_units <- .normalise_unit(time_units, "time_units")
  .assert_scalar_number(gap_tolerance, "gap_tolerance", .Machine$double.eps)
  if (!is.null(interval)) {
    .assert_scalar_number(interval, "interval", .Machine$double.eps)
  }
  for (nm in c("target", "lower_spec", "upper_spec")) {
    value_to_check <- get(nm)
    if (!is.null(value_to_check)) {
      .assert_scalar_number(value_to_check, nm)
    }
  }
  if (!is.null(lower_spec) && !is.null(upper_spec) && lower_spec >= upper_spec) {
    stop("lower_spec must be below upper_spec", call. = FALSE)
  }
  if (!is.null(target) &&
      ((!is.null(lower_spec) && target < lower_spec) ||
       (!is.null(upper_spec) && target > upper_spec))) {
    warning("target is outside specification limits", call. = FALSE)
  }
  time_expr <- substitute(time)
  value_expr <- substitute(value)
  time_missing <- is.null(time_expr)
  value_missing <- is.null(value_expr)
  if (inherits(data, "tbl_ts") && (time_missing || value_missing)) {
    if (!requireNamespace("tsibble", quietly = TRUE)) {
      stop(
        "tsibble input requires the optional 'tsibble' package; install it with ",
        "install.packages('tsibble') or supply time and value",
        call. = FALSE
      )
    }
    index_name <- as.character(tsibble::index_var(data))
    if (time_missing) time_expr <- as.name(index_name)
    if (value_missing) {
      key_names <- as.character(tsibble::key_vars(data))
      candidates <- names(data)[vapply(data, is.numeric, logical(1))]
      candidates <- setdiff(candidates, c(index_name, key_names))
      if (length(candidates) != 1L) {
        stop("value must be supplied for a tsibble with zero or multiple numeric measured variables", call. = FALSE)
      }
      value_expr <- as.name(candidates)
    }
  }
  if (is.null(time_expr) || is.null(value_expr)) {
    stop("time and value must be supplied, unless inferred from a tsibble", call. = FALSE)
  }
  tt <- .col_value(data, time_expr, parent.frame())
  yy <- .col_value(data, value_expr, parent.frame())
  event_indicator <- if ("event_indicator" %in% names(data)) data$event_indicator else NULL
  event_type <- if ("event_type" %in% names(data)) data$event_type else NULL
  if (length(tt) != nrow(data) || length(yy) != nrow(data)) stop("time and value must match data rows", call. = FALSE)
  if (!(is.numeric(tt) || inherits(tt, c("Date", "POSIXt")))) stop("time must be numeric or date-time", call. = FALSE)
  if (!is.numeric(yy) || any(!is.finite(yy[!is.na(yy)]))) stop("non-missing values must be finite numeric", call. = FALSE)
  tn <- .time_numeric(tt)
  if (any(!is.finite(tn))) stop("timestamps must be finite", call. = FALSE)
  if (anyDuplicated(tn)) stop("duplicate timestamps are not allowed", call. = FALSE)
  if (is.unsorted(tn)) {
    warning("rows were sorted into chronological order", call. = FALSE)
    o <- order(tn)
    tt <- tt[o]
    yy <- yy[o]
    tn <- tn[o]
    if (!is.null(event_indicator)) event_indicator <- event_indicator[o]
    if (!is.null(event_type)) event_type <- event_type[o]
  }
  d <- diff(tn)
  if (is.null(interval)) {
    tab <- table(signif(d[d > 0], 12))
    best <- which(tab == max(tab))
    if (!length(tab) || length(best) != 1L) stop("interval cannot be defensibly inferred; supply interval", call. = FALSE)
    interval <- as.numeric(names(tab)[best])
  }
  .assert_scalar_number(interval, "interval", .Machine$double.eps)
  regular <- abs(d - interval) <= sqrt(.Machine$double.eps) *
    pmax(1, abs(interval), abs(d))
  short_irregular <- !regular & d <= gap_tolerance * interval
  if (any(short_irregular)) {
    stop(
      "irregular intervals within gap_tolerance are not valid lag steps; ",
      "resample to a regular grid or revise interval/gap_tolerance",
      call. = FALSE
    )
  }
  starts <- c(TRUE, is.na(utils::head(yy, -1L)) | is.na(utils::tail(yy, -1L)) | d > gap_tolerance * interval)
  seg <- cumsum(starts)
  seg[is.na(yy)] <- NA_integer_
  out <- data.frame(time = tt, value = yy, segment = seg, index = seq_along(yy))
  class(out) <- c("chrono_data", "data.frame")
  attr(out, "interval") <- interval
  attr(out, "time_units") <- if (inherits(tt, "POSIXt")) {
    "seconds"
  } else if (inherits(tt, "Date")) {
    "days"
  } else {
    time_units
  }
  attr(out, "value_units") <- units
  attr(out, "target") <- target
  attr(out, "lower_spec") <- lower_spec
  attr(out, "upper_spec") <- upper_spec
  attr(out, "event_indicator") <- event_indicator
  attr(out, "event_type") <- event_type
  attr(out, "call") <- match.call()
  out
}
