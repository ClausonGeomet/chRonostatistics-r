.assert_scalar_number <- function(x, name, lower = -Inf, integer = FALSE,
                                  upper = Inf) {
  valid <- length(x) == 1L && is.numeric(x) && !is.na(x) && is.finite(x) &&
    x >= lower && x <= upper && (!integer || x == floor(x))
  if (!valid) {
    range_text <- if (is.finite(upper)) {
      paste0(" between ", lower, " and ", upper)
    } else {
      paste0(" >= ", lower)
    }
    stop(
      name, " must be a finite ", if (integer) "whole " else "",
      "number", range_text,
      call. = FALSE
    )
  }
  invisible(x)
}

.assert_scalar_logical <- function(x, name) {
  if (length(x) != 1L || !is.logical(x) || is.na(x)) {
    stop(name, " must be TRUE or FALSE", call. = FALSE)
  }
  invisible(x)
}

.assert_seed <- function(seed) {
  .assert_scalar_number(
    seed, "seed", lower = 0, integer = TRUE,
    upper = .Machine$integer.max
  )
}

.assert_start_time <- function(start) {
  if (length(start) != 1L || !inherits(start, "POSIXt") ||
      !is.finite(as.numeric(start))) {
    stop("start must be one finite POSIX date-time", call. = FALSE)
  }
  invisible(start)
}

.normalise_unit <- function(x, name) {
  if (is.null(x)) return(NULL)
  if (length(x) != 1L || !is.character(x) || is.na(x) || !nzchar(x)) {
    stop(name, " must be NULL or one non-empty character string", call. = FALSE)
  }
  x
}

.safe_lm <- function(formula, data = NULL, weights = NULL, label = "local fit") {
  fit_args <- list(formula = formula, data = data)
  if (!is.null(weights)) fit_args$weights <- weights
  fit <- tryCatch(
    do.call(stats::lm, fit_args),
    error = function(e) NULL
  )
  if (is.null(fit)) stop(label, " could not be fitted", call. = FALSE)
  mm <- stats::model.matrix(fit)
  cf <- stats::coef(fit)
  if (fit$rank < ncol(mm) || any(!is.finite(cf))) {
    stop(label, " is singular or has nonfinite coefficients", call. = FALSE)
  }
  fit
}

.time_numeric <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.numeric(x))
  }
  if (inherits(x, "Date")) {
    return(as.numeric(x))
  }
  as.numeric(x)
}

.col_value <- function(data, expr, env) {
  if (is.name(expr) && as.character(expr) %in% names(data)) {
    return(data[[as.character(expr)]])
  }
  value <- eval(expr, envir = env)
  if (is.character(value) && length(value) == 1L) {
    if (!value %in% names(data)) {
      stop("unknown column: ", value, call. = FALSE)
    }
    return(data[[value]])
  }
  value
}

.trap_cumulative <- function(x, y) {
  if (!length(x)) {
    return(numeric())
  }
  c(0, cumsum(diff(x) * (utils::head(y, -1L) + utils::tail(y, -1L)) / 2))
}
