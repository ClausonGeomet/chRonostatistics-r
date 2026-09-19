#' Estimate chronostatistical variance components
#' @param variogram A chronological variogram.
#' @param intercept Intercept estimation mode. The default `"pitard"` uses
#'   the Part X endpoint correction and the first three eligible lags;
#'   `"modern"` retains the AICc-selected local fit.
#' @param intercept_lags Optional finite, distinct eligible lag indices. At
#'   least two must be supplied.
#' @param cycle_lag Explicit finite period/minimum lag.
#' @param cycle_peak_lag Optional lag for the tangent/peak used to estimate the
#'   cycle. Defaults to the largest preceding ordinate.
#' @param cycle_method Cyclic contribution convention. `"pitard"` reports a
#'   half amplitude using the selected peak/tangent and a minimum tangent
#'   anchored at `V(0)`; `"peak_to_trough"` retains the full selected
#'   peak-to-trough difference.
#' @param sill Optional finite scalar plateau value.
#' @param weights Pair-count or equal regression weights.
#' @param nonnegative Clamp only reported engineering components; raw
#'   estimates remain available in the `*_raw` columns.
#' @return A one-row `chrono_components` record. The `selected_rows` and `fit`
#'   attributes retain the local intercept fit, and the `target` attribute is
#'   propagated from the variogram.
#' @references
#' Pitard, F. F. (2019). *Theory of Sampling and Sampling Practice*, third
#' edition, Part X. CRC Press.
#' \doi{10.1201/9781351105934}.
#' @export
chrono_components <- function(variogram, intercept = c("pitard", "modern"), intercept_lags = NULL,
                              cycle_lag = NULL, cycle_peak_lag = NULL,
                              cycle_method = c("pitard", "peak_to_trough"),
                              sill = NULL, weights = c("pairs", "equal"), nonnegative = TRUE) {
  intercept <- match.arg(intercept)
  cycle_method <- match.arg(cycle_method)
  weights <- match.arg(weights)
  .assert_scalar_logical(nonnegative, "nonnegative")
  if (!is.null(cycle_lag)) {
    .assert_scalar_number(cycle_lag, "cycle_lag", 1, TRUE)
  }
  if (!is.null(cycle_peak_lag)) {
    .assert_scalar_number(cycle_peak_lag, "cycle_peak_lag", 1, TRUE)
  }
  if (!is.null(sill)) .assert_scalar_number(sill, "sill")
  integ <- chrono_integral_variogram(variogram, integration = if (intercept == "pitard") "pitard" else "trapezoid")
  good <- which(variogram$eligible & is.finite(integ$W))
  if (!is.null(intercept_lags)) {
    if (!is.numeric(intercept_lags) || length(intercept_lags) < 2L ||
        any(!is.finite(intercept_lags)) || any(intercept_lags <= 0) ||
        any(intercept_lags != floor(intercept_lags))) {
      stop(
        "intercept_lags must contain at least two finite positive whole-number lags",
        call. = FALSE
      )
    }
    if (anyDuplicated(intercept_lags)) {
      stop("intercept_lags must not contain duplicates", call. = FALSE)
    }
    sel <- match(intercept_lags, variogram$lag, nomatch = 0L)
    if (any(sel == 0L)) {
      stop("intercept_lags contains an unknown lag", call. = FALSE)
    }
    if (any(!sel %in% good)) {
      stop("intercept_lags must identify finite eligible variogram rows", call. = FALSE)
    }
  } else if (intercept == "pitard") {
    if (length(good) < 3L) {
      stop("at least three eligible lags are required for the default Pitard fit", call. = FALSE)
    }
    sel <- utils::head(good, 3L)
  } else {
    if (length(good) < 3L) {
      stop("at least three eligible lags are required for the modern fit", call. = FALSE)
    }
    candidates <- 3:min(8L, length(good))
    score <- rep(Inf, length(candidates))
    fits <- vector("list", length(candidates))
    for (k in seq_along(candidates)) {
      ii <- utils::head(good, candidates[k])
      ww <- if (weights == "pairs") variogram$n_pairs[ii] else rep(1, length(ii))
      fit_data <- data.frame(W = integ$W[ii], lag_time = variogram$lag_time[ii], weight = ww)
      fits[[k]] <- .safe_lm(
        W ~ lag_time, data = fit_data, weights = fit_data$weight,
        label = "modern local fit"
      )
      p <- 2
      nn <- length(ii)
      score[k] <- nn * log(sum(stats::residuals(fits[[k]])^2) / nn) + 2 * p + if (nn > p + 1) 2 * p * (p + 1) / (nn - p - 1) else Inf
    }
    sel <- utils::head(good, candidates[which.min(score)])
  }
  ww <- if (intercept == "modern" && weights == "pairs") variogram$n_pairs[sel] else rep(1, length(sel))
  if (intercept == "pitard") {
    # Part X includes V(0)/2 as the left endpoint of W(j). Solve the
    # resulting intercept relation instead of treating that endpoint as zero.
    j <- variogram$lag[sel]
    base_w <- integ$W[sel]
    endpoint <- 1 / (2 * j)
    fit_data <- data.frame(
      base_w = base_w,
      endpoint = endpoint,
      lag_time = variogram$lag_time[sel]
    )
    base_fit <- .safe_lm(base_w ~ lag_time, data = fit_data, label = "Pitard base fit")
    endpoint_fit <- .safe_lm(endpoint ~ lag_time, data = fit_data, label = "Pitard endpoint fit")
    base_intercept <- unname(stats::coef(base_fit)[1L])
    endpoint_intercept <- unname(stats::coef(endpoint_fit)[1L])
    endpoint_denominator <- 1 - endpoint_intercept
    endpoint_tolerance <- sqrt(.Machine$double.eps)
    if (!is.finite(endpoint_denominator) ||
        abs(endpoint_denominator) <= endpoint_tolerance * max(1, abs(endpoint_intercept))) {
      stop("Pitard endpoint solution is numerically singular", call. = FALSE)
    }
    raw_v0 <- base_intercept / endpoint_denominator
    if (!is.finite(raw_v0)) {
      stop("Pitard endpoint solution produced a nonfinite V(0)", call. = FALSE)
    }
    fitted_w <- base_w + raw_v0 * endpoint
    final_data <- data.frame(fitted_w = fitted_w, lag_time = variogram$lag_time[sel])
    fit <- .safe_lm(fitted_w ~ lag_time, data = final_data, label = "corrected Pitard fit")
  } else {
    fit_data <- data.frame(W = integ$W[sel], lag_time = variogram$lag_time[sel], weight = ww)
    fit <- .safe_lm(
      W ~ lag_time, data = fit_data, weights = fit_data$weight,
      label = "modern local fit"
    )
    raw_v0 <- unname(stats::coef(fit)[1])
  }
  cf <- stats::coef(fit)
  v1row <- match(1L, variogram$lag)
  if (is.na(v1row) || !is.finite(variogram$semivariance[v1row])) {
    stop("lag-one semivariance V(1) is unavailable or nonfinite", call. = FALSE)
  }
  v1ord <- variogram$semivariance[v1row]
  raw_interval <- v1ord - raw_v0
  if (!is.finite(raw_interval)) {
    stop("interval-process variance is nonfinite", call. = FALSE)
  }
  if (is.null(sill)) {
    tailn <- max(3L, floor(nrow(variogram) / 5))
    sill <- stats::median(utils::tail(variogram$semivariance[variogram$eligible], tailn), na.rm = TRUE)
  }
  if (!is.finite(sill)) stop("sill is unavailable or nonfinite", call. = FALSE)
  peak_lag <- peak_v <- trough_v <- cycle_v <- cycle_time <- NA_real_
  if (!is.null(cycle_lag)) {
    ci <- match(cycle_lag, variogram$lag)
    if (is.na(ci) || ci < 2L || !is.finite(variogram$semivariance[ci])) {
      stop("cycle_lag must identify a finite non-initial variogram row", call. = FALSE)
    }
    if (is.null(cycle_peak_lag)) {
      preceding <- seq_len(ci - 1L)
      preceding <- preceding[is.finite(variogram$semivariance[preceding])]
      if (!length(preceding)) {
        stop("no finite peak row precedes cycle_lag", call. = FALSE)
      }
      pi <- preceding[which.max(variogram$semivariance[preceding])]
    } else {
      pi <- match(cycle_peak_lag, variogram$lag)
      if (is.na(pi) || pi >= ci || !is.finite(variogram$semivariance[pi])) {
        stop("cycle_peak_lag must identify a finite lag before cycle_lag", call. = FALSE)
      }
    }
    peak_lag <- variogram$lag[pi]
    peak_v <- variogram$semivariance[pi]
    trough_v <- variogram$semivariance[ci]
    cycle_v <- if (cycle_method == "pitard") {
      # The minimum tangent is taken to begin at V(0), as in Part X.
      (peak_v - raw_v0) / 2
    } else {
      peak_v - trough_v
    }
    cycle_time <- variogram$lag_time[ci]
  }
  clamp <- function(z) if (nonnegative && is.finite(z)) max(0, z) else z
  out <- data.frame(
    mean = attr(variogram, "mean"), V0 = clamp(raw_v0), V0_raw = raw_v0, V1 = v1ord,
    sampling_variance = clamp(raw_v0), interval_process_variance = clamp(raw_interval), interval_process_variance_raw = raw_interval,
    sill = sill, cycle_lag = if (is.null(cycle_lag)) NA_real_ else cycle_lag, cycle_time = cycle_time,
    cycle_contribution = clamp(cycle_v), cycle_contribution_raw = cycle_v,
    cycle_peak_lag = peak_lag, cycle_peak_ordinate = peak_v,
    cycle_trough_ordinate = trough_v, cycle_method = cycle_method,
    trend = if (cf[2] > 0) "increasing local integral" else "non-increasing local integral", method = intercept,
    intercept_lags = paste(variogram$lag[sel], collapse = ","), intercept_slope = unname(cf[2]),
    residual_scale = if (stats::df.residual(fit) > 0) stats::sigma(fit) else 0,
    conditioning = kappa(stats::model.matrix(fit)), clamped = nonnegative && any(c(raw_v0, raw_interval, cycle_v) < 0, na.rm = TRUE)
  )
  class(out) <- c("chrono_components", "data.frame")
  attr(out, "selected_rows") <- sel
  attr(out, "fit") <- fit
  attr(out, "target") <- attr(variogram, "target")
  attr(out, "call") <- match.call()
  out
}
