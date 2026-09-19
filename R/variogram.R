#' Direct chronological semivariogram
#' @param x A `chrono_data` object or numeric vector.
#' @param max_lag Largest integer lag. It is capped at `floor(n / 2)` to
#'   preserve the central-pair convention described in Part X.
#' @param relative One non-missing logical; divide by the squared series mean.
#' @param min_pairs Minimum eligible pair count. Part X treats 20 as a minimum
#'   practical count and commonly recommends 30 or more.
#' @param pair_rule `"segment"` respects segments; `"complete"` treats all
#' complete values as one sequence.
#' @param keep_differences One non-missing logical; retain pair differences as
#'   an attribute.
#' @return A `chrono_variogram` data frame with `lag`, physical `lag_time`,
#'   `semivariance`, `n_pairs`, and `eligible` columns, plus source metadata.
#' @references
#' Napier-Munn, T. J. (2025). *Statistical Methods for Mineral Engineers:
#' How to Design Experiments and Analyse Data*, revised edition, Chapter 9.
#' Julius Kruttschnitt Mineral Research Centre.
#' [Book information](https://jktech.com.au/statsbook).
#'
#' Pitard, F. F. (2019). *Theory of Sampling and Sampling Practice*, third
#' edition, Part X. CRC Press.
#' \doi{10.1201/9781351105934}.
#' @export
chrono_variogram <- function(x, max_lag = NULL, relative = FALSE, min_pairs = 20L,
                             pair_rule = c("segment", "complete"), keep_differences = FALSE) {
  pair_rule <- match.arg(pair_rule)
  .assert_scalar_logical(relative, "relative")
  .assert_scalar_logical(keep_differences, "keep_differences")
  .assert_scalar_number(min_pairs, "min_pairs", 1, TRUE)
  if (inherits(x, "chrono_data")) {
    y <- x$value
    seg <- x$segment
    interval <- attr(x, "interval")
    units <- attr(x, "time_units")
  } else {
    if (!is.numeric(x)) stop("x must be numeric or chrono_data", call. = FALSE)
    y <- x
    seg <- rep(1L, length(y))
    interval <- 1
    units <- NULL
  }
  ok <- is.finite(y)
  n <- sum(ok)
  if (n < 2L) stop("at least two finite observations are required", call. = FALSE)
  mu <- mean(y[ok])
  vv <- stats::var(y[ok])
  if (relative && abs(mu) <= sqrt(.Machine$double.eps) *
      max(1, stats::sd(y[ok]))) {
    stop("relative variogram requires a mean safely separated from zero", call. = FALSE)
  }
  max_allowed <- max(1L, floor(n / 2))
  if (is.null(max_lag)) max_lag <- max_allowed
  .assert_scalar_number(max_lag, "max_lag", 1, TRUE)
  if (max_lag > max_allowed) {
    warning("max_lag exceeds half the series; capped at floor(n / 2) following the Pitard convention", call. = FALSE)
  }
  max_lag <- min(max_lag, max_allowed, length(y) - 1L)
  diffs <- vector("list", max_lag)
  ans <- lapply(seq_len(max_lag), function(j) {
    a <- seq_len(length(y) - j)
    b <- a + j
    use <- ok[a] & ok[b] & (pair_rule == "complete" | (!is.na(seg[a]) & seg[a] == seg[b]))
    dd <- y[b[use]] - y[a[use]]
    diffs[[j]] <<- dd
    v <- if (length(dd)) sum(dd^2) / (2 * length(dd)) else NA_real_
    if (relative) v <- v / mu^2
    data.frame(
      lag = j, lag_time = j * interval, semivariance = v,
      n_pairs = length(dd), eligible = length(dd) >= min_pairs
    )
  })
  out <- do.call(rbind, ans)
  class(out) <- c("chrono_variogram", "data.frame")
  attr(out, "mean") <- mu
  attr(out, "variance") <- vv
  attr(out, "relative") <- relative
  attr(out, "interval") <- interval
  attr(out, "time_units") <- units
  attr(out, "value_units") <- if (inherits(x, "chrono_data")) attr(x, "value_units") else NULL
  attr(out, "target") <- if (inherits(x, "chrono_data")) attr(x, "target") else NULL
  attr(out, "min_pairs") <- min_pairs
  attr(out, "max_lag") <- max_lag
  attr(out, "call") <- match.call()
  if (keep_differences) attr(out, "differences") <- diffs
  out
}
