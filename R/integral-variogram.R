#' Integral chronological variograms
#' @param variogram A `chrono_variogram`.
#' @param order Which integrals to return.
#' @param integration Numerical convention. The default `pitard` uses
#' consecutive integer lag indices `j` beginning at one. `trapezoid`, `left`,
#' and `right` integrate on physical `lag_time`. The Pitard implementation
#' returns the zero-origin working curve `W_0(j)`;
#' `chrono_components()` restores `V(0)/(2j)` and regresses against elapsed
#' lag `t_j` for the Part X intercept.
#' @return The input `lag`, `lag_time`, `n_pairs`, and `eligible` columns with
#'   `W` and/or `W2`, classed as `chrono_integral_variogram`.
#' @references
#' Pitard, F. F. (2019). *Theory of Sampling and Sampling Practice*, third
#' edition, Part X. CRC Press.
#' \doi{10.1201/9781351105934}.
#' @export
chrono_integral_variogram <- function(variogram, order = c("both", "first", "second"),
                                      integration = c("pitard", "trapezoid", "left", "right")) {
  order <- match.arg(order)
  integration <- match.arg(integration)
  if (!inherits(variogram, "chrono_variogram")) stop("variogram must be a chrono_variogram", call. = FALSE)
  if (!nrow(variogram)) stop("variogram must contain at least one row", call. = FALSE)
  x <- variogram$lag_time
  y <- variogram$semivariance
  if (!is.numeric(y) || any(!is.finite(y))) {
    stop("integrals require finite variogram ordinates", call. = FALSE)
  }
  if (!is.numeric(x) || any(!is.finite(x)) || any(x <= 0) || any(diff(x) <= 0)) {
    stop("integrals require finite, positive, increasing lag_time values", call. = FALSE)
  }
  if (integration == "pitard") {
    lag <- variogram$lag
    if (!is.numeric(lag) || any(!is.finite(lag)) ||
        !identical(as.integer(lag), seq_len(nrow(variogram))) ||
        any(lag != as.integer(lag))) {
      stop(
        "pitard integration requires consecutive unit lag indices beginning at 1",
        call. = FALSE
      )
    }
    # Work from a zero-origin curve. chrono_components() then solves for the
    # V(0)/2 endpoint contribution when fitting the Part X intercept.
    s <- cumsum(c(y[1L] / 2, (utils::head(y, -1L) + utils::tail(y, -1L)) / 2))
    sp <- cumsum(c(s[1L] / 2, (utils::head(s, -1L) + utils::tail(s, -1L)) / 2))
    j <- variogram$lag
    W <- s / j
    W2 <- 2 * sp / j^2
  } else {
    xx <- c(0, x)
    yy <- c(0, y)
    if (integration == "trapezoid") area <- .trap_cumulative(xx, yy)[-1L]
    if (integration == "left") area <- cumsum(diff(xx) * utils::head(yy, -1L))
    if (integration == "right") area <- cumsum(diff(xx) * utils::tail(yy, -1L))
    W <- area / x
    area2 <- .trap_cumulative(xx, c(0, W))[-1L]
    W2 <- 2 * area2 / x
  }
  out <- variogram[, c("lag", "lag_time", "n_pairs", "eligible")]
  if (order != "second") out$W <- W
  if (order != "first") out$W2 <- W2
  class(out) <- c("chrono_integral_variogram", "data.frame")
  attr(out, "integration") <- integration
  attr(out, "call") <- match.call()
  out
}
