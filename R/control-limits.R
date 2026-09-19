#' Construct progressive chronostatistical engineering limits
#' @param components A `chrono_components` record.
#' @param center Mean or target centering.
#' @param include A nonempty, unique ordering of `"sampling"`, `"interval"`,
#'   and `"cycle"`. Each row adds the next named contribution.
#' @param convention Pitard linear-root additions or RSS comparison. In the
#'   Pitard presentation, `interval` is `V(1)`; in the RSS presentation it is
#'   the non-overlapping interval-process contribution `V(1) - V(0)`.
#' @param sigma Nonnegative finite multiplier. These limits are descriptive
#'   engineering limits, not prediction intervals.
#' @return A `chrono_limits` data frame. Its contribution columns report the
#'   exact variance quantities used to calculate each row's width.
#' @references
#' Pitard, F. F. (2019). *Theory of Sampling and Sampling Practice*, third
#' edition, Part X. CRC Press.
#' \doi{10.1201/9781351105934}.
#' @export
chrono_control_limits <- function(components, center = c("mean", "target"), include = c("sampling", "interval", "cycle"), convention = c("pitard", "rss"), sigma = 3) {
  center <- match.arg(center)
  convention <- match.arg(convention)
  .assert_scalar_number(sigma, "sigma", 0)
  if (!inherits(components, "chrono_components")) stop("components must be chrono_components", call. = FALSE)
  if (!is.character(include) || !length(include) || anyNA(include)) {
    stop("include must be a nonempty character vector", call. = FALSE)
  }
  if (anyDuplicated(include)) {
    stop("include contributions must be unique", call. = FALSE)
  }
  bad <- setdiff(include, c("sampling", "interval", "cycle"))
  if (length(bad)) stop("unknown contribution: ", bad[1], call. = FALSE)
  ctr <- if (center == "mean") components$mean else attr(components, "target")
  if (length(ctr) != 1L || !is.numeric(ctr) || !is.finite(ctr)) {
    stop(
      if (center == "target") "target center must be one finite value" else
        "mean center must be one finite value",
      call. = FALSE
    )
  }
  pitard_vals <- c(
    sampling = components$sampling_variance,
    interval = components$V1,
    cycle = components$cycle_contribution
  )
  rss_vals <- c(
    sampling = components$sampling_variance,
    interval = components$interval_process_variance,
    cycle = components$cycle_contribution
  )
  vals <- if (convention == "pitard") pitard_vals else rss_vals
  rows <- lapply(seq_along(include), function(i) {
    use <- include[seq_len(i)]
    selected <- vals[use]
    if (any(!is.finite(selected)) || any(selected < 0)) {
      stop("requested contributions must be available, finite, and nonnegative", call. = FALSE)
    }
    width <- if (convention == "pitard") {
      (if ("sampling" %in% use) sigma * sqrt(vals["sampling"]) else 0) +
        sum(sqrt(vals[intersect(c("interval", "cycle"), use)]))
    } else {
      sigma * sqrt(sum(selected))
    }
    data.frame(
      stage = paste(use, collapse = "+"), center = ctr,
      lower = ctr - width, upper = ctr + width,
      sampling = if ("sampling" %in% use) vals["sampling"] else 0,
      interval = if ("interval" %in% use) vals["interval"] else 0,
      cycle = if ("cycle" %in% use) vals["cycle"] else 0,
      convention = convention
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  class(out) <- c("chrono_limits", "data.frame")
  attr(out, "call") <- match.call()
  out
}
