#' Create a report-style explanation of variance components
#'
#' `report()` provides a compact, descriptive interpretation of a
#' `chrono_components` result. It is deliberately dependency-free and returns
#' both readable findings and a tidy variance table, in the spirit of report
#' packages in the easystats ecosystem.
#'
#' @param x An object to explain.
#' @param ... Reserved for method extensions.
#' @return A `chrono_report` object containing `overview`, `findings`,
#'   `variance_table` and `caveats`.
#' @export
report <- function(x, ...) {
  UseMethod("report")
}

#' Build a tidy variance breakdown
#'
#' The returned table keeps the reported engineering components separate from
#' `V(1)`, which already includes `V(0)`. Percentages are descriptive shares of
#' the supplied sill, not inferential estimates.
#'
#' @param components A `chrono_components` result.
#' @return A data frame with estimates, standard deviations, percentages and
#'   interpretations.
#' @export
variance_breakdown <- function(components) {
  if (!inherits(components, "chrono_components")) {
    stop("components must be chrono_components", call. = FALSE)
  }
  sill <- components$sill[[1L]]
  cycle <- components$cycle_contribution[[1L]]
  cycle_available <- is.finite(cycle)
  assigned <- c(
    "Sampling and measurement" = components$sampling_variance[[1L]],
    "Process movement within one interval" = components$interval_process_variance[[1L]],
    "Selected cyclic variation" = if (cycle_available) cycle else NA_real_
  )
  assigned_total <- sum(assigned[is.finite(assigned)])
  remainder <- if (is.finite(sill)) sill - assigned_total else NA_real_
  estimate <- c(assigned, "Unallocated remainder" = remainder)
  pct <- if (is.finite(sill) && sill > 0) 100 * estimate / sill else rep(NA_real_, length(estimate))
  standard_deviation <- ifelse(
    is.finite(estimate) & estimate >= 0,
    sqrt(pmax(estimate, 0)),
    NA_real_
  )
  data.frame(
    component = names(estimate),
    estimate = unname(estimate),
    standard_deviation = unname(standard_deviation),
    percent_of_sill = unname(pct),
    interpretation = c(
      "V(0): sampling, preparation and measurement variation",
      "V(1) - V(0): movement during one sampling interval",
      if (cycle_available && identical(components$cycle_method[[1L]], "pitard")) {
        "Pitard-style half cyclic amplitude with the minimum tangent anchored at V(0)"
      } else if (cycle_available) {
        "Selected peak-to-trough cyclic contribution"
      } else {
        "No cycle_lag selected"
      },
      "Sill not assigned to the listed engineering components"
    ),
    row.names = NULL,
    check.names = FALSE
  )
}

#' @rdname report
#' @export
report.chrono_components <- function(x, ...) {
  table <- variance_breakdown(x)
  cycle_text <- if (is.finite(x$cycle_lag[[1L]])) {
    paste0("A cycle contribution was estimated at lag ", x$cycle_lag[[1L]], ".")
  } else {
    "No cycle contribution was estimated because cycle_lag was not supplied."
  }
  trend_text <- switch(
    x$trend[[1L]],
    "increasing local integral" = "The local integral variogram is increasing, indicating growing process movement over the fitted short-lag range.",
    "The local integral variogram is non-increasing over the fitted short-lag range."
  )
  findings <- c(
    paste0("V(0) is ", signif(x$V0[[1L]], 4),
           "; this is the reported sampling/measurement-scale variance."),
    paste0("V(1) is ", signif(x$V1[[1L]], 4),
           "; interval-process variance is ", signif(x$interval_process_variance[[1L]], 4), "."),
    paste0("The descriptive sill is ", signif(x$sill[[1L]], 4), "."),
    cycle_text,
    trend_text
  )
  structure(
    list(
      title = "Chronostatistical variance report",
      overview = paste0("Method: ", x$method[[1L]], "; intercept lags: ", x$intercept_lags[[1L]], "."),
      findings = findings,
      variance_table = table,
      caveats = c(
        "V(1) includes V(0); do not add V(1) and V(0) together.",
        "Percentages are descriptive shares of the supplied sill, not uncertainty intervals.",
        "Cycle selection remains an analyst decision and should be checked against process knowledge."
      ),
      components = x
    ),
    class = c("chrono_report", "list")
  )
}

#' @rdname report
#' @export
print.chrono_report <- function(x, ...) {
  cat(x$title, "\n\n", x$overview, "\n\n", sep = "")
  cat("Key findings\n")
  cat(paste0("- ", x$findings, collapse = "\n"), "\n\n", sep = "")
  cat("Variance breakdown\n")
  print(x$variance_table, row.names = FALSE, ...)
  cat("\nCaveats\n")
  cat(paste0("- ", x$caveats, collapse = "\n"), "\n", sep = "")
  invisible(x)
}
