#' Plot chronological observations
#' @param object A `chrono_data` object.
#' @param ... Reserved for extensions.
#' @return A `ggplot` object.
#' @export
autoplot.chrono_data <- function(object, ...) {
  moving_average <- rep(NA_real_, nrow(object))
  for (segment in stats::na.omit(unique(object$segment))) {
    rows <- which(object$segment == segment)
    if (length(rows) < 5L) {
      # Short runs occur naturally around plant outages. Retain the observed
      # values instead of asking stats::filter() for a longer window than the
      # segment can support.
      moving_average[rows] <- object$value[rows]
    } else {
      moving_average[rows] <- as.numeric(stats::filter(
        object$value[rows], rep(1 / 5, 5),
        sides = 2
      ))
    }
  }
  plot_data <- object
  plot_data$moving_average_5 <- moving_average
  time_units <- attr(object, "time_units")
  value_units <- attr(object, "value_units")
  unit_label <- function(label, units) {
    if (is.null(units) || !nzchar(units)) label else paste0(label, " (", units, ")")
  }
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(.data$time, .data$value, group = .data$segment)
  ) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$moving_average_5),
      colour = "#0072B2", linewidth = 0.7, na.rm = TRUE
    )
  for (item in c("target", "lower_spec", "upper_spec")) {
    value <- attr(object, item)
    if (!is.null(value) && is.finite(value)) {
      p <- p + ggplot2::geom_hline(
        yintercept = value,
        linetype = if (item == "target") "dashed" else "dotted",
        colour = if (item == "target") "#009E73" else "#D55E00"
      )
    }
  }
  event_indicator <- attr(object, "event_indicator")
  if (!is.null(event_indicator) && length(event_indicator) == nrow(object)) {
    event_rows <- which(!is.na(event_indicator) & event_indicator)
    if (length(event_rows)) {
      p <- p + ggplot2::geom_point(
        data = plot_data[event_rows, , drop = FALSE],
        colour = "#CC79A7", shape = 8, size = 2, na.rm = TRUE
      )
    }
  }
  p + ggplot2::theme_bw() +
    ggplot2::labs(
      x = unit_label("Time", time_units),
      y = unit_label("Value", value_units)
    )
}
#' Base plot method for chronological observations
#' @param x A `chrono_data` object.
#' @param ... Passed to `ggplot2::autoplot()`.
#' @return The plotted `ggplot` object, invisibly.
#' @export
plot.chrono_data <- function(x, ...) {
  p <- ggplot2::autoplot(x, ...)
  print(p)
  invisible(p)
}
#' Plot a chronological semivariogram
#' @param object A `chrono_variogram` object.
#' @param components Optional `chrono_components` result. When supplied, the
#'   plot is annotated in a Pitard-style with `V(0)`, `V(1)`, the sill and the
#'   selected cyclic contribution.
#' @param cycle_lag Optional cycle lag to annotate. Defaults to the lag stored
#'   in `components`. If supplied, it must equal that stored lag; cycle
#'   annotations are never combined with components fitted for another cycle.
#' @param ... Reserved for extensions.
#' @return A `ggplot` object.
#' @export
autoplot.chrono_variogram <- function(object, components = NULL, cycle_lag = NULL, ...) {
  if (!is.null(components) && !inherits(components, "chrono_components")) {
    stop("components must be chrono_components", call. = FALSE)
  }
  if (!is.null(cycle_lag)) .assert_scalar_number(cycle_lag, "cycle_lag", 1, TRUE)
  if (!is.null(cycle_lag) && is.null(components)) {
    stop("cycle_lag requires a matching chrono_components record", call. = FALSE)
  }
  if (!is.null(components)) {
    component_cycle <- components$cycle_lag[[1L]]
    if (!is.null(cycle_lag) &&
        (!is.finite(component_cycle) || !isTRUE(all.equal(cycle_lag, component_cycle)))) {
      stop("cycle_lag must match the cycle_lag stored in components", call. = FALSE)
    }
    if (is.null(cycle_lag) && is.finite(component_cycle)) {
      cycle_lag <- component_cycle
    }
  }
  time_units <- attr(object, "time_units")
  value_units <- attr(object, "value_units")
  x_label <- if (is.null(time_units) || !nzchar(time_units)) "Lag time" else paste0("Lag time (", time_units, ")")
  y_label <- if (isTRUE(attr(object, "relative"))) {
    "Relative semivariance"
  } else if (is.null(value_units) || !nzchar(value_units)) {
    "Semivariance"
  } else {
    paste0("Semivariance (", value_units, "^2)")
  }
  p <- ggplot2::ggplot(object, ggplot2::aes(.data$lag_time, .data$semivariance, alpha = .data$eligible)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_alpha_manual(values = c(.35, 1), guide = "none") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = x_label, y = y_label)
  if (is.null(components)) return(p)
  if (!all(is.finite(c(components$V0, components$V1, components$sill)))) {
    stop("components must contain finite V0, V1 and sill values", call. = FALSE)
  }
  reference_lines <- data.frame(
    value = c(components$V0, components$V1, components$sill),
    label = c("V(0)", "V(1)", "sill")
  )
  p <- p + ggplot2::geom_hline(
    data = reference_lines,
    ggplot2::aes(yintercept = .data$value, colour = .data$label),
    linetype = "dashed", inherit.aes = FALSE
  ) +
    ggplot2::scale_colour_manual(
      values = c("V(0)" = "#D55E00", "V(1)" = "#009E73", sill = "#0072B2"),
      name = NULL
    )
  if (!is.null(cycle_lag)) {
    cycle_row <- match(cycle_lag, object$lag)
    if (is.na(cycle_row) || cycle_row < 2L) {
      stop("cycle_lag must identify a non-initial variogram row", call. = FALSE)
    }
    peak_lag <- components$cycle_peak_lag[[1L]]
    peak_row <- match(peak_lag, object$lag)
    if (is.na(peak_row) || peak_row >= cycle_row ||
        any(!is.finite(c(object$semivariance[cycle_row], peak_lag,
                         components$cycle_peak_ordinate[[1L]],
                         components$cycle_trough_ordinate[[1L]])))) {
      stop("components must contain a finite selected peak before cycle_lag", call. = FALSE)
    }
    cycle_span <- components$cycle_contribution[[1L]]
    cycle_midpoint <- if (identical(components$cycle_method[[1L]], "pitard")) {
      mean(c(components$V0[[1L]], components$cycle_peak_ordinate[[1L]]))
    } else {
      mean(c(
        components$cycle_peak_ordinate[[1L]],
        components$cycle_trough_ordinate[[1L]]
      ))
    }
    if (!is.finite(cycle_span) || cycle_span < 0) {
      stop("components must contain a finite nonnegative cycle contribution", call. = FALSE)
    }
    cycle_data <- data.frame(
      x = object$lag_time[cycle_row],
      ymin = cycle_midpoint - cycle_span / 2,
      ymax = cycle_midpoint + cycle_span / 2,
      label = "cyclic contribution"
    )
    p <- p +
      ggplot2::geom_vline(
        xintercept = object$lag_time[cycle_row],
        linetype = "dotted", colour = "#CC79A7"
      ) +
      ggplot2::geom_segment(
        data = cycle_data,
        ggplot2::aes(x = .data$x, xend = .data$x, y = .data$ymin, yend = .data$ymax),
        inherit.aes = FALSE, colour = "#CC79A7", linewidth = 1.1
      ) +
      ggplot2::annotate(
        "text", x = cycle_data$x, y = cycle_midpoint,
        label = "cyclic contribution", colour = "#CC79A7", hjust = -0.05
      )
  }
  p
}
#' Base plot method for a chronological semivariogram
#' @param x A `chrono_variogram` object.
#' @param ... Passed to `ggplot2::autoplot()`.
#' @return The plotted `ggplot` object, invisibly.
#' @export
plot.chrono_variogram <- function(x, ...) {
  p <- ggplot2::autoplot(x, ...)
  print(p)
  invisible(p)
}
#' Plot chronostatistical variance components
#' @param object A `chrono_components` object.
#' @param ... Reserved for extensions.
#' @return A `ggplot` object.
#' @export
autoplot.chrono_components <- function(object, ...) {
  d <- data.frame(component = c("sampling", "interval", "cycle"), variance = c(object$sampling_variance, object$interval_process_variance, object$cycle_contribution))
  ggplot2::ggplot(d, ggplot2::aes(.data$component, .data$variance)) +
    ggplot2::geom_col() +
    ggplot2::theme_bw()
}
#' Base plot method for chronostatistical components
#' @param x A `chrono_components` object.
#' @param ... Passed to `ggplot2::autoplot()`.
#' @return The plotted `ggplot` object, invisibly.
#' @export
plot.chrono_components <- function(x, ...) {
  p <- ggplot2::autoplot(x, ...)
  print(p)
  invisible(p)
}
#' Plot progressive engineering limits
#' @param object A `chrono_limits` object.
#' @param ... Reserved for extensions.
#' @return A `ggplot` object.
#' @export
autoplot.chrono_limits <- function(object, ...) {
  ggplot2::ggplot(object, ggplot2::aes(.data$stage, .data$center, group = 1)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lower, ymax = .data$upper), width = .1) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Progressive stage", y = "Limit")
}
#' Base plot method for progressive engineering limits
#' @param x A `chrono_limits` object.
#' @param ... Passed to `ggplot2::autoplot()`.
#' @return The plotted `ggplot` object, invisibly.
#' @export
plot.chrono_limits <- function(x, ...) {
  p <- ggplot2::autoplot(x, ...)
  print(p)
  invisible(p)
}
