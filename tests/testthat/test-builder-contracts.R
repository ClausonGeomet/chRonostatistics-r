test_that("bare and character columns work", {
  x <- data.frame(stamp = 1:6, assay = 11:16)
  a <- chrono_data(x, stamp, assay, interval = 1)
  b <- chrono_data(x, "stamp", "assay", interval = 1)
  expect_s3_class(a, "chrono_data")
  expect_equal(
    a[, c("time", "value", "segment", "index")],
    b[, c("time", "value", "segment", "index")]
  )
})

test_that("tsibble input can infer its index and single measured value", {
  testthat::skip_if_not_installed("tsibble")
  x <- tsibble::tsibble(
    timestamp = as.POSIXct("2026-01-01", tz = "UTC") + 0:5 * 3600,
    product_fe = 60 + seq_len(6) / 10,
    index = timestamp
  )
  out <- chrono_data(x, interval = 3600)
  expect_s3_class(out, "chrono_data")
  expect_equal(out$value, x$product_fe)
  expect_equal(out$time, x$timestamp)
})

test_that("tsibble input requires value when several measured variables exist", {
  testthat::skip_if_not_installed("tsibble")
  x <- tsibble::tsibble(
    timestamp = as.POSIXct("2026-01-01", tz = "UTC") + 0:5 * 3600,
    product_fe = 60 + seq_len(6) / 10,
    recovery = 80 + seq_len(6) / 10,
    index = timestamp
  )
  expect_error(chrono_data(x, interval = 3600), "value must be supplied")
  expect_s3_class(chrono_data(x, value = recovery, interval = 3600), "chrono_data")
})

test_that("sub-gap irregularity errors and large gaps segment", {
  expect_error(chrono_data(data.frame(t = c(0, 1, 2.2), y = 1:3), t, y,
    interval = 1, gap_tolerance = 1.5
  ), "irregular intervals")
  x <- chrono_data(data.frame(t = c(0, 1, 4, 5), y = 1:4), t, y, interval = 1)
  expect_equal(x$segment, c(1L, 1L, 2L, 2L))
  v <- chrono_variogram(x, max_lag = 1, min_pairs = 1)
  expect_equal(v$n_pairs, 2)
})

test_that("variogram follows the half-series lag and pair conventions", {
  expect_warning(
    capped <- chrono_variogram(1:20, max_lag = 19, min_pairs = 1),
    "capped at floor"
  )
  expect_equal(max(capped$lag), 10)
  expect_equal(chrono_variogram(1:20, min_pairs = 20)$n_pairs[1], 19)
})

test_that("Pitard rejects uneven lag rows", {
  x <- chrono_data(data.frame(t = 1:30, y = sin(1:30)), t, y)
  v <- chrono_variogram(x, max_lag = 5, min_pairs = 1)
  uneven <- v[c(1, 3, 5), ]
  class(uneven) <- class(v)
  expect_error(
    chrono_integral_variogram(uneven, integration = "pitard"),
    "consecutive unit lag"
  )
})

test_that("plant delays are whole numbers and valid calls are reproducible", {
  expect_error(simulate_plant_process(residence_time_hours = 1.5), "whole")
  expect_error(simulate_plant_process(lab_interval_hours = 2.5), "whole")
  expect_identical(
    simulate_plant_process(n = 168, seed = 42),
    simulate_plant_process(n = 168, seed = 42)
  )
  short <- simulate_plant_process(n = 168, seed = 42)
  expect_length(short$maintenance_event, 168)
  expect_false(any(short$maintenance_event))
})

test_that("plant plots handle short segments around outages", {
  plant <- simulate_plant_process(n = 24 * 30, seed = 42)
  x <- chrono_data(plant, timestamp, product_fe, interval = 3600)
  expect_s3_class(ggplot2::autoplot(x), "ggplot")
})

test_that("data plots expose engineering context and physical units", {
  sim <- simulate_chronostatistics(n = 120, interval = 60, seed = 3)
  x <- chrono_data(
    sim, time, value,
    interval = 60, units = "grade",
    target = 5.5, lower_spec = 5.2, upper_spec = 5.8
  )
  p <- ggplot2::autoplot(x)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "Time (seconds)")
  expect_equal(p$labels$y, "Value (grade)")
  expect_true(any(vapply(p$layers, function(z) inherits(z$geom, "GeomHline"), logical(1))))
  expect_true(any(vapply(p$layers, function(z) inherits(z$geom, "GeomPoint"), logical(1))))
  expect_true(any(vapply(p$layers, function(z) {
    !is.null(z$mapping$y) && rlang::as_label(z$mapping$y) == "moving_average_5"
  }, logical(1))))

  v <- chrono_variogram(x, max_lag = 10, min_pairs = 1)
  pv <- ggplot2::autoplot(v)
  expect_equal(pv$labels$x, "Lag time (seconds)")
  expect_equal(pv$labels$y, "Semivariance (grade^2)")

  components <- chrono_components(v, cycle_lag = 5)
  pitard <- ggplot2::autoplot(v, components = components)
  expect_s3_class(pitard, "ggplot")
  # ggplot2 stores the three reference values in one multi-row hline layer.
  expect_true(any(vapply(pitard$layers, function(z) inherits(z$geom, "GeomHline"), logical(1))))
  expect_true(any(vapply(pitard$layers, function(z) inherits(z$geom, "GeomSegment"), logical(1))))
})

test_that("component reports explain and tabulate the variance breakdown", {
  x <- simulate_chronostatistics(n = 120, events = FALSE, seed = 12)
  data <- chrono_data(x, time, value, interval = 2)
  v <- chrono_variogram(data, max_lag = 10, min_pairs = 1)
  components <- chrono_components(v, cycle_lag = 5)
  breakdown <- variance_breakdown(components)
  expect_s3_class(breakdown, "data.frame")
  expect_equal(nrow(breakdown), 4)
  expect_true(all(c("estimate", "standard_deviation", "percent_of_sill") %in% names(breakdown)))
  explained <- report(components)
  expect_s3_class(explained, "chrono_report")
  expect_length(explained$findings, 5)
  expect_output(print(explained), "Chronostatistical variance report")

  without_cycle <- chrono_components(v)
  unassigned <- variance_breakdown(without_cycle)
  expect_true(is.na(unassigned$estimate[3]))
  expect_equal(
    unassigned$estimate[4],
    without_cycle$sill - without_cycle$sampling_variance -
      without_cycle$interval_process_variance
  )
})

test_that("focused latent components reconcile", {
  x <- simulate_chronostatistics(n = 120, seed = 1)
  reconstructed <- with(x, baseline + trend + autocorrelated + cycle +
    measurement_error + event_effect)
  ok <- is.finite(x$value)
  expect_equal(x$value[ok], reconstructed[ok])
})

test_that("focused simulation defaults show a positive trend with restrained noise", {
  x <- simulate_chronostatistics(n = 720, events = FALSE, seed = 11)
  expect_gt(unname(coef(stats::lm(value ~ seq_along(value), data = x))[2]), 0)
  expect_gt(tail(x$trend, 1), head(x$trend, 1))
  expect_lt(stats::sd(x$cycle), 0.02)
})

test_that("variogram and trapezoid integrals match hand calculations", {
  v <- chrono_variogram(c(1, 3, 6, 10), max_lag = 2, min_pairs = 1)
  expect_equal(v$semivariance, c(29 / 6, 18.5))
  expect_equal(v$n_pairs, c(3L, 2L))
  expect_true(all(v$eligible))
  rel <- chrono_variogram(c(1, 3, 6, 10),
    max_lag = 2,
    min_pairs = 1, relative = TRUE
  )
  expect_equal(rel$semivariance, v$semivariance / 25)
  integ <- chrono_integral_variogram(v, integration = "trapezoid")
  expected_w2 <- (29 / 12 + (29 / 6 + 18.5) / 2) / 2
  expect_equal(integ$W, c(29 / 12, expected_w2))
})

test_that("component and limit arithmetic is internally consistent", {
  x <- chrono_data(
    data.frame(t = 1:80, y = 5 + sin(2 * pi * (1:80) / 10)), t, y
  )
  v <- chrono_variogram(x, max_lag = 20, min_pairs = 20)
  z <- chrono_components(v, intercept_lags = 1:3, cycle_lag = 10)
  expect_gte(z$V0, 0)
  expect_equal(z$interval_process_variance, max(0, z$interval_process_variance_raw))
  expect_equal(z$cycle_method, "pitard")
  expect_equal(
    z$cycle_contribution_raw,
    (z$cycle_peak_ordinate - z$V0_raw) / 2
  )
  legacy <- chrono_components(
    v, intercept_lags = 1:3, cycle_lag = 10,
    cycle_method = "peak_to_trough"
  )
  expect_equal(
    legacy$cycle_contribution_raw,
    legacy$cycle_peak_ordinate - legacy$cycle_trough_ordinate
  )
  lim <- chrono_control_limits(z)
  expect_equal(lim$upper - lim$center, lim$center - lim$lower)
  expect_equal(lim$upper[1] - lim$center[1], 3 * sqrt(z$sampling_variance))
  expect_s3_class(ggplot2::autoplot(z), "ggplot")
  expect_s3_class(ggplot2::autoplot(lim), "ggplot")
})

test_that("constant variograms produce components without fit warnings", {
  v <- structure(
    data.frame(
      lag = 1:3,
      lag_time = 1:3,
      semivariance = rep(4, 3),
      n_pairs = rep(30L, 3),
      eligible = rep(TRUE, 3)
    ),
    class = c("chrono_variogram", "data.frame"),
    mean = 10,
    variance = 4
  )

  expect_warning(components <- chrono_components(v), NA)
  expect_equal(components$V0_raw, 4, tolerance = 1e-12)
  expect_equal(components$interval_process_variance_raw, 0, tolerance = 1e-12)
  expect_equal(components$sill, 4)
  expect_equal(components$intercept_slope, 0, tolerance = 1e-12)
  expect_equal(components$residual_scale, 0, tolerance = 1e-12)
})

test_that("time and value units remain distinct", {
  numeric_time <- chrono_data(
    data.frame(t = 0:5, y = 1:6), t, y,
    interval = 1, units = "grade", time_units = "hours"
  )
  expect_identical(attr(numeric_time, "value_units"), "grade")
  expect_identical(attr(numeric_time, "time_units"), "hours")

  date_time <- chrono_data(
    data.frame(t = as.Date("2026-01-01") + 0:5, y = 1:6), t, y,
    interval = 1, units = "grade", time_units = "ignored"
  )
  expect_identical(attr(date_time, "time_units"), "days")

  posix_time <- chrono_data(
    data.frame(
      t = as.POSIXct("2026-01-01", tz = "UTC") + 0:5 * 60,
      y = 1:6
    ),
    t, y, interval = 60, units = "grade", time_units = "ignored"
  )
  expect_identical(attr(posix_time, "time_units"), "seconds")
})

test_that("chrono_data validates scalar process metadata", {
  d <- data.frame(t = 1:6, y = 1:6)
  expect_error(chrono_data(d, t, y, interval = c(1, 2)), "interval")
  expect_error(chrono_data(d, t, y, interval = 1, target = Inf), "target")
  expect_error(
    chrono_data(d, t, y, interval = 1, lower_spec = 2, upper_spec = 2),
    "lower_spec"
  )
  expect_warning(
    chrono_data(d, t, y, interval = 1, target = 7, upper_spec = 6),
    "outside specification"
  )
})

test_that("variogram switches and integral ordinates are validated", {
  expect_error(chrono_variogram(1:10, relative = NA), "relative")
  expect_error(chrono_variogram(1:10, keep_differences = c(TRUE, FALSE)),
    "keep_differences"
  )
  v <- chrono_variogram(1:20, max_lag = 5, min_pairs = 1)
  v$semivariance[2] <- Inf
  expect_error(chrono_integral_variogram(v), "finite variogram ordinates")
})

test_that("component row selections fail clearly when unusable", {
  d <- chrono_data(
    data.frame(t = 1:80, y = 5 + sin(2 * pi * (1:80) / 10)),
    t, y, interval = 1
  )
  v <- chrono_variogram(d, max_lag = 20, min_pairs = 20)
  expect_error(chrono_components(v, intercept_lags = c(1, 1)), "duplicates")
  expect_error(chrono_components(v, intercept_lags = c(1, 99)), "unknown")
  expect_error(chrono_components(v, intercept_lags = 1), "at least two")
  expect_error(chrono_components(v, cycle_lag = 10, cycle_peak_lag = 11),
    "before cycle_lag"
  )
  expect_error(chrono_components(v, sill = c(1, 2)), "sill")
})

test_that("engineering limits report exactly the variances used", {
  d <- chrono_data(
    data.frame(t = 1:80, y = 5 + sin(2 * pi * (1:80) / 10)),
    t, y, interval = 1
  )
  v <- chrono_variogram(d, max_lag = 20, min_pairs = 20)
  z <- chrono_components(v, cycle_lag = 10)

  expect_error(chrono_control_limits(z, include = character()), "nonempty")
  expect_error(chrono_control_limits(z, include = c("sampling", "sampling")),
    "unique"
  )
  expect_error(chrono_control_limits(z, center = "target"), "target center")

  pitard <- chrono_control_limits(z)
  rss <- chrono_control_limits(z, convention = "rss")
  expect_equal(pitard$interval[2], z$V1)
  expect_equal(rss$interval[2], z$interval_process_variance)
  expect_equal(
    pitard$upper[2] - pitard$center[2],
    3 * sqrt(z$sampling_variance) + sqrt(z$V1)
  )
  expect_equal(
    rss$upper[2] - rss$center[2],
    3 * sqrt(z$sampling_variance + z$interval_process_variance)
  )
})

test_that("variogram annotations require matching component cycles", {
  d <- chrono_data(
    data.frame(t = 1:80, y = 5 + sin(2 * pi * (1:80) / 10)),
    t, y, interval = 1
  )
  v <- chrono_variogram(d, max_lag = 20, min_pairs = 20)
  z <- chrono_components(v, cycle_lag = 10, cycle_peak_lag = 5)
  expect_error(ggplot2::autoplot(v, cycle_lag = 10), "matching")
  expect_error(ggplot2::autoplot(v, components = z, cycle_lag = 11), "must match")
  expect_s3_class(ggplot2::autoplot(v, components = z, cycle_lag = 10), "ggplot")
})

test_that("simulators reject invalid public scalar arguments", {
  expect_error(simulate_chronostatistics(events = NA), "events")
  expect_error(simulate_chronostatistics(ar = 1), "strictly between")
  expect_error(simulate_chronostatistics(cycle_period = numeric()), "nonempty")
  expect_error(simulate_chronostatistics(innovation_sd = -1), "innovation_sd")
  expect_error(simulate_chronostatistics(seed = -1), "seed")
  expect_error(simulate_chronostatistics(start = NA), "start")

  expect_error(simulate_plant_process(n = 167), "n")
  expect_error(simulate_plant_process(seed = .Machine$integer.max + 1), "seed")
  expect_error(simulate_plant_process(start = Inf), "start")
})
