#' Simulate a focused chronological process
#' @param n Whole number of observations, at least 10.
#' @param interval Positive finite seconds between observations.
#' @param start One finite POSIX date-time.
#' @param mean,trend Baseline and per-index trend.
#' @param ar Finite AR(1) coefficient strictly between -1 and 1.
#' @param innovation_sd,measurement_sd Finite nonnegative noise standard
#'   deviations.
#' @param cycle_period,cycle_amplitude Matched, finite, nonempty cycle vectors;
#'   periods must be greater than one.
#' @param events One non-missing logical; add deterministic events.
#' @param seed Whole-number random seed between zero and the maximum supported
#'   R integer.
#' @return A data frame whose latent components reconcile to `value` where present.
#' @export
simulate_chronostatistics <- function(
  n = 720L, interval = 2, start = as.POSIXct("2026-01-01", tz = "UTC"), mean = 5.5,
  trend = 0.0008, ar = 0.55, innovation_sd = 0.006, cycle_period = c(20L, 60L), cycle_amplitude = c(0.012, 0.006),
  measurement_sd = 0.006, events = TRUE, seed = 20260721
) {
  .assert_scalar_number(
    n, "n", lower = 10, integer = TRUE,
    upper = .Machine$integer.max
  )
  .assert_scalar_number(interval, "interval", .Machine$double.eps)
  .assert_start_time(start)
  .assert_scalar_logical(events, "events")
  for (nm in c("mean", "trend", "ar", "innovation_sd", "measurement_sd")) {
    .assert_scalar_number(
      get(nm), nm,
      if (nm %in% c("innovation_sd", "measurement_sd")) 0 else -Inf
    )
  }
  .assert_seed(seed)
  if (abs(ar) >= 1) stop("ar must be strictly between -1 and 1", call. = FALSE)
  if (!is.numeric(cycle_period) || !is.numeric(cycle_amplitude) ||
      !length(cycle_period) || length(cycle_period) != length(cycle_amplitude) ||
      any(!is.finite(cycle_period)) || any(!is.finite(cycle_amplitude)) ||
      any(cycle_period <= 1)) {
    stop("cycle vectors must be finite, nonempty, matched, and periods > 1", call. = FALSE)
  }
  set.seed(seed)
  idx <- seq_len(n) - 1L
  z <- numeric(n)
  innovations <- stats::rnorm(n, sd = innovation_sd)
  z[1] <- innovations[1] / sqrt(1 - ar^2)
  for (i in 2:n) z[i] <- ar * z[i - 1] + innovations[i]
  phase <- seq_along(cycle_period) * pi / 7
  cyc <- rowSums(vapply(seq_along(cycle_period), function(k) cycle_amplitude[k] * sin(2 * pi * idx / cycle_period[k] + phase[k]), numeric(n)))
  err <- stats::rnorm(n, sd = measurement_sd)
  q <- numeric(n)
  event_type <- rep("none", n)
  event_indicator <- rep(FALSE, n)
  if (events) {
    shift <- floor(n * c(.35, .48))
    q[shift[1]:shift[2]] <- 0.06
    event_type[shift[1]:shift[2]] <- "level_shift"
    rec <- floor(n * c(.7, .75))
    q[rec[1]:rec[2]] <- seq(0.05, 0, length.out = rec[2] - rec[1] + 1)
    event_type[rec[1]:rec[2]] <- "maintenance_recovery"
    outage <- floor(n * .86) + 0:2
    outage <- outage[outage <= n]
    event_type[outage] <- "outage"
    event_indicator[event_type != "none"] <- TRUE
  }
  baseline <- rep(mean, n)
  tr <- trend * idx
  value <- baseline + tr + z + cyc + err + q
  value[event_type == "outage"] <- NA_real_
  data.frame(time = start + idx * interval, value = value, baseline = baseline, trend = tr, autocorrelated = z, cycle = cyc, measurement_error = err, event_effect = q, event_indicator = event_indicator, event_type = event_type)
}

#' Simulate a realistic mineral-processing plant time series
#'
#' Produces synthetic hourly plant data containing ore-domain changes,
#' autocorrelation, cyclic behaviour, process delays, control responses,
#' downtime, equipment degradation, measurement noise and sparse laboratory
#' sampling.
#'
#' @param n Whole number of hourly observations, at least 168.
#' @param start One finite POSIX starting date-time.
#' @param residence_time_hours Nonnegative whole-number feed-to-product delay.
#' @param lab_interval_hours Positive whole-number laboratory sampling interval.
#' @param seed Whole-number random seed between zero and the maximum supported
#'   R integer.
#'
#' @return A data frame of synthetic plant observations.
#' @export
simulate_plant_process <- function(
  n = 24 * 90,
  start = as.POSIXct(
    "2026-01-01 00:00:00",
    tz = "Australia/Perth"
  ),
  residence_time_hours = 3,
  lab_interval_hours = 4,
  seed = 20260717
) {
  .assert_scalar_number(
    n, "n", lower = 168, integer = TRUE,
    upper = .Machine$integer.max
  )
  .assert_start_time(start)
  .assert_scalar_number(
    residence_time_hours,
    "residence_time_hours",
    lower = 0,
    integer = TRUE
  )
  .assert_scalar_number(
    lab_interval_hours,
    "lab_interval_hours",
    lower = 1,
    integer = TRUE
  )
  .assert_seed(seed)

  set.seed(seed)

  #------------------------------------------------------------
  # Helpers
  #------------------------------------------------------------

  simulate_ar1 <- function(n, phi, innovation_sd, initial = 0) {
    x <- numeric(n)
    x[1] <- initial

    for (i in 2:n) {
      x[i] <- phi * x[i - 1] +
        stats::rnorm(1, sd = innovation_sd)
    }

    x
  }

  lag_vector <- function(x, lag) {
    if (lag == 0) {
      return(x)
    }

    c(rep(NA_real_, lag), utils::head(x, -lag))
  }

  rolling_mean <- function(x, window) {
    stats::filter(
      x,
      filter = rep(1 / window, window),
      sides = 1
    ) |>
      as.numeric()
  }

  logistic <- function(x) {
    1 / (1 + exp(-x))
  }

  #------------------------------------------------------------
  # Time index and cycles
  #------------------------------------------------------------

  timestamp <- seq(
    from = start,
    by = "hour",
    length.out = n
  )

  hour <- as.integer(format(timestamp, "%H"))
  day_index <- as.numeric(difftime(
    timestamp,
    start,
    units = "days"
  ))

  shift <- factor(
    ifelse(
      hour < 6,
      "night",
      ifelse(hour < 18, "day", "night")
    ),
    levels = c("day", "night")
  )

  day_cycle <- sin(2 * pi * hour / 24)
  day_cycle_cos <- cos(2 * pi * hour / 24)

  weekly_cycle <- sin(
    2 * pi * day_index / 7
  )

  # A weak 12-hour operational cycle.
  half_day_cycle <- sin(
    2 * pi * seq_len(n) / 12
  )

  #------------------------------------------------------------
  # Slowly changing ore domains
  #------------------------------------------------------------

  domain <- character(n)
  domain[1] <- "A"

  transition_matrix <- matrix(
    c(
      0.992, 0.006, 0.002,
      0.005, 0.990, 0.005,
      0.003, 0.007, 0.990
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(
      c("A", "B", "C"),
      c("A", "B", "C")
    )
  )

  for (i in 2:n) {
    domain[i] <- sample(
      c("A", "B", "C"),
      size = 1,
      prob = transition_matrix[domain[i - 1], ]
    )
  }

  domain <- factor(
    domain,
    levels = c("A", "B", "C")
  )

  domain_fe <- c(A = 58.2, B = 56.8, C = 59.1)
  domain_clay <- c(A = 7.0, B = 13.0, C = 5.0)
  domain_hardness <- c(A = 0.55, B = 0.40, C = 0.75)

  #------------------------------------------------------------
  # Feed characteristics
  #------------------------------------------------------------

  geological_fe <- simulate_ar1(
    n,
    phi = 0.97,
    innovation_sd = 0.09
  )

  geological_clay <- simulate_ar1(
    n,
    phi = 0.95,
    innovation_sd = 0.30
  )

  geological_fines <- simulate_ar1(
    n,
    phi = 0.90,
    innovation_sd = 0.50
  )

  feed_fe_true <- unname(domain_fe[domain]) +
    geological_fe +
    0.20 * weekly_cycle

  feed_clay_true <- unname(domain_clay[domain]) +
    geological_clay +
    0.7 * weekly_cycle

  feed_clay_true <- pmax(feed_clay_true, 0.5)

  feed_fines_true <- 18 +
    0.85 * feed_clay_true +
    geological_fines

  feed_fines_true <- pmin(
    pmax(feed_fines_true, 10),
    45
  )

  hardness_true <- unname(domain_hardness[domain]) +
    simulate_ar1(
      n,
      phi = 0.92,
      innovation_sd = 0.025
    )

  hardness_true <- pmin(
    pmax(hardness_true, 0.20),
    0.95
  )

  #------------------------------------------------------------
  # Equipment condition and maintenance
  #------------------------------------------------------------

  equipment_condition <- numeric(n)
  equipment_condition[1] <- 1

  maintenance_event <- rep(FALSE, n)

  planned_maintenance_starts <- if (n >= 24 * 14) {
    seq(from = 24 * 14, to = n, by = 24 * 21)
  } else {
    integer()
  }

  for (start_index in planned_maintenance_starts) {
    end_index <- min(start_index + 7, n)

    maintenance_event[
      start_index:end_index
    ] <- TRUE
  }

  for (i in 2:n) {
    degradation <- stats::runif(
      1,
      min = 0.00015,
      max = 0.00045
    )

    equipment_condition[i] <-
      equipment_condition[i - 1] - degradation

    if (maintenance_event[i]) {
      equipment_condition[i] <- min(
        1,
        equipment_condition[i] + 0.15
      )
    }

    equipment_condition[i] <- pmin(
      pmax(equipment_condition[i], 0.65),
      1
    )
  }

  #------------------------------------------------------------
  # Availability and downtime
  #------------------------------------------------------------

  unplanned_stop <- rep(FALSE, n)

  stop_probability <- logistic(
    -7.2 +
      0.09 * (feed_clay_true - 10) +
      3.5 * (0.80 - equipment_condition)
  )

  i <- 1

  while (i <= n) {
    if (
      !maintenance_event[i] &&
        stats::runif(1) < stop_probability[i]
    ) {
      stop_length <- sample(
        c(1, 2, 3, 4, 6, 8),
        size = 1,
        prob = c(0.30, 0.25, 0.18, 0.12, 0.10, 0.05)
      )

      stop_end <- min(n, i + stop_length - 1)
      unplanned_stop[i:stop_end] <- TRUE
      i <- stop_end + 1
    } else {
      i <- i + 1
    }
  }

  availability <- ifelse(
    maintenance_event | unplanned_stop,
    0,
    1
  )

  downtime_type <- ifelse(
    maintenance_event,
    "planned",
    ifelse(
      unplanned_stop,
      "unplanned",
      "operating"
    )
  )

  #------------------------------------------------------------
  # Throughput
  #------------------------------------------------------------

  throughput_noise <- simulate_ar1(
    n,
    phi = 0.65,
    innovation_sd = 10
  )

  operator_shift_effect <- ifelse(
    shift == "day",
    5,
    -3
  )

  throughput_target <- 760 +
    operator_shift_effect +
    15 * day_cycle -
    110 * (hardness_true - 0.55) -
    3.0 * (feed_clay_true - 8) +
    12 * weekly_cycle +
    8 * half_day_cycle

  throughput_tph <- throughput_target +
    throughput_noise

  throughput_tph <- throughput_tph *
    availability

  throughput_tph <- pmin(
    pmax(throughput_tph, 0),
    900
  )

  # Ramp-up following downtime.
  for (i in 2:n) {
    if (
      availability[i] == 1 &&
        availability[i - 1] == 0
    ) {
      ramp_end <- min(n, i + 2)
      ramp <- seq(
        0.45,
        0.85,
        length.out = ramp_end - i + 1
      )

      throughput_tph[i:ramp_end] <-
        throughput_tph[i:ramp_end] * ramp
    }
  }

  #------------------------------------------------------------
  # Water addition and density control
  #------------------------------------------------------------

  water_noise <- simulate_ar1(
    n,
    phi = 0.50,
    innovation_sd = 8
  )

  water_rate_m3h <- 240 +
    0.36 * throughput_tph +
    2.2 * (feed_fines_true - 25) +
    water_noise

  water_rate_m3h <- pmax(
    water_rate_m3h,
    0
  )

  density_error <- simulate_ar1(
    n,
    phi = 0.75,
    innovation_sd = 0.006
  )

  density_target <- 1.52 -
    0.0015 * (feed_clay_true - 8)

  density <- density_target +
    density_error +
    0.00020 * (
      throughput_tph - throughput_target
    )

  density[availability == 0] <- NA_real_

  #------------------------------------------------------------
  # Delayed feed variables
  #------------------------------------------------------------

  delayed_feed_fe <- lag_vector(
    feed_fe_true,
    residence_time_hours
  )

  delayed_feed_clay <- lag_vector(
    feed_clay_true,
    residence_time_hours
  )

  delayed_feed_fines <- lag_vector(
    feed_fines_true,
    residence_time_hours
  )

  delayed_throughput <- lag_vector(
    throughput_tph,
    residence_time_hours
  )

  delayed_density <- lag_vector(
    density,
    residence_time_hours
  )

  #------------------------------------------------------------
  # Recovery and product quality
  #------------------------------------------------------------

  recovery_noise_sd <- 0.35 +
    0.025 * pmax(delayed_feed_clay - 8, 0)

  recovery_noise <- simulate_ar1(
    n,
    phi = 0.60,
    innovation_sd = 0.30
  )

  recovery_pct <- 83.5 +
    0.25 * (delayed_feed_fe - 58) -
    0.33 * (delayed_feed_clay - 8) -
    0.055 * (delayed_feed_fines - 25) -
    20 * abs(delayed_density - 1.52) -
    0.004 * pmax(delayed_throughput - 800, 0) +
    6 * (equipment_condition - 0.85) +
    recovery_noise *
      recovery_noise_sd / 0.35

  recovery_pct <- pmin(
    pmax(recovery_pct, 45),
    95
  )

  recovery_pct[availability == 0] <- NA_real_

  separation_efficiency <- (
    recovery_pct - 75
  ) / 20

  product_noise <- simulate_ar1(
    n,
    phi = 0.45,
    innovation_sd = 0.12
  )

  product_fe_true <- 61.7 +
    0.25 * (delayed_feed_fe - 58) +
    0.55 * separation_efficiency -
    0.055 * (delayed_feed_clay - 8) +
    product_noise

  product_fe_true[availability == 0] <- NA_real_

  tails_fe_true <- delayed_feed_fe +
    0.035 * (100 - recovery_pct) +
    0.04 * (delayed_feed_clay - 8) +
    simulate_ar1(
      n,
      phi = 0.50,
      innovation_sd = 0.14
    )

  tails_fe_true[availability == 0] <- NA_real_

  #------------------------------------------------------------
  # Power draw
  #------------------------------------------------------------

  power_noise <- simulate_ar1(
    n,
    phi = 0.70,
    innovation_sd = 60
  )

  power_kw <- 1800 +
    3.8 * throughput_tph +
    900 * hardness_true +
    1100 * (1 - equipment_condition) +
    power_noise

  power_kw <- power_kw * availability
  power_kw <- pmax(power_kw, 0)

  #------------------------------------------------------------
  # Sensor measurement error
  #------------------------------------------------------------

  feed_fe_sensor <- feed_fe_true +
    stats::rnorm(n, sd = 0.18)

  feed_clay_sensor <- feed_clay_true +
    stats::rnorm(n, sd = 0.45)

  feed_fines_sensor <- feed_fines_true +
    stats::rnorm(n, sd = 0.70)

  product_fe_sensor <- product_fe_true +
    stats::rnorm(n, sd = 0.10)

  tails_fe_sensor <- tails_fe_true +
    stats::rnorm(n, sd = 0.13)

  #------------------------------------------------------------
  # Sparse laboratory sampling with reporting delay
  #------------------------------------------------------------

  lab_sample <- seq_len(n) %% lab_interval_hours == 0

  # Occasionally miss a scheduled sample.
  lab_sample[
    lab_sample &
      stats::runif(n) < 0.08
  ] <- FALSE

  lab_product_fe <- rep(NA_real_, n)

  lab_product_fe[lab_sample] <-
    product_fe_true[lab_sample] +
    stats::rnorm(
      sum(lab_sample),
      sd = 0.07
    )

  lab_reporting_delay <- sample(
    c(2, 3, 4, 5, 6),
    n,
    replace = TRUE,
    prob = c(0.10, 0.25, 0.35, 0.20, 0.10)
  )

  lab_result_available <- rep(NA_real_, n)

  sample_indices <- which(lab_sample)

  for (sample_index in sample_indices) {
    result_index <- sample_index +
      lab_reporting_delay[sample_index]

    if (result_index <= n) {
      lab_result_available[result_index] <-
        lab_product_fe[sample_index]
    }
  }

  #------------------------------------------------------------
  # Random sensor gaps
  #------------------------------------------------------------

  sensor_gap <- stats::runif(n) < 0.012

  product_fe_sensor[sensor_gap] <- NA_real_
  tails_fe_sensor[sensor_gap] <- NA_real_

  # One longer instrumentation outage.
  if (n >= 500) {
    outage_start <- sample(
      200:(n - 24),
      size = 1
    )

    outage_indices <- outage_start:min(
      n,
      outage_start + 10
    )

    product_fe_sensor[outage_indices] <- NA_real_
  }

  #------------------------------------------------------------
  # Final dataset
  #------------------------------------------------------------

  data.frame(
    timestamp = timestamp,
    hour = hour,
    shift = shift,
    ore_domain = domain,
    day_cycle = day_cycle,
    weekly_cycle = weekly_cycle,
    availability = availability,
    downtime_type = downtime_type,
    maintenance_event = maintenance_event,
    equipment_condition = equipment_condition,
    feed_fe_true = feed_fe_true,
    feed_fe = feed_fe_sensor,
    feed_clay_true = feed_clay_true,
    feed_clay = feed_clay_sensor,
    feed_fines_true = feed_fines_true,
    feed_fines = feed_fines_sensor,
    hardness = hardness_true,
    throughput_target_tph = throughput_target,
    throughput_tph = throughput_tph,
    water_rate_m3h = water_rate_m3h,
    density = density,
    recovery_pct = recovery_pct,
    product_fe_true = product_fe_true,
    product_fe = product_fe_sensor,
    tails_fe_true = tails_fe_true,
    tails_fe = tails_fe_sensor,
    power_kw = power_kw,
    lab_sample = lab_sample,
    lab_product_fe = lab_product_fe,
    lab_result_available = lab_result_available
  )
}
