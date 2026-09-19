# Load the package so this example runs directly with Rscript --vanilla.
library(chRonostatistics)

# Simulate known trend, autocorrelation, cycles, noise, and events.
set.seed(20260721)
sim <- simulate_chronostatistics(seed=20260721)
dat <- chrono_data(sim, time, value, interval=2, units="grade",
                   target=5.5, lower_spec=5.25, upper_spec=5.75)

# A centred five-point average shows medium-scale movement without endpoints.
sim$moving_average_5 <- as.numeric(stats::filter(sim$value, rep(0.2,5), sides=2))
events <- sim[sim$event_indicator, c("time","value","event_type")]

# Short lags estimate local components; long lags expose cycles and trend.
short_v <- chrono_variogram(dat, max_lag=40, min_pairs=20)
long_v <- chrono_variogram(dat, max_lag=240, min_pairs=20)
short_i <- chrono_integral_variogram(short_v, integration="pitard")

# The simulated 20-observation period makes graphical cycle choice explicit.
pitard <- chrono_components(long_v, cycle_lag=20)
modern <- chrono_components(long_v, intercept="modern", cycle_lag=20)
limits <- chrono_control_limits(pitard)
limits_rss <- chrono_control_limits(pitard, convention="rss")

print(summary(short_v)); print(summary(pitard)); print(modern); print(limits)

# Compare the decomposition and the two engineering-limit conventions.
component_comparison <- rbind(
  transform(as.data.frame(pitard), intercept = "pitard"),
  transform(as.data.frame(modern), intercept = "modern")
)
limit_comparison <- rbind(as.data.frame(limits), as.data.frame(limits_rss))
print(component_comparison[, c("intercept", "V0", "V1", "interval_process_variance",
                               "cycle_contribution", "sill", "trend")])
print(limit_comparison[, c("stage", "lower", "upper", "convention")])

# These objects cover chronological, short/long variogram, component, and
# progressive-limit figures; users can add target/specification/event layers.
p_data <- ggplot2::autoplot(dat)
p_short <- ggplot2::autoplot(short_v)
p_long <- ggplot2::autoplot(long_v)
p_components <- ggplot2::autoplot(pitard)
p_limits <- ggplot2::autoplot(limits)

# Optional physical-trapezoid versus printed/rectangular comparison.
integration_comparison <- do.call(rbind, lapply(
  c("trapezoid","left","right","pitard"),
  function(method) {
    z <- chrono_integral_variogram(short_v, integration=method)
    data.frame(lag=z$lag, W=z$W, method=method)
  }
))

# Reproduce the short-range V, W, and W' view used to extrapolate V(0).
short_functions <- merge(
  short_v[, c("lag_time", "semivariance")],
  short_i[, c("lag_time", "W", "W2")], by = "lag_time"
)
short_functions <- stats::reshape(short_functions,
  varying = c("semivariance", "W", "W2"), v.names = "value",
  timevar = "function_name", times = c("V", "W", "W_prime"), direction = "long"
)
short_functions$value[short_functions$function_name == "W"] <-
  short_functions$value[short_functions$function_name == "W"] +
  pitard$V0_raw / (2 * short_v$lag)
p_short_functions <- ggplot2::ggplot(short_functions,
  ggplot2::aes(lag_time, value, colour = function_name)) +
  ggplot2::geom_line() + ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = pitard$V0, linetype = "dashed") +
  ggplot2::theme_bw() + ggplot2::labs(x = "Lag time", y = "Variance function")

# Contrast integration conventions and annotate the selected cycle and sill.
p_integration <- ggplot2::ggplot(integration_comparison,
  ggplot2::aes(lag, W, colour = method)) + ggplot2::geom_line() +
  ggplot2::theme_bw()
p_annotated <- p_long +
  ggplot2::geom_hline(yintercept = pitard$sill, linetype = "dashed") +
  ggplot2::geom_vline(xintercept = pitard$cycle_time, linetype = "dotted")

if (interactive()) {
  print(p_data); print(p_short); print(p_short_functions); print(p_long)
  print(p_annotated); print(p_integration); print(p_components); print(p_limits)
}
