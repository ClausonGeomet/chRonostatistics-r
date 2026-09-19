# Rebuild the ggplot2 figures embedded in README.md.

source(file.path("R", "utils.R"))
source(file.path("R", "data-validation.R"))
source(file.path("R", "simulation.R"))
source(file.path("R", "variogram.R"))
source(file.path("R", "integral-variogram.R"))
source(file.path("R", "components.R"))

dir.create(file.path("man", "figures"), showWarnings = FALSE, recursive = TRUE)

draw_two_panels <- function(path, top, bottom, width = 1600, height = 900) {
  grDevices::png(path, width = width, height = height, res = 180)
  grid::grid.newpage()
  layout <- grid::viewport(layout = grid::grid.layout(2, 1))
  grid::pushViewport(layout)
  print(top, vp = grid::viewport(layout.pos.row = 1))
  print(bottom, vp = grid::viewport(layout.pos.row = 2))
  grid::popViewport()
  grDevices::dev.off()
}

iron <- simulate_chronostatistics(
  n = 1511, interval = 4 * 3600, mean = 66.1, ar = 0.75,
  innovation_sd = 0.18, cycle_period = c(17, 208),
  cycle_amplitude = c(0.28, 0.50), measurement_sd = 0.25,
  events = FALSE, seed = 2008
)
iron_data <- chrono_data(
  iron, time, value, interval = 4 * 3600, units = "% Fe",
  target = 66.1, lower_spec = 65, upper_spec = 67.5
)
iron_variogram <- chrono_variogram(iron_data, max_lag = 300, min_pairs = 30)
iron_components <- chrono_components(iron_variogram, cycle_lag = 208)

iron_top <- ggplot2::ggplot(iron_data, ggplot2::aes(time, value)) +
  ggplot2::geom_line(colour = "#245b75") +
  ggplot2::geom_hline(
    ggplot2::aes(yintercept = y, colour = label, linetype = label),
    data = data.frame(
      y = c(65, 66.1, 67.5),
      label = c("specification", "target", "specification")
    ), inherit.aes = FALSE
  ) +
  ggplot2::scale_colour_manual(values = c(specification = "#bb4444", target = "#555555")) +
  ggplot2::scale_linetype_manual(values = c(specification = "dashed", target = "dotted")) +
  ggplot2::theme_bw() + ggplot2::theme(legend.position = "bottom") +
  ggplot2::labs(title = "Synthetic 4-hour iron-ore stream", x = "Time",
                y = "% Fe", colour = NULL, linetype = NULL)

iron_bottom <- ggplot2::ggplot(iron_variogram,
                               ggplot2::aes(lag, semivariance)) +
  ggplot2::geom_line(colour = "#b35c2e", linewidth = 0.8) +
  ggplot2::geom_vline(xintercept = c(17, 208),
                      colour = c("#777777", "#245b75"), linetype = "dashed") +
  ggplot2::annotate("text", x = 20, y = max(iron_variogram$semivariance) * .88,
                    label = "2.8 days", hjust = 0, colour = "#777777") +
  ggplot2::annotate("text", x = 211, y = max(iron_variogram$semivariance) * .94,
                    label = "35 days", hjust = 0, colour = "#245b75") +
  ggplot2::theme_bw() + ggplot2::labs(title = "Chronological semivariogram",
                                      x = "Lag (4-hour samples)", y = "% Fe squared")

draw_two_panels(file.path("man", "figures", "readme-iron-ore.png"),
                iron_top, iron_bottom)

pitard_lines <- data.frame(
  value = c(iron_components$V0, iron_components$V1, iron_components$sill),
  label = c("V(0)", "V(1)", "sill")
)
cycle_row <- match(iron_components$cycle_lag, iron_variogram$lag)
peak_row <- which.max(iron_variogram$semivariance[seq_len(cycle_row - 1L)])
cycle_values <- c(iron_variogram$semivariance[peak_row], iron_variogram$semivariance[cycle_row])
pitard_cycle <- data.frame(
  x = iron_variogram$lag_time[cycle_row],
  ymin = mean(c(iron_components$V0, iron_components$cycle_peak_ordinate)) -
    iron_components$cycle_contribution / 2,
  ymax = mean(c(iron_components$V0, iron_components$cycle_peak_ordinate)) +
    iron_components$cycle_contribution / 2
)
pitard_plot <- ggplot2::ggplot(
  iron_variogram,
  ggplot2::aes(lag_time, semivariance, alpha = eligible)
) +
  ggplot2::geom_line(colour = "#b35c2e") +
  ggplot2::geom_point(colour = "#b35c2e") +
  ggplot2::geom_hline(
    data = pitard_lines,
    ggplot2::aes(yintercept = value, colour = label),
    linetype = "dashed", inherit.aes = FALSE
  ) +
  ggplot2::scale_colour_manual(
    values = c("V(0)" = "#D55E00", "V(1)" = "#009E73", sill = "#0072B2"),
    name = NULL
  ) +
  ggplot2::geom_vline(
    xintercept = pitard_cycle$x, linetype = "dotted", colour = "#CC79A7"
  ) +
  ggplot2::geom_segment(
    data = pitard_cycle,
    ggplot2::aes(x = x, xend = x, y = ymin, yend = ymax),
    inherit.aes = FALSE, colour = "#CC79A7", linewidth = 1.1
  ) +
  ggplot2::annotate(
    "text", x = pitard_cycle$x, y = mean(cycle_values),
    label = "selected cyclic contribution", colour = "#CC79A7", hjust = -0.02
  ) +
  ggplot2::scale_alpha_manual(values = c(.35, 1), guide = "none") +
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = "Pitard-style variance interpretation",
    x = "Lag time (seconds)", y = "% Fe squared"
  )
ggplot2::ggsave(
  file.path("man", "figures", "readme-pitard-variogram.png"),
  pitard_plot, width = 9, height = 5, units = "in", dpi = 180, bg = "white"
)

copper <- simulate_chronostatistics(
  n = 217, interval = 3 * 3600, mean = 10, trend = 0, ar = 0.6,
  innovation_sd = 1.2, cycle_period = 19, cycle_amplitude = 1.7,
  measurement_sd = 1.1, events = FALSE, seed = 351
)
copper_data <- chrono_data(
  copper, time, value, interval = 3 * 3600, units = "% Cu",
  target = 10, lower_spec = 7, upper_spec = 14
)
copper_variogram <- chrono_variogram(copper_data, max_lag = 60, min_pairs = 30)
copper_components <- chrono_components(
  copper_variogram, cycle_lag = 19, cycle_peak_lag = 13
)
copper_ma <- as.numeric(stats::filter(copper_data$value, rep(1 / 5, 5), sides = 2))
copper_plot_data <- copper_data
copper_plot_data$moving_average_5 <- copper_ma
copper_top <- ggplot2::ggplot(copper_plot_data, ggplot2::aes(time, value)) +
  ggplot2::geom_line(colour = "#245b75", na.rm = TRUE) +
  ggplot2::geom_line(
    ggplot2::aes(y = moving_average_5), colour = "#b35c2e", linewidth = 0.8,
    na.rm = TRUE
  ) +
  ggplot2::geom_hline(
    data = data.frame(y = c(7, 10, 14), label = c("specification", "target", "specification")),
    ggplot2::aes(yintercept = y, linetype = label), inherit.aes = FALSE,
    colour = "#555555"
  ) +
  ggplot2::scale_linetype_manual(values = c(specification = "dashed", target = "dotted")) +
  ggplot2::theme_bw() + ggplot2::theme(legend.position = "bottom") +
  ggplot2::labs(title = "Synthetic copper-slag process", x = "Time (3-hour samples)",
                y = "% Cu", linetype = NULL)
copper_lines <- data.frame(
  value = c(copper_components$V0, copper_components$V1, copper_components$sill),
  label = c("V(0)", "V(1)", "sill")
)
copper_bottom <- ggplot2::ggplot(
  copper_variogram, ggplot2::aes(lag, semivariance)
) +
  ggplot2::geom_line(colour = "#b35c2e") +
  ggplot2::geom_point(colour = "#b35c2e", size = 0.7) +
  ggplot2::geom_hline(
    data = copper_lines,
    ggplot2::aes(yintercept = value, colour = label),
    linetype = "dashed", inherit.aes = FALSE
  ) +
  ggplot2::scale_colour_manual(
    values = c("V(0)" = "#D55E00", "V(1)" = "#009E73", sill = "#0072B2"),
    name = NULL
  ) +
  ggplot2::geom_vline(xintercept = 19, linetype = "dotted", colour = "#CC79A7") +
  ggplot2::theme_bw() +
  ggplot2::labs(title = "Short- and long-range variogram interpretation",
                x = "Lag (3-hour samples)", y = "% Cu squared")
draw_two_panels(
  file.path("man", "figures", "readme-copper-slag.png"),
  copper_top, copper_bottom
)

plant <- simulate_plant_process(n = 24 * 90, seed = 20260717)
plant$moving_average <- as.numeric(stats::filter(
  plant$product_fe, rep(1 / 24, 24), sides = 1
))

plant_top <- ggplot2::ggplot(plant, ggplot2::aes(timestamp, throughput_tph)) +
  ggplot2::geom_line(colour = "#27705f") +
  ggplot2::geom_point(data = plant[plant$availability == 0, , drop = FALSE],
                      colour = "#bb4444", size = 0.5) +
  ggplot2::theme_bw() + ggplot2::labs(
    title = "Synthetic plant process with operating interruptions",
    x = "Time", y = "Throughput (t/h)"
  )

plant_bottom <- ggplot2::ggplot(plant, ggplot2::aes(timestamp)) +
  ggplot2::geom_line(ggplot2::aes(y = product_fe), colour = "#245b75", na.rm = TRUE) +
  ggplot2::geom_line(ggplot2::aes(y = moving_average), colour = "#b35c2e",
                     linewidth = 0.8, na.rm = TRUE) +
  ggplot2::theme_bw() + ggplot2::labs(title = "Delayed product quality",
                                      x = "Time", y = "% Fe")

draw_two_panels(file.path("man", "figures", "readme-plant-process.png"),
                plant_top, plant_bottom)
