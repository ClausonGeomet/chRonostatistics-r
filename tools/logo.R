# Recreate the package logo with:
# source("tools/logo.R")

if (!requireNamespace("hexSticker", quietly = TRUE)) {
  stop("Install the hexSticker package before generating the logo.")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Install the ggplot2 package before generating the logo.")
}

logo_data <- data.frame(
  lag = 0:12,
  semivariance = c(
    0.05, 0.18, 0.34, 0.52, 0.68, 0.76, 0.69,
    0.55, 0.43, 0.48, 0.65, 0.83, 0.92
  )
)

variogram_icon <- ggplot2::ggplot(
  logo_data,
  ggplot2::aes(lag, semivariance)
) +
  ggplot2::geom_hline(
    yintercept = 0.72,
    colour = "#8FA7A2",
    linewidth = 0.7,
    linetype = "dashed"
  ) +
  ggplot2::geom_line(
    colour = "#D36B3D",
    linewidth = 2.2,
    lineend = "round"
  ) +
  ggplot2::geom_point(
    colour = "#174F5E",
    fill = "#F4F1E8",
    shape = 21,
    size = 2.8,
    stroke = 0.8
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.margin = grid::unit(c(0, 0, 0, 0), "pt")
  )

dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)

hexSticker::sticker(
  subplot = variogram_icon,
  package = "chRonostatistics",
  p_size = 12.5,
  p_color = "#174F5E",
  p_family = "sans",
  p_y = 1.52,
  s_x = 1,
  s_y = 0.87,
  s_width = 1.35,
  s_height = 0.88,
  h_fill = "#F4F1E8",
  h_color = "#174F5E",
  h_size = 2,
  url = "github.com/ClausonGeomet/chRonostatistics-r",
  u_color = "#174F5E",
  u_size = 2.8,
  filename = "man/figures/logo.png",
  dpi = 300
)
