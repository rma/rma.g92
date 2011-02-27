#
# This file contains a custom theme (Zenburn) for the ggplot2 library.
#
# The colours in this theme are based on the original Zenburn theme for VIM
# (http://slinky.imukuppi.org/zenburnpage/) and the updated Emacs versions
# (http://www.emacswiki.org/emacs/ColorThemeZenburn).
#
# Instructions for ggplot2 theme creation were taken from:
#   https://github.com/hadley/ggplot2/wiki/User-contributed-themes
#

# The default theme (theme_bw) from ggplot2.
theme_test <- function(base_size = 12) {
  small_size <- base_size * 0.8
  large_size <- base_size * 1.2

  structure(list(
    axis.line = theme_blank(),
    axis.text.x = theme_text(size = small_size, lineheight = 0.9, vjust = 1),
    axis.text.y = theme_text(size = small_size, lineheight = 0.9, hjust = 1),
    axis.title.x = theme_text(size = base_size, vjust = 1),
    axis.title.y = theme_text(size = base_size, angle = 90, vjust = 0.5),

    axis.ticks = theme_segment(colour = "black", size = 0.2),
    axis.ticks.length = unit(0.3, "lines"),
    axis.ticks.margin = unit(0.5, "lines"),

    legend.background = theme_rect(colour = NA),
    legend.key = theme_rect(colour = "grey80"),
    legend.key.size = unit(1.2, "lines"),
    legend.text = theme_text(size = small_size),
    legend.title = theme_text(size = small_size, face = "bold", hjust = 0),
    legend.position = "right",

    panel.background = theme_rect(fill = "white", colour = NA),
    panel.border = theme_rect(fill = NA, colour = "grey50"),
    panel.grid.major = theme_line(colour = "grey90", size = 0.2),
    panel.grid.minor = theme_line(colour = "grey98", size = 0.5),
    panel.margin = unit(0.25, "lines"),

    strip.background = theme_rect(fill = "grey80", colour = "grey50"),
    strip.text.x = theme_text(size = small_size),
    strip.text.y = theme_text(size = small_size, angle = -90),

    plot.background = theme_rect(colour = NA),
    plot.title = theme_text(size = large_size),
    plot.margin = unit(c(1, 1, 0.5, 0.5), "lines")),
  class = "options")
}

# You can copy the default theme and modify it to create a new theme,
zenburn_theme <- function(base_size = 12){
  # TODO -- set the zenburn colours
  modifyList(theme_bw(base_size), list(axis.ticks.length = unit(0.1,"line")))
}
