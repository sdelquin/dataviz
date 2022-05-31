library(tidyverse)
library(showtext)
library(glue)
library(forcats)
library(scales)
library(ggtext)

# Init theme --------------------------------------------------------------
showtext_auto()
font_add_google('Zilla Slab', 'zilla', regular.wt = 500, bold.wt = 700)
theme_set(theme_minimal(base_family = 'zilla'))

# Load data ---------------------------------------------------------------
df <- read_csv('../data/minigolf.csv') %>%
  mutate(
    jugador = factor(jugador)
  ) %>%
  group_by(fecha, jugador) %>%
  arrange(hoyo) %>%
  mutate(
    ratio = cumsum(golpes) / hoyo,
  ) %>%
  ungroup()

# Plot data ---------------------------------------------------------------

# https://coolors.co/25283d-8f3985-98dfea-07beb8-efd9ce
COLOR_LUCIA = '#1F618D'
COLOR_SERGIO = '#F82600'
COLOR_BG = 'gray85'

plot <- function(data, fecha) {
  diff <- data %>%
    group_by(hoyo) %>%
    summarise(
      ymin = min(ratio),
      ymax = max(ratio),
      best = jugador[which.min(ratio)]
    ) %>%
    filter(hoyo < max(hoyo))
    
  data %>%
    ggplot(aes(x = hoyo, y = ratio, color = jugador)) +
    geom_rect(
      data = diff,
      aes(
        xmin = hoyo, xmax = hoyo + 1,
        ymin = ymin, ymax = ymax,
        fill = best, fill = after_scale(colorspace::lighten(fill, .5))),
      inherit.aes = F, alpha = .6
    ) +
    geom_step(size = 1.5) +
    geom_point(size = 2.75) +
    geom_text(
      data = filter(data, hoyo == max(hoyo)),
      aes(label = jugador),
      family = 'zilla', fontface = 'bold', size = 5, hjust = -0.2
    ) +
    geom_text(
      data = filter(data, hoyo == max(hoyo)),
      aes(label = sprintf('%1.2f', ratio)),
      family = 'zilla', nudge_x = -0.45, fontface = 'bold', size = 4.5
    ) +
    scale_x_continuous(breaks = 1:max(data$hoyo), expand = c(0, 0.5)) +
    scale_y_continuous(
      trans = 'reverse',
      breaks = seq(floor(max(data$ratio)), floor(min(data$ratio)), -1),
      expand = c(0, 0)
    ) +
    scale_color_manual(values = c(COLOR_LUCIA, COLOR_SERGIO)) +
    scale_fill_manual(values = c(COLOR_LUCIA, COLOR_SERGIO), drop = F) +
    coord_cartesian(clip = 'off') +
    labs(
      title = glue('Minigolf <span style="color: {COLOR_LUCIA}"> Lucía</span> vs <span style="color: {COLOR_SERGIO}">Sergio</span>'),
      subtitle = glue('Partida en {data$lugar} el día {format(fecha, "%d de %B de %Y")}'),
      caption = 'Visualización: @sdelquin | Datos: Recogida en campo',
      x = 'Hoyo',
      y = 'Golpes por Hoyo'
    ) +
    theme(
      plot.background = element_rect(fill = COLOR_BG),
      plot.title.position = 'plot',
      plot.caption.position = 'plot',
      plot.title = element_markdown(size = 25, face = 'bold'),
      plot.subtitle = element_markdown(size = 18, face = 'bold', color = 'gray40', margin = margin(b = 20)),
      plot.caption = element_text(size = 13, color = 'gray30', margin = margin(t = 25)),
      axis.title.x = element_text(margin = margin(t = 15), size = 13, color = 'gray20'),
      axis.title.y = element_text(margin = margin(r = 10), size = 13, color = 'gray20'),
      axis.text.x = element_text(margin = margin(t = 10), size = 11),
      axis.text.y = element_text(margin = margin(r = 5), size = 12, face = 'bold'),
      panel.grid = element_line(color = 'gray60'),
      panel.grid.major.y = element_line(linetype = 'dotted'),
      legend.position = 'none',
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 10, r = 50, b = 20, l = 20)
    )
  ggsave(glue('../plots/minigolf_{fecha}.pdf'), width = 12, height = 8, device = cairo_pdf)
  ggsave(glue('../plots/minigolf_{fecha}.jpg'), width = 12, height = 8)
}

# Just for testing purposes
df %>%
  # filter(fecha == '2022-05-21') %>%
  filter(fecha == '2018-07-24') %>%
  plot(fecha = .$fecha)

df %>%
  group_by(fecha) %>%
  group_walk(~ plot(.x, .y$fecha))
  
