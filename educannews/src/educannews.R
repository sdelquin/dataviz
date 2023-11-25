library(tidyverse)
library(ggrepel)

PLOT_FONT <- 'Roboto'
ANNOT_FONT <- 'DiamondGirl'
LABEL_FONT <- 'Roboto'
TITLE_FONT <- 'Cubano'

df <- read_csv('../data/educannews.csv', col_types = '?i') |>
  mutate(fecha = parse_date(fecha, '%d/%m/%Y'))

df |>
  ggplot(aes(x = fecha, y = suscriptores)) +
  geom_line(color = 'red', size = 1) +
  geom_point(fill = 'white', color = 'darkred', size = 2, shape = 21, stroke = 2) +
  geom_label_repel(
    aes(label = suscriptores),
    family = LABEL_FONT,
    nudge_y = 450,
    size = 3.2,
    alpha = .7,
    segment.linetype = 3,
    color = 'red'
  ) +
  geom_label_repel(
    aes(label = format(fecha, '%d-%m-%Y')),
    family = LABEL_FONT,
    nudge_x = -140,
    size = 3.2,
    alpha = .3,
    segment.linetype = 3,
    color = 'blue'
  ) +
  geom_area(fill = 'red', alpha = 0.1) +
  geom_text_repel(
    data = data.frame(fecha = dmy(210723), suscriptores = 3200),
    label = 'Lanzamiento de\nofertas de nombramientos',
    point.padding = 1,
    nudge_x = -300,
    nudge_y = 500,
    segment.curvature = 0.3,
    arrow = arrow(length = unit(.02, "npc")),
    family = ANNOT_FONT
  ) +
  geom_segment(
    aes(x = fecha, y = 0, xend = fecha, yend = suscriptores),
    linetype = 2,
    alpha = .3,
  ) +
  scale_x_date(breaks = '4 months', date_labels = '%d-%m-%Y', minor_breaks = '1 month') +
  labs(
    title = "Canal Telegram Novedades Consejería de Educación (GOBCAN) [No oficial]",
    subtitle = "Evolución de suscriptores/as (@educannews)",
    caption = "Elaborado por @sdelquin | Origen de datos: Seguimiento individual"
  ) +
  theme_bw() +
  theme(
    text = element_text(family = PLOT_FONT),
    plot.title = element_text(family = TITLE_FONT, size = 15, color = 'grey30'),
    plot.subtitle = element_text(margin = margin(b = 20), color = 'gray30'),
    plot.caption = element_text(margin = margin(t = 20), color = 'gray30'),
    plot.margin = margin(.5, .5, .5, .5, 'cm'),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10),
    axis.title = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )

