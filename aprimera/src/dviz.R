library(tidyverse)
library(showtext)

here::i_am('dviz.R')

# Init theme --------------------------------------------------------------
showtext_auto()
font_add_google('Bitter')
theme_set(theme_minimal(base_family = 'Bitter'))


df <- read_csv('../data/data.csv') %>%
  mutate(
    team = fct_inorder(
      recode(team,
             TEN = 'Tenerife',
             GIR = 'Girona',
             LPA = 'Las Palmas',
             OVI = 'Oviedo')
    ),
    result = factor(result),
    position = factor(position)
  ) %>%
  group_by(team, position) %>%
  summarise(n = n()) %>%
  mutate(prob = n / sum(n))

df %>%
  ggplot(aes(x = position, y = prob, fill = position)) +
    geom_col(position = position_dodge2()) +
    geom_text(aes(label = scales::percent(prob)), position = position_dodge2(0.9), vjust = 1.4, size = 3.2, color = 'white') +
    scale_y_continuous(labels = scales::percent) +
    #viridis::scale_fill_viridis(option = 'inferno', discrete = T) +
    scale_fill_discrete(type = viridis::plasma(4, begin = .20, end = .80)) +
    lemon::facet_rep_wrap(~team, repeat.tick.labels = 'bottom') +
    labs(
      x = NULL, y = NULL,
      title = 'Puestos de playoff a Liga Santander',
      subtitle = 'Probabilidades de terminar en puestos de playoff según combinatoria',
      caption = 'Visualización por @sdelquin  |  Datos: Calculados de forma automatizada',
      fill = 'Puesto'
    ) +
    theme(
      legend.position = c(.5, .9),
      legend.direction = 'horizontal',
      panel.grid = element_blank(),
      plot.title = element_text(size = 26, hjust = 0, face = 'bold'),
      plot.subtitle = element_text(size = 14, hjust = 0, color = 'gray50', face = 'bold'),
      plot.caption = element_text(size = 10, color = 'gray40'),
      axis.text.x = element_text(size = 10, vjust = 3, color = 'gray10'),
      axis.text.y = element_blank(),
      strip.text.x = element_text(size = 14, face = 'bold'),
      strip.background.x = element_rect(fill = 'gray90', color = 'white')
    )

ggsave(here::here('../plots/puestos_playoff.pdf'), width = 8, height = 8, device = cairo_pdf)
ggsave(here::here('../plots/puestos_playoff.jpg'), width = 8, height = 9)
