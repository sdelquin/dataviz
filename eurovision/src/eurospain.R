library(tidyverse)
library(showtext)
library(ggbump)
library(ggrepel)
library(magrittr)
library(ggtext)

# Init theme --------------------------------------------------------------
showtext_auto()
font_add_google('Bree Serif', 'bree')
theme_set(theme_minimal(base_family = 'bree'))

# Load data ---------------------------------------------------------------
df_votes <- read_csv('../data/eurovision-votes.csv')
df_resul <- read_csv('../data/eurovision.csv')

df <- df_resul %>%
  filter(
    str_detect(section, 'final'),
    artist_country == 'Spain'
  ) %>%
  select(year, artist, country = artist_country, rank, points = total_points) %>%
  left_join(
    df_votes %>%
      filter(
        semi_final == 'f',
        to_country == 'Spain',
        jury_or_televoting == 'J'
      ) %>%
      group_by(year) %>%
      summarise(points = sum(points)),
    by = 'year'
  ) %>%
  mutate(
    points = if_else(year %in% 1961:1974, points.x, points.y)
  ) %>%
  select(year, artist, rank, points)

COLOR_M1 = '#F83A8D'
COLOR_M2 = 'gray40'
COLOR_M3 = '#279E8C'
RANK_THRESHOLD = c(3, 23)

df_votes %>%
  filter(country == 'Spain')

# Plot data ---------------------------------------------------------------
df %>%
  mutate(
    color = case_when(
      rank <= RANK_THRESHOLD[1] ~ COLOR_M1,
      rank >= RANK_THRESHOLD[2] ~ COLOR_M3,
      TRUE ~ COLOR_M2
    )
  ) %>%
  ggplot(aes(x = year, y = rank)) +
    geom_hline(aes(yintercept = RANK_THRESHOLD[1]), color = COLOR_M1, linetype = 'dashed') +
    geom_hline(aes(yintercept = RANK_THRESHOLD[2]), color = COLOR_M3, linetype = 'dashed') +
    geom_smooth(se = F, method = 'gam', size = 2, color = '#D3D0CB') +
    geom_bump(size = 1) +
    geom_point(
      size = 3, shape = 21, stroke = 2, fill = 'white',
      aes(color = I(color))
    ) +
    geom_label_repel(
      aes(
        label = artist,
        color = I(color),
        fontface = I(if_else(rank > RANK_THRESHOLD[1] & rank, 'plain', 'bold')),
        size = I(if_else(rank > RANK_THRESHOLD[1], 3.5, 4)),
      ),
      family = 'bree',
      seed = 10
    ) +
    annotate('text', x = 2021, y = 3.5, label = '228p + 231p', family = 'bree', color = COLOR_M1, hjust = 1) +
    annotate('text', x = 1958, y = 0, label = 'Puesto', family = 'bree', hjust = 0.7, vjust = -0.3) +
    coord_cartesian(clip = 'off') +
    scale_x_continuous(expand = c(0, 0), breaks = seq(1960, 2022, 10), limits = c(1958, 2022)) +
    scale_y_continuous(trans = 'reverse', breaks = 1:25, limits = c(25, 0), expand = c(0, 0.5)) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = 'gray80'),
      panel.grid.major.y = element_line(color = 'gray80', linetype = 'dotted'),
      axis.title = element_blank(),
      plot.title.position = 'plot',
      plot.caption.position = 'plot',
      plot.title = element_markdown(size = 30, margin = margin(b = 10)),
      plot.subtitle = element_markdown(size = 14, margin = margin(b = 15), lineheight = 1.2, color = 'gray40'),
      plot.caption = element_markdown(size = 10, margin = margin(t = 20), color = 'gray40'),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 10, margin = margin(l = 8, r = 5)),
      plot.background = element_rect(fill = 'gray95'),
      plot.margin = margin(20, 45, 30, 25)
    ) +
    labs(
      title = glue::glue('El <b style="color: {COLOR_M1}">Chanelazo</b> visto en perspectiva'),
      subtitle = glue::glue('La artista catalana Chanel Terrero consiguió la mejor puntuación',
                            'de la historia de España en Eurovisión<br>',
                            'así como un destacado <b style="color: black">/tercer puesto/</b> tras Ucrania y Reino Unido.'),
      caption = 'Visualización: @sdelquin | Datos: TidyTuesday (R for Data Science)'
    )

ggsave('../plots/eurospain.pdf', width = 12, height = 8, device = cairo_pdf)
ggsave('../plots/eurospain.jpg', width = 12, height = 8)
