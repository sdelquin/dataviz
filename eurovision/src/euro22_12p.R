library(tidyverse)
library(ggalluvial)
library(showtext)

here::i_am('euro22_12p.R')

# Init theme --------------------------------------------------------------
showtext_auto()
font_add_google('Bitter')
theme_set(theme_minimal(base_family = 'Bitter'))

# Load data ---------------------------------------------------------------
df <- read_csv(here::here('../data/eurovision-votes.csv')) %>%
  filter(
    year == 2022,
    points == 12,
    jury_or_televoting == 'J',
    semi_final == 'f'
  )

# Plot data ---------------------------------------------------------------
df %>%
  ggplot(aes(axis1 = from_country, axis2 = to_country, y = points)) +
    geom_alluvium(aes(fill = to_country), alpha = 0.75) +
    geom_stratum(aes(fill = to_country, color = to_country)) +
    geom_text(stat = 'stratum', aes(label = after_stat(stratum), family = 'Bitter'), color = 'gray15') +
    scale_x_discrete(limits = c('From', 'To'), expand = c(.1, .1)) +
    labs(
      x = NULL, y = NULL,
      title = 'Our 12 points go to...',
      subtitle = '2022 Eurovision song contest. Countries giving 12 points (Final | Jury voting)',
      caption = 'Visualization by @sdelquin  |  Data: TidyTuesday (R for Data Science)'
    ) +
    theme(
      legend.position = 'none',
      panel.grid = element_blank(),
      plot.title = element_text(size = 26, hjust = 0, face = 'bold'),
      plot.subtitle = element_text(size = 14, hjust = 0, color = 'gray50', face = 'bold'),
      plot.caption = element_text(size = 10, color = 'gray40'),
      axis.text.x = element_text(face = 'bold', size = 14, vjust = 4, color = 'gray30'),
      axis.text.y = element_blank()
    )
  
ggsave(here::here('eurovision/plots/euro22_12p.pdf'), width = 8, height = 9, device = cairo_pdf)
ggsave(here::here('eurovision/plots/euro22_12p.jpg'), width = 8, height = 9)
