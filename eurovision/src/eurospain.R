library(tidyverse)
library(ggalluvial)
library(showtext)

here::i_am('eurospain.R')

# Init theme --------------------------------------------------------------
showtext_auto()
font_add_google('Bitter')
theme_set(theme_minimal(base_family = 'Bitter'))

# Load data ---------------------------------------------------------------
df_votes <- read_csv(here::here('../data/eurovision-votes.csv'))
df_resul <- read_csv(here::here('../data/eurovision.csv'))

table(df_votes$year)

df_resul %>%
  filter(
    str_detect(section, 'final'),
    artist_country == 'Spain'
  ) %>%
  select(year, artist, country = artist_country, rank) %>%
  left_join(
    df_votes %>%
      filter(
        semi_final == 'f',
        to_country == 'Spain',
        jury_or_televoting == 'J'
      ) %>%
      group_by(year) %>%
      summarise(points = sum(points))
  ) %>% View()


# Plot data ---------------------------------------------------------------
aux %>%
  ggplot(aes(x = year, y = total_points)) +
    geom_col()

    