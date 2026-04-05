library(tidyverse)
library(rvest)
library(furrr)
library(ggiraph)
library(scales)

plan(multisession, workers = 8)

get_page <- function(url) {
  read_html(url) |>
    html_elements('div[data-elementor-type="loop-item"]') |>
    map_df(
      \(item) {
        features <- item |>
          html_elements("div.eael-infobox")
        tibble(
          vehicle = item |> html_element("h2") |> html_text2(),
          price = item |> html_element("div.jet-listing-dynamic-field__content") |> html_text2(),
          autonomy = features[[1]] |> html_element("span.archive-vehic-feature-data") |> html_text2(),
          battery = features[[2]] |> html_element("span.archive-vehic-feature-data") |> html_text2(),
          charge = features[[3]] |> html_element("span.archive-vehic-feature-data") |> html_text2(),
        )
      }
    )
}

base_url <- "https://todoselectricos.com/"

items <- read_html(base_url) |>
  html_elements('a.page-numbers:not(.next)') |>
  tail(1) |>
  html_attr("href") |>
  str_extract("/([^/]+)=(.*)", group = c(1, 2))

page_url <- paste0(base_url, paste0(items[1], "="))
num_pages <- items[2]
urls <- paste0(base, 1:num_pages)

df_raw <- future_map_dfr(urls, \(url) {
  get_page(url)
})

df <- df_raw |>
  extract(
    vehicle,
    into = c("manufacturer", "model"),
    regex = "(\\S+)\\s*(.*)"
  ) |>
  mutate(
    price = parse_number(price, locale = locale(grouping_mark = ".")),
    price = na_if(price, 0),
    autonomy = parse_number(autonomy),
    battery = parse_number(battery),
    charge = parse_number(str_extract(charge, "^\\s*(\\d+)")),
    manufacturer = factor(str_to_upper(manufacturer)),
    display = paste(manufacturer, model, paste0(price, "€"), sep = "<br>")
  )

p <- df |>
  filter(price < 50000) |>
  ggplot(
    aes(
      x = autonomy,
      y = price
    )) +
    geom_point_interactive(aes(tooltip = display)) +
    geom_smooth() +
    scale_x_continuous(
      breaks = breaks_width(100),
      labels = label_number(suffix = " km")
    ) +
    scale_y_continuous(
      # breaks = breaks_width(5000),
      labels = label_number(suffix = " €", big.mark = "")
    ) +
    labs(
      title = "Precio vs Autonomía",
      subtitle = "Vehículos 100% eléctricos",
      caption = "Datos extraidos de todoselectricos.com",
      x = NULL,
      y = NULL,
    ) +
    theme_minimal() +
    theme(
      plot.caption = element_text(size = 7, margin = margin(t = 30), color = "gray50")
    )
  
girafe(p)
