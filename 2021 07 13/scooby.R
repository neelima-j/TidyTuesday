library(tidyverse)
library(tidytuesdayR)
library(skimr)
library(RColorBrewer)
library(lubridate)

# Load data --------------------------------------------------------------------
tt_data <- tt_load("2021-07-13")
df <- tt_data$scoobydoo

# Cleaning ---------------------------------------------------------------------
df <- df %>%
  mutate(
    imdb = as.numeric(imdb),
    season = as.numeric(season),
    engagement = as.numeric(engagement),
    monster_real = as.logical(monster_real)
  ) %>%
  mutate_at(vars(contains(
    c("caught", "real", "captured", "arrested", "snack", "unmask")
  )),
  as.logical, 0)

# EXPLORATION ------------------------------------------------------------------

skimr::skim_tee(df)
#' 75 columns! I'm not a big enough fan of ScoobyDoo
#' Dates are complete and correctly formatted. That should be fun

# PLOT 1 -----------------------------------------------------------------------
#' When was it groovy? jeepers. jinkies, my_glasses, zoinks

words <- df %>%
  mutate(year = factor(year(date_aired))) %>%
  select(year, zoinks, groovy, jeepers, jinkies, my_glasses, run_time)

words[words == "NULL"] <- "0"

words <- words %>%
  mutate(across(where(is.character), as.numeric)) %>% 
  filter(run_time > 0) %>%
  group_by(year) %>%
  summarise(
    n_groovy = sum(groovy) / sum(run_time), # Calculating words per minute of runtime
    n_zoinks = sum(zoinks) / sum(run_time),
    n_jeepers = sum(jeepers) / sum(run_time),
    n_jinkies = sum(jinkies) / sum(run_time),
    n_my_glasses = sum(my_glasses) / sum(run_time)
  ) %>%
  pivot_longer(!year, names_to = "word", values_to = "count")


p <- words %>%   ggplot(aes( x = year, y = count, fill = word, group = word)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  theme(
    plot.title = element_text(
      color = "dark grey",
      size = 12,
      hjust = 1
    ),
    plot.subtitle = element_text(
      color = "dark grey",
      size = 9,
      hjust = 1
    ),
    axis.title.x = element_text(color = "dark grey"),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(color = "black"),
    panel.border = element_blank(),
    legend.direction = "horizontal",
    legend.position = "top"
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_brewer(palette = "Set2") +
  labs(
    y = "Frequency: Words per minute of runtime",
    x = "",
    title = "When was it Groovy?",
    subtitle = "The retro craze seems to come in waves: The late 90s, and now!",
    caption = "Visualization: Neelima-J | Source: Kaggle | #TidyTuesday"
  )

p + scale_fill_discrete(
  name = "",
  labels = c("Groovy", "Jeepers", "Jinkies",
             "My Glasses", "Zoinks"),
  guide = guide_legend(reverse = TRUE)
)
