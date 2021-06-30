library(tidyverse)
library(ggplot2)
library(tidytuesdayR)
library(skimr)

# Load data -----------------------------------------
tt_data <- tt_load("2021-06-08")
df <- tt_data$fishing

# Examine data -----------------------------------------
skim_without_charts(df)
unique(df$region)

# USA vs Canada over the years -----------------------------------------
canada <- df %>%
  subset(region %in% c("Canada (ONT)", "Total Canada (ONT)")) %>%
  select(-grand_total,-comments) %>%
  drop_na() %>%
  group_by(year, lake, species) %>%
  summarise(fish = sum(values))
canada$country <- "Canada"

usa <- df %>%
  subset(region %in% c("U.S. Total (NY)",
                       "U.S. Total (MI)",
                       "U.S. Total")) %>%
  select(-grand_total, -comments) %>%
  drop_na() %>%
  group_by(year, lake) %>%
  summarise(fish = sum(values))
usa$country <- "USA"


all_fish <- rbind(usa, canada)

p <- all_fish %>%
  filter(year > 1915) %>%
  group_by(year, country) %>%
  summarise(n = sum(fish)) %>%
  mutate(percentage = n * 100 / sum(n)) %>%
  ggplot(aes(x = year, y = percentage, fill = country)) +
  geom_area(alpha = 0.6 ,
            size = 1,
            colour = "white")

p +  labs(title = "100 years of Fishing the Great Lakes: 1916 to 2015",
          fill = "",
          caption = "Visualization: Neelima-J | Source: Great Lakes Fishery Commission | #TidyTuesday") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +
  annotate("text",
           x = 2000,
           y = 75,
           label = "Canada") +
  annotate("text",
           x = 2000,
           y = 15,
           label = "USA") +
  
  theme(
    plot.title = element_text(
      color = "dark grey",
      size = 12,
      hjust = 1
    ),
    plot.caption = element_text(color = "black"),
    legend.position = 'none',
    panel.border = element_blank()
  ) +
  labs(x = "", y = "%")
