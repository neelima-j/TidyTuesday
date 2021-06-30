library(tidyverse)
library(ggplot2)
library(tidytuesdayR)
library(skimr)

# Load data --------------------------------------------------------------------
tt_data <- tt_load("2021-06-22")
df <- tt_data$parks
# This dataset contains data on the components of the Trust for Public Land 
# (TPL) ParkScore index, a comparison of park systems across the 100 most 
# populated cities in the United States. Published annually, the index measures
# park systems according to five categories: access, investment, amenities, 
# acreage, and—new for 2021—equity.
# points are yearly normalized values (higher points = better).
# https://www.tpl.org/parkscore


# Examine data -----------------------------------------------------------------
skim_without_charts(df)
# Spending per resident in points
# park_pct_city_points parkland as percentage of city area points
# pct_near_park_points Percent of residents in city within 10 minute walk to park points
#  med_park_size_points Median park size within city in points (get max min cities)
# park_pct_city_data Parkland as percentage of city area
# spend_per_resident_points Spending per resident in points
# med_park_size_data Median park size within city in acres
# Basketball hoops per 10,000 residents in city (points)
# _data Dog parks per 10,000 residents in city
# Recreation and senior centers per 20,000 residents in city
# Restrooms per 10,000 residents in city
# data vs points
# Splashgrounds and splashpads per 100,000 residents in city (points)
# number of park benches in city




# Seattle vs New York over the years -------------------------------------------
df <- df %>%
  subset(city %in% c("New York", "Seattle"))

df %>% ggplot(aes(x=year,y=rank,group = city,color=city, label = rank))+
  geom_line()+
  geom_point()+
  scale_y_reverse()+
  geom_text(aes(x=year,y=rank-.5))+
  theme_classic()+
  annotate("text",
           x = 2018,
           y = 5,
           label = "New York",
           color = "Red") +
  annotate("text",
           x = 2018,
           y = 13,
           label = "Seattle",
           color = "dark Green")+
  theme(
    plot.title = element_text(
      color = "dark grey",
      size = 12,
      hjust = 1
    ),
    plot.caption = element_text(color = "black"),
    legend.position = 'none',
    panel.border = element_blank()
  )
ggsave("seattle_ny_ranks.png")  
  
#---------------
df %>% ggplot(aes(x=year,y=total_pct,group = city,color=city, label = rank))+
  geom_line()+
  geom_point(color = "white",size=5)+
  geom_text(aes(x=year,y=total_pct))+
 ylim(50,85)+
    theme_classic()+
  labs(title = "Scores are ~constant, Ranks are changing",
       subtitle = "Other cities are improving",
       y = "Percentage score")+
  annotate("label",
           x = 2017,
           y = 76,
           label = "New York",
           color = "Red") +
  annotate("label",
           x = 2017,
           y = 71,
           label = "Seattle",
           color = "Blue")+
  annotate("text",
           x = 2013,
           y = 70,
           label = "|\nRank\n|",
           color = "dark grey") +
  theme(
    plot.title = element_text(
      color = "dark grey",
      size = 12,
      hjust = 1
    ),
    plot.subtitle = element_text(
      color = "dark grey",
      hjust = 1
    ),
    plot.caption = element_text(color = "black"),
    legend.position = 'none',
    panel.border = element_blank(),
    axis.title.x = element_blank()
  )
ggsave("seattle_ny_ranks_2.png")  


df_2020 <- df %>% 
  filter(year == 2020)

df_2020 %>% 
  pivot_longer(
    cols = med_park_size_data :park_benches, 
    names_to = "type", 
    values_to = "values"
    )
#---------------
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
