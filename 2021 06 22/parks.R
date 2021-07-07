library(tidyverse)
library(ggplot2)
library(tidytuesdayR)
library(skimr)
library(ggbump)

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




# Cleaning ---------------------------------------------------------------------
#' TODO: Washington DC typo
df$city <- str_replace(df$city, "^[wW]ashington.*", "Washington D.C.")

#' TODO: percentage variable are char
df <- df %>% 
  mutate(park_pct_city_data = parse_number(park_pct_city_data),
         pct_near_park_data = parse_number(pct_near_park_data),
         spend_per_resident_data = parse_number(spend_per_resident_data)) %>% 
  mutate(across(where(is.character), factor)) %>% 
  select(-city_dup)

# Extracting Seattle New York---------------------------------------------------
df_choose <- df %>%
  subset(city %in% c("New York", "Seattle"))


# EXPLORATION ------------------------------------------------------------------

#' Top ranks usual suspects
top <- df %>% 
  filter(rank < 11) %>% 
  select(year, city) %>% 
  group_by(city) %>% 
  summarise (year_l = list(year))


# Extracting Seattle New York---------------------------------------------------
df <- df %>%
  subset(city %in% c("New York", "Seattle"))



# PLOT -------------------------------------------------------------------------
p1 <- ggplot()+
  geom_bump(data = df, aes(x=year,y=rank,group = city),color = "grey")+
  geom_bump(data = df_choose, aes(x=year,y=rank,group = city,color=city))+
  geom_point(data = df_choose, aes(x=year,y=rank,group = city,color=city))+
  scale_y_reverse()   #Because a lower rank is better
  
p1 + theme_minimal() +
  labs(title = "My brother moved from New York to Seattle",
       subtitle = "City rankings: ParkScore index, a comparison of park systems across the 100 most populated cities in the United States. 
       The index measures park systems according to access, investment, amenities, acreage, equity",
       y = "Rank",
       x = " ",
       caption = "Visualization: Neelima-J | Source: Trust for Public Land ParkScore index| #TidyTuesday")+
  theme(
    plot.title = element_text(
      color = "dark grey",
      size = 12,
      hjust = 1
    ),
    plot.caption = element_text(color = "black"),
    legend.position = 'none',
    panel.border = element_blank()
  )+
  annotate("text",
           x = 2020.5,
           y = 7,
           label = "New York",
           color = "Red") +
  annotate("text",
           x = 2020.5,
           y = 15,
           label = "Seattle",
           color = "dark Cyan")
