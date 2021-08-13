library(tidyverse)
library(tidytuesdayR)
library(skimr)

# Load data --------------------------------------------------------------------
tt_data <- tt_load("2021-07-27")
df <- tt_data$olympics
regions <- tt_data$regions

# Explore ----------------------------------------------------------------------

skim_without_charts(df)
# Incomplete variables : medal, age, height, weight
# sex : only M F
# age: 0-97  ! interesting, median age 24
# plot the 42 cities?


# Plot 1 -----------------------------------------------------------------------

df %>% 
  filter(year >= 1948, season == "Summer") %>% 
  drop_na() %>% 
  ggplot(aes(x=sex, y=age, color = sex)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2, size = 0.3, shape = 1)+
  facet_wrap(~medal)+
  theme_minimal()+
  theme(legend.position = "none",
        plot.title = element_text(size = 22),
        plot.caption = element_text(size = 22),
        plot.subtitle = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        strip.text = element_text(size = 20)) +
  labs(x = "",
       y = "Age",
       title = "Is there any difference between Gold, Silver and Bronze medallists?",
       subtitle = "Nope! The median Male winner is older than the female, but age isn't a predictor of medal",
       caption = "Visualization: Neelima-J | Source: Kaggle | #TidyTuesday") 

# Plot 2 -----------------------------------------------------------------------

df %>% 
  filter( !is.na(age), season == "Summer") %>% 
  group_by(year,sex) %>% 
  summarise(age = median(age)) %>% 
  ggplot(aes(x=year, y= age, group = sex, color = sex)) +
  geom_line() +
  theme_minimal()+
  theme(legend.position = "none") +
  labs(x = "",
       y = "",
       title = "Is there any difference in median age of each gender?",
       subtitle = "Yes, women are younger, but the gap is narrowing",
       caption = "Visualization: Neelima-J | Source: Kaggle | #TidyTuesday") +
  annotate("text",x = 2020, y = 26.25, label = "Male")+
  annotate("text",x = 2022, y = 24.75, label = "Female") +
  annotate("text", x=1930, y = 54, size = 3,
           label = "Team USA Archers were aged 63, 58, 55, 44 & 24")
  
# who were the women in 1904?
# USA Archery
  
