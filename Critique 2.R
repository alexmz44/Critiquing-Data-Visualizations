library(tidyverse)
library(tweenr)
library(ggthemes)
library(hrbrthemes)
mortality <- readxl::read_excel(here::here("global_mortality.xlsx"))
glimpse(mortality)

world <- mortality %>% 
  filter(country=="World") %>% 
  select(-country_code) %>% 
  gather(key=disease, value ="percent", -(country:year))
top5_world <- world %>% 
  filter(year==2016) %>% 
  arrange(desc(percent)) %>% 
  top_n(5) 
mortality_edit <- world %>% 
  filter(disease %in% top5_world$disease) %>% 
  select(-country) %>% 
  rename(x=year,y=percent,time=year) %>%
  mutate(ease="linear") 

mortality_tween <- tween_elements(mortality_edit, time="time", group="disease", ease="ease", nframes=150) %>% 
  mutate(year = round(time), disease = .group) %>%
  left_join(world, by=c("disease","year")) %>% 
  mutate(disease=Hmisc::capitalize(str_replace(disease, pattern="_", " ")))

mortality_tween <- mortality_tween%>%
  mutate(maxdeath = ifelse(year == 1990 |year == 2016 , percent, NA)) %>%
  filter(time == 1990.000 | time == 1991.000 | time == 1992.000 | time == 1993.000 | time == 1994.000 | time == 1995.000 |
           time == 1996.000 | time == 1997.000 | time == 1998.000 |time == 1999.000 | time == 2000.000 | time == 2001.000 |
           time == 2002.000 | time == 2003.000 | time == 2004.000 | time == 2005.000 | time == 2006.000 | time == 2007.000 |
           time == 2008.000 | time == 2009.000 | time == 2010.000 | time == 2011.000 | time == 2012.000 | time == 2013.000 |
           time == 2014.000 | time == 2015.000 | time == 2016.000 )
         
mortality_tween%>%
  mutate(maxdeath = ifelse(year == 1990 |year == 2016 , round(percent,1), NA)) %>%
  group_by(disease) %>%
  mutate(med = ifelse(percent == median(percent, na.rm = T), paste(round(percent,1),"%"), NA)) %>%
  ggplot(aes(x=year, y=percent)) +
  geom_point(aes(alpha = .3,
             
             color=disease ),
             show.legend = F)+
  geom_label(aes(label = med,
                 vjust = 0,
                 family = "mono"))+
  geom_line(aes(color=disease), size=1) + theme_clean() + 
  scale_x_continuous(breaks = seq(1990,2016,4)) +
  scale_y_continuous(breaks = seq(0, 35, 5)) +
  ggtitle("Top 5 causes of death in the world") + ylab("% of the Total Deaths") +
  xlab("Year") + scale_colour_discrete(name  ="Disease Group")+ labs(color = "DISEASE GROUP",
                                                                     subtitle = "Annotated Percentages are the Median Percent of Each Group") +
  scale_color_ipsum()
