library(tidytuesdayR)
library(tidyverse)
library(ggrepel)
library(gridExtra)
library(cowplot)
# Read data
tuesdata <- tidytuesdayR::tt_load(2022, week = 15)

fuel <- tuesdata$fuel_gdp
fuel <- fuel %>% filter(Year == 2016)

death <- tuesdata$death_source
death <- death %>% filter(Year == 2016)

fuel <- fuel %>% left_join(death, by = c('Entity'='Entity'))

fuel <- rename( fuel, Access = `Access to clean fuels and technologies for cooking (% of population)`,
                GDP = `GDP per capita, PPP (constant 2017 international $)`,
                Population = `Population (historical estimates)`,
                Deaths = `Deaths - Cause: All causes - Risk: Household air pollution from solid fuels - Sex: Both - Age: Age-standardized (Rate)`)

ggplot(fuel, aes(log(GDP), Access, size = Deaths)) + 
  geom_smooth(formula = y ~ log(x), method = 'lm', alpha = 0.3, color = 'plum2', se = FALSE, show.legend = FALSE)+
  geom_point(alpha = 0.7, color = 'navy') +
  ylim(0,100) + ylab('Access to clean fuels and technologies for cooking (% of population)') +
  xlab('GDP per capita (Log scale)') + labs(size = 'Indoor pollution\n death rate') +
  theme(panel.background = element_rect(fill = "lightcyan1",
                                        colour = "lightcyan1",
                                        size = 0.5, linetype = "solid")) +
  ggtitle('Indoor Pollution Worldwide 2016')

# # # # # #My way to make this visualization better
# remove all the NA in the continent column
fuel_new <- tuesdata$fuel_gdp %>% drop_na(Continent)
fuel_new <- fuel_new %>% filter(Year == 2015)
# get the other relevant table
death_new <- tuesdata$death_source %>%
  filter(Year == 2015)
# left join the new table
fuel_complete <- fuel_new %>% 
  left_join(death_new, by = "Entity") 
#rename variables like the original 
fuel_complete <- rename( fuel_complete, Access = `Access to clean fuels and technologies for cooking (% of population)`,
                GDP = `GDP per capita, PPP (constant 2017 international $)`,
                Population = `Population (historical estimates)`,
                Deaths = `Deaths - Cause: All causes - Risk: Household air pollution from solid fuels - Sex: Both - Age: Age-standardized (Rate)`)
# Highlight some countries in the plot
countries <- c("United States", "Nigeria", "Germany", "United Arab Emirates",
               "China", "Australia", "Argentina")
fuel_complete <- fuel_complete %>%
  mutate(label = ifelse(Access == max(Access, na.rm = T) |Access == min(Access, na.rm =T), Entity, ""))
# New plot
par
together_graph <- fuel_complete %>%
  filter(Continent != "Antarctica")%>%
  ggplot(aes(x = log(GDP),
             y = Access,
             color = Continent,
             size = Deaths
             ), show.legend = F) +
  geom_point(alpha = .6)+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("GDP Per Capita(Log Scale)") +
  ylab("% of Population with Access to Clean Resources to Cook")
separated_graph <- fuel_complete %>%
  filter(Continent != "Antarctica")%>%
  ggplot(aes(x = log(GDP),
             y = Access,
             color = Continent,
             size = Deaths),show.legend = F) +
  facet_wrap(~Continent)+
  xlim(0, 120000) +
  geom_point(alpha = .6)+
  theme(axis.text.x = element_text(angle = 90))+
 xlab("GDP Per Capita") +
  ylab("% of Population with Access to Clean Resources to Cook")
# make a title
title <- ggdraw() + draw_label("Asia and Africa Still Suffer from Lack of Basic Resources", fontface='bold')
plots <- grid.arrange(together_graph,
                      separated_graph,
                      nrow = 1,
                      top = "Asia and Africa Still Suffer from Lack of Basic Resources")+
  theme_clean()
plots


together_ <- together_graph + labs(title = "Asia and Africa Still Struggle With Accessing Basic Necessities",
                      subtitle = "GDP and Deaths Seem to be Negatively Correlated") +
  theme_classic()
together_

