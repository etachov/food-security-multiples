library(tidyr) 
library(dplyr) 
library(ggplot2) 
library(grid) 

## read in the food security data, which was scraped from a USDA pdf report (http://www.ers.usda.gov/media/1565415/err173.pdf) using Tabula (http://tabula.technology/)
food <- read.csv("food_security_usda.csv", header = T, stringsAsFactors = F)
states <- read.csv("USAbrvs.csv", header = T, stringsAsFactors = F)

## tidy the data by (1) selecting relevant columns (2) converty wide to long (3) filter to rust belt states and (4) adding year
food_vlow <- food %>%
  select(Geo, vlow_13,  vlow_10,  vlow_03) %>%
  gather(Year, Percent, vlow_13:vlow_03) %>% 
  filter(Geo %in% c("OH", "MI", "IN", "WV", "IL", "PA")) %>%
  mutate(Year = as.numeric(paste("20", gsub("vlow_", "", Year), sep = "")))

## convert abbrevs to full state names and remove unneeded rows
food_vlow <- inner_join(food_vlow, states, by = c("Geo" = "ANSI.2."))
x <- food_vlow[ , 1:4]

## ordered state names, which we'll use for the facet grid 
food_vlow$Name <- factor(food_vlow$Name, 
                          ordered = T, 
                          c("Ohio", "Indiana", "Michigan", "West Virginia", "Pennsylvania", "Illinois"))

## minimal themes for chart 
panel_theme <- theme(panel.grid = element_blank(),
                        panel.background = element_blank(),
                        axis.ticks = element_blank()) 

text_theme <-theme(plot.title = element_text(size = 26,
                                                hjust = 0,
                                                vjust = 0,
                                                family = "Garamond", 
                                                face = "bold", 
                                                colour = "#252525")) +
            theme(axis.title = element_blank()) +
            theme(axis.text = element_blank()) 
      
## small multiples with minimal theme
ggplot(food_vlow, aes(x = Year, y = Percent)) + 
  geom_smooth(size = 2, color = "#DD7049") + 
  geom_point(size = 3, color = "#4A4F50") +
  facet_wrap(~ Name, ncol = 3) + 
  ylim(0, 8) + 
  xlim(2002, 2014) +
  panel_theme + 
  text_theme + 
  geom_text(aes(label = Percent), size = 5, vjust = -1.20) + 
  geom_text(aes(label = Year), size = 5, vjust = 2) +
  theme(panel.margin = unit(1.5, "lines")) +
  theme(strip.text = element_text(family = "Avenir Medium", face = "bold", size = 20),
        strip.background = element_blank()) +
  labs(title = "Percent of Population with Very Low Food Security\n") 

ggsave("Food Security.png")

