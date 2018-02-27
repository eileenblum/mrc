#devtools::install_github("jvcasillas/untidydata")
#devtools::install_github("yihui/xaringan")
library(untidydata)
library(xaringan)
library(plot3D)
library(tidyverse)

str(language_diversity)
head(language_diversity)
unique(language_diversity$Measurement)

ld <- language_diversity %>%
  filter(., Continent == 'Africa') %>% 
  spread(., key = Measurement, value = Value) %>%
  select(., country = Country, pop = Population, area = Area, lang = Langs) %>%
  mutate(., logArea = log(area), logPop = log(pop))
View(ld)


ld %>%
  ggplot(., aes(x = pop, y = lang)) +
  geom_point()
ld %>%
  ggplot(., aes(x = area, y = lang)) +
  geom_point()
hist(ld$area)
hist(log(ld$area))
hist(ld$pop)
hist(log(ld$pop))

ld %>%
  ggplot(., aes(x = logArea, y = lang)) +
  geom_point()
ld %>%
  ggplot(., aes(x = logPop, y = lang, color = logArea)) +
  geom_point()


my_mod <- lm(lang ~ logPop + logArea, data = ld)
my_int <- lm(lang ~ logPop + logArea + logPop:logArea, data = ld)
my_int <- lm(lang ~ logPop * logArea, data = ld)
summary(my_mod)
summary(my_int)