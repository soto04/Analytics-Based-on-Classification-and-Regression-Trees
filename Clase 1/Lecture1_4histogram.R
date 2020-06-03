library(gapminder)
library(dplyr)
library(ggplot2)


gapminder_2007 <- gapminder %>% 
  filter(year == 2007)
gapminder_2007
#binwidth en este caso podemos controlar el tamaño de la caja por año
#aes la variable que vamos a llamar
ggplot(gapminder_2007, aes(x = lifeExp)) +
  geom_histogram(binwidth = 2) # + scale_x_log10()

# by continent
#gráfica por continente
#para cambiar el color en este caso es fill
by_continent <- gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(meanLifeExp = mean(lifeExp))
ggplot(by_continent, aes(x = continent, y = meanLifeExp, fill = continent)) +
  geom_col()