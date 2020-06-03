library(gapminder)
library(dplyr)
library(ggplot2)

#Asignamos una varibale
gapminder_2007 <- gapminder %>% 
  filter(year == 2007)
gapminder_2007
#ggplot(datos,aes(x,y (variables respectivas))) 
#aes() podemos llamar color, tamaño, coordenadas
ggplot(gapminder_2007, aes(x= gdpPercap, y = lifeExp)) +
  geom_point()
#Para poner una escala log10 +scale_x_log10()
#ggplot diseñado para funcionar en capas
#base de datos, coordenadas, sumamos cosas (queremos puntos, escala)

ggplot(gapminder_2007, aes(x= gdpPercap, y = lifeExp)) +
  geom_point() + scale_x_log10()

#Color por contiente
ggplot(gapminder_2007, aes(x= gdpPercap, y = lifeExp, color = continent)) +
  geom_point() + scale_x_log10()
#Dividido por continente y a la población que tiene
ggplot(gapminder_2007, aes(x= gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point() + scale_x_log10()

#Facet_wrap graficar subplots, 
#de acuerdo a las categorías que existen en continenteen
# Por contiente, 5 sublopts para su variable de población de life exp
gapminder_1952 <- gapminder %>%
  filter(year == 1952)
ggplot(gapminder_1952, aes(x= pop, y = lifeExp)) +
  geom_point() + scale_x_log10() + facet_wrap(~ continent)

# Ejercicio:
# Scatter plot comparing gdpPercap and lifeExp, with color representing continent
# and size representing population, faceted by year
ggplot(gapminder,aes(x=gdpPercap, y =lifeExp, color = continent, size =pop)) +
  geom_point() + scale_x_log10() + facet_wrap( ~ year)
