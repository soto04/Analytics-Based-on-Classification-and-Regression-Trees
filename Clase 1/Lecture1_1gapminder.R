library(tidyverse)
#Ejemplo prueba flor
seq(-3,3,by=.01) %>%
  expand.grid(x=., y=.) %>%
    ggplot(aes(x=(1-x-sin(y^2)), y=(1+y-cos(x^2)))) +
    geom_point(alpha=.05, shape=20, size=0)+
    theme_void()+
    coord_polar()

library(gapminder)
library(dplyr)
options(scipen = 99999)
#Muestra todos los datos
gapminder
# %>% pipe para filtrar los datos sin modificarlos los originales
gapminder %>% 
  filter(year==1957)
gapminder %>% 
  filter(country == 'Austria')
gapminder %>% 
  filter(country == 'China')
#Orden ascendente
gapminder %>% 
  arrange(pop)
#Orden descendente
gapminder %>% 
  arrange(desc(pop))
#Filtrar por año y ordenar por orden descendente población 
gapminder %>% 
  filter(year==1957) %>% 
  arrange(desc(pop))

#Mutate, crear o cambiar una de las columnas
#Dividimos la población entre un millón
gapminder %>% 
  mutate(pop = pop/1000000)

#Crear una columna (nombre diferente)
gapminder %>% 
  mutate(gdp = gdpPercap*pop)

# Use mutate to change lifeExp to be in months
gapminder %>% 
  mutate(lifeExp = lifeExp * 12)
# Use mutate to create a new column called lifeExpMonths
gapminder %>% 
  mutate(lifeExpMonths = lifeExp * 12)

# find the countries with the highest life expectancy,
# in months, in the year 2007
gapminder %>% 
  filter(year==2007) %>% 
  mutate(lifeExpMonths = lifeExp *12) %>% 
  arrange(desc(lifeExpMonths))


