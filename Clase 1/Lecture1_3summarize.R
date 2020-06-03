library(gapminder)
library(dplyr)
#summarize para aplicar operaciones
#promedio de vida del año 2007
gapminder %>%
  filter(year == 2007) %>%
  summarize(meanLifeExp = mean(lifeExp))
#promedio del vidal del año 2007, total de la población
#Ojo as.numeric(pop) warning porque el número es muy grande
gapminder %>%
  filter(year == 2007) %>%
  summarize(meanLifeExp = mean(lifeExp), totalPop = sum(as.numeric(pop)))

#Ejercicios
# Summarize to find the median life expectancy
gapminder %>% 
  summarize(medianLifeExp = median(lifeExp))
# Filter for 1957 then summarize the median life expectancy
gapminder %>% 
  filter(year == 1957) %>% 
  summarize(medianLifeExp = median(lifeExp))
# Filter for 1957 then summarize the median life
# expectancy and the maximum GDP per capita
gapminder %>% 
  filter(year==1957) %>% 
  summarize(medianLifeExp = median(lifeExp), maxgdpPercap = max(gdpPercap))

#####Groupby
# summarizing by year
gapminder %>%
  group_by(year) %>%
  summarize(meanLifeExp = mean(lifeExp), totalPop = sum(as.numeric(pop)))

# summarizing by continent and year
gapminder %>%
  group_by(year, continent) %>%
  summarize(meanLifeExp = mean(lifeExp), totalPop = sum(as.numeric(pop)))


#Ejercicios2
# Find median life expectancy and maximum GDP per capita in each year
gapminder %>% 
  group_by(year) %>% 
  summarize(medianLifeExp = median(lifeExp), maxgdpPercap =max(gdpPercap))
# Find median life expectancy and maximum GDP per capita in each continent in 1957
gapminder %>% 
  filter(year==1957) %>% 
  group_by(continent) %>% 
  summarize(medianLifeExp = median(lifeExp), maxgdpPercap= max(gdpPercap))


# Find median life expectancy and maximum GDP per
# capita in each continent/year combination

gapminder %>% 
  group_by(year, continent) %>% 
  summarize(medianLiefeExp = median(lifeExp), maxgdpPercap = max(gdpPercap))
