#install.packages("tidyverse")
library(tidyverse)
library(readxl)

# read in data
income <- read_xlsx("income.xlsx")
lifeex <- read_xlsx("life_expectancy.xlsx")

# clean income data
income2015 <- select(income,'GDP per capita',`2015.0`)
income2015 <- filter(income2015,!is.na(`2015.0`))
income2015 <- rename(income2015,country = `GDP per capita`,GDP=`2015.0`)

# clean life expectancy data
lifeex2015 <- select(lifeex,'Life expectancy',`2015.0`)
lifeex2015 <- filter(lifeex2015,!is.na(`2015.0`))
lifeex2015 <- rename(lifeex2015,country = `Life expectancy`,Life_Expectancy=`2015.0`)

# make the join
world_data <- inner_join(lifeex2015,income2015)

# initial plot
ggplot(data=world_data)+
  geom_point(mapping=aes(x=GDP,y=Life_Expectancy))+
  scale_x_log10()

# add in size of country by population
popula <- read_xlsx("population.xlsx")
popula2015 <- select(popula,'Total population',`2015.0`)
popula2015 <- filter(popula2015,!is.na(`2015.0`))
popula2015 <- rename(popula2015,country='Total population',pop=`2015.0`)
world_data <- inner_join(world_data,popula2015)

# next diagnostic plot
ggplot(data=world_data)+
  geom_point(mapping=aes(x=GDP,y=Life_Expectancy,size=pop))+
  scale_x_log10()+
  scale_size_continuous(range = c(1,12))


# add in coloring by continent
#install.packages('countrycode')
library(countrycode)
world_data <- mutate(world_data,continent=countrycode(country,'country.name','continent'))

# make final plot
ggplot(data=world_data)+
  geom_point(mapping=aes(x=GDP,y=Life_Expectancy,size=pop,color=continent))+
  scale_x_log10()+
  scale_size_continuous(range = c(1,12))
