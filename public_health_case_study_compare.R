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

income1815 <- select(income,'GDP per capita',`1815.0`)
income1815 <- filter(income1815,!is.na(`1815.0`))
income1815 <- rename(income1815,country = `GDP per capita`,GDP=`1815.0`)

# clean life expectancy data
lifeex2015 <- select(lifeex,'Life expectancy',`2015.0`)
lifeex2015 <- filter(lifeex2015,!is.na(`2015.0`))
lifeex2015 <- rename(lifeex2015,country = `Life expectancy`,Life_Expectancy=`2015.0`)

lifeex1815 <- select(lifeex,'Life expectancy',`1815.0`)
lifeex1815 <- filter(lifeex1815,!is.na(`1815.0`))
lifeex1815 <- rename(lifeex1815,country = `Life expectancy`,Life_Expectancy=`1815.0`)

# make the join
world_data2015 <- inner_join(lifeex2015,income2015)
world_data1815 <- inner_join(lifeex1815,income1815)


# initial plot
ggplot(data=world_data2015)+
  geom_point(mapping=aes(x=GDP,y=Life_Expectancy))+
  scale_x_log10()

# add in size of country by population
popula <- read_xlsx("population.xlsx")
popula2015 <- select(popula,'Total population',`2015.0`)
popula2015 <- filter(popula2015,!is.na(`2015.0`))
popula2015 <- rename(popula2015,country='Total population',pop=`2015.0`)
world_data2015 <- inner_join(world_data2015,popula2015)

popula1815 <- select(popula,'Total population',`1820.0`)
popula1815 <- filter(popula1815,!is.na(`1820.0`))
popula1815 <- rename(popula1815,country='Total population',pop=`1820.0`)
world_data1815 <- inner_join(world_data1815,popula1815)

# next diagnostic plot
ggplot(data=world_data2015)+
  geom_point(mapping=aes(x=GDP,y=Life_Expectancy,size=pop))+
  scale_x_log10()+
  scale_size_continuous(range = c(1,12))


# add in coloring by continent
#install.packages('countrycode')
library(countrycode)
world_data2015 <- mutate(world_data2015,continent=countrycode(country,'country.name','continent'))

world_data1815 <- mutate(world_data1815,continent=countrycode(country,'country.name','continent'))

# make final plots
ggplot(data=world_data2015)+
  geom_point(mapping=aes(x=GDP,y=Life_Expectancy,size=pop,color=continent))+
  scale_size_continuous(range = c(1,12))+
  scale_x_log10(limits = c(250,200000))+
  ylim(10,90)
  
ggplot(data=world_data1815)+
  geom_point(mapping=aes(x=GDP,y=Life_Expectancy,size=pop,color=continent))+
  scale_x_log10(limits = c(250,200000))+
  scale_size_continuous(range = c(1,12))+
  ylim(10,90)


# combined version
ggplot(data=world_data2015)+
  geom_point(mapping=aes(x=GDP,y=Life_Expectancy,size=pop,color=continent))+
  scale_size_continuous(range = c(1,12))+
  geom_point(data=world_data1815,mapping=aes(x=GDP,y=Life_Expectancy,size=pop,color=continent))+
  scale_x_log10(limits = c(250,200000))+
  ylim(10,90)+
  annotate("text", x = 4000, y = 33, label = "1815")+
  annotate("text", x = 700, y = 75, label = "2015")

#Africa
ggplot(data=filter(world_data2015,continent=='Africa'))+
  geom_point(mapping=aes(x=GDP,y=Life_Expectancy,size=pop,color=continent))+
  scale_size_continuous(range = c(1,12))+
  geom_point(data=filter(world_data1815,continent=='Africa'),mapping=aes(x=GDP,y=Life_Expectancy,size=pop,color=continent))+
  scale_x_log10(limits = c(250,200000))+
  ylim(10,90)+
  annotate("text", x = 4000, y = 33, label = "1815")+
  annotate("text", x = 700, y = 75, label = "2015")

#Americas
ggplot(data=filter(world_data2015,continent=='Americas'))+
  geom_point(mapping=aes(x=GDP,y=Life_Expectancy,size=pop,color=continent))+
  scale_size_continuous(range = c(1,12))+
  geom_point(data=filter(world_data1815,continent=='Americas'),mapping=aes(x=GDP,y=Life_Expectancy,size=pop,color=continent))+
  scale_x_log10(limits = c(250,200000))+
  ylim(10,90)+
  annotate("text", x = 4000, y = 33, label = "1815")+
  annotate("text", x = 700, y = 75, label = "2015")

#Asia
ggplot(data=filter(world_data2015,continent=='Asia'))+
  geom_point(mapping=aes(x=GDP,y=Life_Expectancy,size=pop,color=continent))+
  scale_size_continuous(range = c(1,12))+
  geom_point(data=filter(world_data1815,continent=='Asia'),mapping=aes(x=GDP,y=Life_Expectancy,size=pop,color=continent))+
  scale_x_log10(limits = c(250,200000))+
  ylim(10,90)+
  annotate("text", x = 4000, y = 33, label = "1815")+
  annotate("text", x = 700, y = 75, label = "2015")

#Africa and Americas
ggplot(data=filter(world_data2015,continent=='Americas'| continent=='Africa'))+
  geom_point(mapping=aes(x=GDP,y=Life_Expectancy,size=pop,color=continent))+
  scale_size_continuous(range = c(1,12))+
  geom_point(data=filter(world_data1815,continent=='Americas'| continent=='Africa'),mapping=aes(x=GDP,y=Life_Expectancy,size=pop,color=continent))+
  scale_x_log10(limits = c(250,200000))+
  ylim(10,90)+
  annotate("text", x = 4000, y = 33, label = "1815")+
  annotate("text", x = 700, y = 75, label = "2015")
