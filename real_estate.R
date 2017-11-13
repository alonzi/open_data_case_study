library(tidyverse)
library(readr)
library(lubridate)

# http://opendata.charlottesville.org/datasets/real-estate-sales
real_estate <- read_csv("Real_Estate_Sales.csv")

# remove zeroes
real_estate <- filter(real_estate,SaleAmount>100)

# start in 1970, look for correlation
ggplot(data=filter(real_estate,year(SaleDate)>1969))+
  geom_hex(mapping=aes(x=year(SaleDate),y=SaleAmount))+
  scale_y_log10()+
  labs(x = "year",y="Transaction Ammount [$]")


# look at two slices
year1 <- 2006
year2 <- 2011
ggplot() +
  geom_histogram(data=filter(real_estate,year(SaleDate)==year1),mapping=aes(x=SaleAmount),fill='red',alpha=0.6)+
  geom_histogram(data=filter(real_estate,year(SaleDate)==year2),mapping=aes(x=SaleAmount),fill='blue',alpha=0.6)+
  scale_x_log10()+
  labs(x = "Transaction Ammount [$]",y="Transactions")+
  annotate("text", x = 500000, y = 280,color='red', label = toString(year1))+
  annotate("text", x = 500000, y = 115,color='blue',label = toString(year2))




