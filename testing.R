library(tidyverse)
library(readxl)
file = "CurrencyT.xlsx"
file.1 = read_excel(file)
ggplot(file.1,aes(x=Currency,y=Country))+geom_point()
