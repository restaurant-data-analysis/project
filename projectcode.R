# #------INSTALL PACKAGES--------------
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("leaflet")
# install.packages("rworldmap")

#------LOAD DATA------------
library(tidyverse)
library(leaflet)
library(readxl)
library(rworldmap)
library(gridExtra)
library(broom)

###### T I D Y I N G  D A T A ######
#---LOAD FILES-----
CCodesfile="Country-Code.xlsx"
Zomatofile="zomato.xlsx"
CurrencyFile="CurrencyRates.xlsx"

CCodes=read_excel(CCodesfile) #importing country code file
zomato=read_excel(Zomatofile) #importing main zomato file
Currency=read_excel(CurrencyFile) #importing currency file

#------ GET RID OF SOME COLUMNS & ADD NEW ONES -------------
zomato1=zomato %>% select (-c(ID, Rating_Color,Switch_To_Order_Menu,Price_Range,Address,Locality_Verbose,Currency))
#we remove currency column as it is incorrect and contains too many unique symbols

#add in new columns: Country Names according to Country Code & proper currency names with conversion rates
zomato2 = zomato1 %>% left_join(CCodes) 
zomato3 = zomato2 %>% left_join(Currency) 

zomato3 %>% select(Currency, Country) #checking to see if country matched up with new currency values

#add in new column: USD Cost (for equal comparison)
zomato4 = zomato3 %>% mutate(Avg_Cost_USD = Average_Cost_For_Two*`Conversion Rate (USD)`) 
#Multiplied Currency Column by it's conversion rate

#replacing Cuisines col. with Principal_Cuisines (the first category of cuisines for every country row)
zomato5= zomato4 %>% separate(Cuisines,into=c("Principal_Cuisines")) #store "primary" cuisine types into new column and replace old with this

#make a seperate cuisine file for principal cuisine + restaurant
CCFile = zomato5 %>% select(Country, Principal_Cuisines)
head(CCFile)
#---------REMOVE MISSING VALUES----------
#we notice that there are several "0" rated stores for even expensive places so we decided to remove no rating stores
zomato5 %>% count(Aggregate_Rating) # there are 2148 restaurants without any ratings

zomato5[zomato5 == 0] = NA #remove values that have zero
zomato5[zomato5 == "Not rated"] = NA #remove unrated values

#------- GRAPHING DATA ----------
#let's check how pricing & ratings compare using side by side boxplots
zomato5 = zomato5 %>% mutate(Rating_Factor=ordered(Rating_Text,levels=c("Excellent","Very Good","Good","Average","Poor")))

ratingt.cost= ggplot(zomato5,aes(x=Rating_Factor,y=Avg_Cost_USD))+geom_boxplot() #general rating category
ratingt.cost
rating.cost= ggplot(zomato5,aes(x=factor(Aggregate_Rating),y=Avg_Cost_USD))+geom_boxplot() #more in-depth view on ratings (treating rating as factor)
rating.cost
rating.costplot= ggplot(zomato5,aes(x=(Aggregate_Rating),y=Avg_Cost_USD))+geom_point()
rating.costplot



#-------------- REGRESSION (FOR ALL COUNTRIES) ----------
#Counting Y/N columns
zomato5%>% count(Has_Table_Booking)
zomato5%>% count(Has_Online_Delivery)
zomato5%>% count(Is_Delivering_Now) #insignificant number of yes' so we can disregard

## List of Variables that MIGHT affect rating
#price
#location - country
#has online deliver
#has table booking

## we want to run both a linear & multivariable regression with these (weighted by votes)

#### WEIGHTED REGRESSION ####
costreg = lm(Aggregate_Rating~Avg_Cost_USD,data=zomato5,weights = Votes)
summary(costreg)
#check residual fitted plot
costreg.plot = ggplot(data=costreg,aes(y=.resid, x=.fitted))+geom_point()+geom_smooth(se=F)
costreg.plot #not random scatters

countryreg = lm(Aggregate_Rating~Country,data=zomato5,weights = Votes)
summary(countryreg)
#check residual fitted plot
countryreg.plot = ggplot(data=countryreg,aes(y=.resid, x=.fitted))+geom_point()+geom_smooth(se=F)
countryreg.plot #also not random

cuisinereg = lm(Aggregate_Rating~Principal_Cuisines,data=zomato5,weights=Votes)
summary(cuisinereg)
#check residual fitted plot
cuisinereg.plot = ggplot(data=cuisinereg,aes(y=.resid,x=.fitted))+geom_point()+geom_smooth(se=F)
cuisinereg.plot #pretty random and straight!

#multiple regression for: Cost + Cuisine + Country
cccreg = lm(Aggregate_Rating~Avg_Cost_USD + Principal_Cuisines + Country, data = zomato5, weights = Votes) #effect of price on ratings goes down
summary(cccreg)
drop1(cccreg,test="F") #shows how each variable is significant for our regression

#plot residuals
cccreg.plot = ggplot(cccreg, aes(y=.resid,x=.fitted))+geom_point()+geom_smooth(se=F)
cccreg.plot #pretty random BUT kind of below 0 

# REGRESSION FOR BOOKING & DELIVERY (we suspect it won't affect much)

#Regression on Booking
bookingreg = lm(Aggregate_Rating~Has_Table_Booking, zomato5, weights = Votes)
summary(bookingreg) #0.2% R Squared
#run residual plot
bookingreg.plot = ggplot(bookingreg,aes(y=.resid,x=.fitted))+geom_point()+geom_smooth(se=F)
bookingreg.plot #yikes

#Regression on Online Delivery
deliveryreg = lm(Aggregate_Rating~Has_Online_Delivery, zomato5,weights = Votes)
summary(deliveryreg) #better R squared than booking BUT still low (4%)
deliveryreg.plot = ggplot(data=deliveryreg,aes(y=.resid,x=.fitted))+geom_point()+geom_smooth(se=F)
deliveryreg.plot #yikes - find better solution

#boxplot - delivery vs residual
augment(deliveryreg) %>% 
  ggplot(aes(x=Has_Online_Delivery,y=.resid))+geom_boxplot() #median is below zero meaning we may have skewed resids
augment(deliveryreg) %>% ggplot(aes(sample=.resid))+stat_qq()+
  stat_qq_line()+facet_wrap(~Has_Online_Delivery)

##multiple regression (2) - Does booking have any affect on multireg
multireg = lm(Aggregate_Rating~Avg_Cost_USD+Has_Table_Booking+Principal_Cuisines+Country,data=zomato5,weights = Votes)
summary(multireg) #went up from 23.9% to 24.2% - not much help
##multiple regression (2) - Does delivery have any affect on multireg
multireg.2 = lm(Aggregate_Rating~Avg_Cost_USD+Has_Online_Delivery+Principal_Cuisines+Country,data=zomato5,weights = Votes)
summary(multireg.2) #went from 23.9 to 24.3 - not much
#what about both?
multireg.3 = lm(Aggregate_Rating~Avg_Cost_USD+Has_Table_Booking+Has_Online_Delivery+Principal_Cuisines+Country,data=zomato5,weights = Votes)
summary(multireg.3) #went from 23.9 to 24.5 - let's include both then

multireg.plot = ggplot(data=multireg.3,aes(y=.resid,x=.fitted))+geom_point()+geom_smooth(se=F)
multireg.plot #random but a bit below 0
drop1(multireg,test = "F")

##we need to set a limit so predicted ratings do not go above 5
#might help with some of the skewness of our data
zomato6 = zomato5 %>% mutate(Transformed_Rating = log10(Aggregate_Rating/(5-Aggregate_Rating)))
zomato6 %>% select(Aggregate_Rating,Transformed_Rating) #check to see if it worked

#rerun regression on cost
costreg2= lm(Transformed_Rating~Avg_Cost_USD,data=zomato6,weights = Votes)
summary(costreg2) #9.8%
costreg2.plot = ggplot(costreg2,aes(y=.resid,x=.fitted))+geom_point()+geom_smooth(se=F)
#compare residual graphs 
grid.arrange(costreg.plot,costreg2.plot) #not much of a difference

#note that it's relativly random but skewed by outliers
#working with a lot of data so can we disregard outliers -> check density
costreg2.outliers = ggplot(costreg2,aes(y=.resid,x=.fitted))+geom_point(alpha=0.4)+geom_smooth(se=F)
costreg2.outliers #dense of the data is randomly scattered BUT below 0

countryreg2 = lm(Transformed_Rating~Country,data=zomato6,weights=Votes)
summary(countryreg2)
countryreg2.plot = ggplot(countryreg2,aes(y=.resid,x=.fitted))+geom_point()+geom_smooth(se=F)
countryreg2.plot #pretty good minus the massive dip at one point due to missing value
#compare reidual graphs
grid.arrange(countryreg.plot,countryreg2.plot)
#straightened it out a bit more; some dips and bumps but caused by missing values

multireg2 = lm(Transformed_Rating~Avg_Cost_USD+Has_Table_Booking+Has_Online_Delivery+Principal_Cuisines+Country,data=zomato6,weights = Votes)
summary(multireg2) #28% R squared (4% increase)
multireg2.plot = ggplot(data=multireg2,aes(y=.resid,x=.fitted))+geom_point()+geom_smooth(se=F)
#compare residual graphs
grid.arrange(multireg.plot,multireg2.plot) #relatively same
#check if outliers are dense
multireg.outliers = ggplot(data=multireg2,aes(y=.resid,x=.fitted))+geom_point(alpha=0.4)+geom_smooth(se=F)
multireg.outliers #good

#run some more diagnostic graphs/tests
plot(multireg2,1)
#residual vs fitted = relatively random and horizontal line -> linear relation is good
#notice it is below zero tho

plot(multireg2,2)
#QQ Plot = doesn't follow the line that well -> residuals may not be normally dist
#factors to consider: heavy skewness at top due to some large outliers (Same for bottom too)

plot(multireg2,3)
#Scale-Location = not a horizontal line -> heteroscadesticity problem




#-------------MAPPING DATA-----------
#seperate data with only country, restaurant & coordinates
map.data = zomato6 %>% select(Country,Restaurant_Name,Longitude,Latitude)
head(map.data) #check

#general overview map
newmap = getMap(resolution = "li")
plot(newmap)
worldmap = points(map.data$Longitude, map.data$Latitude, col = "red", cex = .6)
worldmap

## LEAFLET MAP - DYNAMIC
#let us take a closer look at the more dense areas
map.data %>% count(Country) #seems to be India & America

#start with America
America = map.data %>% filter(Country == 'United States')
head(America) #check
leaflet(data=America,) %>% addTiles() %>% addCircleMarkers(lng=America$Longitude,lat=America$Latitude,radius = 2,opacity=0.5) %>% setView(-99,39,zoom=4)
#clearly missing information on lots of states

#Check India
India=map.data %>% filter(Country=="India")
leaflet(India) %>% addTiles() %>% addCircleMarkers(lng=India$Longitude,lat=India$Latitude,radius = 2,opacity=0.5) %>% setView(78.9629,20,zoom=4)
#seems to be lot's of restaurants all over India - figures we have 8600+ restaurants (86% of the data is from India)

## ---------- INTERVENTION: Maybe we should focus on just India b/c 86% of the data is pertaning to it-----
#lets make a new datafile
india.data = zomato6 %>% filter(Country == 'India')
head(india.data)


## ------------ GRAPHS (INDIA DATA ONLY)-------------

# ADD 
# CODE
# HERE
#(hi owishee teehee)

#----------- REGRESSION ONLY ON INDIA -------
costreg.i = lm(Aggregate_Rating~Avg_Cost_USD,data=india.data,weights = Votes)
summary(costreg.i)
#check residual fitted plot
costregi.plot = ggplot(data=costreg.i,aes(y=.resid, x=.fitted))+geom_point()+geom_smooth(se=F)
costregi.plot
grid.arrange(costreg.plot,costregi.plot) # more straight
cost_outliers = ggplot(data=costreg.i,aes(y=.resid, x=.fitted))+geom_point(alpha=0.4)+geom_smooth(se=F)
cost_outliers

localityreg = lm(Aggregate_Rating~Locality,data=india.data,weights = Votes)
summary(localityreg)
#check residual fitted plot
localityreg.plot = ggplot(data=localityreg,aes(y=.resid, x=.fitted))+geom_point()+geom_smooth(se=F)
localityreg.plot #beautiful

cuisinereg.i = lm(Aggregate_Rating~Principal_Cuisines,data=zomato5,weights=Votes)
summary(cuisinereg.i)
#check residual fitted plot
cuisineregi.plot = ggplot(data=cuisinereg.i,aes(y=.resid,x=.fitted))+geom_point()+geom_smooth(se=F)
cuisineregi.plot
grid.arrange(cuisinereg.plot,cuisineregi.plot) #same-ish


#multiple regression for: Cost + Cuisine + Locality
cccreg.i = lm(Aggregate_Rating~Avg_Cost_USD + Principal_Cuisines + Locality, data = india.data, weights = Votes) #effect of price on ratings goes down
summary(cccreg.i)

#plot residuals
cccregi.plot = ggplot(cccreg.i, aes(y=.resid,x=.fitted))+geom_point()+geom_smooth(se=F)
cccregi.plot #pretty random
grid.arrange(cccreg.plot,cccregi.plot)

# REGRESSION FOR BOOKING & DELIVERY (we suspect it won't affect much)

#Regression on Booking
bookingreg.i = lm(Aggregate_Rating~Has_Table_Booking, india.data, weights = Votes)
summary(bookingreg.i) #horribly low R squared and p-value of 0.25; no good to us

#Regression on Online Delivery
deliveryreg.i = lm(Aggregate_Rating~Has_Online_Delivery, india.data,weights = Votes)
summary(deliveryreg.i) #better R squared than booking BUT still low (1%)
deliveryregi.plot = ggplot(data=deliveryreg.i,aes(y=.resid,x=.fitted))+geom_point()+geom_smooth(se=F)
deliveryregi.plot #yikes - find better solution

#boxplot - delivery vs residual
library(broom)
augment(deliveryreg.i) %>% 
  ggplot(aes(x=Has_Online_Delivery,y=.resid))+geom_boxplot() #median is below zero meaning we may have skewed resids

augment(deliveryreg) %>% ggplot(aes(sample=.resid))+stat_qq()+
  stat_qq_line()+facet_wrap(~Has_Online_Delivery)

##multiple regression (2) - Does booking have any affect on multireg
multireg.i = lm(Aggregate_Rating~Avg_Cost_USD+Has_Table_Booking+Principal_Cuisines+Locality,data=india.data,weights = Votes)
summary(multireg.i) #went up from 58.9% to 59.3% - not much help in multiregression either

##multiple regression (2) - Does delivery have any affect on multireg
multireg.2i = lm(Aggregate_Rating~Avg_Cost_USD+Has_Online_Delivery+Principal_Cuisines+Locality,data=india.data,weights = Votes)
summary(multireg.2i) #went from 58.9 to 58.92 - practically nothing

#stick to our cccreg.i - cuisine + cost + locality
multireg.i=cccreg.i
multiregi.plot = ggplot(data=multireg.i,aes(y=.resid,x=.fitted))+geom_point()+geom_smooth(se=F)
multiregi.plot #random but a bit below 0

##we need to set a limit so predicted ratings do not go above 5
#might help with some of the skewness of our data

#rerun regression on cost
costreg2i= lm(Transformed_Rating~Avg_Cost_USD,data=india.data,weights = Votes)
summary(costreg2i) #9.8%
costreg2i.plot = ggplot(costreg2i,aes(y=.resid,x=.fitted))+geom_point()+geom_smooth(se=F)
#compare residual graphs 
grid.arrange(costregi.plot,costreg2i.plot) #smoothed out a bit
costreg2i.plot
#note that it's relativly random but skewed by outliers
#working with a lot of data so can we disregard outliers -> check density
costreg2i.outliers = ggplot(costreg2i,aes(y=.resid,x=.fitted))+geom_point(alpha=0.4)+geom_smooth(se=F)
costreg2i.outliers #dense of the data is randomly scattered BUT below 0

localityreg2i = lm(Transformed_Rating~Locality,data=india.data,weights=Votes)
summary(localityreg2i)
localityreg2i.plot = ggplot(localityreg2i,aes(y=.resid,x=.fitted))+geom_point()+geom_smooth(se=F)
localityreg2i.plot #even steven
#compare reidual graphs
grid.arrange(localityreg.plot,localityreg2i.plot)
#straightened it out a bit more

multireg2i = lm(Transformed_Rating~Avg_Cost_USD+Principal_Cuisines+Locality,data=india.data,weights = Votes)
summary(multireg2i) #65% R Squared
multireg2i.plot = ggplot(data=multireg2i,aes(y=.resid,x=.fitted))+geom_point()+geom_smooth(se=F)
#compare residual graphs
grid.arrange(multiregi.plot,multireg2i.plot) #straighter
multireg2i.plot

#check if outliers are dense
multireg2i.outliers = ggplot(data=multireg2i,aes(y=.resid,x=.fitted))+geom_point(alpha=0.4)+geom_smooth(se=F)
multireg2i.outliers #good

#DIAGNOSTIC CHECK ON TRANSFORMED MULTI REG
plot(multireg2i,1) #good
plot(multireg2i,2)
plot(multireg2i,3)

#--- IGNORE-----
#library("googleway")
#if(!requireNamespace("devtools")) install.packages("devtools")
#devtools::install_github("dkahle/ggmap", ref = "tidyup")

#library("ggmap")
#register_google(key="AIzaSyB40bPMJbzkyBTdvOx60nA1LWyafO660QU")

#set_key("AIzaSyB40bPMJbzkyBTdvOx60nA1LWyafO660QU")
#lat <- c(4,41) #India lat boundaries
#lon <- c(68,99) #India long boundaries
#center = c(mean(lat), mean(lon))
#indiamap = google_map(data=map.data,location = center, zoom = 4)
#indiamap

# 
# api = "AIzaSyB40bPMJbzkyBTdvOx60nA1LWyafO660QU"
# get_map(location = "India",zoom="auto")