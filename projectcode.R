# ##------INSTALL PACKAGES--------------
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("leaflet")
# install.packages("rworldmap")
install.packages("flexdashboard", type = "source")

#------LOAD DATA------------
library(tidyverse)
library(leaflet)
library(readxl)
library(rworldmap)
library(gridExtra)
library(broom)

###### T I D Y I N G  D A T A ######
#------LOAD FILES---------
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
indiamap = leaflet(India) %>% addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions()) %>%
  setView(78.9629,20,zoom=4)
indiamap

#seems to be lot's of restaurants all over India - figures we have 8600+ restaurants (86% of the data is from India)
#can we view restaurants based on:
##Price
##Rating

#TRYING A NEW APPROACH FOR MAPS

#remove NA long + lat:
india = india.data[complete.cases(india.data[,c("Longitude", "Latitude")]),]

#color coordinate by budget
india$costcol <- ifelse(india$Avg_Cost_USD < 15, "orange",
                        ifelse(india$Avg_Cost_USD >= 15 & india$Avg_Cost_USD<30, "green",
                               ifelse(india$Avg_Cost_USD >= 30 & india$Avg_Cost_USD<50, "red", "black")))

india$costf <- factor(india$Avg_Cost_USD,
                      levels=c(3:0),
                      labels=c("Expensive",
                               "High Budget",
                               "Average Budget",
                               "Low Budget"))
#map of restaurants by price
leaflet() %>% 
  addTiles() %>% 
  setView(78.9629,20,zoom=4) %>% 
  addCircleMarkers(india$Longitude, 
                   india$Latitude, 
                   color = india$costcol, 
                   radius = 1, 
                   fill = T,
                   fillOpacity = 0.2,
                   opacity = 0.6,
                   popup = paste(india$City,
                                 india$Restaurant_Name, 
                                 sep = "")) %>%
  addLegend("bottomleft", 
            colors = c("orange","green", "red", "black"),
            labels = c("Low Budget",
                       "Average Budget",
                       "High Budget",
                       "Expensive"), 
            opacity = 0.8)


#Set up ranges for Price & Ratings
#levels can be: 0-10, 10-20, 20-30, 40-50, 50+
lowbudget = india.data %>% filter(Avg_Cost_USD<15)
avgbudget = india.data %>% filter(Avg_Cost_USD>=15 & Avg_Cost_USD<30)
highbudget = india.data %>% filter(Avg_Cost_USD>=30 & Avg_Cost_USD<45)
expensive = india.data %>% filter(Avg_Cost_USD>=45)


#plot restaurants with below $10
leaflet(lowbudget) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())
#seems spread BUT 6000+ of the restaurants are in New Dehli

#LOW BUDGET
#remove NA long + lat:
lowbudget.1 = lowbudget[complete.cases(lowbudget[,c("Longitude", "Latitude")]),]

#color coordinate by budget
lowbudget.1$costcol <- ifelse(lowbudget.1$Aggregate_Rating < 2.5, "red",
                              ifelse(lowbudget.1$Aggregate_Rating >= 2.5 & lowbudget.1$Aggregate_Rating<3.5, "orange",
                                     ifelse(lowbudget.1$Aggregate_Rating >= 3.5 & lowbudget.1$Aggregate_Rating<4.5, "green", "black")))

leaflet() %>% 
  addTiles() %>% 
  setView(78.9629,20,zoom=4.5) %>% 
  addCircleMarkers(lowbudget.1$Longitude, 
                   lowbudget.1$Latitude, 
                   color = lowbudget.1$costcol, 
                   radius = 1, 
                   fill = T,
                   fillOpacity = 0.2,
                   opacity = 0.6,
                   popup = paste(lowbudget.1$Restaurant_Name, 
                                 sep = "")) %>%
  addLegend("bottomleft", 
            colors = c("red","orange", "green", "black"),
            labels = c("Poor Rating",
                       "Average Rating",
                       "Good Rating",
                       "Excellent Rating"), 
            opacity = 0.8)

#AVG BUDGET
avgbudget.1 = avgbudget[complete.cases(avgbudget[,c("Longitude", "Latitude")]),]

#color coordinate by budget
avgbudget.1$costcol <- ifelse(avgbudget.1$Aggregate_Rating < 2.5, "red",
                              ifelse(avgbudget.1$Aggregate_Rating >= 2.5 & avgbudget.1$Aggregate_Rating<3.5, "orange",
                                     ifelse(avgbudget.1$Aggregate_Rating >= 3.5 & avgbudget.1$Aggregate_Rating<4.5, "green", "black")))

leaflet() %>% 
  addTiles() %>% 
  setView(78.9629,20,zoom=4.5) %>% 
  addCircleMarkers(avgbudget.1$Longitude, 
                   avgbudget.1$Latitude, 
                   color = avgbudget.1$costcol, 
                   radius = 1, 
                   fill = T,
                   fillOpacity = 0.2,
                   opacity = 0.6,
                   popup = paste(avgbudget.1$Restaurant_Name, 
                                 sep = "")) %>%
  addLegend("bottomleft", 
            colors = c("red","orange", "green", "black"),
            labels = c("Poor Rating",
                       "Average Rating",
                       "Good Rating",
                       "Excellent Rating"), 
            opacity = 0.8)

leaflet(avgbudget) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())
#700 in New Dehli

leaflet(highbudget) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())
#only New Dehli


leaflet(expensive) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())
#expensive restaurants seem to be in New Dehli only
expensive.1 = expensive[complete.cases(expensive[,c("Longitude", "Latitude")]),]

#color coordinate by budget
expensive.1$costcol <- ifelse(expensive.1$Aggregate_Rating < 2.5, "red",
                              ifelse(expensive.1$Aggregate_Rating >= 2.5 & expensive.1$Aggregate_Rating<3.5, "orange",
                                     ifelse(expensive.1$Aggregate_Rating >= 3.5 & expensive.1$Aggregate_Rating<4.5, "green", "black")))

leaflet() %>% 
  addTiles() %>% 
  setView(77.2090,28.6139,zoom=11) %>% 
  addCircleMarkers(expensive.1$Longitude, 
                   expensive.1$Latitude, 
                   color = lowbudget.1$costcol, 
                   radius = 5, 
                   fill = T,
                   fillOpacity = 0.2,
                   opacity = 0.6,
                   popup = paste(expensive.1$Restaurant_Name, 
                                 sep = "")) %>%
  addLegend("bottomleft", 
            colors = c("red","orange", "green", "black"),
            labels = c("Poor Rating",
                       "Average Rating",
                       "Good Rating",
                       "Excellent Rating"), 
            opacity = 0.8)
#notice through the maps that majority of the data is actually coming from New Dehli
#let's investigate
leaflet(india.data) %>% addTiles() %>% addCircles(opacity=0.5) %>% setView(77.2090,28.6139,zoom=11)
leaflet(india.data) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions()) %>% setView(77.2090,28.6139,zoom=11)
#most restaurants seem to be near airport
leaflet(lowbudget) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions()) %>% setView(77.2090,28.6139,zoom=11)

#what about the best rated restaurants?
best = india.data %>% filter(Rating_Text == 'Excellent')
leaflet(best) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())
#aside from new delhi we see Bengaluru has the second highest number of best restaurants
#How does Bengaluru compare in price to New Dehli?
leaflet(lowbudget) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions()) %>% setView(77.5946,12.9716,zoom=4)
leaflet(best) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions()) %>% setView(77.5946,12.9716,zoom=4)
#108 cheap with 32 best 

leaflet(lowbudget) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions()) %>% setView(77.2090,28.6139,zoom=4)
leaflet(best) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions()) %>% setView(77.2090,28.6139,zoom=4)
#6873 cheap with 54 best

#is Bengaluru your best bang for your buck?

#more options -> go to north -> food for all budgets
#strict budget -> go to south -> more high quality restaurants for low cost




## ---------- INTERVENTION: Maybe we should focus on just India b/c 86% of the data is pertaning to it-----
#lets make a new datafile
india.data = zomato6 %>% filter(Country == 'India')
head(india.data)

#make data into a binary file (India vs Rest of the World)
zomato7 = zomato6 %>% mutate(IndiaYN = ifelse(Country =='India','Yes','No'))


## ------------ GRAPHS (INDIA DATA ONLY)-------------
#RATING VS COST
ggplot(india.data,aes(x=Avg_Cost_USD,y=Rating_Factor))+geom_boxplot()
#interesting how this boxplot looks, I will try switching the variables below
ggplot(india.data,aes(x=Rating_Factor,y=Avg_Cost_USD))+geom_boxplot() #looks more like a boxlot 
#does tend to follow a trend but turns out but notice how higher prices got very good
#instead of excellent >> there is a cuttoff price for consumers in india, when exceeded
#it takes away from their utility

#grouped boxplot on india vs the other countries
ggplot(zomato7,aes(x=Rating_Factor,y=Avg_Cost_USD,colour=IndiaYN))+
  geom_boxplot() 

ggplot(zomato7 %>% filter(Avg_Cost_USD<100),aes(x=Rating_Factor,y=Avg_Cost_USD,colour=IndiaYN))+
  geom_boxplot() #cost 0-100 for a closer look
#indians tend to spend less on food compared to rest of the world 

#RATING VS PRINCIPAL CUISINE
ggplot(india.data,aes(x=Rating_Factor,fill=Principal_Cuisines))+geom_bar()

india.data2 = india.data %>% mutate(IndianFoodYN = ifelse(Principal_Cuisines ==c('North','Agra'),'Yes','No'))
ggplot(india.data2,aes(x=Rating_Factor,fill=IndianFoodYN))+geom_bar(position="dodge")

zo#(hi owishee teehee)

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
ggplot ()
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

#how does india fair to the rest of the world?
india.row = lm(zomato7,)


#--------- PREDICTING WHAT RATING RESTAURANT YOU WILL ATTEND -------
budget = data.frame(Avg_Cost_USD=15)
lograting = predict(costreg2i,budget)
lograting
rating = (5*(10^lograting))/(1+(10^lograting))
rating



#----------INDIA VS THE WORLD-----------
#so how does india compare to the rest of the world?
#more specifically how 
#limitations: 86% of data is for india and only 24% is for the rest
#set new data frame
world.data = zomato6 %>% filter(Country != 'India')
#wow only 899 values vs 8652
worldaov = aov(Aggregate_Rating~Country,world.data)
summary(worldaov)
TukeyHSD((worldaov))

#--- IGNORE-----
#library("ggmap")
#set_key("AIzaSyB40bPMJbzkyBTdvOx60nA1LWyafO660QU")
#lat <- c(4,41) #India lat boundaries
#lon <- c(68,99) #India long boundaries
#center = c(mean(lat), mean(lon))
#indiamap = google_map(data=map.data,location = center, zoom = 4)
#indiamap

# 
# api = "AIzaSyB40bPMJbzkyBTdvOx60nA1LWyafO660QU"
# get_map(location = "India",zoom="auto")


## ANOVA TABLE - Can we compare india to RoW?
countries.aov = aov(Aggregate_Rating~Country,data=zomato6)
summary(countries.aov)
#reject null that means rating is equal
TukeyHSD(countries.aov)


#--------- IDENTIFYING WHATS INDIAN FOOD---------

cuisineinfo = zomato4 %>% count(Cuisines)
zomatoc = zomato4 %>% mutate(ind.cuisine = ifelse(grepl('India',Cuisines),'Yes', 'No'))
zomatoc %>% count(ind.cuisine)
#didn't do anything bc principal cuisines over simplified

#can try to bring back old column containing the list of cuisines
# but we want this ONLY for india and its an old data set
india.data3 = india.data  %>% left_join(zomato4)
india.data3 %>% select(Principal_Cuisines,Cuisines)                               
#missing some cuisines (?) showing as NA 

#try something else:
cuisine.list = zomato4 %>% select(Restaurant_Name,Cuisines)
india.data3 = india.data %>% left_join(cuisine.list)
india.data3 %>% select(Principal_Cuisines,Cuisines)
##worked

#test it now:
india.data4 = india.data3 %>% mutate(ind.cuisine = ifelse(grepl('India',Cuisines),'Yes', 'No'))
india.data4 %>% count(ind.cuisine)
#wait a minute, why are we getting 46,375 values (4x the data we had) somethings up

#over matched by restaurant bc overlaps, find a diff distinct variable
cuisine.list = zomato4 %>% select(Restaurant_Name,City,Cuisines)
india.data3 = india.data %>% left_join(cuisine.list)
india.data3 %>% select(Principal_Cuisines,Cuisines)
#still overlapped

india.data = india.data %>% mutate(ID = 1:n()) %>% select(ID,everything())
zomato.id = zomato4 %>% mutate(ID = 1:n()) %>% select(ID,everything())
cuisine.list = zomato.id %>% select(ID,Cuisines)
india.data3 = india.data %>% left_join(cuisine.list)
india.data3 %>% select(Principal_Cuisines,Cuisines)
#finally worked

india.data4 = india.data3 %>% mutate(`Indian Food Served?` = ifelse(grepl('India',Cuisines),'Yes', 'No'))
#looks good to me


#regression on this
indiacuisine.reg = lm(Transformed_Rating~`Indian Food Served?`,india.data4,weights = Votes)
summary(indiacuisine.reg) #small p value but very low R square
#indian food actually decreases rating slightly 
ggplot(indiacuisine.reg,aes(x=.fitted,y = .resid))+geom_point()+geom_smooth(se=F)
#binary
plot(indiacuisine.reg,1)
plot(indiacuisine.reg,2) #not bad minus top and bottom outliers
ggplot(indiacuisine.reg,aes(sample=.resid))+stat_qq(alpha=0.2)+
  stat_qq_line()+facet_wrap(~`Indian Food Served?`)
#not very normal distribution
#conclusion - insignificant effect

