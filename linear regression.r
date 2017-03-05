getwd()
setwd('C:\\Users\\iimra\\Documents\\SDM')
install.packages("readxl")
library(readxl) # to read xlsx file
library(tidyr)  #for separate function to split columns to two
library(gdata)  #for trim function to remove trailing blanks
emissions <- read_excel("C:\\Users\\iimra\\Documents\\SDM\\Table_12.1_Carbon_Dioxide_Emissions_From_Energy_Consumption_by_Source.xlsx",sheet=1,skip=6)#reading sheet=1 from xlsx file by skipping first 6 rows
emissions <- emissions[complete.cases(emissions),] # to remove NA values

Id <- seq.int(nrow(emissions)) #to generate sequence of numbers of range equal to rows in input dataset
emissions <- cbind(Id, emissions) #adding id column to emissions object

emissions$Month <- trim(emissions$Month)#removing trailing blanks from the Month column
emissions <- separate(emissions,Month,c("Year","Month"),sep=" ")#splitting month-year column to two month and year columns

headers <- c("Period","Year","Month","CO2Emissions","Natural Gas","Aviation","Fuel Oil","Jet Fuel","Kerosene","LPG","Lubricants","Motor","Petroleum coke","Resdiual fuel","Other Petroleum","Petroluem w/o biofuel","Total")#creting headers vector
colnames(emissions) <- headers #adding headers to emissoins object

emissions_2015 <- emissions[emissions$Year >=1973 & emissions$Year <=2015,]#chossing emissions from 1973 to 2015 to trian regression model
linearreg1 <- lm(emissions_2015$CO2Emissions~emissions_2015$Period)#training regression model

summary(linearreg1) #co2emissions = 114.45011 + 0.12466*Period
plot(emissions_2015$Period,emissions_2015$CO2Emissions,xlab="Period",ylab="CO2Emissions")
abline(linearreg1)


emissions_2016 <- data.frame(Period = emissions[emissions$Year==2016 & match(substr(emissions$Month,1,3),month.abb)<=9,1,],
                             Year = emissions[emissions$Year==2016 & match(substr(emissions$Month,1,3),month.abb)<=9,2,],
                             Month =  emissions[emissions$Year==2016 & match(substr(emissions$Month,1,3),month.abb)<=9,3],
                             Linearprediction = 114.45011 + 0.12466*emissions[emissions$Year==2016& match(substr(emissions$Month,1,3),month.abb)<=9,1,],
                             Actual = emissions[emissions$Year==2016& match(substr(emissions$Month,1,3),month.abb)<=9,4,] )
emissions_2016 #predicted and actual emissions of 2016


emissions_2015$Periodsq <- (emissions_2015$Period)^2 #adding period^2 column to emissions training data

quadraticreg2 <- lm(emissions_2015$CO2Emissions~emissions_2015$Period+emissions_2015$Periodsq)
abline(quadraticreg2)
summary(quadraticreg2)#co2emissions = (8.013e+01)+ (5.222e-01)*Period + (-7.689e-04)*Periodsq

emissions_2016$Periodsq <- (emissions_2016$Period)^2 #adding period^2 column to emissions testing data

emissions_2016$Quadraticpred <- (8.013e+01)+ (5.222e-01)*emissions_2016$Period + (-7.689e-04)*emissions_2016$Periodsq #adding quadtratic predictions to the testing data

Monthvector <- as.vector(emissions_2015$Month) #changing month column as vector
Monthvector <- substr(Monthvector,1,3) #changing month from January to Jan,.... like wise
Month2int <- match(Monthvector,month.abb) #changing Jan to 1,.....like wise

emissions_2015$Intmonth <- Month2int #adding month column as integer to emissions training data

Seasonalpred<-lm(emissions_2015$CO2Emissions~emissions_2015$Period+emissions_2015$Periodsq+emissions_2015$Intmonth)
abline(Seasonalpred)
summary(Seasonalpred)#co2emissions =(7.782e+01)+(5.220e-01)*Period+(-7.689e-04)*Periodq+(3.630e-01)*Intmonth

emissions_2016$Intmonth <- match(substr(as.vector(emissions_2016$Month),1,3),month.abb)
emissions_2016$Seasonalpred <-(7.782e+01)+(5.220e-01)*emissions_2016$Period+(-7.689e-04)*emissions_2016$Periodsq+(3.630e-01)*emissions_2016$Intmonth

emissions_2015$Period3 <- (emissions_2015$Period)^3
emissions_2015$Period4 <- (emissions_2015$Period)^4

fourthorderpred <- lm(emissions_2015$CO2Emissions~emissions_2015$Period+emissions_2015$Periodsq+emissions_2015$Period3+emissions_2015$Period4+emissions_2015$Intmonth)
summary(fourthorderpred)
#co2emissions =(9.148e+01)+(2.936e-01)*Period+(-6.217e-04)*Periodsq+(3.510e-06)*Period3+(-6.264e-09)*Period4+(4.420e-01)*Intmonth

emissions_2016$Period3 <- (emissions_2016$Period)^3
emissions_2016$Period4 <- (emissions_2016$Period)^4

emissions_2016$fourthorderpred <- (9.148e+01)+(2.936e-01)*emissions_2016$Period+(-6.217e-04)*emissions_2016$Periodsq+(3.510e-06)*emissions_2016$Period3+(-6.264e-09)*emissions_2016$Period4+(4.420e-01)*emissions_2016$Intmonth
emissions_2016

library(ggplot2)
library(grid)
library(dplyr)
library(plotly)


#' plotting graphs
plot1 <- emissions_2016 %>%
  select(Intmonth, Linearprediction) %>%
  na.omit() %>%
  ggplot() +
  geom_line(aes(x = Intmonth, y = Linearprediction), size = 1, alpha = 0.75) +
  ylab("linear pred") + xlab("Month") +
  theme_minimal() +
  theme(axis.title.x = element_blank())

plot2 <- emissions_2016 %>%
  select(Intmonth, Quadraticpred) %>%
  na.omit() %>%
  ggplot() +
  geom_line(aes(x = Intmonth, y = Quadraticpred), size = 1, alpha = 0.75) +
  ylab("quad pred") + xlab("Month") +
  theme_minimal() +
  theme(axis.title.x = element_blank())


plot3 <- emissions_2016 %>%
  select(Intmonth, Seasonalpred) %>%
  na.omit() %>%
  ggplot() +
  geom_line(aes(x = Intmonth, y = Seasonalpred), size = 1, alpha = 0.75) +
  ylab("seasonal pred") + xlab("Month") +
  theme_minimal() +
  theme(axis.title.x = element_blank())

plot4 <- emissions_2016 %>%
  select(Intmonth,fourthorderpred ) %>%
  na.omit() %>%
  ggplot() +
  geom_line(aes(x = Intmonth, y = fourthorderpred), size = 1, alpha = 0.75) +
  ylab("Fourthordered prediction") + xlab("Month") +
  theme_minimal() +
  theme(axis.title.x = element_blank())

plot5 <- emissions_2016 %>%
  select(Intmonth, Actual) %>%
  na.omit() %>%
  ggplot() +
  geom_point(aes(x = Intmonth, y = Actual), size = 1.5, alpha = 1) +
  ylab("Actual emissions") + xlab("Month") +
  theme_minimal() +
  theme(axis.title.x = element_blank())

grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2),ggplotGrob(plot3),ggplotGrob(plot4),ggplotGrob(plot5), size = "last"))

#plotting acutal emissions as smooth curve using plotly package
plot_ly(emissions_2016, x =~Intmonth, y = ~Actual, name = 'Actual emissions', type = 'scatter', mode = 'lines') 


