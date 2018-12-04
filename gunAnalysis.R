#Read the file
getwd()
dataSet = read.csv('~/Documents/Ecole/5A/AnalyseDonnees/gun-violence-data_01-2013_03-2018.csv',header = TRUE,',')

#Describe file
str(dataSet)
head(dataSet)

#If you need to install ggplot2:
#install.packages("ggplot2")

#Libraries:
library(ggplot2)
library(plyr)

#Command to extract year from date yy-mm-dd
dataSet$Year = format(as.Date(dataSet$date, format="%Y-%m-%d"),"%Y-%m")

nbrKilledByYear = ddply(dataSet, 'Year', summarise, Somme = sum(n_killed))
ggplot(nbrKilledByYear,aes(x=Year,y=Somme, group = 1)) + geom_point() + geom_line(size=2)


# ratio n_killed / n_injured
# top 5 states where max killed --> bigger states (???)
# top 5 weapons (???)
# age and weapons in terms of age (???)