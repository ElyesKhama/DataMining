#Read the file
getwd()
dataSet = read.csv('gun-violence-data_01-2013_03-2018.csv',header = TRUE,',')

#Describe file
str(dataSet)
head(dataSet)

#If you need to install ggplot2:
#install.packages("ggplot2")

#Libraries:
library(ggplot2)
library(plyr)

#Command to extract Month and Year from date yy-mm-dd
dataSet$Month = format(as.Date(dataSet$date, format="%Y-%m-%d"),"%Y-%m")
dataSet$Year = format(as.Date(dataSet$date, format="%Y-%m-%d"),"%Y")

#add population ??

#TODO: See if there's something simpler
nbrInjuredByMonth = ddply(dataSet, 'Month', summarise, SommeInjured = sum(n_injured))
nbrKilledByMonth = ddply(dataSet, 'Month', summarise, SommeKilled = sum(n_killed))
test = merge(nbrInjuredByMonth, nbrKilledByMonth, by="Month")

ggplot(test, aes(Month, group=1)) + geom_line(aes(y=SommeInjured, colour="Injured")) + geom_line(aes(y=SommeKilled, colour="Killed")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Total of every years
nbrKilledByState = ddply(dataSet, 'state', summarise, Somme = sum(n_killed))
nbrKilledByStateTop5 = arrange(nbrKilledByState, -Somme)[1:5,]
nbrKilledByStateAndYear = ddply(dataSet, c('state', 'Year'), summarise, Somme = sum(n_killed))
nbrKilledByStateAndYearTop5 = subset(nbrKilledByStateAndYear, nbrKilledByStateAndYear$state %in% nbrKilledByStateTop5$state)

ggplot(nbrKilledByStateAndYearTop5, aes(x=reorder(state, -Somme), y=Somme)) + geom_bar(stat="identity") + facet_wrap("Year", scales = "free") 

### Something better exists ? ? ? ?
nbrKilledByStateAndYear2 = ddply(dataSet, c('state', 'Year'), summarise, Somme = sum(n_killed))
nbrKilledByStateAndYear2014 = subset(nbrKilledByStateAndYear2, nbrKilledByStateAndYear2$Year == '2014')
nbrKilledByStateAndYear2014Top5 = arrange(nbrKilledByStateAndYear2014, -Somme)[1:5,]
nbrKilledByStateAndYear2015 = subset(nbrKilledByStateAndYear2, nbrKilledByStateAndYear2$Year == '2015')
nbrKilledByStateAndYear2015Top5 = arrange(nbrKilledByStateAndYear2015, -Somme)[1:5,]
nbrKilledByStateAndYear2016 = subset(nbrKilledByStateAndYear2, nbrKilledByStateAndYear2$Year == '2016')
nbrKilledByStateAndYear2016Top5 = arrange(nbrKilledByStateAndYear2016, -Somme)[1:5,]
nbrKilledByStateAndYear2017 = subset(nbrKilledByStateAndYear2, nbrKilledByStateAndYear2$Year == '2017')
nbrKilledByStateAndYear2017Top5 = arrange(nbrKilledByStateAndYear2017, -Somme)[1:5,]

nbrKilledByStateAndYearFinalTop5 = rbind(nbrKilledByStateAndYear2014Top5, nbrKilledByStateAndYear2015Top5, nbrKilledByStateAndYear2016Top5, nbrKilledByStateAndYear2017Top5)

library(RColorBrewer)
ggplot(nbrKilledByStateAndYearFinalTop5, aes(x=reorder(state, -Somme), y=Somme, fill=Somme))+ geom_bar(stat="identity") + facet_wrap("Year", scales = "free") + scale_fill_gradient(low = "green", high = "red")
#Age ? 




# ratio n_killed / n_injured
# top 5 states where max killed --> bigger states (???)
# top 5 weapons (???)
# age and weapons in terms of age (???)