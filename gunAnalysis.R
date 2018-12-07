#Read the file
getwd()
dataSet = read.csv('gun-violence-data_01-2013_03-2018.csv',header = TRUE,',')
dataSetStatePopulation = read.csv('populationStates.csv', header = TRUE, ',')

#Describe file
str(dataSet)
head(dataSet)
head(dataSet$state)
#If you need to install ggplot2:
#install.packages("ggplot2")
install.packages("maps")

#Libraries:
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(stringr)
library(maps)

#Command to extract Mois and Year from date yy-mm-dd
dataSet$Mois = format(as.Date(dataSet$date, format="%Y-%m-%d"),"%Y-%m")
dataSet$Year = format(as.Date(dataSet$date, format="%Y-%m-%d"),"%Y")
dataSet = dataSet[format(as.Date(dataSet$date, format="%Y-%m-%d"),"%Y") != "2013", ]
str(dataSet)

################# FIRST GRAPH ################# 
nbrInjuredByMois = ddply(dataSet, 'Mois', summarise, SommeInjured = sum(n_injured))
nbrKilledByMois = ddply(dataSet, 'Mois', summarise, SommeKilled = sum(n_killed))
nbrKilledInjured = merge(nbrInjuredByMois, nbrKilledByMois, by="Mois")

ggplot(nbrKilledInjured, aes(Mois, group=1)) + geom_line(aes(y=SommeInjured, colour="Blessés")) + geom_line(aes(y=SommeKilled, colour="Tués")) + ggtitle("Nombre de tués/bléssés depuis 2014") + labs(color="") + ylab("Somme") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

################# FIRST GRAPH ################# 

################# SECOND GRAPH ################# 

nbrKilledByStateAndYear2 = ddply(dataSet, c('state', 'Year'), summarise, Somme = sum(n_killed))
nbrKilledByStateAndYear2014 = subset(nbrKilledByStateAndYear2, nbrKilledByStateAndYear2$Year == '2014')
nbrKilledByStateAndYear2014Top5 = arrange(nbrKilledByStateAndYear2014, -Somme)[1:10,]
nbrKilledByStateAndYear2015 = subset(nbrKilledByStateAndYear2, nbrKilledByStateAndYear2$Year == '2015')
nbrKilledByStateAndYear2015Top5 = arrange(nbrKilledByStateAndYear2015, -Somme)[1:10,]
nbrKilledByStateAndYear2016 = subset(nbrKilledByStateAndYear2, nbrKilledByStateAndYear2$Year == '2016')
nbrKilledByStateAndYear2016Top5 = arrange(nbrKilledByStateAndYear2016, -Somme)[1:10,]
nbrKilledByStateAndYear2017 = subset(nbrKilledByStateAndYear2, nbrKilledByStateAndYear2$Year == '2017')
nbrKilledByStateAndYear2017Top5 = arrange(nbrKilledByStateAndYear2017, -Somme)[1:10,]
nbrKilledByStateAndYearFinalTop5 = rbind(nbrKilledByStateAndYear2014Top5, nbrKilledByStateAndYear2015Top5, nbrKilledByStateAndYear2016Top5, nbrKilledByStateAndYear2017Top5)

ggplot(nbrKilledByStateAndYearFinalTop5, aes(x=reorder(state, -Somme), y=Somme, fill=Somme))+ geom_bar(stat="identity") + facet_wrap("Year", scales = "free") + scale_fill_gradient(low = "green", high = "red") + xlab("État") + ggtitle("Nombre de tués par état et par année") +ylab("Nombre de tués") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

################# SECOND GRAPH ################# 

################# THIRD GRAPH ################# 

nbrKilledByStateAndYear = ddply(dataSet, c('state', 'Year'), summarise, Somme = sum(n_killed))
nbrKilledByStateAndYearWPopulation = merge(nbrKilledByStateAndYear, dataSetStatePopulation, by="state")
nbrKilledByStateAndYearWPopulation$ratio = nbrKilledByStateAndYearWPopulation$Somme / nbrKilledByStateAndYearWPopulation$population *100

ggplot(nbrKilledByStateAndYearWPopulation, aes(x=state,y=ratio)) + geom_boxplot(fill='#A4A4A4', color="darkred") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Répartition du ratio tués/population sur 2013-2018 par état") + xlab("État") + ylab("Ratio tués/population (%)")  

################# THIRD GRAPH ################# 

############################################# Map ##########################################################
#load us map data
all_states <- map_data("state")
#plot all states with ggplot
dataSetPost<-dataSet[dataSet$latitude < 50,]
dataSetPreFinal<-dataSetPost[dataSetPost$longitude> -140,]
dataSetFinal<-dataSetPreFinal[dataSetPreFinal$longitude< -30,]
p <- ggplot()+ geom_polygon( data=all_states, aes(x=long, y=lat, group = group),fill = "red", color = "blue" )
p <- p + geom_point(data=data.frame(long = dataSetFinal$longitude,lat =dataSetFinal$latitude, stringsAsFactors = FALSE),aes(x=long, y=lat, size = 4),shape=".",size = 4, color="Yellow") 
p
################################################ Gun ######################################################
Armes=""
for(armUsed in dataSet$gun_type ){
  elems <-unlist(str_split(armUsed,"\\|\\|"))
  for (value in elems) {
    if (value=="") {}
    else {
      arms<-unlist(str_split(value,"[0-9]+::"))
      for(arm in arms)
        if(arm=="" || arm=="0:Unknown"){}
      else{
        Armes=c(Armes,arm)
      }
    }
  }
}
mostUsed=as.data.frame(sort(table(Armes),TRUE)[1:5])
ggplot(data=mostUsed, aes(x=Armes, y=Freq, fill=Armes)) + geom_bar(stat="identity")+ xlab("Type d'arme ") + ylab("Nombre de fois utilisée")
################################################## Age #####################################################
Ages=""
for(ageUsed in dataSet$participant_age ){
  elems <-unlist(str_split(ageUsed,"\\|\\|"))
  for (value in elems) {
    if (value=="") {}
    else {
      ages<-unlist(str_split(value,"[0-9]+::"))
      for(age in ages)
        if(age=="" || age=="0:Unknown"|| age=="Unknown"){}
      else{
        Ages=c(Ages,age)
      }
    }
  }
}
mostUsed=as.data.frame(sort(table(Ages),TRUE)[1:14])
ggplot(data=mostUsed, aes(x=Ages, y=Freq, fill=Ages)) + geom_bar(stat="identity")+ xlab("Age ") + ylab("Nombre de fois ")
