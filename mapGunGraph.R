#Read the file
getwd()
dataSet = read.csv('/home/hamid/Téléchargements/gun-violence-data/gun-violence-data_01-2013_03-2018.csv',header = TRUE,',')
#Describe file
library(ggplot2)
library(plyr)
library(stringr)
library(maps)
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
