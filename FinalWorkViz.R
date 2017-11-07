####Reading Filtered Data###
analysis <- read.csv("Mynewnaddata.csv")
View(analysis)
analysis$imonth
#####Dataset all for Visualization####
df <- read.csv("filtter.csv")
View(df)
###visualizing region and attack type###
library(dplyr)
library(ggplot2)
library(maps)
library(scales)
library(DT)
library(sqldf)
library(ggplot2)
newd <- sqldf("SELECT gname, latitude, longitude from analysis where gname in ('Taliban','Boko Haram','Irish Republican Army (IRA)',
              'Shining Path (SL)','Urban Guerrilla War', 'Al-Badar','Al-Qaida','Islamic State of Iraq and the Levant (ISIL)', 
              'Urhobo Gbagbako','Yakariya Bango Insurgent Group','Moro National Liberation Front (MNLF)','Lashkar-e-Taiba (LeT)','Naxalites')")

View(newd)
CreateMyMap <- function() {
  
#####Establishing the World Map Structure######
  WorldStructure <- map_data("world")
  
#####Creating a blank plot#######
  Pl <- ggplot() + coord_fixed() +   xlab("") + ylab("")
  
#####Add map to base plot#####
  BaseWorldStructure<- Pl + 
    geom_polygon(data=WorldStructure, aes(x=long, y=lat, group=group), colour="black", fill="white")
#####Cleaning the plot#####
  cleanup <- theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(), 
                   panel.background = element_rect(fill = 'white', colour = 'white'), 
                   axis.line = element_blank(),
                   axis.ticks= element_blank(), 
                   axis.text.x=element_blank(),
                   axis.text.y=element_blank())
  
#####Final plot to map the data points#####
  BaseWorld <- BaseWorldStructure + cleanup
  
  return(BaseWorld)
}
CreateMyMap() +
  geom_point(data = newd, 
             aes(x=longitude, y=latitude, colour=gname), size = 2) +
  theme(legend.position = "right") +
  ggtitle("Attacks of Top 12 Terrorist Organizations by Number of Attacks")


####Visualization Part 1#####
##install.packages("leaflet")
library(leaflet, warn.conflicts = FALSE)
vis1 <- analysis[,c("iyear", "city", "country_txt", "latitude","longitude", "attacktype1_txt", "targtype1_txt", "targsubtype1_txt", 
                    "target1", "weaptype1_txt","weapsubtype1_txt", "gname", "motive", "summary")]
myvis1 <- 
  leaflet() %>% 
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
           attribution='Map tiles by 
           <a href="http://stamen.com">Stamen Design</a>, 
           <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> 
           &mdash; 
           Map data &copy; 
           <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%
  setView(15, 40, zoom= 2)

vis1[vis1==""] <- NA
vis1 = na.omit(vis1)

myvis1 %>% addCircles (data= myvis1, lat= ~latitude, lng = ~longitude, 
                       popup=paste(
                         "<strong>Year: </strong>", vis1$iyear,
                         "<br><strong>City: </strong>", vis1$city, 
                         "<br><strong>Country: </strong>", vis1$country_txt, 
                         "<br><strong>Attack type: </strong>", vis1$attacktype1_txt, 
                         "<br><strong>Target: </strong>", vis1$targtype1_txt, 
                         " | ", vis1$targsubtype1_txt, 
                         " | ", vis1$target1, 
                         "<br><strong>Weapon: </strong>", vis1$weaptype1_txt, 
                         "<br><strong>Group: </strong>", vis1$gname, 
                         "<br><strong>Motive: </strong>", vis1$motive, 
                         "<br><strong>Summary: </strong>", vis1$summary),
                       weight = 0.8, color="#8B1A1A", stroke = TRUE, fillOpacity = 0.6)


leaflet() %>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', 
                       attribution='Map tiles by Stamen Design, CC BY 3.0 — Map data © OpenStreetMap') 
setView(15, 40, zoom= 2)
addCircles (data=vis1, lat= ~latitude, lng = ~longitude, popup=paste
            ( "Year: ", vis1$iyear, "
            City: ", vis1$city, "
            Country: ", vis1$country_txt, "
            Attack type: ", vis1$attacktype1_txt, "
            Target: ", vis1$targtype1_txt, " | ", vis1$targsubtype1_txt, " | ", vis1$target1, "
            Weapon: ", vis1$weaptype1_txt, "
            Group: ", vis1$gname, "
            Motive: ", vis1$motive, "
            Summary: ", vis1$summary), weight = 0.8, color="#8B1A1A", stroke = TRUE, fillOpacity = 0.6)


##Visualization Part 2##
library(ggplot2)
library(maps)
library(RColorBrewer)
library(data.table)
library(reshape2)
library(leaflet, warn.conflicts = FALSE)

viz1= analysis[,c("iyear", "city", "country_txt", "latitude","longitude", "attacktype1_txt", "targtype1_txt", "targsubtype1_txt", 
           "target1", "weaptype1_txt","weapsubtype1_txt", "gname", "motive", "summary")]

mymap <- 
  leaflet() %>% 
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
           attribution='Map tiles by 
           <a href="http://stamen.com">Stamen Design</a>, 
           <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> 
           &mdash; 
           Map data &copy; 
           <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%
  setView(15, 40, zoom= 2)
mymap %>%
  addCircles (data=viz1, 
              lat= ~latitude,
              lng= ~longitude, 
              popup=paste(viz1$ArrestDate, 
                          " | Year: ", viz1$iyear,
                          " | City: ", viz1$city, 
                          " | Country_txt: ", viz1$country_txt, 
                          " | attack1_txt: ", viz1$attacktype1_txt,
                          " | targtype1_txt: ", viz1$targtype1_txt,
                          " | targsubtype1_txt: ", viz1$targsubtype1_txt,
                          " | target1: ", viz1$target1, 
                          " | weaptype1_txt: ", viz1$weaptype1_txt,
                          " | gname: ", viz1$gname, 
                          " | motive: ", viz1$motive, 
                          " | summary: ", viz1$summary), 
              weight = 0.8, 
              color="#8B1A1A", 
              stroke = TRUE, 
              fillOpacity = 0.6)


##Visualization Part 3##
library(dplyr)
library(ggplot2)
library(maps)
library(scales)
library(DT)
getmyMap <- function() {
###Getting the world map###
  world_map <- map_data("analysis")
###creating a blank plot###
  p <- ggplot() + coord_fixed() +   xlab("") + ylab("")
  
  #Add map to base plot
  base_world_messy <- p + 
    geom_polygon(data=world_map, aes(x=long, y=lat, group=group), colour="black", fill="white")
  
  ## Cleaning up the plot
  cleanup <- theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(), 
                   panel.background = element_rect(fill = 'white', colour = 'white'), 
                   axis.line = element_blank(),
                   axis.ticks= element_blank(), 
                   axis.text.x=element_blank(),
                   axis.text.y=element_blank())
  
  ## Final plot to map the points
  base_world <- base_world_messy + cleanup
  
  return(base_world)
}
getmyMap() +geom_point(data=analysis, aes(x=longitude, y=latitude), size = 1, color = "red") +
ggtitle("Global Terrorist Attacks 1970-2015")

##Viz 4###
getmyMap() +
  geom_point(data=analysis, 
             aes(x=longitude, y=latitude, color = weaptype1_txt), size = 2) +
  theme(legend.position = "bottom")  + 
  labs(title = "Global Terrorist Attacks 1970-2015- By Weapon", color = "Weapon")
#####################################################################################



