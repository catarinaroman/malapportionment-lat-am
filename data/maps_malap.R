## Maps on malapportionment

# Pre-processing
rm(list=ls())
library(ggplot2); require(plyr)
library(rgdal); library(xlsx);
library(grid); require(stringr)
setwd('~/Dropbox/Academic/NYU/2013 Fall/Comparative/Paper2')

## Argentina
html <- url("http://biogeo.ucdavis.edu/data/gadm2/R/ARG_adm1.RData")
load(html)

# Input the data on malapportionment...
arq_data <- read.xlsx2('data_ABC.xlsx', sheetIndex=1, startRow=2, endRow=26)[,1:3]
arq_data$Provincia<-gsub('Provincia de ', '', arq_data$Provincia)
arq_data$Provincia<-gsub('Provincia del ', '', arq_data$Provincia)
arq_data$Provincia<-gsub(' Autónoma', '', arq_data$Provincia)
dtf <- data.frame(Provincia=gadm$NAME_1, ID_1 = as.numeric(gadm$ID_1))
dtf$Provincia<-enc2utf8(as.character(dtf$Provincia))
arq_data$Provincia<-enc2utf8(arq_data$Provincia)

arq_data <- join(arq_data, dtf)
arq_data <- arq_data[order(arq_data$ID_1), ]

gadm$malapportion <- as.numeric(as.character(arq_data$Population)) / as.numeric(as.character(arq_data$Seats))
col = heat.colors(gadm$malapportion, alpha = .5)

pdf('mal_arg.pdf', width=6, height=7)
spplot(gadm, "malapportion", col.regions=col, main="Malapportionment Provincias Argentinas")
grid.text("Population / Seats", x=unit(.9, "npc"), y=unit(.5, "npc"), rot=90)
dev.off()


## Brazil
rm(list=ls())
html <- url("http://biogeo.ucdavis.edu/data/gadm2/R/BRA_adm1.RData")
load(html)

# Input the data on malapportionment...
arq_data <- read.xlsx2('data_ABC.xlsx', sheetIndex=2, startRow=2, endRow=29, stringsAsFactors=F, strip.white=T)[,1:3]
dtf <- data.frame(State=as.character(gadm$NAME_1), ID_1 = as.numeric(gadm$ID_1))
dtf$State <- str_trim(enc2utf8(as.character(dtf$State)))
arq_data$State <- str_trim(enc2utf8(as.character(arq_data$State)))

arq_data <- join(arq_data, dtf)
arq_data <- arq_data[order(arq_data$ID_1), ]

gadm$malapportion <- as.numeric(as.character(arq_data$Population)) / as.numeric(as.character(arq_data$Seats))/1000
col = heat.colors(gadm$malapportion, alpha = .5)

pdf('mal_bra.pdf', width=8, height=6)
spplot(gadm, "malapportion", col.regions=col, main="Malapportionment States Brazil")
grid.text("Population / Seats", x=unit(.95, "npc"), y=unit(.5, "npc"), rot=90)
dev.off()


## Colombia
rm(list=ls())
html <- url("http://biogeo.ucdavis.edu/data/gadm2/R/COL_adm1.RData")
load(html)

# Input the data on malapportionment...
arq_data <- read.xlsx2('data_ABC.xlsx', sheetIndex=3, startRow=3, endRow=36, stringsAsFactors=F, strip.white=T)[,1:3]
dtf <- data.frame(Departamientos=as.character(gadm$NAME_1), ID_1 = as.numeric(gadm$ID_1))
dtf$Departamientos <- str_trim(enc2utf8(as.character(dtf$Departamientos)))
arq_data$Departamientos <- str_trim(enc2utf8(as.character(arq_data$Departamientos)))

arq_data <- join(arq_data, dtf)
arq_data <- na.omit(arq_data)
arq_data <- arq_data[order(arq_data$ID_1), ]

gadm$malapportion <- as.numeric(as.character(arq_data$Population)) / as.numeric(as.character(arq_data$Seats))/1000
col = heat.colors(gadm$malapportion, alpha = .5)

pdf('mal_col.pdf', width=6, height=6)
spplot(gadm, "malapportion", col.regions=col, main="Malapportionment Departamientos Colombia")
grid.text("Population / Seats", x=unit(.95, "npc"), y=unit(.5, "npc"), rot=90)
dev.off()