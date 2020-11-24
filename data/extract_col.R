## Extracting Colombia votings

## Pre-processing
rm(list=ls())
setwd('~/Dropbox/Academic/NYU/2013 Fall/Comparative/Paper2')
library(XML); require(xlsx)
library(plyr)


# Load urls...
theurls <- readLines('vots_colombia.txt', warn=F)
theurls<-unique(theurls)
list_rollcalls <- list()

# Extracting
for (i in 1:length(theurls)) list_rollcalls[[i]] <- readHTMLTable(theurls[[i]])

# Saving
save.image('rollcalls_col_2007to20010.RData')
#load('rollcalls_col_2007to20010.RData')

# Take out the votings
vots <- data.frame()
for (i in 1:length(list_rollcalls)) {
  for (j in 1:length(list_rollcalls[[i]])) {
    if (!is.na(sum(names(list_rollcalls[[i]][[j]])=='Voto'))&sum(names(list_rollcalls[[i]][[j]])=='Voto')==1) {
      aux <- list_rollcalls[[i]][[j]]
      aux <- data.frame(urlvot = rep(theurls[i], dim(aux)[1]), aux)
      vots <- rbind(vots, aux)
    }
  }
}
rm(aux, dat, dat2, obj, dfx, i, j, n.rows, tables, theurl, rlin)

nomecom <- as.character(unique(vots$Congresista))
write.xlsx2(data.frame(mcong=nomecom), 'congr_col_nam.xlsx', 'CongCol', row.names=F)

## Input Departamento name for each politician
nomecom <- read.xlsx2('congr_col.xlsx', 1)
nomecom<-nomecom[nomecom$Departamiento!='', ]

names(vots)[2:3]<-c('Diputado','Part')

vots2 <- join(vots, nomecom)
vots2<-vots2[!is.na(vots2$Partido),]
vots2$Part<-NULL
vots2$idvot<-as.numeric(vots2$urlvot)
res <- reshape(vots2[,c(6,2,3,5)], v.names = 'Voto', idvar = 'Diputado', 
        timevar='idvot', direction = 'wide')
length(unique(theurls))
head(res)

for(i in 3:dim(res)[2]) {
  res[,i] <- as.character(res[,i])
  res[is.na(res[,i]),i] = 'Absent'
}
head(res)

# Saving
write.xlsx2(res, 'votscol_20062010.xlsx', 'colvot', row.names=F)
save.image('Colvotins.RData')
