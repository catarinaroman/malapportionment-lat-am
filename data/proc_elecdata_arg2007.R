## Collapse dataset Argentina electoral data

## Pre-processing
setwd('~/Dropbox/Academic/NYU/2013 Fall/Comparative/Paper2')
require(plyr)

## load data
dat <- read.csv('electoral-2007-dn.csv')
head(dat)

# Clean
dat$departamentoId<-NULL
dat$departamento<-NULL
dat$provinciaId<-NULL
dat$eleccion<-NULL
dat<-subset(dat, partidoId<900)
head(dat)

# ddply...
dat2 <- ddply(dat, .(anio,provincia,partidoId), summarize, vot=sum(votos))

# join...
write.csv(join(dat2, dat[,c(3,4)], match='first'), 'votarg2007.csv', row.names=F)
