#######
## Paper: Legislative reapportionment and voting patterns
## Author: Catarina Roman*
#######

# Load packages
library(ggplot2); require(tidyverse)
library(rgdal); library(haven)
library(grid); require(stringr)
library(wnominate); library(xtable)
library(readxl); library(sp)
library(gridExtra); library(lattice)
set.seed(399404) # random.org

#####
## Malapportionment maps
#####

# Pre-processing
rm(list=ls())

## Argentina
html <- url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_ARG_1_sp.rds")
gadmARG <- readRDS(html)
arq_data <- read_xlsx('data_ABC.xlsx', sheet=1)[1:24,1:3]
arq_data$Province <- gsub('(Provincia de |Provincia del | Autónoma)', 
                        '', arq_data$Province)
arq_data$Province <- enc2utf8(arq_data$Province)

arq_data <- left_join(arq_data, 
                      data.frame(Province=gadmARG$NAME_1, GID_1 = gadmARG$GID_1))
  
arq_data <- arq_data[order(as.numeric(gsub('(ARG.|_1)', 
                                           '', arq_data$GID_1))), ]

gadmARG$malapportion <- (arq_data$Population/arq_data$Seats)/1000
colARG = heat.colors(gadmARG$malapportion, alpha = .5)

## Brazil
html <- url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_BRA_1_sp.rds")
gadmBRA <- readRDS(html)
arq_data <- read_xlsx('data_ABC.xlsx', sheet=2, trim_ws = T)[1:27,1:3]
dtf <- data.frame(State=as.character(gadmBRA$NAME_1), GID_1 = gadmBRA$GID_1)
dtf$State <- str_trim(enc2utf8(as.character(dtf$State)))
arq_data$State <- str_trim(enc2utf8(as.character(arq_data$State)))

arq_data <- left_join(arq_data, dtf)
arq_data <- arq_data[order(as.numeric(gsub('(BRA.|_1)', 
                                           '', arq_data$GID_1))), ]

gadmBRA$malapportion <- (arq_data$Population/arq_data$Seats)/1000
colBRA = heat.colors(gadmBRA$malapportion, alpha = .5)

## Colombia
html <- url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_COL_1_sp.rds")
gadmCOL <- readRDS(html)
arq_data <- read_xlsx('data_ABC.xlsx', sheet=3, trim_ws = T)[1:33,1:3]
dtf <- data.frame(Departamientos=as.character(gadmCOL$NAME_1), GID_1 = gadmCOL$GID_1)
dtf$Departamientos <- str_trim(enc2utf8(as.character(dtf$Departamientos)))
arq_data$Departamientos <- str_trim(enc2utf8(as.character(arq_data$Departamientos)))

arq_data <- left_join(arq_data, dtf)
arq_data <- na.omit(arq_data)
arq_data <- arq_data[order(as.numeric(gsub('(COL.|_1)', 
                                           '', arq_data$GID_1))), ]

gadmCOL$malapportion <- (arq_data$Population/arq_data$Seats)/1000
colCOL = heat.colors(gadmCOL$malapportion, alpha = .5)

png('distr_seats.png', width=800, height=800)
grid.arrange(spplot(gadmARG, "malapportion", col.regions=colARG, main="Argentina"),
             spplot(gadmBRA, "malapportion", col.regions=colBRA, main="Brazil"),
             spplot(gadmCOL, "malapportion", col.regions=colCOL, main="Colombia"),
             nrow=2)
dev.off()
rm(dtf, arq_data, gadmARG, gadmBRA, gadmCOL, colARG, colBRA, colCOL, html)

#####
## Malapportionment provincial behavior
#####

# Pre-processing: function for plotting many states
plot_brasil <- function (x, main.title = "W-NOMINATE Coordinates", d1.title = "First Dimension", 
                         d2.title = "Second Dimension", dims = c(1, 2), plotBy = "party", 
                         color = TRUE, shape = TRUE, cutline = NULL, Legend = TRUE, 
                         legend.x = 0.8, legend.y = 1, colorlist = c("darkblue", "firebrick", 
                                                                     "darkcyan", "darkgreen", "darkmagenta", "darkolivegreen", "darkorange", "darkorchid", 
                                                                     "darkred", "darksalmon", "darkseagreen", "darkslateblue", "darkslategray", 
                                                                     "darkturquoise", "darkviolet", "deeppink", "deepskyblue", "dodgerblue", "black", 
                                                                     "gray70", "blue","red", "green", "yellow", "orange", "powderblue"), ...) {
  if (!class(x) == "nomObject") 
    stop("Input is not of class 'nomObject'.")
  if (!any(colnames(x$legislators) == plotBy)) {
    warning("Variable '", plotBy, "' does not exist in your W-NOMINATE object.")
    types <- rep("Leg", dim(x$legislators)[1])
  }
  else {
    types <- x$legislators[, plotBy]
  }
  if (length(dims) != 2 & x$dimensions != 1) 
    stop("'dims' must be an integer vector of length 2.")
  nparties <- length(unique(types))
  shapes <- rep(c(16, 15, 17, 18, 19, 3, 4, 8), 4)
  if (color == FALSE) 
    colorlist <- sample(colors()[160:220], 50)
  if (shape == FALSE) 
    shapes <- rep(16, 50)
  if (x$dimensions == 1) {
    coord1D <- x$legislators[, "coord1D"]
    ranking <- rank(x$legislators[, "coord1D"])
    plot(seq(-1, 1, length = length(coord1D)), 1:length(coord1D), 
         type = "n", cex.main = 1.2, cex.lab = 1.2, font.main = 2, 
         xlab = "First Dimension Nominate", ylab = "Rank", 
         main = "1D W-NOMINATE Plot")
    if (Legend) 
      legend(0.67, 0.7 * length(coord1D), unique(types), 
             pch = shapes[1:nparties], col = colorlist[1:nparties], 
             cex = 0.7)
    for (i in 1:nparties) suppressWarnings(points(coord1D[types == 
                                                            unique(types)[i]], ranking[types == unique(types)[i]], 
                                                  pch = shapes[i], col = colorlist[i], cex = 1.1, lwd = 2))
  }
  else {
    coord1D <- x$legislators[, paste("coord", dims[1], "D", 
                                     sep = "")]
    coord2D <- x$legislators[, paste("coord", dims[2], "D", 
                                     sep = "")]
    suppressWarnings(symbols(x = 0, y = 0, circles = 1, inches = FALSE, 
                             asp = 1, main = main.title, xlab = d1.title, ylab = d2.title, 
                             xlim = c(-1, 1), ylim = c(-1, 1), cex.main = 1.2, 
                             cex.lab = 1.2, font.main = 2, lwd = 2, fg = "grey", 
                             frame.plot = FALSE, ...))
    if (!is.null(cutline)) {
      for (i in 1:length(cutline)) {
        if (all(is.na(x$rollcalls[cutline[i], ]))) 
          stop("Roll call for cutline did not meet minimum lopsidedness requirements.")
        add.cutline(c(x$rollcalls[cutline[i], paste("midpoint", 
                                                    dims[1], "D", sep = "")], x$rollcalls[cutline[i], 
                                                                                          paste("spread", dims[1], "D", sep = "")], x$rollcalls[cutline[i], 
                                                                                                                                                paste("midpoint", dims[2], "D", sep = "")], 
                      x$rollcalls[cutline[i], paste("spread", dims[2], 
                                                    "D", sep = "")]), weight = x$weights[dims[2]]/x$weights[dims[1]], 
                    lwd = 2)
      }
    }
    if (Legend) 
      legend(legend.x, legend.y, unique(types), pch = shapes[1:nparties], 
             bty = "n", col = colorlist[1:nparties], cex = 0.7)
    for (i in 1:nparties) suppressWarnings(points(coord1D[types == 
                                                            unique(types)[i]], coord2D[types == unique(types)[i]], 
                                                  pch = shapes[i], col = colorlist[i], cex = 1.1, lwd = 2))
  }
}

## Loading data
load('wnomarg.RData'); load('wnombra.RData')
load('wnomcol.RData')

png('partyProvBehavior.png', width = 8, height = 10, 
    units = 'in', res = 300)
par('mfrow' = c(3,2))
plot_brasil(wnomarg, legend.x=1, 
            main.title = "Argentina W-NOMINATE by Party", 
            d1.title = "", 
            d2.title = "")
plot_brasil(wnomarg, legend.x=1, plotBy='provincia', 
            main.title = "Argentina W-NOMINATE by Province", 
            d1.title = "", 
            d2.title = "")
plot_brasil(wnombra, legend.x=1, 
            main.title = "Brazil W-NOMINATE by Party", 
            d1.title = "", 
            d2.title = "")
plot_brasil(wnombra, legend.x=1, plotBy='state', 
            main.title = "Brazil W-NOMINATE by Province", 
            d1.title = "", 
            d2.title = "")
plot_brasil(wnomcol, legend.x=1, 
            main.title = "Colombia W-NOMINATE by Party", 
            d1.title = "", 
            d2.title = "")
plot_brasil(wnomcol, legend.x=1, plotBy='prov', 
            main.title = "Colombia W-NOMINATE by Province", 
            d1.title = "", 
            d2.title = "")
par('mfrow' = c(1,1))
dev.off()

#####
## Simulating ideal points for new houses
#####

## Argentina
arg_data <- read_xlsx('data_ABC.xlsx', sheet = 1) 
arg_data_corr <- read_xlsx('data_ABC.xlsx', sheet=4) %>%
  filter(notcorr>0|Corr_ov_2>0)
ncorr<-tapply(arg_data_corr$notcorr, arg_data_corr$partido, sum)
corr <- tapply(arg_data_corr$Corr_ov_2, arg_data_corr$partido, sum) 
ncorr <- data.frame(ncorr, corr)
rm(corr)
ncorr$diff <- ncorr$ncorr-ncorr$corr
ncorr_arg<-ncorr
ncorr_arg <- na.omit(ncorr_arg)
names(ncorr_arg) <- c('corr', 'ncorr', 'diff')

# Table parties
write_excel_csv(ncorr_arg[order(ncorr$diff, decreasing=T), ], 'arg_tabpart.csv')

# Ideal points
load('wnomarg.RData')

# Regression
iparg <- lm(coord1D~as.factor(party)+as.factor(provincia), data=wnomarg$legislators)
write_excel_csv(anova(iparg), 'anovaARG_d1.csv')
iparg <- lm(coord2D~as.factor(party)+as.factor(provincia), data=wnomarg$legislators)
write_excel_csv(anova(iparg), 'anovaARG_d2.csv')
iparg <- wnomarg$legislators
iparg <- na.omit(iparg)

## Avgprov
avg_prov <- function (dat) {
  if (dim(dat)[1]==0) return(c(runif(2,-1,1), runif(2, .1, .5)))
  if (dim(dat)[1]==1) return(c(ifelse(!is.na(mean(dat$coord1D, na.rm=T)),
                                      yes= mean(dat$coord1D, na.rm=T),no=runif(1,-1,1)),
                               ifelse(!is.na(mean(dat$coord1D, na.rm=T)),
                                      yes= mean(dat$coord1D, na.rm=T),no=runif(1,-1,1)),
                               runif(2, .1, .5)))
  if (dim(dat)[1]>1) return(c(ifelse(!is.na(mean(dat$coord1D, na.rm=T)), 
                                     yes= mean(dat$coord1D, na.rm=T),no=runif(1,-1,1)),
                              ifelse(!is.na(mean(dat$coord2D, na.rm=T)), 
                                     yes= mean(dat$coord2D, na.rm=T),no=runif(1,-1,1)),
                              ifelse(!is.na(sd(dat$coord1D, na.rm=T)), 
                                     yes=sd(dat$coord1D, na.rm=T),no=runif(1,.1,.5)),
                              ifelse(!is.na(sd(dat$coord2D, na.rm=T)), 
                                     yes=sd(dat$coord2D, na.rm=T),no=runif(1,.1,.5))))
  return(c(runif(2,-1,1), runif(2, .1, .5)))
}

## ComplParty
# Enter: dat: prov, party, ncorr, corr
# Enter: dat2: party, prov, coord1D, coord2D
genrand <- function(dat, dat2, ttlr=F, N=200) {
  legs <- list()
  rem <- list()
  names(dat) <- c('provincia', 'party', 'ncorr', 'corr')
  names(dat2) <- c('party', 'provincia', 'coord1D', 'coord1D')
  dat2 <- na.omit(dat2)
  row.names(dat2)<-NULL
  incr <- dat$corr-dat$ncorr
  # Totally random: baseline for test
  for (i in 1:length(incr)) {
    if (incr[i]>0) { # Add case
      if (ttlr==T) {
        for (j in 1:incr[i]){
          an1D <- runif(N, -1, 1); an2D <- runif(N, -1, 1)
          legs <- append(legs, list(data.frame(provincia = as.character(dat$provincia[i]), 
                                               party = as.character(dat$party[i]),
                                               coord1D=an1D, coord2D = an2D)))
        }
      } else {
        for (j in 1:incr[i]) {
          aux <- subset(dat2, party==as.character(dat$party[i]))
          if(dim(aux)[1]==0) aux <- dat2
          aux <- subset(aux, provincia==as.character(dat$provincia[i]))
          vecdat <- avg_prov(aux)
          an1D <- rnorm(N, mean=vecdat[1], sd=vecdat[3])
          an2D <- rnorm(N, mean=vecdat[2], sd=vecdat[4])
          legs <- append(legs, list(data.frame(provincia = as.character(dat$provincia[i]), 
                                               party = as.character(dat$party[i]),
                                               coord1D=an1D, coord2D = an2D)))
        }
      }
    } else if (incr[i]<0) {
      aux <- subset(dat2, party==as.character(dat$party[i]))
      if(dim(aux)[1]==0) aux <- dat2
      aux <- subset(aux, provincia==as.character(dat$provincia[i]))
      if(dim(aux)[1]==0) aux <- subset(dat2, provincia==as.character(dat$provincia[i]))
      if(dim(aux)[1]!=0) {
        takeout <- row.names(aux)
        takeout <- sample(takeout, abs(incr[i]))
        rem <- append(rem, list(data.frame(provincia = as.character(dat$provincia[i]), 
                                           party = as.character(dat$party[i]),
                                           takeout=takeout)))
      }
    }
  }
  return(list(legs, rem))
}

## Argentina generate random
argrand <- genrand(arg_data_corr[,c(1,2,3,5)], iparg[,c('party', 'provincia', 'coord1D', 'coord2D')], ttlr=T)
argrand_pattern <- genrand(arg_data_corr[,c(1,2,3,5)], iparg[,c('party', 'provincia', 'coord1D', 'coord2D')])

## Run rand
build_df <- function(dat, dat2, vots) {
  row.names(dat2) <- NULL
  tkot <- dat[[2]]
  simdat <- dat[[1]]
  vectkot <- character()
  for (i in 1:length(tkot)) {
    vectkot <- append(vectkot, as.character(tkot[[i]][,'takeout']))
  }
  dat2 <- na.omit(dat2)
  matvot <- as.data.frame(vots$votes)
  namevots <- names(matvot)
  matvot$diputado <- row.names(matvot)
  resbds <- list()
  for (i in 1:200) {
    resrapd <- data.frame()
    for (j in 1:length(simdat)) {
      resrapd <- rbind(resrapd, data.frame(diputado='Inserted', data.frame(simdat[[j]][i,])))
    }
    aux <- dat2
    aux <- aux[setdiff(row.names(dat2),vectkot),]
    aux <- rbind(dat2, resrapd)
    aux <- left_join(aux, matvot[,c('diputado',namevots)])
    resbds[[i]] <- aux
  }
  return(resbds)
}
load('argvotes.RData')
load('rcarg.RData')

resbds_arg_aleat <- build_df(argrand, iparg[,c('diputado', 'provincia', 'party', 'coord1D', 'coord2D')], rcarg)
resbds_arg_patt<- build_df(argrand_pattern, iparg[,c('diputado', 'provincia', 'party', 'coord1D', 'coord2D')], rcarg)

# Regs and preds
gen_patterns <- function(bdd, aff, neg) {
  nams <- names(bdd[[1]][,-c(1:5)])
  for (i in 1:length(bdd)) {
    aux <- bdd[[i]]
    vf <- numeric()
    vc <- numeric()
    vr <- numeric()
    for (j in 6:dim(aux)[2]) {
      vd <- rep(NA, dim(aux)[2]-5)
      aux[,j] = as.character(aux[,j])
      vd[aux[,j]==aff] = 1
      vd[aux[,j]==neg] = 0
      model <- glm(vd~coord1D+coord2D, family=binomial('logit'), data=aux)
      if (i==1) {
        vf[j-5] = sum(vd, na.rm=T)
        vc[j-5] = sum(!vd, na.rm=T)
      }
      vr[j-5] = sum(predict(model, aux[,c('coord1D', 'coord2D')], type='response')>.5, na.rm=T)
    }
    if (i==1) {
      res <- data.frame(vf,vc,vr)
    } else {
      res <- data.frame(res, vr)
    }
    vf <- NULL
    vc <- NULL
    vr <- NULL
  }
  return(res)
}

## End analysis argentina
aux <- gen_patterns(resbds_arg_aleat, 'AFIRMATIVO', 'NEGATIVO')
aux2 <- gen_patterns(resbds_arg_patt, 'AFIRMATIVO', 'NEGATIVO')

## Graphs Argentina case
lcalc <- function(res, vm = 65) {
  res2 <- t(res)
  res2 <- res2[-c(1,2),]
  mms <- apply(res2, 2, mean)
  sds <- apply(res2, 2, sd)
  vote <- seq(1, length(mms), 1)
  vots <- data.frame(vote, mms=mms-vm, sds, vc=t(res)[2,], vf=t(res)[1,])
  vots<-vots[order(mms), ]
  vots <- subset(vots, sds>0)
  vots$vote <- 1:(dim(vots)[1])
  return(vots)
}

## Some numbers
round(prop.table(table(aux$vf/(aux$vc+aux$vf)<.6)), digits=2)
round(prop.table(table(aux$vf>aux$vc)), digits=2)

## Graphs
data1<-lcalc(aux2)
p<-ggplot(data1, aes(x=vote, y=mms)) + 
  geom_line( aes(x=as.numeric(vote), y=mms)) + 
  geom_line( aes(x=as.numeric(vote), y=vf), colour="red") +
  geom_line( aes(x=as.numeric(vote), y=vc), colour="blue") +
  scale_colour_manual(values = c("black","red", "blue"))+
  geom_hline(yintercept=257/2)
p
ggsave(filename='argvot.pdf',plot=p, width=8, height=6)

data1<-lcalc(aux)
p<-ggplot(data1, aes(x=vote, y=mms)) + 
  geom_line( aes(x=as.numeric(vote), y=mms)) + 
  geom_line( aes(x=as.numeric(vote), y=vf), colour="red") +
  geom_line( aes(x=as.numeric(vote), y=vc), colour="blue") +
  scale_colour_manual(values = c("black","red", "blue"))+
  geom_hline(yintercept=257/2)
p


## Brazil
bra_data <- read.xlsx('data_ABC.xlsx', sheetIndex=2, as.data.frame=T) 
bra_data_corr <- read.xlsx('data_ABC.xlsx', sheetIndex=5, as.data.frame=T)
bra_data_corr$X0<-NULL
bra_data_corr<-subset(bra_data_corr, notcorr>0|corr>0)
ncorr<-tapply(bra_data_corr$notcorr, bra_data_corr$Partido, sum)
corr <- tapply(bra_data_corr$corr, bra_data_corr$Partido, sum) 
ncorr <- data.frame(ncorr, corr)
ncorr$diff <- ncorr$corr-ncorr$ncorr
rm(corr)
ncorr<-na.omit(ncorr)
ncorr_bra<-ncorr
rm(ncorr)
xtable(ncorr_bra[order(ncorr_bra$diff, decreasing=T),], digits=0)
xtable(bra_data)

load('wnombra.RData')
ipbra <- lm(coord1D~as.factor(party)+as.factor(state), data=wnombra$legislators)
xtable(anova(ipbra), digits=2)
ipbra <- lm(coord2D~as.factor(party)+as.factor(state), data=wnombra$legislators)
xtable(anova(ipbra), digits=2)
ipbra <- wnombra$legislators

# Simulation

## ComplParty
# Enter: dat: prov, party, ncorr, corr
# Enter: dat2: party, prov, coord1D, coord2D
brarand <- genrand(bra_data_corr[,c(1,2,3,4)], ipbra[,c('party', 'state', 'coord1D', 'coord2D')], ttlr=T)
brarand_patterns <- genrand(bra_data_corr[,c(1,2,3,4)], ipbra[,c('party', 'state', 'coord1D', 'coord2D')])

load('Analyze.RData')

build_df <- function(dat, dat2, vots) {
  row.names(dat2) <- NULL
  tkot <- dat[[2]]
  simdat <- dat[[1]]
  vectkot <- character()
  for (i in 1:length(tkot)) {
    vectkot <- append(vectkot, as.character(tkot[[i]][,'takeout']))
  }
  dat2 <- na.omit(dat2)
  matvot <- as.data.frame(vots$votes)
  namevots <- names(matvot)
  matvot$dep <- row.names(matvot)
  resbds <- list()
  for (i in 1:200) {
    resrapd <- data.frame()
    for (j in 1:length(simdat)) {
      resrapd <- rbind(resrapd, data.frame(dep='Inserted', data.frame(simdat[[j]][i,])))
    }
    aux <- dat2
    names(aux)<-c('dep', 'state', 'party', 'coord1D', 'coord2D')
    names(resrapd)<-c('dep', 'state', 'party', 'coord1D', 'coord2D')
    aux <- aux[setdiff(row.names(dat2),vectkot),]
    aux <- rbind(aux, resrapd)
    aux <- join(aux, matvot[,c('dep',namevots)])
    resbds[[i]] <- aux
  }
  return(resbds)
}

resbds_bra_aleat <- build_df(brarand, ipbra[,c('dep', 'state', 'party', 'coord1D', 'coord2D')], rcbra)
resbds_bra_patt<- build_df(brarand_patterns, ipbra[,c('dep', 'state', 'party', 'coord1D', 'coord2D')], rcbra)

# Regs and preds
gen_patterns <- function(bdd, aff, neg) {
  nams <- names(bdd[[1]][,-c(1:5)])
  for (i in 1:length(bdd)) {
    aux <- bdd[[i]]
    vf <- numeric()
    vc <- numeric()
    vr <- numeric()
    for (j in 6:dim(aux)[2]) {
      if (i==50) print('Aeee 50!')
      if (i==100) print('Aeee 100!')
      if (i==150) print('Aeee 150!')
      vd <- rep(NA, dim(aux)[2]-5)
      aux[,j] = as.character(aux[,j])
      vd[aux[,j]==aff] = 1
      vd[aux[,j]==neg] = 0
      model <- glm(vd~coord1D+coord2D, family=binomial('logit'), data=aux)
      if (i==1) {
        vf[j-5] = sum(vd, na.rm=T)
        vc[j-5] = sum(!vd, na.rm=T)
      }
      vr[j-5] = sum(predict(model, aux[,c('coord1D', 'coord2D')], 
                            type='response')>.5, na.rm=T) > sum(predict(model, 
                                                                        aux[,c('coord1D', 'coord2D')], 
                                                                        type='response')<.5, na.rm=T)
      vr[j-5] = as.numeric(vr[j-5] == (sum(vd)>sum(!vd)))
    }
    if (i==1) {
      res <- data.frame(vf,vc,vr)
    } else {
      res <- data.frame(res, vr)
    }
    vf <- NULL
    vc <- NULL
    vr <- NULL
  }
  return(res)
}

## End analysis argentina
brares1 <- gen_patterns(resbds_bra_aleat, 1, 0)
brares2 <- gen_patterns(resbds_bra_patt, 1, 0)

## Some numbers
round(prop.table(table(brares1$vf/(brares1$vc+brares1$vf)<.6)), digits=2)
round(prop.table(table(brares1$vf>brares1$vc)), digits=2)

round(prop.table(table(brares2$vf/(brares2$vc+brares2$vf)<.6)), digits=2)
round(prop.table(table(brares2$vf>brares2$vc)), digits=2)


## Graphs
data1<-lcalc(brares2)
p<-ggplot(data1, aes(x=vote, y=mms)) + 
  geom_line( aes(x=as.numeric(vote), y=mms)) + 
  geom_line( aes(x=as.numeric(vote), y=vf), colour="red") +
  geom_line( aes(x=as.numeric(vote), y=vc), colour="blue") +
  scale_colour_manual(values = c("black","red", "blue"))+
  geom_hline(yintercept=257/2)
p
ggsave(filename='bravot.pdf',plot=p, width=8, height=6)

data1<-lcalc(aux)
p<-ggplot(data1, aes(x=vote, y=mms)) + 
  geom_line( aes(x=as.numeric(vote), y=mms)) + 
  geom_line( aes(x=as.numeric(vote), y=vf), colour="red") +
  geom_line( aes(x=as.numeric(vote), y=vc), colour="blue") +
  scale_colour_manual(values = c("black","red", "blue"))+
  geom_hline(yintercept=257/2)
p


## Colombia
col_data <- read.xlsx('data_ABC.xlsx', sheetIndex=3, as.data.frame=T) 
xtable(col_data)
col_data_corr <- read.xlsx('data_ABC.xlsx', sheetIndex=6, as.data.frame=T) 
col_data_corr<-subset(col_data_corr, notcorr>0|Corr>0)
ncorr<-tapply(col_data_corr$notcorr, col_data_corr$PARTIDO, sum)
corr <- tapply(col_data_corr$Corr, col_data_corr$PARTIDO, sum) 
ncorr <- data.frame(ncorr, corr)
rm(corr)
ncorr$diff <- ncorr$corr-ncorr$ncorr
ncorr_col<-ncorr
ncorr_col<-na.omit(ncorr_col)
xtable(ncorr_col[order(ncorr_col$diff, decreasing=T),], digits=0)

load('wnomcol.RData')
ipcol <- lm(coord1D~as.factor(party)+as.factor(prov), data=wnomcol$legislators)
xtable(anova(ipcol), digits=2)
ipcol <- lm(coord2D~as.factor(party)+as.factor(prov), data=wnomcol$legislators)
xtable(anova(ipcol), digits=2)
ipcol <- wnomcol$legislators

## Start with Argentina
## Avgprov
avg_prov <- function (dat) {
  if (dim(dat)[1]==0) return(c(runif(2,-1,1), runif(2, .1, .5)))
  if (dim(dat)[1]==1) return(c(ifelse(!is.na(mean(dat$coord1D, na.rm=T)),
                                      yes= mean(dat$coord1D, na.rm=T),no=runif(1,-1,1)),
                               ifelse(!is.na(mean(dat$coord1D, na.rm=T)),
                                      yes= mean(dat$coord1D, na.rm=T),no=runif(1,-1,1)),
                               runif(2, .1, .5)))
  if (dim(dat)[1]>1) return(c(ifelse(!is.na(mean(dat$coord1D, na.rm=T)), 
                                     yes= mean(dat$coord1D, na.rm=T),no=runif(1,-1,1)),
                              ifelse(!is.na(mean(dat$coord2D, na.rm=T)), 
                                     yes= mean(dat$coord2D, na.rm=T),no=runif(1,-1,1)),
                              ifelse(!is.na(sd(dat$coord1D, na.rm=T)), 
                                    yes=sd(dat$coord1D, na.rm=T),no=runif(1,.1,.5)),
                              ifelse(!is.na(sd(dat$coord2D, na.rm=T)), 
                                    yes=sd(dat$coord2D, na.rm=T),no=runif(1,.1,.5))))
  return(c(runif(2,-1,1), runif(2, .1, .5)))
}

## ComplParty
# Enter: dat: prov, party, ncorr, corr
# Enter: dat2: party, prov, coord1D, coord2D
genrand <- function(dat, dat2, ttlr=F, N=200) {
  legs <- list()
  rem <- list()
  names(dat) <- c('provincia', 'party', 'ncorr', 'corr')
  names(dat2) <- c('party', 'provincia', 'coord1D', 'coord1D')
  dat2 <- na.omit(dat2)
  row.names(dat2)<-NULL
  incr <- dat$corr-dat$ncorr
  # Totally random: baseline for test
  for (i in 1:length(incr)) {
    if (incr[i]>0) { # Add case
      if (ttlr==T) {
        for (j in 1:incr[i]){
          an1D <- runif(N, -1, 1); an2D <- runif(N, -1, 1)
          legs <- append(legs, list(data.frame(provincia = as.character(dat$provincia[i]), 
                                               party = as.character(dat$party[i]),
                                               coord1D=an1D, coord2D = an2D)))
        }
      } else {
        for (j in 1:incr[i]) {
          aux <- subset(dat2, party==as.character(dat$party[i]))
          if(dim(aux)[1]==0) aux <- dat2
          aux <- subset(aux, provincia==as.character(dat$provincia[i]))
          vecdat <- avg_prov(aux)
          an1D <- rnorm(N, mean=vecdat[1], sd=vecdat[3])
          an2D <- rnorm(N, mean=vecdat[2], sd=vecdat[4])
          legs <- append(legs, list(data.frame(provincia = as.character(dat$provincia[i]), 
                                               party = as.character(dat$party[i]),
                                               coord1D=an1D, coord2D = an2D)))
        }
      }
    } else if (incr[i]<0) {
      aux <- subset(dat2, party==as.character(dat$party[i]))
      if(dim(aux)[1]==0) aux <- dat2
      aux <- subset(aux, provincia==as.character(dat$provincia[i]))
      if(dim(aux)[1]==0) aux <- subset(dat2, provincia==as.character(dat$provincia[i]))
      if(dim(aux)[1]!=0) {
        takeout <- row.names(aux)
        takeout <- sample(takeout, abs(incr[i]))
        rem <- append(rem, list(data.frame(provincia = as.character(dat$provincia[i]), 
                                           party = as.character(dat$party[i]),
                                           takeout=takeout)))
      }
    }
  }
  return(list(legs, rem))
}

## Argentina generate random
load('wnomarg.RData'); iparg<-wnomarg$legislators
argrand <- genrand(arg_data_corr[,c(1,2,3,5)], iparg[,c('party', 'provincia', 'coord1D', 'coord2D')], ttlr=T)
argrand_pattern <- genrand(arg_data_corr[,c(1,2,3,5)], iparg[,c('party', 'provincia', 'coord1D', 'coord2D')])

## Run rand
build_df <- function(dat, dat2, vots) {
  row.names(dat2) <- NULL
  tkot <- dat[[2]]
  simdat <- dat[[1]]
  vectkot <- character()
  for (i in 1:length(tkot)) {
    vectkot <- append(vectkot, as.character(tkot[[i]][,'takeout']))
  }
  dat2 <- na.omit(dat2)
  matvot <- as.data.frame(vots$votes)
  namevots <- names(matvot)
  matvot$diputado <- row.names(matvot)
  resbds <- list()
  for (i in 1:200) {
    resrapd <- data.frame()
    for (j in 1:length(simdat)) {
      resrapd <- rbind(resrapd, data.frame(diputado='Inserted', data.frame(simdat[[j]][i,])))
    }
    aux <- dat2
    aux <- aux[setdiff(row.names(dat2),vectkot),]
    aux <- rbind(dat2, resrapd)
    aux <- join(aux, matvot[,c('diputado',namevots)])
    resbds[[i]] <- aux
  }
  return(resbds)
}
load('argvotes.RData')
load('rcarg.RData')

resbds_arg_aleat <- build_df(argrand, iparg[,c('diputado', 'provincia', 'party', 'coord1D', 'coord2D')], rcarg)
resbds_arg_patt<- build_df(argrand_pattern, iparg[,c('diputado', 'provincia', 'party', 'coord1D', 'coord2D')], rcarg)

# Regs and preds
gen_patterns <- function(bdd, aff, neg) {
  nams <- names(bdd[[1]][,-c(1:5)])
  for (i in 1:length(bdd)) {
    aux <- bdd[[i]]
    vf <- numeric()
    vc <- numeric()
    vr <- numeric()
    for (j in 6:dim(aux)[2]) {
      vd <- rep(NA, dim(aux)[2]-5)
      aux[,j] = as.character(aux[,j])
      vd[aux[,j]==aff] = 1
      vd[aux[,j]==neg] = 0
      model <- glm(vd~coord1D+coord2D, family=binomial('logit'), data=aux)
      if (i==1) {
        vf[j-5] = sum(vd, na.rm=T)
        vc[j-5] = sum(!vd, na.rm=T)
      }
      vr[j-5] = sum(predict(model, aux[,c('coord1D', 'coord2D')], type='response')>.5, na.rm=T)
    }
    if (i==1) {
      res <- data.frame(vf,vc,vr)
    } else {
      res <- data.frame(res, vr)
    }
    vf <- NULL
    vc <- NULL
    vr <- NULL
  }
  return(res)
}

## End analysis argentina
aux <- gen_patterns(resbds_arg_aleat, 'AFIRMATIVO', 'NEGATIVO')
aux2 <- gen_patterns(resbds_arg_patt, 'AFIRMATIVO', 'NEGATIVO')

## Graphs Argentina case
lcalc <- function(res, vm = 65) {
  res2 <- t(res)
  res2 <- res2[-c(1,2),]
  mms <- apply(res2, 2, mean)
  sds <- apply(res2, 2, sd)
  vote <- seq(1, length(mms), 1)
  vots <- data.frame(vote, mms=mms-vm, sds, vc=t(res)[2,], vf=t(res)[1,])
  vots<-vots[order(mms), ]
  vots <- subset(vots, sds>0)
  vots$vote <- 1:(dim(vots)[1])
  return(vots)
}

## Some numbers
round(prop.table(table(aux$vf/(aux$vc+aux$vf)<.6)), digits=2)
round(prop.table(table(aux$vf>aux$vc)), digits=2)

## Graphs
data1<-lcalc(aux2)
p<-ggplot(data1, aes(x=vote, y=mms)) + 
  geom_line( aes(x=as.numeric(vote), y=mms)) + 
  geom_line( aes(x=as.numeric(vote), y=vf), colour="red") +
  geom_line( aes(x=as.numeric(vote), y=vc), colour="blue") +
  scale_colour_manual(values = c("black","red", "blue"))+
  geom_hline(yintercept=257/2)
p
ggsave(filename='argvot.pdf',plot=p, width=8, height=6)

data1<-lcalc(aux)
p<-ggplot(data1, aes(x=vote, y=mms)) + 
  geom_line( aes(x=as.numeric(vote), y=mms)) + 
  geom_line( aes(x=as.numeric(vote), y=vf), colour="red") +
  geom_line( aes(x=as.numeric(vote), y=vc), colour="blue") +
  scale_colour_manual(values = c("black","red", "blue"))+
  geom_hline(yintercept=257/2)
p
