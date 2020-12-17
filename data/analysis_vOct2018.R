#######
## Paper: Legislative outcomes and malapportionment
#######

# Set directory and load packages
library(ggplot2); require(tidyverse)
library(rgdal); library(haven)
library(grid); require(stringr)
library(wnominate); library(xtable)
library(readxl); library(sp)
library(gridExtra); library(lattice)
require(stringi)
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
            main.title = "Argentina W-NOMINATE by Party")
plot_brasil(wnomarg, legend.x=1, plotBy='provincia', 
            main.title = "Argentina W-NOMINATE by Province")
plot_brasil(wnombra, legend.x=1, 
            main.title = "Brazil W-NOMINATE by Party")
plot_brasil(wnombra, legend.x=1, plotBy='state', 
            main.title = "Brazil W-NOMINATE by Province")
plot_brasil(wnomcol, legend.x=1, 
            main.title = "Colombia W-NOMINATE by Party")
plot_brasil(wnomcol, legend.x=1, plotBy='prov', 
            main.title = "Colombia W-NOMINATE by Province")
par('mfrow' = c(1,1))
dev.off()

#####
## Simulating ideal points for new houses
#####

###
## Argentina
###
arg_data <- read_xlsx('data_ABC.xlsx', sheet=4) %>%
  filter(notcorr>0|Corr_ov_2>0)
ncorr_arg <- data.frame(ncorr = tapply(arg_data$notcorr, arg_data$partido, sum), 
                        corr = tapply(arg_data$Corr_ov_2, arg_data$partido, sum))
ncorr_arg$diff <- ncorr_arg$ncorr-ncorr_arg$corr
ncorr_arg <- na.omit(ncorr_arg)
# Table parties
write_excel_csv(ncorr_arg[order(ncorr$diff, decreasing=T), ], 'arg_tabpart.csv')
rm(ncorr_arg)

# ANOVA Argentina
iparg <- lm(coord1D~as.factor(party)+as.factor(provincia), data=wnomarg$legislators)
write_excel_csv(anova(iparg), 'anovaARG_d1.csv')
iparg <- lm(coord2D~as.factor(party)+as.factor(provincia), data=wnomarg$legislators)
write_excel_csv(anova(iparg), 'anovaARG_d2.csv')
iparg <- wnomarg$legislators
iparg <- na.omit(iparg)

## Labels corrections
# Province differences
arg_data <- subset(arg_data, notcorr>0|Corr>0)
# Adapt for the half renew rate in Argentina
arg_data$Corr_ov_2 <- NULL 
arg_data$notcorr = 2*arg_data$notcorr
names(arg_data) <- c('state', 'party', 'notcorr', 'corr')
arg_data$state <- toupper(arg_data$state)
arg_data$party <- toupper(arg_data$party)
arg_data$state <- stri_trans_general(arg_data$state,"Latin-ASCII")
arg_data$party <- stri_trans_general(arg_data$party,"Latin-ASCII")
arg_data$diff <- arg_data$corr - arg_data$notcorr

# Ideal points Argentina
names(iparg)[c(1:3)] <- c('dep', 'party', 'state')
iparg$dep <- toupper(iparg$dep)
iparg$state <- toupper(iparg$state)
iparg$party <- toupper(iparg$party)
iparg$dep <- stri_trans_general(iparg$dep,"Latin-ASCII")
iparg$state <- stri_trans_general(iparg$state,"Latin-ASCII")
iparg$party <- stri_trans_general(iparg$party,"Latin-ASCII")
# sort(unique(iparg$state))
# sort(unique(iparg$party))

# Load Roll call Votes
load('argvots.RData')

# Put form (dep,state,party)
argvots <- argvots[,c('dep', 'state', 'party', names(argvots)[-c(1:3)])]
iparg <- iparg[,c('dep', 'state', 'party', names(iparg)[-c(1:3)])]

## Simmulation
new_ipdata <- function(ipdat, diffs, ttlr = F, niter = 200) {
  res <- list()
  for (k in 1:niter) {
    aux <- data.frame()
    for (i in 1:dim(diffs)[1]) {
      if (diffs$corr[i]!=0) {
        aux2 <- subset(ipdat, state==diffs$state[i])
        aux2 <- subset(aux2, party==diffs$party[i])
        row.names(aux2) <- NULL
        for (j in 1:diffs$corr[i]) {
          if(j<=diffs$notcorr[i]) { # old data
            if(dim(aux2)[1]==0) {
              aux <- rbind(aux, 
                           data.frame(dep = 'INSERTED',
                                      state = diffs$state[i],
                                      party = diffs$party[i],
                                      coord1D = runif(1, -1, 1),
                                      coord2D = runif(1, -1, 1)))
            } else { # Select one row randomly
              aux <- rbind(aux, 
                           aux2[sample(1:dim(aux2)[1], 1),])
            }
          } else {
            if (ttlr) {
              aux <- rbind(aux, 
                           data.frame(dep = 'INSERTED',
                                      state = diffs$state[i],
                                      party = diffs$party[i],
                                      coord1D = runif(1, -1, 1),
                                      coord2D = runif(1, -1, 1)))
            } else {
              if(dim(aux2)[1]==0) {
                aux <- rbind(aux, 
                             data.frame(dep = 'INSERTED',
                                        state = diffs$state[i],
                                        party = diffs$party[i],
                                        coord1D = runif(1, -1, 1),
                                        coord2D = runif(1, -1, 1)))
              } else {
                aux <- rbind(aux, 
                             data.frame(dep = 'INSERTED',
                                        state = diffs$state[i],
                                        party = diffs$party[i],
                                        coord1D = runif(1, min(aux2$coord1D, na.rm = T), max(aux2$coord1D, na.rm = T)),
                                        coord2D = runif(1, min(aux2$coord2D, na.rm = T), max(aux2$coord2D, na.rm = T))))
              }
            }
          }
        }
      }
    }
    row.names(aux) <- NULL
    aux$dep <- as.character(aux$dep)
    aux$state <- as.character(aux$state)
    aux$party <- as.character(aux$party)
    res[[k]] <- aux
  }
  return(res)
}

aux <- new_ipdata(iparg[,c('dep', "state", "party", "coord1D", "coord2D")], 
                   arg_data, niter = 200)

# Estimating data
estdata <- function(vots, simdat, aff, neg) {
  # Start defining the baseline for each voting
  vf <- numeric()
  vc <- numeric()
  for (i in 4:dim(vots)[2]) {
    vf[i-3] = sum(vots[,i]==aff, na.rm = T)
    vc[i-3] = sum(vots[,i]==neg, na.rm = T)
  }
  res <- data.frame(vf,vc)
  # Now simulated data
  for (i in 1:length(simdat)) {
    aux <- simdat[[i]]
    auxval <- na.omit(subset(aux, dep != 'INSERTED'))
    auxinsert <- na.omit(subset(aux, dep == 'INSERTED'))
    vfs <- numeric()
    vcs <- numeric()
    # For every vote
    for (j in 4:dim(vots)[2]) {
      auxestim <- left_join(auxval, na.omit(vots[c(1,j)]), by='dep')
      auxestim[auxestim[,6]==aff&!is.na(auxestim[,6]),6] = 1
      auxestim[auxestim[,6]==neg&!is.na(auxestim[,6]),6] = 0
      auxestim[,6] = as.numeric(auxestim[,6])
      auxestim <- na.omit(auxestim)
      print(c(i,j))
      model <- glm(auxestim[,6]~coord1D+coord2D, 
                   family=binomial('logit'), data=auxestim)
      pred <- predict(model, auxinsert[,c('coord1D', 'coord2D')], type='response')
      vfs[j-3] <- sum(auxestim[,6]==aff, na.rm = T) + 
        sum(as.numeric(pred>.5), na.rm = T)
      vcs[j-3] <- sum(auxestim[,6]==neg, na.rm = T) + 
        sum(as.numeric(pred<.5), na.rm = T)
    }
    res <- data.frame(res, vfs,vcs)
  }
  return(res)
}

# Simulated response data
aux <- estdata(argvots, aux, aff = 'AFIRMATIVO', neg = 'NEGATIVO')

# Compute stats
lcalc <- function(res, lmt = F) {
  res2 <- t(res)
  mgn = res2[1,] / (res2[1,]+res2[2,])
  mgn = mgn>.4 & mgn<.6
  if(lmt) res2 <- res2[,mgn]
  aprov = res2[1,] > res2[2,]
  vecWin = numeric()
  for (j in 1:dim(res2)[2]) {
    fav = res2[seq(3,dim(res2)[1],2),j]
    ctr = res2[seq(4,dim(res2)[1],2),j]
    if (aprov[j]) vecWin[j] = sum(fav>ctr)/200
    else vecWin[j] = sum(fav<ctr)/200
  }
  return(vecWin)
}

## Some numbers
sum(aux$vf>aux$vc)
length(aux$vf)
compt <- ((aux$vf/(aux$vc+aux$vf))>.4)&((aux$vf/(aux$vc+aux$vf))<.6)
sum(compt)
sum(compt)/length(aux$vf)
sum(aux$vf[compt]>aux$vc[compt])

## Graphs
data1 <- data.frame(Country = 'Argentina',
                          Var = 'Full data', 
                          value = mean(lcalc(aux)), 
                          sd = sd(lcalc(aux))/sqrt(200))
data1 <- rbind(data1, 
               data.frame(Country = 'Argentina',
                          Var = 'Competitive votes\n(40-60 disagreement)', 
                          value = mean(lcalc(aux, lmt = T)), 
                          sd = sd(lcalc(aux, lmt = T))/sqrt(200)))
data1

p <- data1 %>%
  filter(Country=='Argentina') %>%
  ggplot() +
  geom_bar(aes(x=Var, y=value), stat="identity", 
           fill="skyblue", alpha=0.5) +
  geom_errorbar( aes(x=Var, ymin=value-1.96*sd, 
                     ymax=ifelse(value+1.96*sd>1,1,value+1.96*sd)), width=0.4, 
                 colour="orange", alpha=0.9, size=1.3)+
  geom_text(aes(x=Var, y = value+0.06,    # nudge above top of bar
                label = paste0(round(value*100, digits = 2), '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 4) + ylim(c(0,1)) +
  labs(x = 'Simulations Argentina', y = 'Percentage correct predicted')+
  theme_bw()
p
ggsave(filename='argvot.png',plot=p, width=8, height=6)

###
## Brazil
###
bra_data <- read_xlsx('data_ABC.xlsx', sheet=5) %>%
  filter(notcorr>0|corr>0)
names(bra_data) <- c('state', 'party', 'notcorr', 'corr')
ncorr_bra <- data.frame(ncorr = tapply(bra_data$notcorr, bra_data$party, sum), 
                        corr = tapply(bra_data$corr, bra_data$party, sum))
ncorr_bra$diff <- ncorr_bra$ncorr-ncorr_bra$corr
ncorr_bra <- na.omit(ncorr_bra)
# Table parties
write_excel_csv(ncorr_bra[order(ncorr_bra$diff, decreasing=T), ], 'bra_tabpart.csv')
rm(ncorr_bra)

# ANOVA Brazil
ipbra <- lm(coord1D~as.factor(party)+as.factor(provincia), data=wnomarg$legislators)
write_excel_csv(anova(ipbra), 'anovaBRA_d1.csv')
ipbra <- lm(coord2D~as.factor(party)+as.factor(provincia), data=wnomarg$legislators)
write_excel_csv(anova(ipbra), 'anovaBRA_d2.csv')
ipbra <- wnombra$legislators
ipbra <- na.omit(ipbra)

## Labels corrections
bra_data$state <- toupper(bra_data$state)
bra_data$party <- toupper(bra_data$party)
bra_data$state <- stri_trans_general(bra_data$state, "Latin-ASCII")
bra_data$party <- stri_trans_general(bra_data$party, "Latin-ASCII")
bra_data$party <- gsub(' ', '', bra_data$party)

# Ideal points Argentina
names(ipbra)[c(1:3)] <- c('state', 'dep', 'party')
ipbra$dep <- toupper(ipbra$dep)
ipbra$state <- toupper(ipbra$state)
ipbra$party <- toupper(ipbra$party)
ipbra$dep <- stri_trans_general(ipbra$dep,"Latin-ASCII")
ipbra$state <- stri_trans_general(ipbra$state,"Latin-ASCII")
ipbra$party <- stri_trans_general(ipbra$party,"Latin-ASCII")
# sort(unique(ipbra$state))
# sort(unique(ipbra$party))
# sort(unique(bra_data$state))
# sort(unique(bra_data$party))

# Fix party name
ipbra$party[ipbra$party=='PV1'] = 'PV'

# Load roll call votes
load('bravots.RData')
bravot$dep <- toupper(bravot$dep)
bravot$state <- toupper(bravot$state)
bravot$party <- toupper(bravot$party)
bravot$dep <- stri_trans_general(bravot$dep,"Latin-ASCII")
bravot$state <- stri_trans_general(bravot$state,"Latin-ASCII")
bravot$party <- stri_trans_general(bravot$party,"Latin-ASCII")
bravot <- bravot[,-184] #remove bad votes

# Put form (dep,state,party)
bravot <- bravot[,c('dep', 'state', 'party', names(bravot)[-c(1:3)])]
ipbra <- ipbra[,c('dep', 'state', 'party', names(ipbra)[-c(1:3)])]

# Simulate data
aux <- new_ipdata(ipbra[,c('dep', "state", "party", "coord1D", "coord2D")], 
                  bra_data, niter = 200)

# Simulated response data
aux <- estdata(bravot, aux, aff = 1, neg = 0)

## Some numbers
sum(aux$vf>aux$vc)
length(aux$vf)
compt <- ((aux$vf/(aux$vc+aux$vf))>.4)&((aux$vf/(aux$vc+aux$vf))<.6)
sum(compt)
sum(compt)/length(aux$vf)
sum(aux$vf[compt]>aux$vc[compt])

## Graphs
data1 <- rbind(data1, 
               data.frame(Country = 'Brazil',
                          Var = 'Full data', 
                          value = mean(lcalc(aux)), 
                          sd = sd(lcalc(aux))/sqrt(200)))
data1 <- rbind(data1, 
               data.frame(Country = 'Brazil',
                          Var = 'Competitive votes\n(40-60 disagreement)', 
                          value = mean(lcalc(aux, lmt = T)), 
                          sd = sd(lcalc(aux, lmt = T))/sqrt(200)))

p <- data1 %>%
  filter(Country=='Brazil') %>%
  ggplot() +
  geom_bar( aes(x=Var, y=value), stat="identity", 
            fill="skyblue", alpha=0.5) +
  geom_errorbar(aes(x=Var, ymin=value-1.96*sd, 
                     ymax=ifelse(value+1.96*sd>1,1,value+1.96*sd)), 
                           width=0.4, 
                 colour="orange", alpha=0.9, size=1.3)+
  geom_text(aes(x=Var, y = value+0.04,    # nudge above top of bar
                label = paste0(round(value*100, digits = 2), '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 4)+
  labs(x = 'Simulation Brazil', y = 'Percentage correct predicted')+
  theme_bw()
p

#####
## Colombia
#####
col_data <- read_xlsx('data_ABC.xlsx', sheet=3) 
col_diffbystate <- read_xlsx('data_ABC.xlsx', sheet=6) 
col_data_corr<-subset(col_diffbystate, notcorr>0|Corr>0)
ncorr<-tapply(col_data_corr$notcorr, col_data_corr$PARTIDO, sum)
corr <- tapply(col_data_corr$Corr, col_data_corr$PARTIDO, sum) 
ncorr <- data.frame(ncorr, corr)
rm(corr)
ncorr$diff <- ncorr$corr-ncorr$ncorr
ncorr_col<-ncorr
ncorr_col<-na.omit(ncorr_col)

# Ideal points
load('wnomcol.RData')
ipcol <- lm(coord1D~as.factor(party)+as.factor(prov), data=wnomcol$legislators)
anova(ipcol)
ipcol <- lm(coord2D~as.factor(party)+as.factor(prov), data=wnomcol$legislators)
anova(ipcol)
ipcol <- wnomcol$legislators

# labels corrections
col_diffbystate <- subset(col_diffbystate, notcorr>0|Corr>0)
names(col_diffbystate) <- c('state', 'party', 'notcorr', 'corr')
col_diffbystate$state <- toupper(col_diffbystate$state)
col_diffbystate$party <- toupper(col_diffbystate$party)
col_diffbystate$state <- stri_trans_general(col_diffbystate$state,"Latin-ASCII")
col_diffbystate$party <- stri_trans_general(col_diffbystate$party,"Latin-ASCII")
col_diffbystate$diff <- col_diffbystate$corr - col_diffbystate$notcorr
names(ipcol)[c(2,3)] <- c('state', 'party')
ipcol$state <- toupper(ipcol$state)
ipcol$party <- toupper(ipcol$party)
ipcol$state <- stri_trans_general(ipcol$state,"Latin-ASCII")
ipcol$party <- stri_trans_general(ipcol$party,"Latin-ASCII")
ipcol$dep <- toupper(ipcol$dep)
ipcol$dep <- stri_trans_general(ipcol$dep,"Latin-ASCII")

# fix wrong names
col_diffbystate$state[col_diffbystate$state=='NOR DE SANTANDER'] = 'NORTE DE SANTANDER'
col_diffbystate$state[col_diffbystate$state=='CAUCA'] = 'VALLE DEL CAUCA'
col_diffbystate$state[col_diffbystate$state=='SAN ANDRES'] = 'SAN ANDRES Y PROVIDENCIA'

# load votings
load('colVots20072010.RData')

aux <- new_ipdata(ipcol[,c('dep', "state", "party", "coord1D", "coord2D")], 
                   col_diffbystate, niter = 200)

# Simulated response data
aux <- estdata(colvots, aux, aff = 'Aprobado', neg = 'Rechazado')

# Compute stats
lcalc <- function(res, lmt = F) {
  res2 <- t(res)
  mgn = res2[1,] / (res2[1,]+res2[2,])
  mgn = mgn>.4 & mgn<.6
  if(lmt) res2 <- res2[,mgn]
  aprov = res2[1,] > res2[2,]
  vecWin = numeric()
  for (j in 1:dim(res2)[2]) {
    fav = res2[seq(3,dim(res2)[1],2),j]
    ctr = res2[seq(4,dim(res2)[1],2),j]
    if (aprov[j]) vecWin[j] = sum(fav>ctr)/200
    else vecWin[j] = sum(fav<ctr)/200
  }
  return(vecWin)
}

## Some numbers
sum(aux$vf>aux$vc)
length(aux$vf)
compt <- ((aux$vf/(aux$vc+aux$vf))>.4)&((aux$vf/(aux$vc+aux$vf))<.6)
sum(compt)
sum(compt)/length(aux$vf)
sum(aux$vf[compt]>aux$vc[compt])

## Graphs
data1 <- rbind(data1, 
               data.frame(Country = 'Colombia',
                          Var = 'Full data', 
                          value = mean(lcalc(aux)), 
                          sd = sd(lcalc(aux))/sqrt(200)))
data1 <- rbind(data1, 
               data.frame(Country = 'Colombia',
                          Var = 'Competitive votes\n(40-60 disagreement)', 
                          value = mean(lcalc(aux, lmt = T)), 
                          sd = sd(lcalc(aux, lmt = T))/sqrt(200)))

data1

p <- data1 %>%
  ggplot() +
  geom_bar( aes(x=Var, y=value), stat="identity", 
            fill="skyblue", alpha=0.5) +
  geom_errorbar( aes(x=Var, ymin=value-1.96*sd, 
                     ymax=ifelse(value+1.96*sd>1,1,value+1.96*sd)), width=0.4, 
                 colour="orange", alpha=0.9, size=1.3)+
  geom_text(aes(x=Var, y = value+3.5*ifelse(sd<.01, .01, sd),    # nudge above top of bar
                label = paste0(round(value*100, digits = 2), '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 4)+
  facet_wrap(~Country)+
  ylim(c(0,1.05)) +
  labs(x = '', y = 'Percentage correct predicted')+
  theme_bw() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = .01))
p

## Save predictions
write_excel_csv(data1, 'predictions.csv')

## All plots
ggsave(filename='simvots.png', plot=p, width=8, height=6)

## EOF
