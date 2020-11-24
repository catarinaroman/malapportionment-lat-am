## Ideal points for each country

# Pre-processing
rm(list=ls())
library(ggplot2); require(plyr)
library(rgdal); library(xlsx)
library(grid); require(stringr)
library(wnominate)
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
setwd('~/Dropbox/Academic/NYU/2013 Fall/Comparative/Paper2')

## Argentina
argvot <- read.xlsx2('votsarg_20082009.xlsx', 1)
rcarg <- rollcall(argvot[,-c(1:3)], yea='AFIRMATIVO', nay='NEGATIVO', missing='ABSTENCION',
               notInLegis='AUSENTE', legis.names=argvot[,1], legis.data=argvot[,c(1:3)])
save(rcarg, file='rcarg.RData')

# Estimating W-Nominate
wnomarg <- wnominate(rc, polarity=c(113,137))

pdf('stbehargparty.pdf', width=8, height=6)
plot_brasil(wnomarg, legend.x=1.1)
dev.off()

pdf('stbehargstate.pdf', width=8, height=6)
plot_brasil(wnomarg, legend.x=1.1, plotBy='provincia')
dev.off()

save(wnomarg, file='wnomarg.RData')

## Brazil
load('Bra_vots.RData')
#bravot <- read.xlsx2('votsbra_20072010.xlsx', 1)
names(bravot)[1:3]<-c('state', 'dep', 'party')
rc <- rollcall(bravot[,-c(1,2,3)], yea=1, nay=0,
               legis.names=bravot[,2], 
               legis.data=bravot[,c(1,2,3)])

# Estimating W-Nominate
wnombra <- wnominate(rc, polarity=c(1,27))

pdf('stbehbraparty.pdf', width=8, height=6)
plot_brasil(wnombra, legend.x=1.1)
dev.off()

pdf('stbehbrastate.pdf', width=8, height=6)
plot_brasil(wnombra, legend.x=1.1, plotBy='state')
dev.off()

save(wnombra, file='wnombra.RData') #XXXX

## Colombia
colvot <- read.xlsx2('votscol_20062010.xlsx', 1)
nomecom <- read.xlsx2('congr_col.xlsx', 1)
nomecom<-nomecom[nomecom$Departamiento!='', ]
nomecom <- join(data.frame(Diputado=colvot[,1]), nomecom)
names(nomecom)<-c('dep', 'prov', 'party')
rccol <- rollcall(colvot[,-c(1,2)], yea='Aprobado', nay='Rechazado', missing='Absent',
               legis.names=colvot[,1], legis.data=nomecom)

# Estimating W-Nominate
wnomcol <- wnominate(rc, polarity=c(18,21))

pdf('stbehcolparty.pdf', width=8, height=6)
plot_brasil(wnomcol, legend.x=1.1)
dev.off()

pdf('stbehcolstate.pdf', width=8, height=6)
plot_brasil(wnomcol, legend.x=1.1, plotBy='prov')
dev.off()

save(wnomcol, file='wnomcol.RData')
