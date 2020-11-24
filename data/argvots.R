## Extracting Argentina votings

## Pre-processing
rm(list=ls())
setwd('~/Dropbox/Academic/NYU/2013 Fall/Comparative/Paper2')
require(xlsx)
library(plyr)

# Reshape votings Argentina
vot_arg <- read.xlsx2('arg_0809.xlsx', 1)
dep_dat <- unique(vot_arg[,c("diputado","distrito")])
vot_arg$idvot <- as.numeric(vot_arg$asunto)

# Put in shape for W Nominate
res <- reshape(vot_arg[,c(10,6,7,9)], v.names = 'voto', idvar = 'diputado', 
               timevar='idvot', direction = 'wide')
head(res)

for(i in 3:dim(res)[2]) {
  res[,i] <- as.character(res[,i])
  res[is.na(res[,i]),i] = 'AUSENTE'
}
head(res)

res2 <- join(res, dep_dat)
res2 <- res2[,c('diputado','bloque','distrito', names(res)[-c(1,2)])]
names(res2)[2:3] <- c('party', 'provincia')

# Saving
write.xlsx2(res2, 'votsarg_20082009.xlsx', 'argvot', row.names=F)
save.image('Argvotes.RData')
