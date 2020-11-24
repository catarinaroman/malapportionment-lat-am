## Extracting Colombia votings

## Pre-processing
rm(list=ls())
setwd('~/Dropbox/Academic/NYU/2013 Fall/Comparative/Paper2')
require(xlsx)
library(plyr)

# Vots depts
vot_bra <- read.xlsx('vot_bra.xlsx', 1)
leg_data <- read.xlsx('depf_dat_bra.xlsx', 1)

res <- reshape(vot_bra, v.names = 'Voto', idvar = 'ID', 
               timevar='Id_Votacao', direction = 'wide')

res <- join(res, leg_data[,c(1,3)])

res = data.frame(UF=res$UF, res[,-284])

row.names(res) <- NULL

bravot <- res

# Saving
write.xlsx2(res, 'votsbra_20072010.xlsx', 'bravot', row.names=F)
save.image('Bra_vots.RData')
