# make ECP object


library(ECP)
library(Slick)
setwd("C:/GitHub/Blue_shark_MSE")
sl = readRDS('Slick/Slick_BSH.rda')
OM = readRDS('OMs/OM_1.rds')
Hist = readRDS('OMs/Hist_1.rds')

nOM = nrow(sl@OMs@Design)
nsim = OM@nsim
curY = OM@CurrentYr
yrs = 2011:2020
yr_lab = 2022 : 2031
ny = length(yrs)
inds = c(1,2,4,5,7,8,9,10)
ni = length(inds)
MP = "Ir_10_t"

ECP = list()

# PPD -----------------------------

PPD = array(NA,c(nsim,nOM,ni,ny))
dimnames(PPD)=list(paste0("Sim_",1:nsim),
                   paste0("Ref_",1:nOM),
                   paste0("Index_",inds),
                   yr_lab)

MSE_dir <- 'MSEs'
mse_files <- paste0(MSE_dir, "/MSE_",1:8,".rds")

for(mm in seq_along(mse_files)){
  mse = readRDS(mse_files[mm])
  MPs = mse@MPs
  MPno = match(MP,MPs)
  Data = mse@PPD[[MPno]]
  yind = match(yrs,Data@Year)
  PPD[,mm,,] = Data@AddInd[,inds,yind]
}

ECP$PPD = PPD

# OM_Design ---------------

ECP$OM_Design = sl@OMs@Design

# Defaults -----------------

Defaults = list()
Index_9 = list()
Index_9$Data = 9
Index_9$yind = 1:10
Index_9$OM$M = c("3/4","4/3")
Index_9$OM$h = c("0.6","0.9")
Index_9$OM$Depln = c("2/3","3/2")
Index_9$alph = 0.05
Index_9$powind = NaN
Index_9$tail = "interval"

ECP$Defaults$Index_9 = Index_9

# Obs ----------------------

ECP$Obs = PPD[1,1,,] # just take a simulation since there are no observations

# Pow -----------------------

Pow = array(FALSE,c(nsim,nOM,1)) # only one power definition

for(mm in seq_along(mse_files)){
  mse=readRDS(mse_files[mm])
  MPs = mse@MPs
  MPno = match(MP,MPs)
  Pow[,mm,1] = apply(mse@SB_SBMSY[,MPno,]<0.75,1,any)
}

ECP$Pow = Pow
# other

ECP$First_Yr = 2024

ECP$Version = packageVersion('ECP')
ECP$Sys.time = Sys.time()


saveRDS(ECP, "ECP/ECP_BSH.rda")

ECP_obj=readRDS("ECP/ECP_BSH.rda")
OMind = 1:8
Iind=6:7
yind = 1:6
powind=NA
tail="LB"
alp = 0.05
dens_Proj_pow(ECP_obj,Iplot=1,OMind=OMind)

plot_dist(ECP_obj, OMind, Iind, yind, powind, tail, alp=alp,legloc='topleft',nspc=0)

plot_all_marg_dens(ECP_obj,OMind, Iind, yind=1:6, col="#0000ff20",adj=3,seed=15,rand=T)
plot_CC(ECP_obj, 0.5,0.75, maxn=10, OMind, Iind,lnam=T,lasinv=T)


