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
yrs = 2014:2020
yr_lab = 2014 : 2020
ny = length(yrs)
inds = c(1,2,4,5,7,8,9,10)
ni = length(inds)
MP = "Ir_10_t"

ECP = list()

# PPD -----------------------------

PPD = array(NA,c(nsim,nOM,ni,ny))
dimnames(PPD)=list(paste0("Sim_",1:nsim),
                   paste0("Ref_",1:nOM),
                   paste0("Ind_",inds),
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
simno = 3
ECP$Obs = PPD[simno,1,,] # just take a simulation since there are no observations

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

ECP$First_Yr = 2014

ECP$Version = packageVersion('ECP')
ECP$Sys.time = Sys.time()


saveRDS(ECP, "ECP/ECP_BSH.rda")

ECP=readRDS("ECP/ECP_BSH.rda")

OMind = 1:8
Iind=6:7
yind = 1:6
powind=NA
tail="LB"
alp = 0.05


jpeg("Figures/ECP_1_dens.jpg",res=400,width=8,height=4.5,units="in")
  plot_dist(ECP, OMind, Iind, yind, powind=NA, tail, alp=alp,legloc='topleft',nspc=0)
dev.off()

jpeg("Figures/ECP_2_dens_std.jpg",res=400,width=7,height=4.5,units="in")
  plot_all_marg_dens(ECP,OMind, Iind, yind=1:6, col="#0000ff20",adj=3,seed=15,rand=T)
dev.off()

jpeg("Figures/ECP_3_CC.jpg",res=400,width=8,height=8,units="in")
  plot_CC(ECP, 0.5,0.75, maxn=10, OMind, Iind, powind=1,dopow=T,lnam=T,lasinv=T)
dev.off()

jpeg("Figures/ECP_4_dens_alt.jpg",res=400,width=8,height=4.5,units="in")
  plot_dist(ECP, OMind, Iind, yind, powind=1, tail, alp=alp,legloc='topleft',nspc=0)
dev.off()

jpeg("Figures/ECP_5_seq_pow.jpg",res=400,width=8,height=4.5,units="in")
  Ind = Seq_Pow_Calc_Marg(ECP, OMind, Iind = 6:7, yind=1:6, powind=1, alp=0.025, tail="interval")
dev.off()

jpeg("Figures/ECP_6_seq_pow_LB.jpg",res=400,width=8,height=4.5,units="in")
  Ind = Seq_Pow_Calc_Marg(ECP, OMind, Iind = 6:7, yind=1:6, powind=1, alp=0.025, tail="LB")
dev.off()

jpeg("Figures/ECP_7_seq_pow_7.jpg",res=400,width=8,height=4.5,units="in")
  Ind = Seq_Pow_Calc_Marg(ECP, OMind, Iind = 7, yind=1:6, powind=1, alp=0.05, tail="interval")
dev.off()

jpeg("Figures/ECP_8_seq_pow_7.jpg",res=400,width=8,height=4.5,units="in")
  Ind = Seq_Pow_Calc_Marg(ECP, OMind, Iind = 6:7, yind=1:6, powind=1, alp=0.05, tail="interval")
dev.off()



