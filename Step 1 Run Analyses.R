# ==================================================================================================================
# === A Demonstration MSE for Atlantic Blue Shark ==================================================================
# ==================================================================================================================

# Tom Carruthers
# August 2024

# Notes 
# Follows the technical components of the MSE roadmap (SCRS/2024/103) 'the roadmap'

# === Prerequisites =============================================================================

library(openMSE)
library(r4ss)
setwd("C:/GitHub/Blue_shark_MSE")
source('Source/MP_tuning.R')


# === Technical Milestone 1 =====================================================================

# --- Condition Reference Set -----------------------------------------------

OM_RefCase = SS2OM('Assessment/Preliminary_Run_6_input',nsim=12)                 # sample var-covar to make OpenMSE class OM
Data = SS2Data('Assessment/Preliminary_Run_6_input')                             # convert SS3 input data to OpenMSE class Data
Data@CAL = array(NA,c(1,1,1))                                                    # don't simulate CAL data
Data@MPrec = Data@Cat[1,ncol(Data@Cat)]                                          # assume that the recent catch observation is the current TAC
OM_RefCase@cpars$Data = Data                                                     # add real data to OM 

OM_grid = expand.grid(Mfac = c(3/4,4/3), h = c(0.6,0.9), dep_fac = c(2/3,3/2))   # reference operating model grid
nOM = nrow(OM_grid)                                                              # 8 total

OM_mod = function(OM, Mfac = 1, h = 0.73, dep_fac = 1, Rec_fut = 1, DCV = 0.05){ # OM modifier
  OM@cpars$M_ageArray = OM@cpars$M_ageArray * Mfac
  OM@h = h
  fut_yrs = (OM@maxage+OM@nyears):(OM@nyears+OM@proyears+OM@maxage)
  OM@cpars$Perr_y[,fut_yrs] = OM@cpars$Perr_y[,fut_yrs] * Rec_fut
  OM@cpars$qs = NULL
  OM@cpars$D = trlnorm(OM@nsim,OM@D[1] * dep_fac, DCV)
  OM
}

for(i in 1:nOM){
  OM = OM_mod(OM_RefCase, OM_grid$Mfac[i], OM_grid$h[i], OM_grid$dep_fac[i])     # make reference case OM
  saveRDS(OM,paste0("OMs/OM_",i,".rds"))                                         # save OM 
  saveRDS(runMSE(OM,Hist=T),paste0("OMs/Hist_",i,".rds"))                        # save historical OM dynamics (inc ref pts etc)
}  


# --- Develop Reference MP --------------------------------------------------

Ref_MP = FMSYref75                                                             # for now, just use 75% FMSY (perfect info) as reference


# === Technical Milestone 2 =====================================================================

# --- MP archetypes ---------------------------------------------------------

Data = readRDS('OMs/Hist_1.rds')@Data; x = 1                                   # Data for designing MPs

# calculates a TAC from a TAC modifier, maximum TAC changes and maxTAC
doRec = function(MPrec, mod, maxchng, maxTAC){ 
  if(mod > (1+maxchng))mod = 1+maxchng
  if(mod < (1-maxchng))mod = 1-maxchng
  Rec = new('Rec')
  Rec@TAC = min(MPrec*mod, maxTAC)
  Rec
}

# Index target MP
I_targ = function(x, Data, reps = 1, targ = 2, nyrs = 3, maxchng = 0.3, maxTAC = 5E5, Ind = 9){
  I = Data@AddInd[x,Ind,]/mean(Data@AddInd[x,Ind,39:43],na.rm=T)
  recI = mean(I[length(I)-((nyrs-1):0)])
  mod = recI/targ
  doRec(Data@MPrec[x], mod, maxchng, maxTAC)
}

# Index ratio MP
I_rat = function(x, Data, reps = 1, targ = 0.5, nyrs = 3, maxchng = 0.3, maxTAC = 5E5, Ind =9){
  CpI = mean(Data@Cat[x,39:43]) / mean(Data@AddInd[x,Ind,39:43],na.rm=T)
  I = Data@AddInd[x,Ind,]
  recI = mean(I[length(I)-((nyrs-1):0)])
  PropTAC = recI * CpI * targ
  mod = PropTAC / Data@MPrec[x]
  #if(ncol(Data@Cat)==50)saveRDS(Data,"C:/temp/Data.rds")
  doRec(Data@MPrec[x], mod, maxchng, maxTAC)
}  

# Index slope MP
I_slp = function(x, Data, reps=1, targ = 0.025, nyrs = 5, fac = 1, maxchng = 0.3, maxTAC = 5E5, Ind = 9){
  I = Data@AddInd[x,Ind,]/mean(Data@AddInd[x,Ind,39:43],na.rm=T)
  slp = lm(y~x,data=data.frame(x=1:nyrs,y=I[length(I)-((nyrs-1):0)]))$coefficients[[2]]
  mod = exp((slp-targ)*fac)
  doRec(Data@MPrec[x], mod, maxchng, maxTAC)
}

class(I_targ) = class(I_rat) = class(I_slp) = "MP"


# === Technical Milestone 3 =====================================================================

# --- MP derivatives --------------------------------------------------------

It_10 = It_30 = It_M30 = I_targ
Ir_10 = Ir_30 = Ir_M30 = I_rat
Is_10 = Is_30 = Is_M30 = I_slp

formals(It_10)$maxchng = formals(Ir_10)$maxchng = formals(Is_10)$maxchng = 0.1 # set max TAC change
formals(It_M30)$maxTAC = formals(Ir_M30)$maxTAC = formals(Is_M30)$maxTAC = 3E4 # set max TAC

class(It_10) = class(It_30) = class(It_M30) = 
  class(Ir_10) = class(Ir_30) = class(Ir_M30) = 
    class(Is_10) = class(Is_30) = class(Is_M30) = "MP"

allMPs = paste(rep(c("It","Ir","Is"),each=3),c("10","30","M30"),sep="_")


# --- Demo MSE --------------------------------------------------------------

Hist_1 = readRDS('OMs/Hist_1.rds')
initMSE = Project(Hist_1,c("It_30","Ir_30","Is_30","FMSYref"))
Pplot(initMSE)
matplot(t(initMSE@Catch[,4,]),type="l")
saveRDS(initMSE,"MSEs/initMSE.rds")


# --- MP Derivatives --------------------------------------------------------

derivMSE = Project(Hist_1, allMPs)
Pplot(derivMSE)
saveRDS(derivMSE,"MSEs/derivMSE.rds")


# --- MP tuning -------------------------------------------------------------

for(i in 1:nOM) assign(paste0("Hist_",i),readRDS(paste0("OMs/Hist_",i,".rds")))
Hist_list = list(Hist_1, Hist_2, Hist_3, Hist_4, Hist_5, Hist_6, Hist_7, Hist_8)

# A function that calculates the squared difference between obtained and target mean PGK 
minfunc = function(MSE_list){ 
  PGKm = sapply(MSE_list,function(X){mean(X@SB_SBMSY>1 & X@F_FMSY < 1)})
  PGKw =  mean(PGKm) # ! this should really be mean() but this way it matches default slick table
  cat(paste0("PGKw = ",round(PGKw,6),"\n"))
  (PGKw - 0.6)^2
}

setup(cpus=8)     # do 8 MSE calcs in parallel (one per OM)
sfExport('doRec') # export any functions used by MPs

# Index target MP tuning

It_30_t = tune_MP(Hist_list,"It_30","targ",c(0.8,1.6),minfunc, tol=1E-3, parallel=T)
It_10_t = tune_MP(Hist_list,"It_10","targ",c(0.8,1.6),minfunc, tol=1E-3, parallel=T)
It_M30_t = tune_MP(Hist_list,"It_M30","targ",c(0.8,1.6),minfunc, tol=1E-3, parallel=T)

saveRDS(It_30_t,"MPs/It_30_t.rda")
saveRDS(It_10_t,"MPs/It_10_t.rda")
saveRDS(It_M30_t,"MPs/It_M30_t.rda")

# Index ratio MP tuning

Ir_30_t = tune_MP(Hist_list,"Ir_30","targ",c(0.5,0.65),minfunc, tol=1E-3, parallel=T)
Ir_10_t = tune_MP(Hist_list,"Ir_10","targ",c(0.5,0.65),minfunc, tol=1E-3, parallel=T)
Ir_M30_t = tune_MP(Hist_list,"Ir_M30","targ",c(0.6,0.85),minfunc, tol=1E-3, parallel=T)

saveRDS(Ir_30_t,"MPs/Ir_30_t.rda")
saveRDS(Ir_10_t,"MPs/Ir_10_t.rda")
saveRDS(Ir_M30_t,"MPs/Ir_M30_t.rda")

# Index slope MP tuning

Is_30_t = tune_MP(Hist_list,"Is_30","targ",c(0,0.05),minfunc, tol=1E-3, parallel=T)
Is_10_t = tune_MP(Hist_list,"Is_10","targ",c(0,0.05),minfunc, tol=1E-3, parallel=T)
Is_M30_t = tune_MP(Hist_list,"Is_M30","targ",c(0,0.05),minfunc, tol=1E-3, parallel=T)

saveRDS(Is_30_t,"MPs/Is_30_t.rda")
saveRDS(Is_10_t,"MPs/Is_10_t.rda")
saveRDS(Is_M30_t,"MPs/Is_M30_t.rda")


# --- Run all tuned MPs on all OMs -----------------------------

allMPs_t = paste0(allMPs,"_t") # MP names

# Load MPs
for(MP in seq_along(allMPs_t))assign(allMPs_t[MP],readRDS(paste0("MPs/",allMPs_t[MP],".rda")))
for(i in 1:nOM) saveRDS(Project(get(paste0("Hist_",i)), allMPs_t),paste0("MSEs/MSE_",i,".rds"))


# --- Slick script ---------------------------------------------

# --- ECP script -----------------------------------------------


# ==== END OF CODE ====================================================================







