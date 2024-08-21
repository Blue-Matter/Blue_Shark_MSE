# ==================================================================================================================
# === A Demonstration MSE for North Atlantic Blue Shark ============================================================
# ==================================================================================================================

# Tom Carruthers
# August 2024

# Notes 
# Follows the technical components of the MSE roadmap (SCRS/2024/103) 'the roadmap'

# === Prerequisites =============================================================================

library(openMSE)
library(r4ss)
source('Source/Input_modifiers.R')
source('Source/SS3_Source.R')


# === Technical Milestone 1 =====================================================================

# --- Condition Reference Set -----------------------------------------------

OM_RefCase = SS2OM('Assessment/Preliminary_Run_6_input',nsim=24)                 # sample var-covar to make OpenMSE class OM
Data = SS2Data('Assessment/Preliminary_Run_6_input')                             # convert SS3 input data to OpenMSE class Data
Data@CAL = array(NA,c(1,1,1))                                                    # don't simulate CAL data
Data@MPrec = Data@Cat[1,ncol(Data@Cat)]                                          # assume that the recent catch observation is the current TAC
OM_RefCase@cpars$Data = Data                                                     # add real data to OM 

OM_grid = expand.grid(Mfac = c(3/4,4/3), h = c(0.6,0.9), dep_fac = c(2/3,3/2))   # reference operating model grid
nOM = nrow(OM_grid)                                                              # 8 total

OM_mod = function(OM, Mfac = 1, h = 0.73, dep_fac = 1, Rec_fut = 1, DCV = 0.15){ # OM modifier
  OM@cpars$M_ageArray = OM@cpars$M_ageArray * Mfac
  OM@h = h
  fut_yrs = (OM@maxage+OM@nyears):(OM@nyears+OM@proyears+OM@maxage)
  OM@cpars$Perr_y[,fut_yrs] = OM@cpars$Perr_y[,fut_yrs] * Rec_fut
  OM@cpars$qs = NULL
  OM@cpars$D = trlnorm(OM@nsim,OM@D * dep_fac, DCV)
  OM
}

for(i in 1:nOM){
  OM = OM_mod(OM_RefCase, OM_grid$Mfac[i], OM_grid$h[i], OM_grid$dep_fac)        # make reference case OM
  saveRDS(runMSE(OM,Hist=T),paste0("OMs/OM_",i,".rds"))                         # save historical OM dynamics (inc ref pts etc)
}  


# --- Develop Reference MP --------------------------------------------------

Ref_MP = FMSYref75()                                                             # for now, just use 75% FMSY (perfect info) as reference


# === Technical Milestone 2 =====================================================================

# --- MP archetypes ---------------------------------------------------------

Data = readRDS('OMs/OM_1.rds')@Data; x = 1

# calculates a TAC from a TAC modifier, maximum TAC changes and maxTAC
doRec = function(Data, mod, maxchng, maxTAC){ 
  if(mod > 1+maxchng)mod = 1+maxchng
  if(mod < 1-maxchng)mod = 1-maxchng
  Rec = new('Rec')
  Rec@TAC = min(Data@MPrec[x]*mod, maxTAC)
  Rec
}

I_targ = function(x, Data, reps = 1, targ = 1, nyrs = 3, maxchng = 0.2, maxTAC = 5E5, Ind = 9){
  I = Data@AddInd[x,Ind,]/mean(Data@AddInd[x,Ind,39:43],na.rm=T)
  recI = mean(I[length(I)-(nyrs-1):0])
  mod = recI/targ
  doRec(Data, mod, maxchng, maxTAC)
}

I_rat = function(x, Data, reps = 1, targ = 2, nyrs = 3, maxchng = 0.2, maxTAC = 5E5, Ind =9){
  CpI = mean(Data@Cat[,39:43]) / mean(Data@AddInd[x,Ind,39:43],na.rm=T)
  I = Data@AddInd[x,Ind,]
  recI = mean(I[length(I)-(nyrs-1):0])
  PropTAC = recI * CpI * targ
  mod = PropTAC / Data@MPrec[x]
  doRec(Data, mod, maxchng, maxTAC)
}  

I_slp = function(x, Data, reps=1, targ = 0, nyrs = 5, fac = 1, maxchng = 0.2, maxTAC = 5E5, Ind = 9){
  I = Data@AddInd[x,Ind,]/mean(Data@AddInd[x,Ind,39:43],na.rm=T)
  slp = lm(y~x,data=data.frame(x=1:nyrs,y=I[length(I)-(nyrs-1):0]))$coefficients[[2]]
  mod = exp(slp*fac)
  doRec(Data, mod, maxchng, maxTAC)
}

class(I_targ) = class(I_rat) = class(I_slp) = "MP"


# === Technical Milestone 3 =====================================================================

# --- MP derivatives --------------------------------------------------------

It_10 = It_20 = It_M40 = I_targ
Ir_10 = Ir_20 = Ir_M40 = I_rat
Is_10 = Is_20 = Is_M40 = I_slp

formals(It_10)$maxchng = formals(Ir_10)$maxchng = formals(Is_10)$maxchng = 0.1
formals(It_M40)$maxTAC = formals(Ir_M40)$maxTAC = formals(Is_M40)$maxTAC = 4E5

class(It_10) = class(It_20) = class(It_M40) = 
  class(Ir_10) = class(Ir_20) = class(Ir_M40) = 
    class(Is_10) = class(Is_20) = class(Is_M40) = "MP"

allMPs = paste(rep(c("It","Ir","Is"),each=3),c("10","20","M40"),sep="_")


# --- Demo MSE --------------------------------------------------------------

OM_1 = readRDS('OMs/OM_1.rds')
testMSE = Project(OM_1,c("It_20","Ir_20","Is_20"))
Pplot(testMSE)

demoMSE = Project(OM_1, allMPs)
saveRDS(demoMSE,"MSEs/demoMSE.rds")

# --- MP tuning -------------------------------------------------------------





# === Results Presentation ======================================================================

# --- Shortlisting ----------------------------------------------------------


# --- Selection -------------------------------------------------------------



# === ECP =======================================================================================








