
# Input modifiers




# --- Stock recruitment ---------------------------------------------------------------------

# R0 init
init_R0 = function(input, lnR0=9, LO = NA, HI = NA,est=T){
  R0row = match("SR_LN(R0)",rownames(input$control$SR_parms))
  input$control$SR_parms$INIT[R0row] = lnR0
  if(!is.na(LO))input$control$SR_parms$LO[R0row] = LO
  if(!is.na(HI))input$control$SR_parms$HI[R0row] = HI
  if(!est)input$control$SR_parms$PHASE[R0row] = -50
  input
}


# specify a prior for steepness
prior_steep = function(input,mu,sd,LB){
  steeprow=grepl("steep",rownames(input$control$SR_parms))
  input$control$SR_parms$INIT[steeprow] = mu
  input$control$SR_parms$PRIOR[steeprow] = mu
  input$control$SR_parms$PR_SD[steeprow] = sd
  input$control$SR_parms$LO[steeprow] = LB
  input$control$SR_parms$PR_type[steeprow] = 2 # 1 is symmetric beta, 2 is beta
  input
}

# specify a fixed level of steepness
spec_steep = function(input, steep){ 
  steeprow=grepl("steep",rownames(input$control$SR_parms))
  input$control$SR_parms$INIT[steeprow] = steep
  input$control$SR_parms$PRIOR[steeprow] = steep
  input$control$SR_parms$PHASE[steeprow] = -1
  input
}


# Selectivity parameters

only_p5_domepar = function(input){
  
  domeind = grep("AgeSel_P_6",rownames(input$control$age_selex_parms))
  input$control$age_selex_parms$PHASE[domeind]=-50
  input$control$age_selex_parms$INIT[domeind] = -999
  input
  
}

same_male_peak = function(input){
  
  domeind = grep("AgeSel_PMalOff_1",rownames(input$control$age_selex_parms))
  input$control$age_selex_parms$PHASE[domeind]=-50
  input$control$age_selex_parms$INIT[domeind] = 0
  input
  
}



prior_dome_selex_weak = function(input, sd=0.5){
  
  domeind = grep("AgeSel_PMalOff_5",rownames(input$control$age_selex_parms))
  input$control$age_selex_parms$INIT[domeind] = 0.5
  input$control$age_selex_parms$PR_SD[domeind] = sd
  input$control$age_selex_parms$PR_type[domeind] = 1 # 1 is symmetric beta, 2 is beta
  input
  
}

prior_selex_weak = function(input, sd=0.5){
  
  #estpar = input$control$age_selex_parms$PHASE > 0
  input$control$age_selex_parms$PR_SD[] = sd
  input$control$age_selex_parms$PR_type[] = 1 # 1 is symmetric beta, 2 is beta
  input
  
}

dome_fem = function(input, phase=3, comptype = c("BottomTrawl(1)","HookLine(2)","SYN(4)","HBLL(5)")){
  
  selnams = rownames(input$control$age_selex_parms)
  p6ind = match(paste0("AgeSel_P_6_",comptype),selnams)
  p4ind = match(paste0("AgeSel_P_4_",comptype),selnams)
  
  input$control$age_selex_parms$PHASE[p6ind] = phase
  input$control$age_selex_parms$PHASE[p4ind] = phase
  
  input
  
}

male_max_sel = function(input,comptype = c("HookLine(2)","IPHC(6)"),newmaxsel=1.5){
  
  selnams = rownames(input$control$age_selex_parms)
  modnams = paste0("AgeSel_PMalOff_5_",comptype)
  istype = match(modnams, selnams) # input$control$age_selex_parms[istype,]
  input$control$age_selex_parms$HI[istype] = newmaxsel
  input
  
}

male_max_sel_prior = function(input,comptype = c("HookLine(2)","IPHC(6)"), PR_type=1, PR_SD=0.5){
  
  selnams = rownames(input$control$age_selex_parms)
  modnams = paste0("AgeSel_PMalOff_5_",comptype)
  istype = match(modnams, selnams) # input$control$age_selex_parms[istype,]
  input$control$age_selex_parms$PR_type[istype] = PR_type
  input$control$age_selex_parms$PR_SD[istype] = PR_SD
  input
  
}



# --- Natural mortality rate -----------------------------------------------------------------

getMs = function(input){
  Mrows=grep("NatM",rownames(input$control$MG_parms))
  input$control$MG_parms$INIT[Mrows]
}

# specify a fixed level of steepness
spec_M = function(input, Mfem, Mmale){ 
  Mrows=grep("NatM",rownames(input$control$MG_parms))
  input$control$MG_parms$INIT[Mrows[1]] = Mfem
  input$control$MG_parms$INIT[Mrows[2]] = Mmale
  input
}


# --- Sigma R --------------------------------------------------------------------------------

# specify a fixed level of sigmaR
spec_sigmaR = function(input, sigmaR){
  sRrow = match("SR_sigmaR",rownames(input$control$SR_parms))
  input$control$SR_parms$INIT[sRrow] = sigmaR
  input
}



# ---Data weighting --------------------------------------------------------------------------

# SS3 Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 9=init_equ_catch; 
# 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 17=F_ballpark; 18=initEQregime

CAA_ESS = function(input, ESS=50,byfleet=T){
 acomp = input$dat$agecomp  
 mubyFt = aggregate(acomp$Nsamp,by=list(Flt = acomp$FltSvy),mean)
 if(byfleet){
   mult = ESS / mubyFt$x
 }else{
   mubyYr = aggregate(acomp$Nsamp,by=list(yr = acomp$Yr),mean)
   mu = mean(mubyYr$x)
   mult =  ESS / mu
 }
 nn = nrow(mubyFt)
 input$control$Variance_adjustment_list = data.frame(Data_type = rep(5,nn), 
                                                     Fleet = mubyFt$Flt, 
                                                     Value = mult, 
                                                     row.names = paste0("Variance_adjustment_list",1:nn))
 input
}

# input$dat$fleetinfo$fleetname
# SS3 Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 9=init_equ_catch; 
# 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 17=F_ballpark; 18=initEQregime

add_wt = function(input,fleet = "IPHC", Data_type = 1, wt=0.5){
  
  fleetno = match(fleet,input$dat$fleetinfo$fleetname)
  newwt = c(Data_type = Data_type, Fleet = fleetno, Value = wt)
  input$control$Variance_adjustment_list = rbind(input$control$Variance_adjustment_list,newwt)
  nlines = nrow(input$control$Variance_adjustment_list)
  row.names(input$control$Variance_adjustment_list) = paste0("Variance_adjustment_list",1:nlines)
  input
}







# --- Forecasting -----------------------------------------------------------------------------

# type: -1=none; 0=simple_1yr; 1=F(SPR); 2=F(MSY) 3=F(Btgt) or F0.1; 4=Ave F (uses first-last relF yrs); 5=input annual F scalar
spec_FC = function(input, ny, type = 2, F_scaler = 1){
  
  input$forecast$Forecast = type # 
  input$forecast$Nforecastyrs = ny
  input$forecast$F_scalar = F_scaler
  input

}


