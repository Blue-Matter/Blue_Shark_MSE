# Generic MP tuning function


# Hist_list = list(Hist_1, Hist_8); MP = "Ir_30"; MP_parname = 'targ'; 

minfunc = lapply(MSElist)

tune_MP = function(OM_list,MP,MP_parname,minfunc,tol=1E-2){
  
  
  assign("MPtest",get(MP))
  formals(MPtest)[[MP_parname]] = 0.6
  
  MSE_list = lapply(Hist_list,Project(X,MPs = MP))
  
  
  
}