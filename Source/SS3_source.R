# === SS R functions ======================================================

# Tom Carruthers
# January 2024

# --- Input / Output / Executing ---------------

# Writes SS files from an input list object to a particular directory
writeSSfiles = function(input,dir){
  # dir = here::here('assessment/SS3/single'); input = readRDS(here::here('assessment/data/Base.rda')) 
  SS_writedat(input$dat, paste0(dir,"/data.ss"),overwrite=T)
  SS_writectl(input$control, paste0(dir,"/control.ss"),overwrite = T)
  SS_writestarter(input$starter, dir,overwrite = T)
  SS_writeforecast(input$forecast,dir,overwrite = T)
  cat(paste("---  SS data, control and starter files written to",dir,"  ---")); cat("\n")
}

removeSSfiles = function(dir){
  files = list.files(dir)
  files = files[files!="ss.exe" & !grepl(".rdata",files) & !grepl(".rds",files)]
  if(length(files)>0){
    for(i in 1:length(files))file.remove(paste0(dir,"/",files[i]))
    cat(paste("---  All ss input/output files deleted from",dir,"  ---")); cat("\n")
  }else{
    cat(paste("---  No additional SS files to be deleted from",dir,"  ---")); cat("\n")
  }
}

runSS3 = function(dir,hess=T,mcmc=F, nits=100000,thin=1000,show.output=F){
  
  mywd = getwd() # get current working directory
  setwd(dir)
  
  if(hess&!mcmc){
    system("ss.exe",wait=T,show.output.on.console = show.output)
    return(paste("SS3 ran with hessian calculation at",dir))
  }else if(!mcmc){
    system("ss.exe -est",wait=T,show.output.on.console = show.output)
    return(paste("SS3 ran at",dir))
  }else{
    system(paste0("ss.exe -mcmc ",nits," -mcsave ",thin),wait=T,show.output.on.console = show.output)
    system("ss.exe -mceval",wait=T,show.output.on.console = show.output)
    return(paste("SS3 ran with mcmc calculation at",dir))
  }
  
  setwd(mywd) # reset working directory
  
}

doSS3run = function(input, dir, analysis="SS3_Run", doplots=T, clean = T, sumfile=NA,
                    hess = T, mcmc = F, nits = 100000, thin = 50, show.output=T){
  
  writeSSfiles(input,dir)                        # write input object to SS input files
  runSS3(dir, hess = hess, mcmc=mcmc, nits=nits, thin=thin, show.output=show.output)           # run SS3 with Hessian estimation
  fit = SS_output(dir)                           # standard SS output
  if(doplots){
    SS_plots(fit, dir=dir)              # standard SS plots
    cat(paste0("Standard SS3 plots greated in ",dir,"/plots \n"))
  }
  sum = extract_SS(fit, name=analysis)           # smaller, slightly easier to use version
  
  if(is.na(sumfile))sumfile = paste0(dir,"/sum.rdata")               # the summary object output file
  saveRDS(sum, sumfile)                          # keep this
  cat(paste0("SS3 run completed and saved to ",sumfile, " \n"))
  if(clean){removeSSfiles(dir); cat(paste0("Assessment files deleted from ",dir, " \n"))}
  
}


# === END =====================================================================================================

