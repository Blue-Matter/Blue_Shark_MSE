
library(Slick)

setwd("C:/GitHub/Blue_shark_MSE")
source("Step 2 Define Performance Metrics.R")
slick <- Slick()

Title(slick) <- 'Demonstration Atlantic Blue Shark MSE Results'
Author(slick) <- 'Tom Carruthers'
Email(slick) <- "[tom@bluematterscience.com](mailto:tom@bluematterscience.com)"
Introduction(slick) <- '
Preliminary results for discussion with the ICCAT species group meeting
<strong>Note:</strong>These results are for demonstration purposes only and are subject to change. The results presented here do not necessarily reflect the point of view of ICCAT or other funders and in no ways anticipate ICCAT future policy in this area.
'

# MPs ----

mps = MPs()
mp_names <- paste(rep(c("It","Ir","Is"),each=3),c("10","30","M40"),sep="_")
Code(mps) <- mp_names


Label(mps) <- paste(rep(c("Index target","Index ratio","Index slope"),each=3),c("10","30","40kt"),sep="_")

Description(mps) <- paste(rep(c("Aims for a target index level","TAC is a fixed factor of index level",
                              "Aims for a target slope in index"),each=3),
                              "with",
                              c("10% max change in TAC","30% max change in TAC","40kt max TAC"))

Preset(mps) <- list('10% TAC change'=which(grepl('_10',Code(mps))),
                    '30% TAC change'=which(grepl('_30',Code(mps))),
                    'Max 40kt TAC' = which(grepl('_M40',Code(mps))))
MPs(slick) <- mps
Check(mps)




# OMs ----
oms <- OMs()

Factors(oms) <- data.frame(Factor=c(rep('M',2),
                                    rep('h', 2),
                                    rep('Depln',2)),
                           Level=c(c(3/4, 4/3),
                                   c(0.6, 0.9),
                                   c(2/3, 3/2)),
                           Description=c(c("Low M","High M"),
                                              c("Low steepness","High steepness"),
                                              c("More depleted","Less depleted"))
)

Design(oms) <- expand.grid(M=c('3/4','4/3'), h = c('0.6','0.9'), Depln =  c('2/3', '3/2'))


Preset(oms) <- list('Reference'=list(1:2, 1:2, 1:2),
                    'Low M'=list(1, 1:2, 1:2),
                    'Low steep'=list(1:2, 1, 1:2),
                    'Depleted'=list(1:2, 1:2, 1))

Check(oms)
OMs(slick) <- oms



# Time Series ----

timeseries <- Timeseries()

Code(timeseries) <- c('SB/SBMSY', 'F/FMSY', 'Catch', 'Index')
Label(timeseries) <- c('SB/SBMSY', 'F/FMSY', 'Catch', 'Index')
Description(timeseries) <- c('Spawning biomass relative to SBMSY',
                             'Fishing mortality relative to FMSY',
                             'Historical catch and projected catch (t)',
                             'Combined Index')


OM_1 = readRDS("OMs/OM_1.rds")

years <- c(rev(seq(OM_1@CurrentYr, by=-1, length.out=OM_1@nyears)),
           seq(OM_1@CurrentYr+1, by=1, length.out=OM_1@proyears))

Time(timeseries) <- years
TimeNow(timeseries) <- 2024
Value(timeseries) <- array(NA, dim=c(OM_1@nsim,
                                     nrow(oms@Design),
                                     length(mps@Code),
                                     length(Code(timeseries)),
                                     length(years))
)


MSE_dir <- 'MSEs'
mse_files <- paste0(MSE_dir, "/MSE_",1:8,".rds")
mp_codes <- unlist(lapply(strsplit(Code(mps), '_'), '[[', 1)) |> unique()

mp_index <- data.frame(MP=rep(1:length(mp_codes), each=3),
                       MP_index=seq_along(Code(mps)))

indno = 9 # index number used by the MPs

nmps = length(mps@Code)
for(mm in seq_along(mse_files)){
    mse <- readRDS(mse_files[mm])
 
    # SB/SBMSY
    histSB <- apply(mse@SSB_hist, 1:2, sum)
    histSB <- replicate(nmps, histSB)|> aperm(perm=c(1,3,2))
    sb <- abind::abind(histSB, mse@SSB, along=3)
    sb_sbmsy <- sb/replicate(nmps, mse@RefPoint$ByYear$SSBMSY)|> aperm(perm=c(1,3,2))
    Value(timeseries)[,mm,, 1, ] <- sb_sbmsy

    # F/FMSY
    FS <- array(NA, dim=c(mse@nsim, mse@nMPs, mse@nyears+mse@proyears))
    FS[,,1:mse@nyears] <-  replicate(mse@nMPs, matrix(mse@FM_hist, mse@nsim, mse@nyears)) |>
      aperm(perm=c(1,3,2))
    FS[,,(mse@nyears+1):(mse@nyears+mse@proyears)] <- mse@FM
    f_fmsy <- FS/aperm(array(mse@RefPoint$ByYear$FMSY, c(mse@nsim, mse@nyears+mse@proyears,mse@nMPs)),c(1,3,2))
    Value(timeseries)[,mm,, 2, ] <- f_fmsy

    # Catch
    Catch <- array(NA, dim=c(mse@nsim, mse@nMPs, mse@nyears+mse@proyears))
    Catch[,,1:mse@nyears] <-  replicate(mse@nMPs, matrix(mse@CB_hist, mse@nsim, mse@nyears)) |>
      aperm(perm=c(1,3,2))
    Catch[,,(mse@nyears+1):(mse@nyears+mse@proyears)] <- mse@Catch
    Value(timeseries)[,mm,, 3, ] <- Catch

    # Index
    for (j in seq_along(mps@Code)) {
      Value(timeseries)[,mm,j, 4, 1:(mse@nyears+mse@proyears-1)] <- mse@PPD[[j]]@AddInd[,indno,]
    }
  
}

Target(timeseries) <- c(1, 1, NA, NA)
Limit (timeseries) <- c(0.4, NA, NA, NA)
Preset(timeseries)
Timeseries(slick) <- timeseries


# ---- Quilt -----
quilt <- Quilt()
Code(quilt) <- c('AvTAC_long', 'AvTAC_med', 'AvTAC_short',
                 'nLRP', 'PGK', 'PGK_med', 'PGK_short',
                 'PNOF', 'VarC')

Label(quilt) <- Code(quilt)
mse <- readRDS(mse_files[1])

# get captions
desc <- rep('', length(Code(quilt)))
for (p in seq_along(Code(quilt))) {
  desc[p] <- get(Code(quilt)[p])(mse)@Caption
}

Description(quilt) <- desc

Value(quilt) <- array(NA, dim=c(nrow(Design(oms)), length(Code(mps)), length(Code(quilt))))

for (mm in seq_along(mse_files)) {
    mse <- readRDS(mse_files[[mm]])
    pms <- Code(quilt)
    for (p in seq_along(pms)) {
      Value(quilt)[mm,,p] <- get(pms[p])(mse)@Mean
    }
}

Color(quilt) <- c('darkblue','white' )
Quilt(slick) <- quilt


# ---- Kobe ----

kobe <- Slick::Kobe()
Code(kobe) <- c('SB/SBMSY', 'F/FMSY')
Label(kobe) <- Code(kobe)
Description(kobe) <- c('SB/SBMSY', 'F/FMSY')
Time(kobe) <- 2021:2054
Value(kobe) <- array(NA, dim=c(mse@nsim, nrow(Design(oms)), length(Code(mps)),
                              length(Code(kobe)), 34))

for (mm in seq_along(mse_files)) {
  mse <- readRDS(mse_files[mm])
  Value(kobe)[,mm, , 1,] <- mse@SB_SBMSY[,,1:34]
  Value(kobe)[,mm, , 2,] <- mse@F_FMSY[,,1:34]
}
 
Kobe(slick) <- kobe

# save Slick ----

saveRDS(slick, "Slick/Slick_BSH.rda")

