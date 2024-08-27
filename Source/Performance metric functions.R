require(dplyr)
require(ggplot2)

.Kobe_verde <- function(MMSEobj, semester, annual = FALSE) {
  semester <- ChkYrs(semester, MMSEobj)
  sem_vector <- seq(semester[1], semester[2])

  PMobj <- new("PMobj")
  #PMobj@Name <- Name
  #PMobj@Caption <- Caption

  SB0 <- MMSEobj@RefPoint$ByYear$SSB0[, 1, 1, MMSEobj@nyears]

  FM <- apply(MMSEobj@FM[, 1, , , ], c(1, 3, 4), sum) # Take the sum of fishing mortality for Chile and Peru
  F_55 <- MMSEobj@multiHist[[1]][[1]]@Ref$ByYear$F_SPR[, "F_55%", MMSEobj@nyears]

  if (annual) {
    # Sum F over year
    year_vector <- rep(seq(1, 0.5 * length(sem_vector)), each = 2)
    FM_annual <- sapply(unique(year_vector), function(y) {
      apply(FM[, , y], 1:2, sum)
    }, simplify = "array")
    F_FMSY <- FM_annual/(2 * F_55)

    # Calculate performance metrics only for biomass at the beginning of the year
    sem_vector_biomass <- sem_vector[as.logical(sem_vector %% 2)]
    B_BMSY <- MMSEobj@SSB[, 1, , sem_vector_biomass]/(0.5 * SB0)
  } else {
    B_BMSY <- MMSEobj@SSB[, 1, , sem_vector]/(0.5 * SB0)
    F_FMSY <- FM[, , sem_vector]/F_55
  }

  PMobj@Ref <- 1
  tt <- B_BMSY >= 0.9 & F_FMSY <= 1.1
  if (is.null(dim(tt)))
    tt <- matrix(tt, nrow=MMSEobj@nsim, ncol=1)
  PMobj@Stat <- tt
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]

  return(PMobj)
}


## Zona verde ----
ZV_CP <- function(MMSEobj = NULL, semester = c(1, 5),
                  Name = "Kobe Green: Probability of being in Green Zone of Kobe Space (B>0.9BMSY & F<FMSY) in Years 1-5 (2024-2028)",
                  Caption = "Prob. Green Zone of Kobe Space (2024-2028)",
                  annual = TRUE, ...) {

  if(!inherits(MMSEobj,'MMSE')) stop('This PM method is designed for objects of class `MMSE`')

  PM <- .Kobe_verde(MMSEobj, semester)
  PM@Name <- Name
  PM@Caption <- Caption
  PM
}
class(ZV_CP) <- "PM"

ZV_MP <- ZV_LP <- ZV_CP

formals(ZV_MP)$semester <- c(6, 15)
formals(ZV_LP)$semester <- c(16, 30)

formals(ZV_MP)$Name <- "Kobe Green: Probability of being in Green Zone of Kobe Space (B>0.9BMSY & F<FMSY) in Years 6-15 (2029-2038)"
formals(ZV_MP)$Caption <- "Prob. Green Zone of Kobe Space (2029-2038)"

formals(ZV_LP)$Name <- "Kobe Green: Probability of being in Green Zone of Kobe Space (B>0.9BMSY & F<FMSY) in Years Years 16-30 (2039-2053)"
formals(ZV_LP)$Caption <- "Prob. Green Zone of Kobe Space (2039-2053)"

class(ZV_MP) <- class(ZV_LP) <- "PM"



.BMSY_PM <- function(MMSEobj, Ref = 0.9, semester = c(1, 4), annual = FALSE) {
  semester <- ChkYrs(semester, MMSEobj)
  sem_vector <- seq(semester[1], semester[2])

  # Calculate performance metrics only for biomass at the beginning of the year
  if (annual) sem_vector <- sem_vector[as.logical(sem_vector %% 2)]

  PMobj <- new("PMobj")

  SB0 <- MMSEobj@RefPoint$ByYear$SSB0[, 1, 1, MMSEobj@nyears]
  B_BMSY <- MMSEobj@SSB/(0.5 * SB0)

  PMobj@Ref <- Ref
  tt <- B_BMSY[, 1, , sem_vector] >= Ref
  if (is.null(dim(tt)))
    tt <- matrix(tt, nrow=MMSEobj@nsim, ncol=1)
  PMobj@Stat <- tt
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]

  return(PMobj)
}

## No sobreexplotado ----
NSE_CP <- function(MMSEobj = NULL, Ref = 0.9, semester = c(1, 5),
                   Name = "P[B > 0.9 BMSY] in Years 1-2 (2024-2028)",
                   Caption = "P[B > 0.9 BMSY] (2024-2028)",
                   annual = TRUE, ...) {

  if(!inherits(MMSEobj,'MMSE')) stop('This PM method is designed for objects of class `MMSE`')

  PMobj <- .BMSY_PM(MMSEobj, Ref, semester)

  PMobj@Name <- Name
  PMobj@Caption <- Caption
  return(PMobj)
}
class(NSE_CP) <- 'PM'

NSE_MP <- NSE_LP <- NSE_CP

formals(NSE_MP)$semester <- c(6, 15)
formals(NSE_LP)$semester <- c(16, 30)

formals(NSE_MP)$Name <- "P[B > 0.9 BMSY] in Years  6-15 (2029-2038)"
formals(NSE_MP)$Caption <- "P[B > 0.9 BMSY] (2029-2038)"

formals(NSE_LP)$Name <- "P[B > 0.9 BMSY] in Years 16-30 (2039-2053)"
formals(NSE_LP)$Caption <- "P[B > 0.9 BMSY] (2039-2053)"

class(NSE_MP) <- class(NSE_LP) <- "PM"

## No zona roja ----
NZR_CP <- NSE_CP

formals(NZR_CP)$Ref <- 0.5

NZR_MP <- NZR_LP <- NZR_CP

formals(NZR_MP)$semester <- c(5, 14)
formals(NZR_LP)$semester <- c(15, 30)

formals(NZR_CP)$Name <- "P[B > 0.5 BMSY] in Years 1-5 (2024-2028)"
formals(NZR_CP)$Caption <- "P[B > 0.5 BMSY] (2024-2028)"

formals(NZR_MP)$Name <- "P[B > 0.5 BMSY] in Years 6-15 (2029-2038)"
formals(NZR_MP)$Caption <- "P[B > 0.5 BMSY] (2029-2038)"

formals(NZR_LP)$Name <- "P[B > 0.5 BMSY] in Years 16-30 (2039-2053)"
formals(NZR_LP)$Caption <- "P[B > 0.5 BMSY] (2039 -2053)"

class(NZR_CP) <- class(NZR_MP) <- class(NZR_LP) <- "PM"


# Annual basis
.Catch_PM <- function(MMSEobj, semester = c(1, 30), var = FALSE) {

  semester <- ChkYrs(semester, MMSEobj)
  sem_vector <- seq(semester[1], semester[2])
  year_vector <- rep(seq(1, 0.5 * length(sem_vector)), each = 2)

  PMobj <- new("PMobj")

  p <- 1 # Anchovy stock
  f <- 1 # Chile catch; 2 = Peru

  require(dplyr)
  #browser()
  # Catch <- MMSEobj@Catch[, p, f, , sem_vector] %>% structure(
  Catch <- apply(MMSEobj@Catch[, p, , , sem_vector],c(1,3,4),sum) %>% structure(  
    dimnames = list(Simulation = 1:MMSEobj@nsim,
                    MP = MMSEobj@MPs[[1]],
                    Year = sem_vector)
  ) 
  CBA_annual = Catch
  #CBA_annual <- reshape2::acast(Catch, Simulation ~ MP ~ Year, value.var = "value")
  # CBA_annual <- Catch_annual/MMSEobj@OM[[p]][[f]]$TACFrac

  if (var) {
    ny <- max(year_vector)
    CBA_var <- abs(CBA_annual[, , 2:ny]/CBA_annual[, , 2:ny - 1] - 1)
    PMobj@Stat <- CBA_var
  } else {
    PMobj@Stat <- CBA_annual
  }

  PMobj@Ref <- NA_real_

  PMobj@Prob <- apply(PMobj@Stat, 1:2, mean)
  PMobj@Mean <- apply(PMobj@Stat, 2, mean)
  PMobj@MPs <- MMSEobj@MPs[[1]]

  return(PMobj)
}

# CBA promedio ----
CBA_CP <- function(MMSEobj = NULL, semester = c(1, 5),
                       Name = "Mean Yield in Years 1-5 (2024-2028)",
                       Caption = "Mean Yield (2024-2028)", ...) {

  if(!inherits(MMSEobj,'MMSE')) stop('This PM method is designed for objects of class `MMSE`')

  PMobj <- .Catch_PM(MMSEobj, semester)

  PMobj@Name <- Name
  PMobj@Caption <- Caption
  return(PMobj)
                       }


class(CBA_CP) <- 'PM'

CBA_MP <- CBA_LP <- CBA_CP

formals(CBA_MP)$semester <- c(6, 15)
formals(CBA_LP)$semester <- c(16, 30)

formals(CBA_MP)$Name <- "Mean Yield in Years 6-15 (2029-2038)"
formals(CBA_MP)$Caption <- "Mean Yield (2029-2038)"

formals(CBA_LP)$Name <- "Mean Yield in Years 16-30 (2039-2053)"
formals(CBA_LP)$Caption <- "Mean Yield (2039-2053)"

class(CBA_MP) <- class(CBA_LP) <- "PM"


# CBA variability
CBAv_CP <- function(MMSEobj = NULL, semester = c(1, 5),
                    Name = "CBA variability in Years 1-5 (2024-2028)",
                    Caption = "CBA variability (2024-2028)", ...) {

  if(!inherits(MMSEobj,'MMSE')) stop('This PM method is designed for objects of class `MMSE`')

  PMobj <- .Catch_PM(MMSEobj, semester, var = TRUE)

  PMobj@Name <- Name
  PMobj@Caption <- Caption
  return(PMobj)
}
class(CBAv_CP) <- 'PM'


CBAv_MP <- CBAv_LP <- CBAv_CP

formals(CBAv_MP)$semester <- c(5, 14)
formals(CBAv_LP)$semester <- c(15, 30)

formals(CBAv_MP)$Name <- "CBA variability in Years Years 6-15 (2029-2038)"
formals(CBAv_MP)$Caption <- "CBA variability (2029-2038)"

formals(CBAv_LP)$Name <- "CBA variability in Years 16-30 (2039-2053)"
formals(CBAv_LP)$Caption <- "CBA variability (2039-2053)"

class(CBAv_MP) <- class(CBAv_LP) <- "PM"


## No sobre pesca ----
.FMSY_PM <- function(MMSEobj, Ref = 1.1, semester = c(1, 5), annual = FALSE) {
  semester <- ChkYrs(semester, MMSEobj)
  sem_vector <- seq(semester[1], semester[2])

  PMobj <- new("PMobj")

  F_55 <- MMSEobj@multiHist[[1]][[1]]@Ref$ByYear$F_SPR[, "F_55%", MMSEobj@nyears]
  FM <- apply(MMSEobj@FM[, 1, , , sem_vector], c(1, 3:4), sum) # Take the sum of fishing mortality for Chile and Peru

  if (annual) {
    year_vector <- rep(seq(1, 0.5 * length(sem_vector)), each = 2)

    FM_annual <- sapply(unique(year_vector), function(y) {
      apply(FM[, , y], 1:2, sum)
    }, simplify = "array")
    F_FMSY <- FM_annual/(2 * F_55)
  } else {
    F_FMSY <- FM/F_55
  }

  PMobj@Ref <- Ref
  tt <- F_FMSY <= Ref
  if (is.null(dim(tt)))
    tt <- matrix(tt, nrow=MMSEobj@nsim, ncol=1)
  PMobj@Stat <- tt
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]

  return(PMobj)
}

NSP_CP <- function(MMSEobj = NULL, Ref = 0.9, semester = c(1, 5),
                   Name = "P[F < 1.1 FMSY] in Years 1-5 (2024-2028)",
                   Caption = "P[F < 1.1 FMSY] (2024-2028)",
                   annual = TRUE, ...) {

  if(!inherits(MMSEobj,'MMSE')) stop('This PM method is designed for objects of class `MMSE`')

  PMobj <- .FMSY_PM(MMSEobj, Ref, semester, annual = annual)

  PMobj@Name <- Name
  PMobj@Caption <- Caption
  return(PMobj)
}
class(NSP_CP) <- 'PM'

NSP_MP <- NSP_LP <- NSP_CP

formals(NSP_MP)$semester <- c(5, 14)
formals(NSP_LP)$semester <- c(15, 30)

formals(NSP_MP)$Name <- "P[F < 1.1 FMSY] in Years 6-15 (2029-2038)"
formals(NSP_MP)$Caption <- "P[F < 1.1 FMSY] (2029-2038)"

formals(NSP_LP)$Name <- "P[F < 1.1 FMSY] in Years 16-30 (2039-2053)"
formals(NSP_LP)$Caption <- "P[F < 1.1 FMSY] (2039-2053)"

class(NSP_MP) <- class(NSP_LP) <- "PM"

CBAcon <- function(mmse, sem = seq(1, 30, 2), Name = "P[CBA2 > CBA1] in Years 1-15 (2024-2038)") {

  p <- sapply(1:mmse@nMPs, function(i) {
    m <- sapply(mmse@PPD[[1]][[1]][[i]]@Misc, function(x) x$CBA[2, sem] >= x$CBA[1, sem])
    if (!is.matrix(m)) m <- matrix(NA_real_, length(sem), mmse@nsim)
    return(m)
  }, simplify = "array") %>%
    aperm(c(2, 3, 1))

  PMobj <- new("PMobj")

  Ref <- 0.5
  PMobj@Ref <- Ref
  tt <- p >= Ref
  if (is.null(dim(tt)))
    tt <- matrix(tt, nrow=mmse@nsim, ncol=1)
  PMobj@Stat <- tt
  PMobj@Prob <- calcProb(PMobj@Stat, mmse)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- mmse@MPs[[1]]

  PMobj@Name <- PMobj@Caption <- Name

  return(PMobj)
}
class(CBAcon) <- "PM"


PM_fn <- function(mmse, PMs, all_sims = FALSE) {

  if (all_sims) {
    PMobj <- sapply(1:length(PMs), function(i) {
      x <- get(PMs[i])(mmse)
      slot(x, 'Prob')
    }, simplify = "array") %>%
      structure(dimnames = list(Sim = 1:mmse@nsim, MP = mmse@MPs[[1]], PM = PMs))

  } else {
    PMobj <- sapply(1:length(PMs), function(i) {
      x <- get(PMs[i])(mmse)
      slot(x, 'Mean')
    }) %>%
      structure(dimnames = list(MP = mmse@MPs[[1]], PM = PMs)) %>%
      as.data.frame() %>%
      mutate(MP = mmse@MPs[[1]])
  }
  return(PMobj)
}

update_mean <- function(old_mean, xnext, n) (n-1)/n * old_mean + xnext/n

cumulative_mean <- function(PMobj) {
  x <- array(NA_real_, dim(PMobj)) %>% structure(dimnames = dimnames(PMobj))
  x[1, , ] <- PMobj[1, , ]
  for(i in 2:dim(x)[1]) {
    x[i, , ] <- update_mean(x[i-1, , ], PMobj[i, , ], i)
  }
  return(x)
}

plot_cumulative_PM <- function(x, MP = dimnames(x)$MP, PM = "NZR_MP") {
  df <- reshape2::melt(x) %>%
    filter(MP %in% .env$MP, PM %in% .env$PM, !is.na(value))

  g <- ggplot(df, aes(Sim, value, colour = MP)) +
    geom_line() +
    facet_wrap(vars(PM), scales = "free_y") +
    theme(
      strip.placement = "outside",
      strip.background = element_blank(),
      legend.position = "bottom"
    ) +
    labs(x = "Simulación de cada MO", y = "Valor acumulado", colour = "PM")
  g
}


# Copied code from https://github.com/pbs-assess/ggmse/blob/master/R/figures-tigure.R#L95
make_gg_df <- function(.probs_dat, .name, mp_order = NULL, do_approx = TRUE, sort_by = "decreasing",
                       relative_max = FALSE, scale_0_1 = FALSE) {
  df <- .probs_dat

  if (is.null(mp_order)) {
    if (sort_by == "decreasing") {
      df$MP <- factor(df$MP, levels = df$MP[do.call(order, df[-1])])
    } else if (sort_by == "increasing") {
      df$MP <- factor(df$MP, levels = df$MP[rev(do.call(order, df[-1]))])
    } else {
      stop("sort_by must be either 'increasing' or 'decreasing'",
           call. = FALSE
      )
    }
  } else {
    df$MP <- factor(df$MP, levels = mp_order)
  }

  df <- reshape2::melt(
    df,
    id.vars = "MP",
    variable.name = "type",
    value.name = "value"
  )

  df$txt <- vapply(df$value, function(x) {
    digits <- ifelse(is.na(x) | x >= 1000, 0,
                     ifelse(x <= 1, 2, 1))
    #digits <- ifelse(x <= 1, 2,
    #                 ifelse(x >= 1000, 0, 1))
    format(round(x, digits), big.mark = ",", decimal.mark = ".", nsmall = digits)
  }, character(1L))

  if (do_approx) {
    OutDec <- options()$OutDec # Decimal is a point or comma?
    df$txt <- gsub(paste0("1\\", OutDec, "00"), paste0(">0", OutDec , "99"), df$txt)
    df$txt <- gsub(paste0("0\\", OutDec, "00"), paste0("<0", OutDec , "01"), df$txt)
  }

  if (relative_max) {
    df <- group_by(df, type) %>%
      mutate(value = value / max(value, na.rm = TRUE)) %>%
      ungroup()
  }
  if (scale_0_1) {
    df <- group_by(df, type) %>%
      mutate(value = (value - min(value)) / (max(value) - min(value))) %>%
      ungroup()
  }
  df$MP <- as.factor(df$MP)
  df$OM <- .name
  return(df)
}

plot_table <- function(
    probs_dat,
    names,
    ncol = NULL,
    relative_max = FALSE,
    scale_0_1 = FALSE,
    sort_by = "decreasing",
    mp_order = NULL,
    satisficed = NULL,
    alpha = 0.6,
    do_approx = FALSE
) {

  require(purrr)
  require(dplyr)
  require(ggplot2)

  if (!is.list(probs_dat)) stop("probs_dat should be a list of data frames")

  df <- Map(make_gg_df, .probs_dat = probs_dat, .name = names,
            MoreArgs = list(mp_order = mp_order, do_approx = do_approx, sort_by = sort_by,
                            relative_max = relative_max, scale_0_1 = scale_0_1)) %>%
    dplyr::bind_rows()

  padding <- 0.52

  g <- ggplot(df, aes(type, MP)) +
    geom_tile(aes(fill = value), alpha = alpha, color = "white") +
    geom_text(aes(x = type, label = txt), size = ggplot2::rel(3)) +
    scale_fill_gradient2(limits = c(0, 1), midpoint = 0.5) +
    #scale_fill_viridis_c(limits = c(0, 1), begin = 0.15, end = 1, alpha = alpha, option = "D", direction = 1) +
    facet_wrap(vars(OM), scales = "free_x", ncol = ncol) +
    guides(fill = "none") +
    labs(x = NULL, y = NULL) +
    coord_cartesian(
      expand = FALSE,
      xlim = range(as.numeric(df$type)) + c(-padding, padding),
      ylim = range(as.numeric(df$MP)) + c(-padding - 0.01, padding + 0.01)
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(color = "grey10"),
      strip.placement = "outside",
      strip.background = element_blank()
    ) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(labels = levels(df$MP)) +

    if (!is.null(satisficed)) {
      h <- purrr::map_df(
        seq_along(satisficed),
        ~ dplyr::filter(df, value > satisficed[[.x]] & type == names(satisficed)[.x])
      )
      g <- g + geom_tile(data = h, color = "grey30", lwd = 0.45, fill = NA)
    }

  g
}


# Copied from https://github.com/pbs-assess/ggmse/blob/master/R/figures-bivariate.R#L26
plot_tradeoff <- function(
    pm_df_list, xvar, yvar, custom_pal = NULL,
    mp = NULL,
    mp_ref = NULL,
    nudge_x = 0, nudge_y = 0.05
) {

  require(dplyr)
  require(ggplot2)

  df <- lapply(names(pm_df_list), function(x) {
    pm_df_list[[x]] %>%
      mutate(scenario = x, MP_label = 1:nrow(.))
  }) %>%
    bind_rows()

  if (!is.null(mp)) {
    df <- dplyr::filter(df, MP %in% mp) %>% mutate(MP = factor(MP, levels = mp))
  }
  df_long <- reshape2::melt(
    df,
    id.vars = c("MP", "scenario", "MP_label"),
    value.name = "prob",
    variable.name = "pm"
  )
  df_wide <- df_long %>%
    reshape2::dcast(MP + MP_label + scenario ~ pm, value.var = "prob") %>%
    dplyr::mutate(`Reference` = MP %in% mp_ref)

  xmin <- pull(df_wide, !!xvar) %>% min()
  ymin <- pull(df_wide, !!yvar) %>% min()
  #xvar <- paste0("`", xvar, "`")
  #yvar <- paste0("`", yvar, "`")

  n_mp <- length(unique(df_wide$MP))
  ref_or_not <- dplyr::select(df_wide, .data$MP, .data$Reference) %>% dplyr::distinct()
  #mp_shapes <- vector(mode = "numeric", length = n_mp)
  mp_shapes <- ifelse(ref_or_not$Reference, 1, 16) %>%
    structure(names = as.character(ref_or_not$MP))
  mp_label <- filter(df_wide, scenario == names(pm_df_list)[[1]]) %>% pull("MP_label")

  g <- ggplot(
    df_wide,
    aes(.data[[xvar]], .data[[yvar]], colour = MP, shape = MP)
  ) +
    geom_text(aes(label = .data$MP_label), nudge_x = nudge_x, nudge_y = nudge_y) +
    geom_point(show.legend = FALSE) +
    guides(colour = guide_legend(override.aes = list(label = mp_label))) +
    facet_wrap(vars(scenario), nrow = 2) +
    scale_shape_manual(values = mp_shapes) +
    theme(
      strip.background = element_blank(),
      panel.grid.major.y = element_line(colour = "grey85"),
      panel.grid.major.x = element_line(colour = "grey85")
    ) +
    labs(colour = "PM", fill = "PM")

  if (!is.null(custom_pal)) {
    g <- g + scale_color_manual(values = custom_pal)
  }

  g
}

plot_array2 = function(MMSE, names, sims,
         type = c("SB", "B_BMSY", "F_FMSY", "CBA", "Catch_Chile", "Catch_Peru", "Acoustic_Chile", "MPH",
                  "rho", "CBA_ratio", "R"),
         annual = FALSE) {
  
  type <- match.arg(type)
  
  if (is.list(MMSE)) {
    array_df <- Map(.get_array, mmse = MMSE, .name = names, type = type, annual = annual) %>%
      dplyr::bind_rows()
    
  } else if (is(MMSE, "MMSE")) {
    array_df <- .get_array(MMSE, names, type = type, annual = annual)
  } else {
    stop("MMSE list or object not found.")
  }
  
  ylab <- switch(type,
                 "SB" = "Biomasa desovante",
                 "B_BMSY" = expression(B/B[RMS]),
                 "F_FMSY" = expression(F/F[RMS]),
                 "CBA" = "CBA",
                 "Catch_Peru" = "Captura peruana",
                 "Catch_Chile" = "Captura chilena",
                 "Acoustic_Chile" = "Crucero acústico",
                 "MPH" = "MPH",
                 "rho" = "rho de Mohn (2do hito)",
                 "CBA_ratio" = expression(CBA[H2]/CBA[H1]),
                 "R" = "Reclutamiento")
  
  if (!missing(sims)) {
    
    array_sims <- filter(array_df, Simulation %in% sims) %>%
      mutate(Simulation = factor(Simulation))
    g <- ggplot(array_sims, aes(Year, value, group = Simulation, linetype = Simulation)) +
      facet_grid(vars(MP), vars(OM)) +
      geom_line() +
      expand_limits(y = 0) +
      theme(panel.spacing = unit(0, "in"),
            axis.text.x = element_text(angle = 45, vjust = 0.5),
            legend.position = "bottom",
            strip.background = element_blank()) +
      coord_cartesian(expand = FALSE) +
      labs(x = "Año", y = ylab, linetype = "Simulación")
    
  } else {
    array_band <- array_df %>%
      group_by(Year, MP, OM) %>%
      summarise(med = median(value),
                lwr = quantile(value, 0.05),
                upr = quantile(value, 0.95),
                lwr2 = quantile(value, 0.25),
                upr2 = quantile(value, 0.75))
    g <- ggplot(array_band, aes(Year, med)) +
      facet_wrap(vars(MP)) +
      #geom_ribbon(fill = "grey90", aes(ymin = lwr, ymax = upr)) +
      #geom_ribbon(fill = "grey70", aes(ymin = lwr2, ymax = upr2)) +
      geom_ribbon(alpha = 0.1, aes(ymin = lwr2, ymax = upr2, fill = OM)) + # Plot only the interquartile range
      geom_line(aes(colour = OM)) +
      expand_limits(y = 0) +
      theme(panel.spacing = unit(0, "in"),
            legend.position = "bottom",
            strip.background = element_blank()) +
      coord_cartesian(expand = FALSE) +
      scale_fill_brewer(palette = "Dark2") +
      scale_colour_brewer(palette = "Dark2") +
      labs(x = "Año", y = ylab, fill = NULL, colour = NULL) +
      guides(fill = guide_legend(ncol = 2),
             colour = guide_legend(ncol = 2))
  }
  if (any(array_df$Year < 2023)) { # 2022 is the historical period
    g <- g + geom_vline(xintercept = 2022.5, linetype = 2)
  }
  g
}

