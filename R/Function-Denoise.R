# DeNoise(spec = specExp,
#         normIntensity = TRUE,
#         intensityNormedMethod = 'bonanza')
#
# DeNoise(spec = specExp,
#         normIntensity = TRUE,
#         intensityNormedMethod = 'gnps')
#
# DeNoise(spec = specExp,
#         normIntensity = TRUE,
#         intensityNormedMethod = 'maximum')
#
# DeNoise(spec = spec,
#         normIntensity = TRUE,
#         intensityNormedMethod = 'maximum')



#' @export
DeNoise <- function(spec,
                    mzIgnored = NULL,
                    mzPrecursor = NULL,
                    ppmPrecursorFilter = 20,
                    ms2range = NULL,
                    includePrecursor = TRUE,
                    normIntensity = FALSE,
                    intensityNormedTo = 1,
                    intensityNormed = FALSE,
                    thrIntensityRel = 0.0,
                    thrIntensityAbs = NULL,
                    snthresh = 3,
                    ms2noise = 3,
                    checkSanity = TRUE,
                    thrIntensitySanity = 50,
                    useMS1ResDefine = TRUE,
                    resDefineAt = 400,
                    intensityNormedMethod = c('maximum', 'bonanza', 'gnps'),
                    ...) {
  intensityNormedMethod <- match.arg(intensityNormedMethod)
  # browser()


  if (is.null(spec)) {
    return(NULL)
  }
  spec <- .Col2Numeric(spec)
  spec <- spec[order(spec[, 'mz']), , drop = FALSE]

  if (intensityNormed) {
    thrIntensityAbs <- 0
  } else {
    if (is.null(thrIntensityAbs)) {
      thrIntensityAbs <- ms2noise * snthresh
    }
  }
  # if (checkSanity & thrIntensityAbs > thrIntensitySanity) {
  #   stop("Noise estimation is too high!")
  # }

  if (is.null(mzPrecursor)) {
    mzPrecursor <- max(spec[, "mz"])
  }

  spec <- .FilterPrecursor(spec = spec,
                           mzPrecursor = mzPrecursor,
                           includePrecursor = includePrecursor,
                           ppmPrecursorFilter = ppmPrecursorFilter,
                           resDefineAt = ifelse(useMS1ResDefine, resDefineAt, 0))

  if (!is.null(ms2range)) {
    spec <- spec[spec[, 1] >= ms2range[1] & spec[, 1] <= ms2range[2], , drop = FALSE]
  }

  if (nrow(spec) == 0) return(NULL)

  nrkeep <- seq(nrow(spec))
  if (!is.null(mzIgnored)) {
    mzrange <- .GetPpmRange(mzIgnored, 25, resDefineAt = resDefineAt)
    nrkeep <- which(spec[, 1] <= mzrange[1] | spec[, 1] >= mzrange[2])
  }

  nrMaxint <- nrkeep[which.max(spec[nrkeep, 2])]
  # bugfix: in case index of max int fragment is changed after fragment is removed
  nmMaxint <- rownames(spec)[nrMaxint]
  thrIntensity <- max(thrIntensityAbs, spec[nrMaxint, "intensity"] * thrIntensityRel)
  spec <- spec[spec[, 2] >= thrIntensity, , drop = FALSE]

  if (nrow(spec) == 0) {
    return(NULL)
  }

  spec <- .RemoveRingEffect(spec)

  if (normIntensity) {
    switch(intensityNormedMethod,
            'maximum' = {
              spec <- .NormalizeSpec(spec, intensityNormedTo = intensityNormedTo,
                                     maxint = spec[nmMaxint, 2])
            },
            'bonanza' = {
              spec <- .NormalizeSpecBonanza(spec, intensityNormedTo = intensityNormedTo,
                                            maxint = spec[nmMaxint, 2])
            },
            'gnps' = {
              spec <- .NormalizeSpecGnps(spec, intensityNormedTo = intensityNormedTo,
                                         maxint = spec[nmMaxint, 2])
            }
    )
  }

  return(spec)
}

.FilterPrecursor <- function(spec, mzPrecursor, includePrecursor = TRUE,
                             ppmPrecursorFilter = 10, resDefineAt = 400) {
  mzrangePrecursor <- .GetPpmRange(mzPrecursor, ppmPrecursorFilter, resDefineAt)
  spec <- spec[spec[, 1] < mzrangePrecursor[ifelse(includePrecursor, 2, 1)], ,
               drop = FALSE]
  return(spec)
}

.NormalizeSpec <- function(spec, intensityNormedTo = 1, maxint = NULL) {
  if (is.null(maxint) | missing(maxint)) {
    maxint <- max(spec[, 2])
  }
  spec[, 2] <- spec[, 2] / maxint * intensityNormedTo
  return(spec)
}


.NormalizeSpecBonanza <- function(spec, intensityNormedTo = 1, maxint = NULL) {
  if (is.null(maxint) | missing(maxint)) {
    maxint <- max(spec[, 2])
  }
  spec[, 2] <- spec[, 2] / sum(spec[, 2]) * intensityNormedTo
  return(spec)
}


.NormalizeSpecGnps <- function(spec, intensityNormedTo = 1, maxint = NULL) {
  if (is.null(maxint) | missing(maxint)) {
    maxint <- max(spec[, 2])
  }
  spec[, 2] <- sqrt(spec[, 2]) / sqrt(sum(spec[, 2])) * intensityNormedTo
  return(spec)
}


.RemoveRingEffect <- function(spec, thrMZdiff = 0.3, thrIntensityRel = 0.2) {
  nrRing <- nrow(spec) + 1
  mz <- spec[, 'mz']

  mzdiff <- diff(mz)
  idxMZdiff <- which(mzdiff <= thrMZdiff)
  if (length(idxMZdiff) == 0) {
    return(spec)
  }

  nrPossibleRing <- sort(unique(c(idxMZdiff, idxMZdiff + 1)))
  while (TRUE) {

    nrMaxint <- which.max(spec[nrPossibleRing, 2])
    nrMaxint <- nrPossibleRing[nrMaxint]
    thrIntensity <- spec[nrMaxint, 2] * thrIntensityRel

    # TODO: decide if use abs
    # wo/ abs in lib gen
    # w/ abs since MetDDA
    mzdiff <- abs(mz[nrPossibleRing[-nrMaxint]] - mz[nrMaxint])
    # mzdiff <- mz[nrPossibleRing[-nrMaxint]] - mz[nrMaxint]

    int <- spec[nrPossibleRing[-nrMaxint], 2]
    nrRing <- append(nrRing, nrPossibleRing[-nrMaxint][which(mzdiff > 0 &
                                                               mzdiff <= thrMZdiff &
                                                               int <= thrIntensity)])
    nrPossibleRing <- nrPossibleRing[!nrPossibleRing %in% c(nrRing, nrMaxint)]
    if (length(nrPossibleRing) == 0) {
      break
    }
  }

  return(spec[-nrRing, , drop = FALSE])
}
