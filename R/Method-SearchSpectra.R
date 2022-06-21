#' @export
setMethod(
  "SearchSpectra",
  signature = c("SpectraData", "SpectraData", "SearchParam"),
  function(dataExp, dataRef, searchParam,
           rtcalExp = NULL, rtcalRef = NULL,
           adductTable = NULL, ...) {

    if (searchParam@scoreRT) {
      if (!is.null(rtcalExp)) {
        dataRef@info <- .CalibrateRT(refInfo = dataRef@info,
                                     rtcalRef = rtcalRef,
                                     rtcalExp = rtcalExp)
      } else {
        warning("RT calibration will be ignored as the RT calibration table is not provided.")
      }
    }

    infoExp <- dataExp@info[names(dataExp@spectra), , drop = FALSE]
    infoRef <- dataRef@info[names(dataRef@spectra), , drop = FALSE]

    infoExp <- .Col2Numeric(infoExp)
    infoRef <- .Col2Numeric(infoRef)
    mzRefParam <- c("metInfo" = list(infoRef),
                    as.list(searchParam))
    mzRef <- do.call(".GetRefMZ", mzRefParam)

    ms1ResDefineAt <- ifelse(searchParam@useMS1ResDefine, searchParam@resDefineAt, 0)
    infoMZMatch <- lapply(rownames(infoExp), function(nmExp) {
      drExp <- infoExp[nmExp, ]
      mzrange <- .GetPpmRange(drExp$mz, ppm = searchParam@ppm,
                              resDefineAt = ms1ResDefineAt)
      idxmatch <- which(mzRef$mz >= mzrange[1] & mzRef$mz <= mzrange[2])
      if (length(idxmatch) == 0) {
        return(NA)
      }
      nmRef <- mzRef$specname[idxmatch]
      infomatch <- infoRef[nmRef, , drop = FALSE]
      infomatch$adduct <- mzRef[idxmatch, "adduct"]
      # bugfix for metabolite adduct type matches
      if (searchParam@updateRefMZ) {
        infomatch$mz <- mzRef$mz[idxmatch]
      }
      # mzError <- .GetPpmDiff(drExp$mz, infomatch$mz, resDefineAt = searchParam@resDefineAt)
      mzError <- .GetPpmDiff(drExp$mz, mzRef$mz[idxmatch], resDefineAt = searchParam@resDefineAt)
      infomatch$errorMZ <- mzError

      if (searchParam@scoreRT) {
        rtExp <- drExp$rt
        rtRef <- infomatch$rt
        # rtError <- rtExp - rtRef
        score <- .TrapezoidalScore(rtExp, rtRef, searchParam@toleranceRT)
        isKeep <- score > 0
        if (all(!isKeep)) {
          return(NA)
        }
        infomatch <- infomatch[isKeep, , drop = FALSE]
        infomatch$errorRT <- attributes(score)$delta[isKeep]
        infomatch$scoreRT <- score[isKeep]
      }
      if (searchParam@scoreCCS) {
        ccsExp <- drExp$ccs
        ccsRef <- infomatch$ccs
        score <- .TrapezoidalScore(ccsExp, ccsRef, searchParam@toleranceCCS,
                                   type = searchParam@typeCCS)
        isKeep <- score > 0
        if (all(!isKeep)) {
          return(NA)
        }
        infomatch <- infomatch[isKeep, , drop = FALSE]
        infomatch$errorCCS <- attributes(score)$delta[isKeep]
        infomatch$scoreCCS <- score[isKeep]
      }

      dExp <- SpectraData(info = drExp,
                          spectra = dataExp@spectra[nmExp])
      dRef <- SpectraData(info = infomatch,
                          spectra = dataRef@spectra[rownames(infomatch)])
      return(list("dataExp" = dExp,
                  "dataRef" = dRef))
    })
    names(infoMZMatch) <- rownames(infoExp)
    res <- infoMZMatch[!is.na(infoMZMatch)]
    return(res)
  })
