# specExp <- data.frame(mz = c(50, 70, 85, 105),
#                       intensity = c(400, 1000, 350, 200),
#                       stringsAsFactors = FALSE)
#
# specRef <- data.frame(mz = c(50, 60, 100, 115, 135),
#                       intensity = c(400, 700, 1000, 350, 300),
#                       stringsAsFactors = FALSE)
#
# fragMatched <- MatchFragment(specExp = specExp,
#                              specRef = specRef,
#                              mzPrecursor = 105,
#                              ppmPrecursorFilter = 20,
#                              ppm = 35,
#                              resDefineAt = 400)

#' @importFrom magrittr '%>%' '%$%'
#' @export
MatchFragment <- function(specExp, specRef,
                          mzPrecursorExp,
                          mzPrecursorRef,
                          ppmPrecursorFilter = 20,
                          ppm = 35,
                          resDefineAt = 400,
                          ...) {
  if (is.null(specExp) || is.null(specRef)) {
    return(NULL)
  }

  # browser()
  specRef <- as.data.frame(specRef)
  specExp <- as.data.frame(specExp)
  resMatchList <- lapply(seq(nrow(specRef)), function(nr) {
    drRef <- specRef[nr, ]
    mzrg <- .GetPpmRange(drRef$mz, ppm, resDefineAt)
    nrExp <- which(specExp$mz >= mzrg[1] & specExp$mz <= mzrg[2])
    if (length(nrExp) == 0) {
      drRef$mzExp <- drRef$mz
      drRef$intensityExp <- 0
    } else {
      if (length(nrExp) > 1) {
        # nrExp <- nrExp[which.max(specExp$intensity[nrExp])]
        nrExp <- nrExp[which.min(abs(specExp$mz[nrExp] - specRef$mz[nr]))]
      }
      drRef$mzExp <- specExp$mz[nrExp]
      drRef$intensityExp <- specExp$intensity[nrExp]
    }
    return(drRef)
  })
  resMatch <- do.call(rbind, resMatchList)
  idxRep <- which(duplicated(resMatch$mzExp))

  # in case one experimental fragment is matched with multiple reference fragment
  # remove it!
  if (length(idxRep) > 0) {
    nrMod <- do.call(c, lapply(resMatch$mzExp[idxRep], function(vRep) {
      nrRep <- which(resMatch$mzExp == vRep)
      return(nrRep[-which.max(resMatch$intensity[nrRep])])
    }))
    resMatch[nrMod, "mzExp"] <- NA
    resMatch[nrMod, "intensityExp"] <- 0
  }

  # add non-matched experimental fragments
  idxAddExp <- which(!specExp$mz %in% resMatch$mzExp)
  if (length(idxAddExp) > 0) {
    addMatch <- data.frame(matrix(ncol = ncol(resMatch),
                                  nrow = length(idxAddExp)))
    colnames(addMatch) <- colnames(resMatch)
    addMatch$mz <- addMatch$mzExp <- specExp$mz[idxAddExp]
    addMatch$intensity <- 0
    addMatch$intensityExp <- specExp$intensity[idxAddExp]
    resMatch <- rbind(resMatch, addMatch)
    resMatch <- resMatch[order(resMatch$mz), , drop = FALSE]
  }

  resMatch$fragPrecursor <- FALSE
  mzrg <- .GetPpmRange(mzPrecursorRef, ppmPrecursorFilter, resDefineAt)
  idxPrecursor <- which(resMatch$mzExp >= mzrg[1] & resMatch$mzExp <= mzrg[2])
  if (length(idxPrecursor) > 0) {
    resMatch$fragPrecursor[idxPrecursor] <- TRUE
  }
  return(resMatch)
}


# mzPrecursorExp <- 105
# mzPrecursorRef <- 135
#
# specExp <- data.frame(mz = c(50, 70, 85, 105),
#                       intensity = c(400, 1000, 350, 200),
#                       stringsAsFactors = FALSE)
#
# specRef <- data.frame(mz = c(50, 60, 100, 115, 135),
#                       intensity = c(400, 700, 1000, 350, 300),
#                       stringsAsFactors = FALSE)
#
# nlMatched <- MatchNeutralLoss(specExp = specExp,
#                               specRef = specRef,
#                               mzPrecursorExp = 105,
#                               mzPrecursorRef = 135,
#                               ppmPrecursorFilter = 20,
#                               ppm = 35,
#                               resDefineAt = 400)

#' @export
MatchNeutralLoss <- function(
  specExp,
  specRef,
  mzPrecursorExp,
  mzPrecursorRef,
  ppmPrecursorFilter = 20,
  ppm = 35,
  resDefineAt = 400,
  ...
) {
  if (is.null(specExp) || is.null(specRef)) {
    return(NULL)
  }

  specRef <- as.data.frame(specRef)
  specExp <- as.data.frame(specExp)
  deltaMzPrecursor <- mzPrecursorExp - mzPrecursorRef

  resMatchList <- lapply(seq(nrow(specRef)), function(nr) {
    # browser()

    drRef <- specRef[nr, ]
    mzrg <- .GetPpmRange(drRef$mz, ppm, resDefineAt)
    mzrgDelta <- mzrg + deltaMzPrecursor

    # nrExp <- which(specExp$mz >= mzrg[1] & specExp$mz <= mzrg[2])
    nrExpDelta <- which(specExp$mz >= mzrgDelta[1] & specExp$mz <= mzrgDelta[2])

    if (length(nrExpDelta) == 0) {
      drRef$mzExp <- drRef$mz
      drRef$intensityExp <- 0
    } else {
      if (length(nrExpDelta) > 1) {
        # nrExp <- nrExp[which.max(specExp$intensity[nrExp])]
        nrExpDelta <- nrExpDelta[which.min(abs(specExp$mz[nrExpDelta] - specRef$mz[nr]))]
      }
      drRef$mzExp <- specExp$mz[nrExpDelta]
      drRef$intensityExp <- specExp$intensity[nrExpDelta]
    }

    return(drRef)
  })
  resMatch <- do.call(rbind, resMatchList)
  idxRep <- which(duplicated(resMatch$mzExp))

  # in case one experimental fragment is matched with multiple reference fragment
  # remove it!
  if (length(idxRep) > 0) {
    nrMod <- do.call(c, lapply(resMatch$mzExp[idxRep], function(vRep) {
      nrRep <- which(resMatch$mzExp == vRep)
      return(nrRep[-which.max(resMatch$intensity[nrRep])])
    }))
    resMatch[nrMod, "mzExp"] <- NA
    resMatch[nrMod, "intensityExp"] <- 0
  }

  # add non-matched experimental fragments
  idxAddExp <- which(!specExp$mz %in% resMatch$mzExp)
  if (length(idxAddExp) > 0) {
    addMatch <- data.frame(matrix(ncol = ncol(resMatch),
                                  nrow = length(idxAddExp)))
    colnames(addMatch) <- colnames(resMatch)
    addMatch$mz <- addMatch$mzExp <- specExp$mz[idxAddExp]
    addMatch$intensity <- 0
    addMatch$intensityExp <- specExp$intensity[idxAddExp]
    resMatch <- rbind(resMatch, addMatch)
    resMatch <- resMatch[order(resMatch$mz), , drop = FALSE]
  }

  resMatch$fragPrecursor <- FALSE
  mzrg <- .GetPpmRange(mzPrecursorExp, ppmPrecursorFilter, resDefineAt)
  idxPrecursor <- which(resMatch$mzExp >= mzrg[1] & resMatch$mzExp <= mzrg[2])
  if (length(idxPrecursor) > 0) {
    resMatch$fragPrecursor[idxPrecursor] <- TRUE
  }
  return(resMatch)
}

#' @export
GetScore.dp <- function(fragMatched,
                        weightMZ = 0,
                        weightIntensity = 1,
                        includePrecursor = FALSE,
                        ...) {
  if (is.null(fragMatched)) {
    res <- c("scoreReverse" = 0,
             "scoreForward" = 0)
  } else {
    if (!includePrecursor) {
      fragMatched <- fragMatched[!fragMatched$fragPrecursor, , drop = FALSE]
    }
    intRef <- .GetWeightedIntensity(fragMatched$mz, fragMatched$intensity,
                                    weightMZ, weightIntensity)
    intExp <- .GetWeightedIntensity(fragMatched$mzExp, fragMatched$intensityExp,
                                    weightMZ, weightIntensity)
    isFind <- fragMatched$intensity > 0
    scoreForward <- .GetDotProduct(intExp, intRef)
    scoreReverse <- .GetDotProduct(intExp[isFind], intRef[isFind])
    res <- c("scoreReverse" = scoreReverse,
             "scoreForward" = scoreForward)
    res[is.nan(res) | is.na(res)] <- 0
  }
  return(res)
}

#' @export
GetScore.ratio <- function(fragMatched,
                        includePrecursor = FALSE,
                        ...) {
  if (is.null(fragMatched)) {
    res <- c("scoreReverse" = 0,
             "scoreForward" = 0)
  } else {
    if (!includePrecursor) {
      fragMatched <- fragMatched[!fragMatched$fragPrecursor, , drop = FALSE]
    }
    isFind <- fragMatched$intensity > 0 & fragMatched$intensityExp > 0
    scoreForward <- sum(isFind)/nrow(fragMatched)
    scoreReverse <- sum(isFind)/sum(fragMatched$intensity > 0)
    res <- c("scoreReverse" = scoreReverse,
             "scoreForward" = scoreForward)
    res[is.nan(res) | is.na(res)] <- 0
  }
  return(res)
}


# GetScore.hybrid(fragMatched = fragMatched,
#                 nlMatched = nlMatched)

#' @export
GetScore.hybrid <- function(fragMatched,
                            nlMatched,
                            ...) {
  if (is.null(fragMatched) & is.null(nlMatched)) {
    res <- c("score" = 0)
  } else {
    fragMatched <- fragMatched %>%
      dplyr::mutate(type = 'exact_match') %>%
      dplyr::filter(intensity > 0 & intensityExp > 0)

    nlMatched <- nlMatched %>%
      dplyr::mutate(type = 'nl_match') %>%
      dplyr::filter(intensity > 0 & intensityExp > 0)

    fragTableMatched <- fragMatched %>%
      dplyr::bind_rows(nlMatched) %>%
      dplyr::distinct(mz, mzExp, .keep_all = TRUE)

    intExp <- fragTableMatched$intensityExp
    intRef <- fragTableMatched$intensity

    # res <- .GetDotProduct(intExp, intRef)
    res <- (sum(sqrt(intExp)*sqrt(intRef)))^2/(sum(intExp)*sum(intRef))
    res <- c('score' = res)

    res[is.nan(res) | is.na(res)] <- 0
  }

  return(res)

}


# GetScore.bonanza(fragMatched = fragMatched,
#                  nlMatched = nlMatched)

#' @export
GetScore.bonanza <- function(fragMatched,
                             nlMatched,
                             ...) {
  if (is.null(fragMatched) & is.null(nlMatched)) {
    res <- c("score" = 0)
  } else {
    fragMatched <- fragMatched %>%
      dplyr::mutate(type = 'exact_match')
    nlMatched <- nlMatched %>%
      dplyr::mutate(type = 'nl_match')

    fragTableMatched <- fragMatched %>%
      dplyr::bind_rows(nlMatched) %>%
      dplyr::filter(intensity > 0 & intensityExp > 0) %>%
      dplyr::distinct(mz, mzExp, .keep_all = TRUE)

    fragTableUnmatched <- fragMatched %>%
      dplyr::bind_rows(nlMatched) %>%
      dplyr::filter(!(intensity > 0 & intensityExp > 0)) %>%
      dplyr::filter(!(mz %in% fragTableMatched$mz) & !(mzExp %in% fragTableMatched$mzExp)) %>%
      dplyr::distinct(mz, mzExp, .keep_all = TRUE)

    intExpMatched <- fragTableMatched$intensityExp
    intRefMatched <- fragTableMatched$intensity

    intExpUnmatched <- fragTableUnmatched$intensityExp
    intRefUnmatched <- fragTableUnmatched$intensity

    dpMatched <- sum(intExpMatched*intRefMatched)
    dpUnmatched <- sum(intExpUnmatched^2) + sum(intRefUnmatched^2)

    res <- dpMatched/(dpMatched + dpUnmatched)
    res <- c('score' = res)

    res[is.nan(res) | is.na(res)] <- 0
  }

  return(res)
}

#' @export
GetScore.gnps <- function(fragMatched,
                          nlMatched,
                          ...) {
  if (is.null(fragMatched) & is.null(nlMatched)) {
    res <- c("score" = 0)
  } else {
    fragMatched <- fragMatched %>%
      dplyr::mutate(type = 'exact_match') %>%
      dplyr::filter(intensity > 0 & intensityExp > 0)

    nlMatched <- nlMatched %>%
      dplyr::mutate(type = 'nl_match') %>%
      dplyr::filter(intensity > 0 & intensityExp > 0)

    fragTableMatched <- fragMatched %>%
      dplyr::bind_rows(nlMatched) %>%
      dplyr::distinct(mz, mzExp, .keep_all = TRUE)

    intExp <- fragTableMatched$intensityExp
    intRef <- fragTableMatched$intensity

    res <- sum(intExp*intRef)
    res <- c('score' = res)

    res[is.nan(res) | is.na(res)] <- 0
  }

  return(res)

}



.GetWeightedIntensity <- function(mz, intensity, weightMZ, weightIntensity) {
  return(mz^weightMZ * intensity ^ weightIntensity)
}

.GetDotProduct <- function(x, y) {
  # return(sum(x * y) ^ 2 / (sum(x ^ 2) + sum(y ^ 2)))
  return(sum(x * y) / sqrt((sum(x ^ 2) * sum(y ^ 2))))
}
