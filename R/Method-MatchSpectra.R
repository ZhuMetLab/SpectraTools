#' @export
setMethod(
  "MatchSpectra",
  signature = c("SpectraData", "SpectraData", "MatchParam"),
  function(dataExp, dataRef, matchParam) {
    # browser()

    matchScore <- do.call(paste0("GetMatchScore.", c(matchParam@methodMatch)),
                          list("dataExp" = dataExp,
                               "dataRef" = dataRef,
                               "matchParam" = matchParam)
                          )
    if (is.null(matchScore)) {
      return(NULL)
    }
    isKeep <- sapply(matchScore, function(score) {
      any(score >= matchParam@cutoff)
    })
    matchScore <- matchScore[isKeep]
    if (length(matchScore) == 0) {
      return(NULL)
    }


    if (matchParam@methodScore %in% c("bonanza", 'gnps', 'hybrid')) {
      info <- cbind(dataRef@info[isKeep, , drop = FALSE],
                    do.call(rbind, matchScore))
      matchedFragments <- lapply(matchScore, function(score) {
        attributes(score)$fragMatched
      })
      nlFragments <- lapply(matchScore, function(score) {
        attributes(score)$nlMatched
      })

      return(MatchScore(info, matchedFragments, nlFragments))
    } else {
      info <- cbind(dataRef@info[isKeep, , drop = FALSE],
                    do.call(rbind, matchScore))
      matchedFragments <- lapply(matchScore, function(score) {
        attributes(score)$fragMatched
      })

      return(MatchScore(info, matchedFragments))
    }



  })



# dataExp <- new(Class = 'SpectraData',
#                info = data.frame(mz = 105,
#                                  rt = 123,
#                                  stringsAsFactors = FALSE),
#                spectra = list(specExp))
#
# dataRef <- new(Class = 'SpectraData',
#                info = data.frame(mz = 135,
#                                  rt = 123,
#                                  stringsAsFactors = FALSE),
#                spectra = list(specRef))
#
# matchParam <- MatchParam(cutoff = 0,
#                          normIntensity = TRUE,
#                          tuneLibSpectra = TRUE,
#                          includePrecursor = TRUE,
#                          intensityNormedMethod = 'bonanza',
#                          methodMatch = 'direct',
#                          methodScore = 'bonanza') %>%
#   new(Class = 'MatchParam')


#' @export
setMethod(
  "GetMatchScore.direct",
  signature = c("SpectraData", "SpectraData", "MatchParam"),
  function(dataExp, dataRef, matchParam) {

    matchParamList <- as.list(matchParam)
    specRef <- dataRef@spectra
    specExp <- do.call("DeNoise",
                       c("spec" = list(dataExp@spectra[[1]]),
                         "intensityNormed" = matchParam@intensityExpNormed,
                         "mzPrecursor" = dataExp@info$mz,
                         matchParamList))
    if (is.null(specExp)) {
      return(NULL)
    }
    mzPrecursor = dataExp@info$mz
    matchArgs <- lapply(seq_along(specRef), function(idx) {
      spec <- specRef[[idx]]
      if (matchParam@tuneLibSpectra) {
        spec <- do.call("DeNoise",
                        c("spec" = list(spec),
                          "intensityNormed" = matchParam@intensityLibNormed,
                          "mzPrecursor" = dataRef@info$mz[idx],
                          matchParamList))
      }
      c("specExp" = list(specExp),
        "specRef" = list(spec),
        "mzPrecursorExp" = mzPrecursor,
        'mzPrecursorRef' = dataRef@info$mz[idx],
        matchParamList)
    })

    res <- lapply(matchArgs, function(arg){
      if (arg$methodScore %in% c("bonanza", 'gnps', 'hybrid')) {
        fragMatched <- do.call("MatchFragment", arg)
        nlMatched <- do.call('MatchNeutralLoss', arg)
        score <- do.call(paste0("GetScore.", matchParam@methodScore),
                         c("fragMatched" = list(fragMatched),
                           'nlMatched' = list(nlMatched),
                           matchParamList))

        attr(score, "fragMatched") <- fragMatched
        attr(score, "nlMatched") <- nlMatched

      } else {
        fragMatched <- do.call("MatchFragment", arg)
        score <- do.call(paste0("GetScore.", matchParam@methodScore),
                         c("fragMatched" = list(fragMatched),
                           matchParamList))
        attr(score, "fragMatched") <- fragMatched
      }

      return(score)
    })

    return(res)
  })

