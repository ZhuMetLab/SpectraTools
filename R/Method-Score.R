#' @export
setMethod(
  "CombineScore",
  signature = c("MatchScore", "CombineParam"),
  function(matchScore, combineParam) {
    scoreName <- c("scoreRT", "scoreCCS", "scoreReverse", "scoreForward")
    names(scoreName) <- c("RT", "CCS", "reverse", "forward")
    scoreWeight <- c(combineParam@weightRT,
                     combineParam@weightCCS,
                     combineParam@weightMSMS)
    usedName <- c("RT", "CCS", combineParam@scoreMSMS)[scoreWeight > 0]
    scoreWeight <- scoreWeight[scoreWeight > 0]
    names(scoreWeight) <- usedName
    scoreName <- scoreName[usedName]

    if (is.null(matchScore)) {
      return(NULL)
    }
    info <- matchScore@info
    frag <- matchScore@matchedFragments
    scores <- info[, scoreName, drop = FALSE]
    scoreCombined <- as.matrix(scores) %*% scoreWeight
    info$score <- scoreCombined <- scoreCombined[, 1]
    scoreOrder <- order(scoreCombined, decreasing = TRUE)
    info <- info[scoreOrder, , drop = FALSE]
    frag <- frag[scoreOrder]
    isKeep <- info$score >= combineParam@cutoff

    if (all(!isKeep)) {
      return(NULL)
    }
    matchScore@info <- info[isKeep, , drop = FALSE]
    matchScore@matchedFragments <- frag[isKeep]
    return(matchScore)
  }
)

#' @export
setMethod(
  "GenOutputScore",
  signature = c("MatchScore"),
  function(matchScore, cutoff = NULL, maxOutput = 10,
           type = c("lipids", "metabolites", "sterol")) {
    type = match.arg(type)
    if (is.null(matchScore)) {
      return(NULL)
    }

    res <- switch(type,
                  "lipids" = .GenOutputScore.lipids(matchScore = matchScore,
                                                    cutoff = cutoff,
                                                    maxOutput = maxOutput),
                  "metabolites" = .GenOutputScore.metabolites(matchScore = matchScore,
                                                              cutoff = cutoff,
                                                              maxOutput = maxOutput),
                  "sterol" = .GenOutputScore.sterol(matchScore = matchScore,
                                                              cutoff = cutoff,
                                                              maxOutput = maxOutput)
    )
    return(res)
  }
)

