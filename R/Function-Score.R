.MakeScoreTable.lipids <- function(info, cl, cutoff) {
  sc <- info[, cl]
  isKeep <- sc >= cutoff
  # nhit <- sum(isKeep)
  infohit <- info[isKeep, c("adduct", "name", "classname", cl), drop = FALSE]
  infohit$adduct <- .MakeRuleName(infohit$adduct, infohit$classname)
  infohit <- infohit[, c(cl, "adduct", "name")]
  colnames(infohit) <- c("score", "adduct", "name")
  infohit <- infohit[order(infohit[, "score"], decreasing = TRUE), , drop = FALSE]
  return(infohit)
}

.MakeScoreTable.metabolites <- function(info, cl, cutoff) {
  sc <- info[, cl]
  isKeep <- sc >= cutoff
  infohit <- info[isKeep, c("adduct", "name", "labid", cl), drop = FALSE]
  infohit <- infohit[, c(cl, "adduct", "name",  "labid")]
  colnames(infohit) <- c("score", "adduct", "name",  "labid")
  infohit <- infohit[order(infohit[, "score"], decreasing = TRUE), , drop = FALSE]
  return(infohit)
}

.MakeScoreTable.sterol <- function(info, cl, cutoff) {
  sc <- info[, cl]
  isKeep <- sc >= cutoff
  infohit <- info[isKeep,
                  c("adduct", "name", "classname", cl),
                  drop = FALSE]
  infohit <- infohit[, c(cl, "adduct", "name", "classname")]
  colnames(infohit) <- c("score", "adduct", "name", 'classname')
  infohit <- infohit[order(infohit[, "score"], decreasing = TRUE), , drop = FALSE]
  return(infohit)
}

.MakeOutputTxt <- function(scoreTable, maxOutput = 10) {
  isKeep <- apply(scoreTable, 1, function(dr) {
    !any(dr %in% "")
  })
  scoreTable <- scoreTable[isKeep, , drop = FALSE]
  if (nrow(scoreTable) == 0) {
    return("")
  }
  if (nrow(scoreTable) > maxOutput) {
    scoreTable <- scoreTable[seq(maxOutput), , drop = FALSE]
  }
  scname <- colnames(scoreTable)

  scinfo <- sapply(scname, function(nm) {
    dc <- scoreTable[, nm]
    if (is.numeric(dc)) {
      dc <- round(dc, 4)
    }
    paste0(nm, "{", dc, "}")
  })
  if (is.vector(scinfo)) {
    res <- paste0(scinfo, collapse = "")
  } else {
    res <- paste0(apply(scinfo, 1, paste, collapse = ""), collapse = ";")
  }
  return(res)
}

.GenOutputScore.lipids <- function(matchScore, cutoff = NULL, maxOutput = 10) {
  info <- matchScore@info

  clScore <- grep("^score", colnames(info), value = TRUE)
  clError <- grep("^error", colnames(info), value = TRUE)

  lipidName <- paste0(info$name, collapse = ";")
  # adductType <- paste0(unique(info$bulk.structure), collapse = ";")
  adductType <- apply(info, 1, function(dr) {
    addu <- regmatches(dr["adduct"], gregexpr("(?<=\\[).+(?=\\])",
                                              dr["adduct"], perl = TRUE))[[1]]
    gsub("^M", dr["classname"], addu)
  })
  adductType <- paste0(unique(adductType), collapse = ";")

  lipSpecies <- paste0(unique(sapply(info$name, .ParseAnnotationLevel, 1)),
                       collapse = ";")

  ndigitError <- c(0, 0, 2)
  names(ndigitError) <- c("errorMZ", "errorRT", "errorCCS")
  infoerror <- infoscore <- sapply(clError, function(cl) {
    paste0(round(info[, cl], ndigitError[cl]), collapse = ";")
  })
  ndigitScore <- c(2, 2, 4, 4 , 4)
  names(ndigitScore) <- c("scoreRT", "scoreCCS", "scoreReverse",
                          "scoreForward", "score")
  infoscore <- sapply(clScore, function(cl) {
    paste0(round(info[, cl], ndigitScore[cl]), collapse = ";")
  })
  res <- append(infoerror, infoscore)
  res <- append(res, c("lipidMolecularSpecies" = lipidName,
                       "adducts" = adductType,
                       "lipidSpecies" = lipSpecies))

  if (!is.null(cutoff)) {
    nhitReverse <- 0
    hitReverse <- ""
    clReverse <- grep("Reverse", clScore, value = TRUE)
    if (length(clReverse) == 1) {
      infoReverse <- .MakeScoreTable.lipids(info, clReverse, cutoff = cutoff)
      nhitReverse <- nrow(infoReverse)
      if (nhitReverse == 0) {
        nhitReverse <- 0
      } else {
        hitReverse <- .MakeOutputTxt(infoReverse, maxOutput)
      }
    }
    nhitForward <- 0
    hitForward <- ""
    clForward <- grep("Forward", clScore, value = TRUE)
    if (length(clForward) == 1) {
      infoForward <- .MakeScoreTable.lipids(info, clForward, cutoff = cutoff)
      nhitForward <- nrow(infoForward)
      if (nhitForward == 0) {
        nhitForward <- 0
      } else {
        hitForward <- .MakeOutputTxt(infoForward)
      }
    }
    res <- append(res, c("nhits.reverse" = nhitReverse,
                         "hits.reverse" = hitReverse,
                         "nhits.forward" = nhitForward,
                         "hits.forward" = hitForward))
  }
  res <- .Col2Numeric(data.frame(t(as.matrix(res)), stringsAsFactors = F))
  return(res)
}

.GenOutputScore.metabolites <- function(matchScore, cutoff = NULL, maxOutput = 10) {
  info <- matchScore@info

  clScore <- grep("^score", colnames(info), value = TRUE)
  clError <- grep("^error", colnames(info), value = TRUE)

  adductType <- paste0(info$adduct, collapse = ";")
  labids <- paste0(info$labid, collapse = ";")

  ndigitError <- c(0, 0, 2)
  names(ndigitError) <- c("errorMZ", "errorRT", "errorCCS")
  infoerror <- infoscore <- sapply(clError, function(cl) {
    paste0(round(info[, cl], ndigitError[cl]), collapse = ";")
  })
  ndigitScore <- c(2, 2, 4, 4 , 4)
  names(ndigitScore) <- c("scoreRT", "scoreCCS", "scoreReverse",
                          "scoreForward", "score")
  infoscore <- sapply(clScore, function(cl) {
    paste0(round(info[, cl], ndigitScore[cl]), collapse = ";")
  })
  res <- append(infoerror, infoscore)
  res <- append(res, c("adducts" = adductType))
  res <- append(res, c("labids" = labids))

  if (!is.null(cutoff)) {
    nhitReverse <- 0
    hitReverse <- ""
    clReverse <- grep("Reverse", clScore, value = TRUE)
    if (length(clReverse) == 1) {
      infoReverse <- .MakeScoreTable.metabolites(info, clReverse, cutoff = cutoff)
      nhitReverse <- nrow(infoReverse)
      if (nhitReverse == 0) {
        nhitReverse <- 0
      } else {
        hitReverse <- .MakeOutputTxt(infoReverse, maxOutput)
      }
    }
    nhitForward <- 0
    hitForward <- ""
    clForward <- grep("Forward", clScore, value = TRUE)
    if (length(clForward) == 1) {
      infoForward <- .MakeScoreTable.metabolites(info, clForward, cutoff = cutoff)
      nhitForward <- nrow(infoForward)
      if (nhitForward == 0) {
        nhitForward <- 0
      } else {
        hitForward <- .MakeOutputTxt(infoForward)
      }
    }
    res <- append(res, c("nhits.reverse" = nhitReverse,
                         "hits.reverse" = hitReverse,
                         "nhits.forward" = nhitForward,
                         "hits.forward" = hitForward))
  }
  res <- .Col2Numeric(data.frame(t(as.matrix(res)), stringsAsFactors = F))
  return(res)
}

.GenOutputScore.sterol <- function(matchScore, cutoff = NULL, maxOutput = 10) {
  info <- matchScore@info

  clScore <- grep("^score", colnames(info), value = TRUE)
  clError <- grep("^error", colnames(info), value = TRUE)

  lipidName <- paste0(info$name, collapse = ";")
  adductType <- paste0(info$adduct, collapse = ";")
  OH <- paste0(info$OH, collapse = ";")
  LMID <- paste0(info$LMID, collapse = ";")
  KEGGID <- paste0(info$KEGGID, collapse = ";")
  classname <- paste0(info$classname, collapse = ";")

  ndigitError <- c(0, 0, 2)
  names(ndigitError) <- c("errorMZ", "errorRT", "errorCCS")
  infoerror <- infoscore <- sapply(clError, function(cl) {
    paste0(round(info[, cl], ndigitError[cl]), collapse = ";")
  })
  ndigitScore <- c(2, 2, 4, 4 , 4)
  names(ndigitScore) <- c("scoreRT", "scoreCCS", "scoreReverse",
                          "scoreForward", "score")
  infoscore <- sapply(clScore, function(cl) {
    paste0(round(info[, cl], ndigitScore[cl]), collapse = ";")
  })
  res <- append(infoerror, infoscore)
  res <- append(res, c("OH" = OH,
                       "LMID" = LMID,
                       "KEGGID" = KEGGID,
                       "classname" = classname,
                       "adducts" = adductType,
                       "lipidMolecularSpecies" = lipidName))

  if (!is.null(cutoff)) {
    nhitReverse <- 0
    hitReverse <- ""
    clReverse <- grep("Reverse", clScore, value = TRUE)
    if (length(clReverse) == 1) {
      infoReverse <- .MakeScoreTable.sterol(info, clReverse, cutoff = cutoff)
      nhitReverse <- nrow(infoReverse)
      if (nhitReverse == 0) {
        nhitReverse <- 0
      } else {
        hitReverse <- .MakeOutputTxt(infoReverse, maxOutput)
      }
    }
    nhitForward <- 0
    hitForward <- ""
    clForward <- grep("Forward", clScore, value = TRUE)
    if (length(clForward) == 1) {
      infoForward <- .MakeScoreTable.lipids(info, clForward, cutoff = cutoff)
      nhitForward <- nrow(infoForward)
      if (nhitForward == 0) {
        nhitForward <- 0
      } else {
        hitForward <- .MakeOutputTxt(infoForward)
      }
    }
    res <- append(res, c("nhits.reverse" = nhitReverse,
                         "hits.reverse" = hitReverse,
                         "nhits.forward" = nhitForward,
                         "hits.forward" = hitForward))
  }
  res <- .Col2Numeric(data.frame(t(as.matrix(res)), stringsAsFactors = F))
  return(res)
}
