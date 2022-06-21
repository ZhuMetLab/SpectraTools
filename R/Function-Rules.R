#' @export
MakeRules <- function(dirRule) {
  if (!dir.exists(dirRule)) {
    stop("Directory ", dirRule, " could not be found! Please check your rules")
  }
  fnRules <- list.files(dirRule, pattern = "(?i).csv$",
                        recursive = FALSE, full.names = TRUE)
  rules <- lapply(fnRules, read.csv, stringsAsFactors = FALSE)
  nmRules <- gsub("(?i).csv$", "", basename(fnRules))
  names(rules) <- nmRules
  return(rules)
}

.MakeRuleName <- function(adducts, classes) {
  adducts <- unlist(regmatches(adducts,
                             gregexpr("(?<=\\[).+(?=\\])",
                                      adducts, perl = TRUE)))
  res <- sapply(seq_along(adducts), function(idx) {
    gsub("M", classes[idx], adducts[idx])
  })
  return(res)
}

.ParseRule.Fragment <- function(rule, fragMatched) {
  frag <- rule
  fragAll <- TRUE

  if (grepl("\\|\\||&&", rule)) {
    frag <- sapply(strsplit(rule, split = "\\|\\||&&")[[1]], function(frag1) {
      gsub("\\(|\\)", "", frag1)
    })
    names(frag) <- NULL
    if (grepl("\\|\\|", rule)) {
      fragAll <- FALSE
    }
  }

  fragMatched <- fragMatched[match(frag, fragMatched$annotation), ]
  if (fragAll) {
    ruleMatched <- ifelse(all(!is.na(fragMatched)) && !is.null(fragMatched), TRUE, FALSE)
  } else {
    ruleMatched <- ifelse(any(!is.na(fragMatched)) && !is.null(fragMatched), TRUE, FALSE)
  }

  return(ruleMatched)
}
.ParseRule.Intensity <- function(rule, fragMatched) {
  intRule <- strsplit(rule,
                       split = "(\\((?:[^()]++|(?1))*\\))(*SKIP)(*F)| ",
                       perl = TRUE)[[1]]
  idxFrag <- grep("\\(.*\\)", intRule)
  nmFrag <- unlist(regmatches(intRule[idxFrag],
                               gregexpr("(?<=\\().+(?=\\))",
                                        intRule[idxFrag],
                                        perl = TRUE)))
  infoInt <- fragMatched[match(nmFrag, fragMatched$annotation), "intensityExp"]
  intRule[idxFrag] <- infoInt
  ruleMatched <- eval(parse(text = (paste(intRule, collapse = " "))))
  return(ruleMatched)
}

.ParseAnnotationLevel <- function(lipName, level) {
  chainInfo <- regmatches(lipName, gregexpr("\\d+:\\d+([/|_]\\d+:\\d+)*",
                                              lipName, perl = TRUE))[[1]]
  chainLenth <- as.numeric(unlist(regmatches(chainInfo,
                                              gregexpr("\\d+(?=:\\d+)",
                                                       chainInfo, perl = TRUE))))
  chainDbond <- as.numeric(unlist(regmatches(chainInfo,
                                              gregexpr("(?<=\\d:)\\d+",
                                                       chainInfo, perl = TRUE))))

  if (length(chainInfo) == 0) {
    warning("Chain info not set in lipid names!")
    return("")
  }
  if (level == 0) {
    return("")
  }
  chainMod <- switch(as.character(level),
                      '1' = paste(sum(chainLenth), sum(chainDbond), sep = ":"),
                      '2' = gsub("/|_", "_", chainInfo),
                      "3" = chainInfo)
  return(gsub(chainInfo, chainMod, lipName))
}

.CheckLevel <- function(idrule, levelRecognizer) {
  isLvl <- grepl(levelRecognizer, idrule)
  if (any(isLvl)) {
    return(idrule[idx[isLvl3]])
  } else {
    return(NULL)
  }
}

.CheckName <- function(lipName) {
  if (grepl("_", lipName)) {
    chainInfo <- regmatches(lipName, gregexpr("\\d+:\\d+([/|_]\\d+:\\d+)*",
                                              lipName, perl = TRUE))[[1]]
    chainLenth <- as.numeric(unlist(regmatches(chainInfo,
                                               gregexpr("\\d+(?=:\\d+)",
                                                        chainInfo, perl = TRUE))))
    chainSep <- strsplit(chainInfo, "_")[[1]]
    chainMod <- paste(chainSep[order(chainSep, decreasing = TRUE)], collapse = "_")
    lipName <- gsub(chainInfo, chainMod, lipName)
  }
  return(lipName)
}

.AssignIDLevel <- function(idrule) {
  lvl <- rep(0, length(idrule))
  lvl[grepl(":", idrule)] <- 1
  lvl[grepl("_", idrule)] <- 2
  lvl[grepl("/", idrule)] <- 3
  return(lvl)
}
