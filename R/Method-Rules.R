#' Apply annotation rules
#' @rdname Method-ApplyRules
#' @export
setMethod(
  "ApplyRules",
  signature = c("MatchScore"),
  function(matchScore, rules, ...) {
    info <- matchScore@info
    fragMatched <- matchScore@matchedFragments
    fragName <- rownames(info)
    ruleName <- .MakeRuleName(info$adduct, info$classname)
    names(ruleName) <- fragName
    idRule <- sapply(fragName, function(nm) {
      rule <- rules[[ruleName[nm]]]
      rInfo <- info[nm, ]
      if (is.null(rule)) {
        return(rInfo$name)
      }
      frag <- fragMatched[[nm]]
      frag <- frag[frag$intensity > 0 & frag$intensityExp > 0, , drop = FALSE]
      # if (checkData) {
      #   write.csv(frag, )
      # }
      nrRule <- nrow(rule)
      idxRule <- 1
      checkRule <- TRUE
      while (checkRule) {
        rRule <- rule[idxRule, ]
        paramList <- list("rule" = rRule$Rule,
                          "fragMatched" = frag)
        ruleMatched <- do.call(paste0(".ParseRule.", rRule$Type), paramList)
        annoLevel <- ifelse(ruleMatched, rRule$AnnotationY, rRule$AnnotationN)
        if (((rRule$Judgement + ruleMatched) %% 2 == 1) || idxRule == nrRule) {
          resAnno <- .ParseAnnotationLevel(rInfo$name, annoLevel)
          checkRule <- FALSE
        } else {
          idxRule <- idxRule + 1
        }
      }
      return(resAnno)
    })

    matchScore@info$idRule <- idRule
    matchScore@info$ruleName <- ruleName
    return(matchScore)
  })

#' Apply annotation rules
#' @rdname Method-ApplyRules
#' @export
setMethod(
  "RefineRuleResult",
  signature = c("MatchScore"),
  function(matchScore, ...) {
    info <- matchScore@info
    inforaw <- info[, c("ruleName", "idRule"), drop = FALSE]
    colnames(inforaw) <- c("adduct", "name")
    inforaw$name <- sapply(inforaw$name, .CheckName)
    resraw <- .MakeOutputTxt(inforaw)
    idrule <- inforaw$name
    idlvl <- .AssignIDLevel(idrule)
    idrefined <- do.call(rbind, lapply(unique(info$ruleName), function(nmrule) {
      idx <- which(info$ruleName == nmrule)
      highestLvl <- max(idlvl[idx])
      data.frame("name" = unique(idrule[idx[idlvl[idx] == highestLvl]]),
                 "adduct" = nmrule, "lvl" = highestLvl)
    }))
    dtrefine <- idrefined[order(idrefined[, "lvl"], decreasing = TRUE),
                          c("adduct", "name"),
                          drop = FALSE]
    resrefine <- .MakeOutputTxt(dtrefine)
    return(c("RuleRefinedResult" = resraw, "FinalResult" = resrefine))
  })
