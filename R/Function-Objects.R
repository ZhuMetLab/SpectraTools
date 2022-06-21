#' Generate SpectraData object
#' @param info \code{data.frame} metabolite information
#' @param spectra \code{list} spectra of metabolite.
#' @export
SpectraData <- function(info, spectra, colSpecName = NULL) {
  nrowinfo <- nrow(info)
  lenspec <- length(spectra)

  if (is.null(names(spectra))) {
    if (nrowinfo == lenspec) {
      if (!is.null(colSpecName)) {
        ncname <- grep(paste0("(?i)^", colSpecName, "$"), colnames(info))
        if (length(ncname) == 1) {
          nmspec <- info[, ncname]
          if (any(duplicated(nmspec))) {
            nmspec <- paste0("#", seq(nrowinfo))
          }
        } else {
          nmspec <- paste0("#", seq(nrowinfo))
        }
        names(spectra) <- rownames(info) <- nmspec
      } else {
        nmspec <- paste0("#", seq(nrowinfo))
        names(spectra) <- rownames(info) <- nmspec
      }
    } else {
      stop("Please make sure the spectra are named or info and spectra are consistant!")
    }
  }

  new("SpectraData",
      "info" = info,
      "spectra" = spectra)
}

#' Generate SpectraData object
#' @param info \code{data.frame} score information
#' @param matchedFragments \code{list} matched fragments info
#' @export
MatchScore <- function(info, matchedFragments, nlFragments = NULL) {
  if (is.null(names(matchedFragments))) {
    nrowinfo <- nrow(info)
    if (nrowinfo == length(matchedFragments)) {
      names(matchedFragments) <- rownames(info) <- paste0("#", seq(nrowinfo))
      if (length(nlFragments) > 0) {
        names(nlFragments) <- paste0("#", seq(nrowinfo))
      }

    } else {
      stop("Please make sure the matchedFragments are named or info and matchedFragments are consistant!")
    }
  }

  if (length(nlFragments) == 0) {
    new("MatchScore",
        "info" = info,
        "matchedFragments" = matchedFragments)
  } else {
    new("MatchScore",
        "info" = info,
        "matchedFragments" = matchedFragments,
        "nlFragments" = nlFragments)
  }

}
