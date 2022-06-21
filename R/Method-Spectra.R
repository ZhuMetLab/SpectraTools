#' Find spectra
#' Find spectra with user defined terms
#' @param spectra \code{SpectraData} Spectra object for the spectra to be found
#' @param findby \code{character} Terms for spectra finding
#' @param value \code{character} Values of terms for spectra finding
#' @return a \code{SpectraData} object
#' @rdname Method-FindSpectra
#' @export
setMethod(
  "FindSpectra",
  signature = c("SpectraData"),
  function(spectra, findby = NULL, values = NULL, colSpecName = NULL){
    isKeep <- apply(spectra@info[, findby, drop = FALSE], 1,
                    function(dr) {
                      all(dr == as.character(values))
                    })
    idxKeep <- which(isKeep)
    if (length(idxKeep) == 0) {
      warning("No spectra found and returning NULL as result!")
      return(NULL)
    } else {
      return(SpectraData(spectra = spectra@spectra[idxKeep],
                         info = spectra@info[idxKeep, , drop = FALSE],
                         colSpecName = colSpecName))
    }
  }
)

#' Filter fragments
#' Filtering fragments in SpectraData with mass range and fragment intensity
#' @param spectra \code{SpectraData} Spectra object for the spectra to be found
#' @param mzrange \code{numeric(2)} Fragment mass range
#' @param thrIntensityAbs \code{numeric} Absolute fragment intensity threshold to be kept
#' @param thrIntensityRel \code{numeric} Relative fragment intensity threshold to be kept
#' @return a \code{SpectraData} object
#' @rdname Method-FindSpectra
#' @export
setMethod(
  "FilterFragments",
  signature = c("SpectraData"),
  function(spectra, mzrange = NULL, thrIntensityAbs = NULL, thrIntensityRel = NULL){
    spectra@spectra <- lapply(spectra@spectra, function(spec) {
      isKeep <- rep(TRUE, nrow(spec))
      if (!is.null(mzrange)) {
        isKeep <- isKeep & spec[, "mz"] >= mzrange[1] & spec[, "mz"] <= mzrange[2]
      }
      if (!is.null(thrIntensityAbs)) {
        isKeep <- isKeep & spec[, "intensity"] >= thrIntensityAbs
      }
      if (!is.null(thrIntensityRel)) {
        isKeep <- isKeep & spec[, "intensity"] >= thrIntensityRel
      }
      spec[isKeep, , drop = FALSE]
    })
    return(spectra)
  }
)

#' Filter NULL spectra
#' Filter the NULL spectra from SpectraData
#' @param spectra \code{SpectraData} Spectra object for the spectra to be found
#' @return a \code{SpectraData} object
#' @rdname Method-FindSpectra
#' @export
setMethod(
  "FilterNULL",
  signature = c("SpectraData"),
  function(spectra, keepInfo = TRUE){
    isKeep <- !sapply(spectra@spectra, is.null)
    idxKeep <- which(isKeep)
    if (length(idxKeep) == 0) {
      warning("No spectra found and returning NULL as result!")
      return(NULL)
    } else {
      if (keepInfo) {
        info <- spectra@info
      } else {
        info <- spectra@info[idxKeep, , drop = FALSE]
      }

      return(SpectraData(spectra = spectra@spectra[idxKeep],
                         info = info))
    }
  }
)
