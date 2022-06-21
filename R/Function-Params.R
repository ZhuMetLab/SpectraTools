#' Parsing Spectra setup
#'
#' @param type \code{character} spectral file types ("MSP", "MGF" and "CSV" format are
#' supported)
#' @param denoise \code{logical} if denoising is needed
#' @param ms2range \code{numeric} fragment mz range
#' @param mzIgnoreded \code{numeric} mz of fragment to be ignored when normalizing or
#' denoising with relative intensity threshold (only one fragment is supported)
#' @param includePrecursor \code{logical} if include precursor fragment
#' @param ppmPrecursorFilter \code{logical} ppm tolerance to determine precursor
#'  fragment
#' @param normIntensity \code{logical} if normalize intensities
#' @param intensityNormedTo \code{numeric} intensity to be normalized to
#' @param thrIntensityAbs \code{numeric} absolute intensity threshold to be removed
#' @param thrIntensityRel \code{numeric} relative intensity threshold to be removed
#' @param intensityNormed \code{logical} if the spectral intensity is normalized
#'  in spectrum files
#' @param colSpectra \code{integer vector} columns for recording mz and intensities
#'  (the first two items must be in the order of "mz" and "intensity")
#' @param nameSpectra \code{character vector} colnames of spectra (the first two items
#' must be "mz" and "intensity")
#' @param skip \code{integer} the number of line of the data file to skip before
#'  beginning to read data for "CSV" spectra.
#' @param labelKeep \code{character vector} labels to be kept in precursor information
#'  in the generated spectra. Labels for "name" and "labid" must be included for
#'  generating library spectra
#' @param labelName \code{character vector} names of the labels to be kept in precursor
#'  information in the generated spectra. "name" and "labid" must be included for
#'  generating library spectra
#' @param labelReparse \code{character vector} names of the labels to be re-parsed in
#'  precursor information in the generated spectra.
#' @param sepReparse \code{character} which character to be seperated by for re-parsing
#' @param labelMerge \code{character vector} names of the labels to be merged in
#'  precursor information in the generated spectra.
#' @param sepMerge \code{character} which character to be seperated by for merging
#' @return an \code{ParseSpectraParam} object
#' @rdname ParseSpectraParam
#' @export
ParseSpectraParam <- function(
  type = c("mgf", "msp", "cef", "raw", "csv", "txt"),
  denoise = TRUE,
  ms2range = NULL,
  mzIgnored = NULL,
  ppmPrecursorFilter = 10,
  includePrecursor = TRUE,
  normIntensity = FALSE,
  intensityNormedTo = 1,
  thrIntensityRel = 0.0,
  thrIntensityAbs = 100,
  intensityNormed = FALSE,
  colSpectra = c(1, 2),
  nameSpectra = c("mz", "intensity"),
  skip = 0,
  labelKeep = NULL,
  labelName = NULL,
  labelReparse = NULL,
  sepReparse = NULL,
  labelMerge = NULL,
  sepMerge = NULL
) {

  type <- match.arg(type)
  if (missing(labelKeep) | missing(labelName)) {
    switch(type,
           "msp" = {
             labelKeep <- c("NAME",
                            "LABID",
                            "CE",
                            "PRECURSORMZ",
                            "POLARITY")
             labelName <- c("name",
                            "labid",
                            "ce",
                            "mz",
                            "polarity")
           },
           "mgf" = {
             labelKeep <- c("PEPMASS", "RTINSECONDS")
             labelName <- c("mz", "rt")
           },
           "cef" = {
             labelKeep <- NULL
             labelName <- NULL
           },
           "csv" = {
             labelKeep <- NULL
             labelName <- NULL
           },
           "raw" = {
             labelKeep <- NULL
             labelName <- NULL
           },
           "txt" = {
             labelKeep <- c("NAME",
                            "LABID",
                            "CE",
                            "PRECURSORMZ",
                            "POLARITY")
             labelName <- c("name",
                            "labid",
                            "ce",
                            "mz",
                            "polarity")
           })
  }
  return(new("ParseSpectraParam",
             type = type,
             denoise = denoise,
             ms2range = ms2range,
             mzIgnored = mzIgnored,
             ppmPrecursorFilter = ppmPrecursorFilter,
             includePrecursor = includePrecursor,
             normIntensity = normIntensity,
             intensityNormedTo = intensityNormedTo,
             thrIntensityRel = thrIntensityRel,
             thrIntensityAbs = thrIntensityAbs,
             intensityNormed = intensityNormed,
             colSpectra = colSpectra,
             nameSpectra = nameSpectra,
             skip = skip,
             labelKeep = labelKeep,
             labelName = labelName,
             labelReparse = labelReparse,
             sepReparse = sepReparse,
             labelMerge = labelMerge,
             sepMerge = sepMerge))
}

#' Searching spectra Parameters
#'
#' Parameters for searching experimentall related SpectraData from refrence
#' SpectraData
#' @param ppm \code{numeric} ppm tolerance for searching precursor mz
#' @param scoreRT \code{logical} if comparing RT
#' @param toleranceRT \code{numeric (2)} c(Tmin, Tmax), RT range for trapezoidal score.
#' \itemize{
#'    \item[*] Tmin: Topline for trapezoidal score
#'    \item[*] Tmax: Baseline for trapezoidal score
#'  }
#' @param scoreCCS \code{logical} if comparing CCS
#' @param toleranceCCS \code{numeric (2)} c(Cmin, Cmax), CCS range for trapezoidal
#'  score.
#' \itemize{
#'    \item[*] Cmin: Topline for trapezoidal score
#'    \item[*] Cmax: Baseline for trapezoidal score
#'  }
#' @param typeCCS \code{character} CCS tolerance type, either "percentage" or
#'  "absolute"
#' @param adductIncluded \code{character} adduct types to be included in MS1 match
#' @param adductExclude \code{character} adduct types to be excluded in MS1 match
#' @param adductFile \code{character} file path for user provided adduct table
#' @param classIncluded \code{character} compound classes to be included in MS1 match
#' @param classExclude \code{character} compound classes to be excluded in MS1 match
#' @param useMS1ResDefine \code{logical} if use resDefineAt when calculating precursor m/z tolerance
#' @param updateRefMZ \code{logical} if update reference precursor m/z with it's corresponding
#' adduct type
#' @param resDefineAt \code{numeric} m/z (Da) value for resolution definition
#' @rdname SearchParam
#' @export
SearchParam <- function(ppm = 25,
                        scoreRT = TRUE,
                        toleranceRT = c(32, 64),
                        scoreCCS = TRUE,
                        toleranceCCS = c(1.34, 2.68),
                        typeCCS = c("percentage", "absolute"),
                        adductIncluded = NULL,
                        adductExcluded = NULL,
                        adductFile = NULL,
                        classIncluded = NULL,
                        classExcluded = NULL,
                        useMS1ResDefine = TRUE,
                        updateRefMZ = TRUE,
                        resDefineAt = 400) {
  typeCCS = match.arg(typeCCS)
  return(new("SearchParam",
             ppm = ppm,
             scoreRT = scoreRT,
             toleranceRT = toleranceRT,
             scoreCCS = scoreCCS,
             toleranceCCS = toleranceCCS,
             typeCCS = typeCCS,
             adductIncluded = adductIncluded,
             adductExcluded = adductExcluded,
             adductFile = adductFile,
             classIncluded = classIncluded,
             classExcluded = classExcluded,
             useMS1ResDefine = useMS1ResDefine,
             updateRefMZ = updateRefMZ,
             resDefineAt = resDefineAt))
}

#' Matching parameter setup
#'
#' @param ppm \code{numeric} ppm tolerance for MS2 m/z match
#' @param cutoff \code{numeric} threshold for a avaliable match
#' @param methodMatch \code{character} method for matching with librarial spectra
#'  (eithor "direct" or "bootstrapping")
#' @param methodScore \code{character} method for scoring the MSMS match ("dp",
#'  "msdial", "nist", "bs", "combined" are avaliable)
#' @param weightMZ \code{numeric} weight of m/z when scoring
#' @param weightIntensity \code{numeric} weight of intensity when scoring
#' @param includePrecursor \code{logical} if consider the precursor fragments when
#'  matching
#' @param ms2range \code{numeric} mass range setup when acquiring MSMS data
#' @param thrIntensityAbs \code{numeric} absolute intensity threshold to be removed
#' @param thrIntensityRel \code{numeric} relative intensity threshold to be removed
#' @param intensityExpNormed \code{logical} if the spectral intensity is normalized
#'  in experiment spectra
#' @param intensityLibNormed \code{logical} if the spectral intensity is normalized
#'  in library spectra

#' @param tuneLibSpectra \code{logical} if apply thrIntensityAbs or thrIntensityRel
#'  to reference
#' @param useMS1ResDefine \code{logical} if use resDefineAt when calculating precursor m/z tolerance
#' @param resDefineAt \code{numeric} m/z threshold for using ppm tolerance for MS1 or
#'  MS2 match, for smaller m/z values, using the tolerance of ppm.ms1 * res.defineat
#'  to mathing experimental and librarial fragments.
#' @return an \code{ParamMatch} object
#' @rdname MatchParam
#' @export
MatchParam <- function(
  ppm = 35,
  cutoff = 0.8,
  methodMatch = c('direct', 'bootstrap'),
  methodScore = c('dp', 'msdial', 'nist', 'bootstrap', 'combined',
                  'ratio', 'gnps', 'bonanza', 'hybrid'),
  weightMZ = 0,
  weightIntensity = 1,
  includePrecursor = FALSE,
  ppmPrecursorFilter = 20,
  ms2range = NULL,
  thrIntensityAbs = 100,
  thrIntensityRel = 0.01,
  intensityExpNormed = FALSE,
  intensityLibNormed = TRUE,
  tuneLibSpectra = FALSE,
  useMS1ResDefine = TRUE,
  resDefineAt = 400,
  normIntensity = FALSE,
  intensityNormedMethod = c('maximum', 'bonanza', 'gnps', 'hybrid')
  ) {

  methodMatch <- match.arg(methodMatch)
  methodScore <- match.arg(methodScore)
  intensityNormedMethod <- match.arg(intensityNormedMethod)

  return(new("MatchParam",
             ppm = ppm,
             cutoff = cutoff,
             methodMatch = methodMatch,
             methodScore = methodScore,
             weightMZ = weightMZ,
             weightIntensity = weightIntensity,
             includePrecursor = includePrecursor,
             ppmPrecursorFilter = ppmPrecursorFilter,
             ms2range = ms2range,
             thrIntensityAbs = thrIntensityAbs,
             thrIntensityRel = thrIntensityRel,
             intensityExpNormed = intensityExpNormed,
             intensityLibNormed = intensityLibNormed,
             tuneLibSpectra = tuneLibSpectra,
             useMS1ResDefine = useMS1ResDefine,
             resDefineAt = resDefineAt,
             normIntensity = normIntensity,
             intensityNormedMethod = intensityNormedMethod))
}

#' Combine score parameter setup
#'
#' @param cutoff \code{numeric} threshold for a avaliable match of combined score
#' @param weightRT \code{numeric} weight of RT when combining the scors
#' @param weightCCS \code{numeric} weight of CCS when combining the scors
#' @param weightMSMS \code{numeric} weight of MSMS when combining the scors
#' @param scoreMSMS \code{character} MSMS score used for score combination
#'  (only "reverse" or "forward" is supported)
#' @return a \code{CombineParam} object
#' @rdname CombineParam
#' @export
CombineParam <- function(cutoff = 0.6,
                       weightRT = 0,
                       weightCCS = 0,
                       weightMSMS = 0,
                       scoreMSMS = c("reverse", "forward")) {
  scoreMSMS <- match.arg(scoreMSMS)
  return(new("CombineParam",
             cutoff = cutoff,
             weightRT = weightRT,
             weightCCS = weightCCS,
             weightMSMS = weightMSMS,
             scoreMSMS = scoreMSMS))
}

## Functions related to the Param class and sub-classes.##
#' @description Extract all slot values and put them into a list, names being
#'     the slot names. If a slot \code{addParams} exist its content will be
#'     appended to the returned list.
#'
#' @param x A Param class.
#'
#' @author Johannes Rainer, modified by YD Yin
#'
#' @noRd
.param2list <- function(x) {
  ## Get all slot names, skip those matching the provided pattern.
  sNames <- slotNames(x)

  skipSome <- grep(sNames, pattern = "^\\.")
  if (length(skipSome) > 0) {
    sNames <- sNames[-skipSome]
  }
  ## handle a slot called "addParams" differently: this is thougth to contain
  ## ... arguments thus we have to skip this one too.
  if (any(sNames == "addParams")) {
    sNames <- sNames[sNames != "addParams"]
    addP <- x@addParams
  } else {
    addP <- list()
  }
  if (length(sNames) > 0) {
    resL <- vector("list", length(sNames))

    for (i in 1:length(sNames)) {
      resL[i] <- list(slot(x, name = sNames[i]))
    }
    names(resL) <- sNames
    resL <- c(resL, addP)
    return(resL)
  }else{
    return(list())
  }
}
