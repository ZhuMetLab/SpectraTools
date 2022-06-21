#' @export
setMethod("as.list", signature(x = "ParamSpectraTools"), function(x, ...) {
  return(.param2list(x))
})

## The 'setAs' method.
setAs("ParamSpectraTools" ,"list", function(from){
  return(.param2list(from))
})

############################################
## ParseSpectraParam
############################################

#' @aliases type
#'
#' @description \code{type},\code{type<-}: getter and setter for the \code{type}
#'     slot of the object.
#'
#' @rdname ParseSpectraParam
#' @export
setMethod("type", "ParseSpectraParam", function(object){ return(object@type)})
#' @aliases type<-
#'
#' @param value The value for the slot.
#'
#' @rdname ParseSpectraParam
#' @export
setReplaceMethod("type", "ParseSpectraParam", function(object, value) {
  object@type <- value
  if (validObject(object))
    return(object)
})

#' @aliases denoise
#'
#' @description \code{denoise},\code{denoise<-}: getter and setter for the \code{denoise}
#'     slot of the object.
#'
#' @rdname ParseSpectraParam
#' @export
setMethod("denoise", "ParseSpectraParam", function(object){ return(object@denoise)})
#' @aliases denoise<-
#'
#' @param value The value for the slot.
#'
#' @rdname ParseSpectraParam
#' @export
setReplaceMethod("denoise", "ParseSpectraParam", function(object, value) {
  object@denoise <- value
  if (validObject(object))
    return(object)
})

#' @aliases ms2range
#'
#' @description \code{ms2range},\code{ms2range<-}: getter and setter for the \code{ms2range}
#'     slot of the object.
#'
#' @rdname ParseSpectraParam
#' @export
setMethod("ms2range", "ParseSpectraParam", function(object){ return(object@ms2range)})
#' @aliases ms2range<-
#'
#' @param value The value for the slot.
#'
#' @rdname ParseSpectraParam
#' @export
setReplaceMethod("ms2range", "ParseSpectraParam", function(object, value) {
  object@ms2range <- value
  if (validObject(object))
    return(object)
})

#' @aliases mzIgnored
#'
#' @description \code{mzIgnored},\code{mzIgnored<-}: getter and setter for the \code{mzIgnored}
#'     slot of the object.
#'
#' @rdname ParseSpectraParam
#' @export
setMethod("mzIgnored", "ParseSpectraParam", function(object){ return(object@mzIgnored)})
#' @aliases mzIgnored<-
#'
#' @param value The value for the slot.
#'
#' @rdname ParseSpectraParam
#' @export
setReplaceMethod("mzIgnored", "ParseSpectraParam", function(object, value) {
  object@mzIgnored <- value
  if (validObject(object))
    return(object)
})

#' @aliases includePrecursor
#'
#' @description \code{includePrecursor},\code{includePrecursor<-}: getter and setter for the \code{includePrecursor}
#'     slot of the object.
#'
#' @rdname ParseSpectraParam
#' @export
setMethod("includePrecursor", "ParseSpectraParam", function(object){ return(object@includePrecursor)})
#' @aliases includePrecursor<-
#'
#' @param value The value for the slot.
#'
#' @rdname ParseSpectraParam
#' @export
setReplaceMethod("includePrecursor", "ParseSpectraParam", function(object, value) {
  object@includePrecursor <- value
  if (validObject(object))
    return(object)
})

#' @aliases ppmPrecursorFilter
#'
#' @description \code{ppmPrecursorFilter},\code{ppmPrecursorFilter<-}: getter and setter for the \code{ppmPrecursorFilter}
#'     slot of the object.
#'
#' @rdname ParseSpectraParam
#' @export
setMethod("ppmPrecursorFilter", "ParseSpectraParam", function(object){ return(object@ppmPrecursorFilter)})
#' @aliases ppmPrecursorFilter<-
#'
#' @param value The value for the slot.
#'
#' @rdname ParseSpectraParam
#' @export
setReplaceMethod("ppmPrecursorFilter", "ParseSpectraParam", function(object, value) {
  object@ppmPrecursorFilter <- value
  if (validObject(object))
    return(object)
})

#' @aliases normIntensity
#'
#' @description \code{normIntensity},\code{normIntensity<-}: getter and setter for the \code{normIntensity}
#'     slot of the object.
#'
#' @rdname ParseSpectraParam
#' @export
setMethod("normIntensity", "ParseSpectraParam", function(object){ return(object@normIntensity)})
#' @aliases normIntensity<-
#'
#' @param value The value for the slot.
#'
#' @rdname ParseSpectraParam
#' @export
setReplaceMethod("normIntensity", "ParseSpectraParam", function(object, value) {
  object@normIntensity <- value
  if (validObject(object))
    return(object)
})

#' @aliases intensityNormedTo
#'
#' @description \code{intensityNormedTo},\code{intensityNormedTo<-}: getter and setter for the \code{intensityNormedTo}
#'     slot of the object.
#'
#' @rdname ParseSpectraParam
#' @export
setMethod("intensityNormedTo", "ParseSpectraParam", function(object){ return(object@intensityNormedTo)})
#' @aliases intensityNormedTo<-
#'
#' @param value The value for the slot.
#'
#' @rdname ParseSpectraParam
#' @export
setReplaceMethod("intensityNormedTo", "ParseSpectraParam", function(object, value) {
  object@intensityNormedTo <- value
  if (validObject(object))
    return(object)
})

#' @aliases thrIntensityRel
#'
#' @description \code{thrIntensityRel},\code{thrIntensityRel<-}: getter and setter for the \code{thrIntensityRel}
#'     slot of the object.
#'
#' @rdname ParseSpectraParam
#' @export
setMethod("thrIntensityRel", "ParseSpectraParam", function(object){ return(object@thrIntensityRel)})
#' @aliases thrIntensityRel<-
#'
#' @param value The value for the slot.
#'
#' @rdname ParseSpectraParam
#' @export
setReplaceMethod("thrIntensityRel", "ParseSpectraParam", function(object, value) {
  object@thrIntensityRel <- value
  if (validObject(object))
    return(object)
})

#' @aliases thrIntensityAbs
#'
#' @description \code{thrIntensityAbs},\code{thrIntensityAbs<-}: getter and setter for the \code{thrIntensityAbs}
#'     slot of the object.
#'
#' @rdname ParseSpectraParam
#' @export
setMethod("thrIntensityAbs", "ParseSpectraParam", function(object){ return(object@thrIntensityAbs)})
#' @aliases thrIntensityAbs<-
#'
#' @param value The value for the slot.
#'
#' @rdname ParseSpectraParam
#' @export
setReplaceMethod("thrIntensityAbs", "ParseSpectraParam", function(object, value) {
  object@thrIntensityAbs <- value
  if (validObject(object))
    return(object)
})

#' @aliases intensityNormed
#'
#' @description \code{intensityNormed},\code{intensityNormed<-}: getter and setter for the \code{intensityNormed}
#'     slot of the object.
#'
#' @rdname ParseSpectraParam
#' @export
setMethod("intensityNormed", "ParseSpectraParam", function(object){ return(object@intensityNormed)})
#' @aliases intensityNormed<-
#'
#' @param value The value for the slot.
#'
#' @rdname ParseSpectraParam
#' @export
setReplaceMethod("intensityNormed", "ParseSpectraParam", function(object, value) {
  object@intensityNormed <- value
  if (validObject(object))
    return(object)
})

#' @aliases colSpectra
#'
#' @description \code{colSpectra},\code{colSpectra<-}: getter and setter for the \code{colSpectra}
#'     slot of the object.
#'
#' @rdname ParseSpectraParam
#' @export
setMethod("colSpectra", "ParseSpectraParam", function(object){ return(object@colSpectra)})
#' @aliases colSpectra<-
#'
#' @param value The value for the slot.
#'
#' @rdname ParseSpectraParam
#' @export
setReplaceMethod("colSpectra", "ParseSpectraParam", function(object, value) {
  object@colSpectra <- value
  if (validObject(object))
    return(object)
})

#' @aliases nameSpectra
#'
#' @description \code{nameSpectra},\code{nameSpectra<-}: getter and setter for the \code{nameSpectra}
#'     slot of the object.
#'
#' @rdname ParseSpectraParam
#' @export
setMethod("nameSpectra", "ParseSpectraParam", function(object){ return(object@nameSpectra)})
#' @aliases nameSpectra<-
#'
#' @param value The value for the slot.
#'
#' @rdname ParseSpectraParam
#' @export
setReplaceMethod("nameSpectra", "ParseSpectraParam", function(object, value) {
  object@nameSpectra <- value
  if (validObject(object))
    return(object)
})

#' @aliases skip
#'
#' @description \code{skip},\code{skip<-}: getter and setter for the \code{skip}
#'     slot of the object.
#'
#' @rdname ParseSpectraParam
#' @export
setMethod("skip", "ParseSpectraParam", function(object){ return(object@skip)})
#' @aliases skip<-
#'
#' @param value The value for the slot.
#'
#' @rdname ParseSpectraParam
#' @export
setReplaceMethod("skip", "ParseSpectraParam", function(object, value) {
  object@skip <- value
  if (validObject(object))
    return(object)
})

#' @aliases labelKeep
#'
#' @description \code{labelKeep},\code{labelKeep<-}: getter and setter for the \code{labelKeep}
#'     slot of the object.
#'
#' @rdname ParseSpectraParam
#' @export
setMethod("labelKeep", "ParseSpectraParam", function(object){ return(object@labelKeep)})
#' @aliases labelKeep<-
#'
#' @param value The value for the slot.
#'
#' @rdname ParseSpectraParam
#' @export
setReplaceMethod("labelKeep", "ParseSpectraParam", function(object, value) {
  object@labelKeep <- value
  if (validObject(object))
    return(object)
})

#' @aliases labelName
#'
#' @description \code{labelName},\code{labelName<-}: getter and setter for the \code{labelName}
#'     slot of the object.
#'
#' @rdname ParseSpectraParam
#' @export
setMethod("labelName", "ParseSpectraParam", function(object){ return(object@labelName)})
#' @aliases labelName<-
#'
#' @param value The value for the slot.
#'
#' @rdname ParseSpectraParam
#' @export
setReplaceMethod("labelName", "ParseSpectraParam", function(object, value) {
  object@labelName <- value
  if (validObject(object))
    return(object)
})

#' @aliases labelReparse
#'
#' @description \code{labelReparse},\code{labelReparse<-}: getter and setter for the \code{labelReparse}
#'     slot of the object.
#'
#' @rdname ParseSpectraParam
#' @export
setMethod("labelReparse", "ParseSpectraParam", function(object){ return(object@labelReparse)})
#' @aliases labelReparse<-
#'
#' @param value The value for the slot.
#'
#' @rdname ParseSpectraParam
#' @export
setReplaceMethod("labelReparse", "ParseSpectraParam", function(object, value) {
  object@labelReparse <- value
  if (validObject(object))
    return(object)
})

#' @aliases sepReparse
#'
#' @description \code{sepReparse},\code{sepReparse<-}: getter and setter for the \code{sepReparse}
#'     slot of the object.
#'
#' @rdname ParseSpectraParam
#' @export
setMethod("sepReparse", "ParseSpectraParam", function(object){ return(object@sepReparse)})
#' @aliases sepReparse<-
#'
#' @param value The value for the slot.
#'
#' @rdname ParseSpectraParam
#' @export
setReplaceMethod("sepReparse", "ParseSpectraParam", function(object, value) {
  object@sepReparse <- value
  if (validObject(object))
    return(object)
})

#' @aliases labelMerge
#'
#' @description \code{labelMerge},\code{labelMerge<-}: getter and setter for the \code{labelMerge}
#'     slot of the object.
#'
#' @rdname ParseSpectraParam
#' @export
setMethod("labelMerge", "ParseSpectraParam", function(object){ return(object@labelMerge)})
#' @aliases labelMerge<-
#'
#' @param value The value for the slot.
#'
#' @rdname ParseSpectraParam
#' @export
setReplaceMethod("labelMerge", "ParseSpectraParam", function(object, value) {
  object@labelMerge <- value
  if (validObject(object))
    return(object)
})

#' @aliases sepMerge
#'
#' @description \code{sepMerge},\code{sepMerge<-}: getter and setter for the \code{sepMerge}
#'     slot of the object.
#'
#' @rdname ParseSpectraParam
#' @export
setMethod("sepMerge", "ParseSpectraParam", function(object){ return(object@sepMerge)})
#' @aliases sepMerge<-
#'
#' @param value The value for the slot.
#'
#' @rdname ParseSpectraParam
#' @export
setReplaceMethod("sepMerge", "ParseSpectraParam", function(object, value) {
  object@sepMerge <- value
  if (validObject(object))
    return(object)
})

############################################
## SearchParam
############################################

#' @aliases ppm
#'
#' @description \code{ppm},\code{ppm<-}: getter and setter for the \code{ppm}
#'     slot of the object.
#'
#' @rdname SearchParam
#' @export
setMethod("ppm", "SearchParam", function(object){ return(object@ppm)})
#' @aliases ppm<-
#'
#' @param value The value for the slot.
#'
#' @rdname SearchParam
#' @export
setReplaceMethod("ppm", "SearchParam", function(object, value) {
  object@ppm <- value
  if (validObject(object))
    return(object)
})

#' @aliases scoreRT
#'
#' @description \code{scoreRT},\code{scoreRT<-}: getter and setter for the \code{scoreRT}
#'     slot of the object.
#'
#' @rdname SearchParam
#' @export
setMethod("scoreRT", "SearchParam", function(object){ return(object@scoreRT)})
#' @aliases scoreRT<-
#'
#' @param value The value for the slot.
#'
#' @rdname SearchParam
#' @export
setReplaceMethod("scoreRT", "SearchParam", function(object, value) {
  object@scoreRT <- value
  if (validObject(object))
    return(object)
})

#' @aliases toleranceRT
#'
#' @description \code{toleranceRT},\code{toleranceRT<-}: getter and setter for the \code{toleranceRT}
#'     slot of the object.
#'
#' @rdname SearchParam
#' @export
setMethod("toleranceRT", "SearchParam", function(object){ return(object@toleranceRT)})
#' @aliases toleranceRT<-
#'
#' @param value The value for the slot.
#'
#' @rdname SearchParam
#' @export
setReplaceMethod("toleranceRT", "SearchParam", function(object, value) {
  object@toleranceRT <- value
  if (validObject(object))
    return(object)
})

#' @aliases scoreCCS
#'
#' @description \code{scoreCCS},\code{scoreCCS<-}: getter and setter for the \code{scoreCCS}
#'     slot of the object.
#'
#' @rdname SearchParam
#' @export
setMethod("scoreCCS", "SearchParam", function(object){ return(object@scoreCCS)})
#' @aliases scoreCCS<-
#'
#' @param value The value for the slot.
#'
#' @rdname SearchParam
#' @export
setReplaceMethod("scoreCCS", "SearchParam", function(object, value) {
  object@scoreCCS <- value
  if (validObject(object))
    return(object)
})

#' @aliases toleranceCCS
#'
#' @description \code{toleranceCCS},\code{toleranceCCS<-}: getter and setter for the \code{toleranceCCS}
#'     slot of the object.
#'
#' @rdname SearchParam
#' @export
setMethod("toleranceCCS", "SearchParam", function(object){ return(object@toleranceCCS)})
#' @aliases toleranceCCS<-
#'
#' @param value The value for the slot.
#'
#' @rdname SearchParam
#' @export
setReplaceMethod("toleranceCCS", "SearchParam", function(object, value) {
  object@toleranceCCS <- value
  if (validObject(object))
    return(object)
})

#' @aliases typeCCS
#'
#' @description \code{typeCCS},\code{typeCCS<-}: getter and setter for the \code{typeCCS}
#'     slot of the object.
#'
#' @rdname SearchParam
#' @export
setMethod("typeCCS", "SearchParam", function(object){ return(object@typeCCS)})
#' @aliases typeCCS<-
#'
#' @param value The value for the slot.
#'
#' @rdname SearchParam
#' @export
setReplaceMethod("typeCCS", "SearchParam", function(object, value) {
  object@typeCCS <- value
  if (validObject(object))
    return(object)
})

#' @aliases adductIncluded
#'
#' @description \code{adductIncluded},\code{adductIncluded<-}: getter and setter for the \code{adductIncluded}
#'     slot of the object.
#'
#' @rdname SearchParam
#' @export
setMethod("adductIncluded", "SearchParam", function(object){ return(object@adductIncluded)})
#' @aliases adductIncluded<-
#'
#' @param value The value for the slot.
#'
#' @rdname SearchParam
#' @export
setReplaceMethod("adductIncluded", "SearchParam", function(object, value) {
  object@adductIncluded <- value
  if (validObject(object))
    return(object)
})

#' @aliases adductExcluded
#'
#' @description \code{adductExcluded},\code{adductExcluded<-}: getter and setter for the \code{adductExcluded}
#'     slot of the object.
#'
#' @rdname SearchParam
#' @export
setMethod("adductExcluded", "SearchParam", function(object){ return(object@adductExcluded)})
#' @aliases adductExcluded<-
#'
#' @param value The value for the slot.
#'
#' @rdname SearchParam
#' @export
setReplaceMethod("adductExcluded", "SearchParam", function(object, value) {
  object@adductExcluded <- value
  if (validObject(object))
    return(object)
})

#' @aliases adductFile
#'
#' @description \code{adductFile},\code{adductFile<-}: getter and setter for the \code{adductFile}
#'     slot of the object.
#'
#' @rdname SearchParam
#' @export
setMethod("adductFile", "SearchParam", function(object){ return(object@adductFile)})
#' @aliases adductFile<-
#'
#' @param value The value for the slot.
#'
#' @rdname SearchParam
#' @export
setReplaceMethod("adductFile", "SearchParam", function(object, value) {
  object@adductFile <- value
  if (validObject(object))
    return(object)
})

#' @aliases classIncluded
#'
#' @description \code{classIncluded},\code{classIncluded<-}: getter and setter for the \code{classIncluded}
#'     slot of the object.
#'
#' @rdname SearchParam
#' @export
setMethod("classIncluded", "SearchParam", function(object){ return(object@classIncluded)})
#' @aliases classIncluded<-
#'
#' @param value The value for the slot.
#'
#' @rdname SearchParam
#' @export
setReplaceMethod("classIncluded", "SearchParam", function(object, value) {
  object@classIncluded <- value
  if (validObject(object))
    return(object)
})

#' @aliases classExcluded
#'
#' @description \code{classExcluded},\code{classExcluded<-}: getter and setter for the \code{classExcluded}
#'     slot of the object.
#'
#' @rdname SearchParam
#' @export
setMethod("classExcluded", "SearchParam", function(object){ return(object@classExcluded)})
#' @aliases classExcluded<-
#'
#' @param value The value for the slot.
#'
#' @rdname SearchParam
#' @export
setReplaceMethod("classExcluded", "SearchParam", function(object, value) {
  object@classExcluded <- value
  if (validObject(object))
    return(object)
})

#' @aliases resDefineAt
#'
#' @description \code{resDefineAt},\code{resDefineAt<-}: getter and setter for the \code{resDefineAt}
#'     slot of the object.
#'
#' @rdname SearchParam
#' @export
setMethod("resDefineAt", "SearchParam", function(object){ return(object@resDefineAt)})
#' @aliases resDefineAt<-
#'
#' @param value The value for the slot.
#'
#' @rdname SearchParam
#' @export
setReplaceMethod("resDefineAt", "SearchParam", function(object, value) {
  object@resDefineAt <- value
  if (validObject(object))
    return(object)
})


############################################
## MatchParam
############################################

#' @aliases ppm
#'
#' @description \code{ppm},\code{ppm<-}: getter and setter for the \code{ppm}
#'     slot of the object.
#'
#' @rdname MatchParam
#' @export
setMethod("ppm", "MatchParam", function(object){ return(object@ppm)})
#' @aliases ppm<-
#'
#' @param value The value for the slot.
#'
#' @rdname MatchParam
#' @export
setReplaceMethod("ppm", "MatchParam", function(object, value) {
  object@ppm <- value
  if (validObject(object))
    return(object)
})

#' @aliases cutoff
#'
#' @description \code{cutoff},\code{cutoff<-}: getter and setter for the \code{cutoff}
#'     slot of the object.
#'
#' @rdname MatchParam
#' @export
setMethod("cutoff", "MatchParam", function(object){ return(object@cutoff)})
#' @aliases cutoff<-
#'
#' @param value The value for the slot.
#'
#' @rdname MatchParam
#' @export
setReplaceMethod("cutoff", "MatchParam", function(object, value) {
  object@cutoff <- value
  if (validObject(object))
    return(object)
})

#' @aliases methodMatch
#'
#' @description \code{methodMatch},\code{methodMatch<-}: getter and setter for the \code{methodMatch}
#'     slot of the object.
#'
#' @rdname MatchParam
#' @export
setMethod("methodMatch", "MatchParam", function(object){ return(object@methodMatch)})
#' @aliases methodMatch<-
#'
#' @param value The value for the slot.
#'
#' @rdname MatchParam
#' @export
setReplaceMethod("methodMatch", "MatchParam", function(object, value) {
  object@methodMatch <- value
  if (validObject(object))
    return(object)
})

#' @aliases methodScore
#'
#' @description \code{methodScore},\code{methodScore<-}: getter and setter for the \code{methodScore}
#'     slot of the object.
#'
#' @rdname MatchParam
#' @export
setMethod("methodScore", "MatchParam", function(object){ return(object@methodScore)})
#' @aliases methodScore<-
#'
#' @param value The value for the slot.
#'
#' @rdname MatchParam
#' @export
setReplaceMethod("methodScore", "MatchParam", function(object, value) {
  object@methodScore <- value
  if (validObject(object))
    return(object)
})

#' @aliases weightMZ
#'
#' @description \code{weightMZ},\code{weightMZ<-}: getter and setter for the \code{weightMZ}
#'     slot of the object.
#'
#' @rdname MatchParam
#' @export
setMethod("weightMZ", "MatchParam", function(object){ return(object@weightMZ)})
#' @aliases weightMZ<-
#'
#' @param value The value for the slot.
#'
#' @rdname MatchParam
#' @export
setReplaceMethod("weightMZ", "MatchParam", function(object, value) {
  object@weightMZ <- value
  if (validObject(object))
    return(object)
})

#' @aliases weightIntensity
#'
#' @description \code{weightIntensity},\code{weightIntensity<-}: getter and setter for the \code{weightIntensity}
#'     slot of the object.
#'
#' @rdname MatchParam
#' @export
setMethod("weightIntensity", "MatchParam", function(object){ return(object@weightIntensity)})
#' @aliases weightIntensity<-
#'
#' @param value The value for the slot.
#'
#' @rdname MatchParam
#' @export
setReplaceMethod("weightIntensity", "MatchParam", function(object, value) {
  object@weightIntensity <- value
  if (validObject(object))
    return(object)
})

#' @aliases includePrecursor
#'
#' @description \code{includePrecursor},\code{includePrecursor<-}: getter and setter for the \code{includePrecursor}
#'     slot of the object.
#'
#' @rdname MatchParam
#' @export
setMethod("includePrecursor", "MatchParam", function(object){ return(object@includePrecursor)})
#' @aliases includePrecursor<-
#'
#' @param value The value for the slot.
#'
#' @rdname MatchParam
#' @export
setReplaceMethod("includePrecursor", "MatchParam", function(object, value) {
  object@includePrecursor <- value
  if (validObject(object))
    return(object)
})

#' @aliases ppmPrecursorFilter
#'
#' @description \code{ppmPrecursorFilter},\code{ppmPrecursorFilter<-}: getter and setter for the \code{ppmPrecursorFilter}
#'     slot of the object.
#'
#' @rdname MatchParam
#' @export
setMethod("ppmPrecursorFilter", "MatchParam", function(object){ return(object@ppmPrecursorFilter)})
#' @aliases ppmPrecursorFilter<-
#'
#' @param value The value for the slot.
#'
#' @rdname MatchParam
#' @export
setReplaceMethod("ppmPrecursorFilter", "MatchParam", function(object, value) {
  object@ppmPrecursorFilter <- value
  if (validObject(object))
    return(object)
})

#' @aliases ms2range
#'
#' @description \code{ms2range},\code{ms2range<-}: getter and setter for the \code{ms2range}
#'     slot of the object.
#'
#' @rdname MatchParam
#' @export
setMethod("ms2range", "MatchParam", function(object){ return(object@ms2range)})
#' @aliases ms2range<-
#'
#' @param value The value for the slot.
#'
#' @rdname MatchParam
#' @export
setReplaceMethod("ms2range", "MatchParam", function(object, value) {
  object@ms2range <- value
  if (validObject(object))
    return(object)
})

#' @aliases thrIntensityAbs
#'
#' @description \code{thrIntensityAbs},\code{thrIntensityAbs<-}: getter and setter for the \code{thrIntensityAbs}
#'     slot of the object.
#'
#' @rdname MatchParam
#' @export
setMethod("thrIntensityAbs", "MatchParam", function(object){ return(object@thrIntensityAbs)})
#' @aliases thrIntensityAbs<-
#'
#' @param value The value for the slot.
#'
#' @rdname MatchParam
#' @export
setReplaceMethod("thrIntensityAbs", "MatchParam", function(object, value) {
  object@thrIntensityAbs <- value
  if (validObject(object))
    return(object)
})

#' @aliases thrIntensityRel
#'
#' @description \code{thrIntensityRel},\code{thrIntensityRel<-}: getter and setter for the \code{thrIntensityRel}
#'     slot of the object.
#'
#' @rdname MatchParam
#' @export
setMethod("thrIntensityRel", "MatchParam", function(object){ return(object@thrIntensityRel)})
#' @aliases thrIntensityRel<-
#'
#' @param value The value for the slot.
#'
#' @rdname MatchParam
#' @export
setReplaceMethod("thrIntensityRel", "MatchParam", function(object, value) {
  object@thrIntensityRel <- value
  if (validObject(object))
    return(object)
})

#' @aliases intensityExpNormed
#'
#' @description \code{intensityExpNormed},\code{intensityExpNormed<-}: getter and setter for the \code{intensityExpNormed}
#'     slot of the object.
#'
#' @rdname MatchParam
#' @export
setMethod("intensityExpNormed", "MatchParam", function(object){ return(object@intensityExpNormed)})
#' @aliases intensityExpNormed<-
#'
#' @param value The value for the slot.
#'
#' @rdname MatchParam
#' @export
setReplaceMethod("intensityExpNormed", "MatchParam", function(object, value) {
  object@intensityExpNormed <- value
  if (validObject(object))
    return(object)
})

#' @aliases intensityLibNormed
#'
#' @description \code{intensityLibNormed},\code{intensityLibNormed<-}: getter and setter for the \code{intensityLibNormed}
#'     slot of the object.
#'
#' @rdname MatchParam
#' @export
setMethod("intensityLibNormed", "MatchParam", function(object){ return(object@intensityLibNormed)})
#' @aliases intensityLibNormed<-
#'
#' @param value The value for the slot.
#'
#' @rdname MatchParam
#' @export
setReplaceMethod("intensityLibNormed", "MatchParam", function(object, value) {
  object@intensityLibNormed <- value
  if (validObject(object))
    return(object)
})

#' @aliases tuneLibSpectra
#'
#' @description \code{tuneLibSpectra},\code{tuneLibSpectra<-}: getter and setter for the \code{tuneLibSpectra}
#'     slot of the object.
#'
#' @rdname MatchParam
#' @export
setMethod("tuneLibSpectra", "MatchParam", function(object){ return(object@tuneLibSpectra)})
#' @aliases tuneLibSpectra<-
#'
#' @param value The value for the slot.
#'
#' @rdname MatchParam
#' @export
setReplaceMethod("tuneLibSpectra", "MatchParam", function(object, value) {
  object@tuneLibSpectra <- value
  if (validObject(object))
    return(object)
})

#' @aliases resDefineAt
#'
#' @description \code{resDefineAt},\code{resDefineAt<-}: getter and setter for the \code{resDefineAt}
#'     slot of the object.
#'
#' @rdname MatchParam
#' @export
setMethod("resDefineAt", "MatchParam", function(object){ return(object@resDefineAt)})
#' @aliases resDefineAt<-
#'
#' @param value The value for the slot.
#'
#' @rdname MatchParam
#' @export
setReplaceMethod("resDefineAt", "MatchParam", function(object, value) {
  object@resDefineAt <- value
  if (validObject(object))
    return(object)
})



############################################
## CombineParam
############################################

#' @aliases cutoff
#'
#' @description \code{cutoff},\code{cutoff<-}: getter and setter for the \code{cutoff}
#'     slot of the object.
#'
#' @rdname CombineParam
#' @export
setMethod("cutoff", "CombineParam", function(object){ return(object@cutoff)})
#' @aliases cutoff<-
#'
#' @param value The value for the slot.
#'
#' @rdname CombineParam
#' @export
setReplaceMethod("cutoff", "CombineParam", function(object, value) {
  object@cutoff <- value
  if (validObject(object))
    return(object)
})

#' @aliases weightRT
#'
#' @description \code{weightRT},\code{weightRT<-}: getter and setter for the \code{weightRT}
#'     slot of the object.
#'
#' @rdname CombineParam
#' @export
setMethod("weightRT", "CombineParam", function(object){ return(object@weightRT)})
#' @aliases weightRT<-
#'
#' @param value The value for the slot.
#'
#' @rdname CombineParam
#' @export
setReplaceMethod("weightRT", "CombineParam", function(object, value) {
  object@weightRT <- value
  if (validObject(object))
    return(object)
})

#' @aliases weightCCS
#'
#' @description \code{weightCCS},\code{weightCCS<-}: getter and setter for the \code{weightCCS}
#'     slot of the object.
#'
#' @rdname CombineParam
#' @export
setMethod("weightCCS", "CombineParam", function(object){ return(object@weightCCS)})
#' @aliases weightCCS<-
#'
#' @param value The value for the slot.
#'
#' @rdname CombineParam
#' @export
setReplaceMethod("weightCCS", "CombineParam", function(object, value) {
  object@weightCCS <- value
  if (validObject(object))
    return(object)
})

#' @aliases weightMSMS
#'
#' @description \code{weightMSMS},\code{weightMSMS<-}: getter and setter for the \code{weightMSMS}
#'     slot of the object.
#'
#' @rdname CombineParam
#' @export
setMethod("weightMSMS", "CombineParam", function(object){ return(object@weightMSMS)})
#' @aliases weightMSMS<-
#'
#' @param value The value for the slot.
#'
#' @rdname CombineParam
#' @export
setReplaceMethod("weightMSMS", "CombineParam", function(object, value) {
  object@weightMSMS <- value
  if (validObject(object))
    return(object)
})

#' @aliases scoreMSMS
#'
#' @description \code{scoreMSMS},\code{scoreMSMS<-}: getter and setter for the \code{scoreMSMS}
#'     slot of the object.
#'
#' @rdname CombineParam
#' @export
setMethod("scoreMSMS", "CombineParam", function(object){ return(object@scoreMSMS)})
#' @aliases scoreMSMS<-
#'
#' @param value The value for the slot.
#'
#' @rdname CombineParam
#' @export
setReplaceMethod("scoreMSMS", "CombineParam", function(object, value) {
  object@scoreMSMS <- value
  if (validObject(object))
    return(object)
})
