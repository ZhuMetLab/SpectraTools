#' @title ParseSpectra
#' @aliases Parse spectra
#' @description  Get spectra data by reading spectrum files
#' @param parseParam ParamParseSpectra object, recording parameters for parsing
#'  spectra
#' @param files character: path of the spectrum files
#' @rdname Method-ParseSpectra
#' @export
setMethod(
  "ParseSpectra",
  signature = c("ParseSpectraParam"),
  function(parseParam, files, ...) {
    # specData <- BiocParallel::bplapply(files, function(fn) {
    specData <- lapply(files, function(fn) {
      spec <- do.call(paste0("Parse", toupper(parseParam@type)),
                      list("file" = fn,
                           "colSpectra" = parseParam@colSpectra,
                           "skip" = parseParam@skip))
    })
    specData <- do.call(c, specData)
    specInfo <- lapply(specData, `[[`, "info")

    if (!is.null(parseParam@labelReparse)) {
      specInfo <- lapply(specInfo, function(info) {
        colName <- colnames(info)
        idxMatch <- match(colName, parseParam@labelReparse)
        idxReparse <- which(!is.na(idxMatch))
        for (idx in seq_along(parseParam@labelReparse)) {
          idxReparsed <- which(idxMatch == idx)
          if (length(idxReparsed) == 0) next
          infoReparse <- .SplitInfo(info[idxReparsed],
                                    split = parseParam@sepReparse[idx])
          colnames(infoReparse) <- paste(parseParam@labelReparse[idx],
                                         colnames(infoReparse), sep = "@")

          colnames(info)[idxReparsed] <- colnames(infoReparse)
          info[1, idxReparsed] <- infoReparse[2, ]
        }
        info
      })
    }

    if (!is.null(parseParam@labelMerge)) {
      specInfo <- lapply(specInfo, function(info) {
        colName <- colnames(info)
        idxMatch <- match(colName, parseParam@labelMerge)
        idxMerge <- which(!is.na(idxMatch))
        for (idx in seq_along(parseParam@labelMerge)) {
          idxMerged <- which(idxMatch == idx)
          if (length(idxMerged) == 0) next
          infoMerge <- gsub(parseParam@sepMerge[idx], "", info[idxMerged])
          info[1, idxMerged] <- infoMerge
        }
        info
      })
    }

    specInfo <- lapply(specInfo, function(info) {
      colnames(info) <- .MakeUniqueNames(colnames(info))
      info
    })
    allNames <- unique(unlist(lapply(specInfo, colnames)))
    tmp <- matrix(ncol = length(allNames), nrow = 1)
    colnames(tmp) <- allNames
    specInfo <- lapply(specInfo, function(info) {
      tmp[, colnames(info)] <- info
      tmp
    })

    if (!is.null(parseParam@labelKeep)) {
      parseParam@labelKeep
      tmpInfo <- rep(NA, length(parseParam@labelKeep))
      names(tmpInfo) <- parseParam@labelKeep
      specInfo <- t(sapply(specInfo, function(info) {
        ############################
        # bugfix for multiple possible names in MSP files,
        #  e.g.:
        #    RT or RETENTION TIME
        #    CCS or COLLISIONCROSSSECTION
        ############################
        # idx <- match(parseParam@labelKeep, colnames(info))
        # tmpInfo[parseParam@labelKeep[!is.na(idx)]] <- info[na.omit(idx), drop = FALSE]
        idx <- unlist(sapply(parseParam@labelKeep,
                             grep,
                             colnames(info),
                             ignore.case = TRUE))
        tmpInfo[names(idx)] <- info[idx]
        tmpInfo <- sapply(tmpInfo, function(info){
          if (grepl('^[0-9. ]*$$', info) && grepl(' ', info)) {
            info <- strsplit(info, split = '\\s')[[1]][1]
          }
          info
        })
        tmpInfo
      }))
      if (!is.null(parseParam@labelName)) {
        colnames(specInfo) <- parseParam@labelName
      }
    } else {
      specInfo <- do.call(rbind, specInfo)
    }

    specInfo <- data.frame(specInfo, stringsAsFactors = FALSE)
    specInfo <- .Col2Numeric(specInfo)

    spec <- lapply(specData, `[[`, "spec")
    spec <- lapply(spec, .Col2Numeric)
    if (parseParam@denoise) {
      parseParamList <- as.list(parseParam)
      spec <- lapply(seq_along(spec), function(idx) {
        mzPrecursor <- specInfo[idx, "mz"]
        do.call("DeNoise", c("spec" = spec[idx],
                             "mzPrecursor" = mzPrecursor,
                             parseParamList))
      })
      isKeep <- !sapply(spec, is.null)
      spec <- spec[isKeep]
      specInfo <- specInfo[isKeep, , drop = FALSE]
    }

    specidx <- paste0("#", seq(nrow(specInfo)))
    names(spec) <- rownames(specInfo) <- specidx

    return(SpectraData(info = specInfo, spectra = spec))
  })

