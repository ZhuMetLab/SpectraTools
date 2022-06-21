
#' @title ParseMSP
#' @aliases MSP spectrum file parser
#' @description  Read a MSP file and parse it to list of spectra with corresponding
#' precursor information
#' @param file character: path of the MSP file
#' @rdname function-parseSpectra
#' @export
ParseMSP <- function(file,
                     colSpectra = c(1, 2),
                     nameSpectra = c("mz", "intensity"),
                     ...) {
  cat("Parsing file --", file, "\n")
  specDataList <- .ListFile(file, septext = "", nlineMod = 1)

  specData <- lapply(specDataList, function(specData) {
    nrNumPk <- grep("Num Peaks", specData)
    info <- .SplitInfo(specData[1:nrNumPk], ": ")
    if (info[2, nrNumPk] != "0") {
      specList <- strsplit(specData[(nrNumPk + 1):length(specData)], " |\\t")
      specList <- lapply(specList, as.numeric)
      spec <- do.call(rbind, specList)[, colSpectra, drop = FALSE]
    } else {
      spec <- matrix(nrow = 0, ncol = length(colSpectra))
    }
    colnames(spec) <- nameSpectra
    list("info" = info[2, , drop = FALSE],
         "spec" = spec)
  })

  return(specData)
}

#' @title ParseMGF
#' @aliases MSP spectrum file parser
#' @description  Read a MGF file and parse it to list of spectra with corresponding
#' precursor information
#' @param file character: path of the MGF file
#' @rdname function-parseSpectra
#' @export
ParseMGF <- function(file,
                     colSpectra = c(1, 2),
                     nameSpectra = c("mz", "intensity"),
                     ...) {
  cat("Parsing file --", file, "\n")
  specDataList <- .ListFile(file, "END IONS", 0)
  specData <- lapply(specDataList, function(specData) {
    nls <- grep("BEGIN IONS", specData)
    nlSpec <- grep("^\\d", specData)
    if (length(nlSpec) == 0) return(NULL)
    infoSource <- specData[seq(nls + 1, nlSpec[1] - 1)]
    info <- .SplitInfo(infoSource, "=")
    spec <- data.frame(do.call(rbind, strsplit(specData[nlSpec], split = "\t|\\s")),
                       stringsAsFactors = F)[, colSpectra, drop = FALSE]
    names(spec) <- nameSpectra
    list("info" = info[2, , drop = FALSE],
         "spec" = spec)
  })
  specData <- specData[!sapply(specData, is.null)]
  return(specData)
}

#' @title ParseCEF
#' @aliases CEF spectrum file parser
#' @description  Read a CEF file and parse it to list of spectra with corresponding
#' precursor information
#' @param file character: path of the CEF file
#' @rdname function-parseSpectra
#' @export
ParseCEF <- function(file,
                     colSpectra = c(1, 2),
                     nameSpectra = c("mz", "intensity"),
                     ...) {
  cat("Parsing file --", file, "\n")
  suppressPackageStartupMessages(require(xml2))
  cefdata <- xml2::read_xml(file)
  ceflist <- xml2::as_list(cefdata, ".//Location")
  infoname <- c("mz", "rt", "intensity", "dt", "ccs")
  ms2info <- lapply(ceflist$CEF$CompoundList, function(cmpd) {
    info <- matrix(as.numeric(do.call(c, attributes(cmpd$Location))), nrow = 1)
    info[1] <- as.numeric(unlist(cmpd$Spectrum$MzOfInterest$mz))
    colnames(info) <- infoname
    info
  })
  names(ms2info) <- NULL
  # ms2info <- as.data.frame(t(sapply(ceflist$CEF$CompoundList, function(cmpd) {
  #   as.numeric(do.call(c, attributes(cmpd$Location)))
  # })), stringsAsFactors = FALSE)
  # colnames(ms2info) <- c("mz", "rt", "intensity", "dt", "ccs")
  # rownames(ms2info) <- NULL
  ms2spec <- lapply(ceflist$CEF$CompoundList, function(cmpd) {
    spectrum <- as.data.frame(t(sapply(cmpd$Spectrum$MSPeaks, function(spec) {
      as.numeric(do.call(c, attributes(spec)))
    })))
    colnames(spectrum) <- c("mz", "intensity")
    rownames(spectrum) <- NULL
    spectrum
  })
  names(ms2spec) <- NULL

  specData <- lapply(seq_along(ms2spec), function(idx) {
    list("info" = ms2info[[idx]],
         "spec" = ms2spec[[idx]])
  })
  return(specData)
}


#' @title ParseTXT
#' @aliases TXT spectrum file parser
#' @description  Read a TXT file of MassBank Euroup and parse it to list of
#' spectra with corresponding precursor information
#' @param file character: path of the TXT file
#' @rdname function-parseSpectra
#' @export
ParseTXT <- function(file,
                    colSpectra = c(1, 2, 3),
                    nameSpectra = c("mz", "intensity", "intensity.rel"),
                    colAnno = c(1, 2),
                    nameAnno = c("mzAnno", "formula"),
                    ppm = 5,
                    ...) {
  cat("Parsing file --", file, "\n")
  specDataList <- .ListFile(file, "//", 1)
  annoTmp <- rep("", length(nameAnno))
  names(annoTmp) <- nameAnno
  specData <- lapply(specDataList, function(specData) {
    specData <- gsub("^ +", "", specData)
    nlFrag <- grep("^\\d", specData)
    slAnno <- grep("^PK\\$ANNOTATION", specData)
    slSpec <- grep("PK\\$PEAK", specData)
    nlAnno <- nlFrag[nlFrag < slSpec]
    nlSpec <- nlFrag[nlFrag > slSpec]
    if (length(nlSpec) == 0) return(NULL)
    infoSource <- specData[-nlFrag]
    info <- .SplitInfo(infoSource, ": ")
    spec <- data.frame(do.call(rbind, strsplit(specData[nlSpec], split = "\t+|\\s+")),
                       stringsAsFactors = F)[, colSpectra, drop = FALSE]
    names(spec) <- nameSpectra
    if (length(nlAnno) > 0) {
      anno <- data.frame(do.call(rbind, strsplit(specData[nlAnno], split = "\t+|\\s+")),
                         stringsAsFactors = F)[, colAnno, drop = FALSE]
      names(anno) <- nameAnno
      specAnno <- t(sapply(as.numeric(spec$mz), function(mz) {
        mzrange <- .GetPpmRange(mz, ppm, resDefineAt = 0)
        mzAnno <- as.numeric(anno$mz)
        idx <- which(mzAnno >= mzrange[1] & mzAnno <= mzrange[2])
        if (length((idx) > 0)) {
          anno[idx, ]
        } else {
          annoTmp
        }
      }))
    } else {
      specAnno <- matrix(rep(annoTmp, nrow(spec)), ncol = length(annoTmp))
      colnames(specAnno) <- nameAnno
    }
    list("info" = info[2, , drop = FALSE],
         "spec" = cbind(spec, specAnno))
  })
  specData <- specData[!sapply(specData, is.null)]
  return(specData)
}

.ListFile <- function(file, septext = "", nlineMod = 1) {
  specData <- readLines(file, encoding = 'UTF-8')
  nlNewDB <- 1
  idxDB <- 1
  dbList <- list()
  lenData <- length(specData)

  for (nl in 1:lenData) {
    if (specData[nl] == septext) {
      if (nl == nlNewDB) {
        nlNewDB <- nl + 1
        next
      }
      dbList[idxDB] <- list(Compound = specData[nlNewDB:(nl - nlineMod)])
      nlNewDB <- nl + 1
      idxDB <- idxDB + 1
    } else if (nl == lenData) {
      dbList[idxDB] <- list(Compound = specData[nlNewDB:nl])
    }
  }
  return(dbList)
}

.SplitInfo <- function(infoSource, split = ": ") {
  infoList <- regmatches(infoSource, regexpr(split, infoSource),
                          invert = TRUE)
  is.na <- sapply(infoList, length) == 1
  info <- do.call(cbind, infoList)
  info[2, is.na] <- NA
  colnames(info) <- info[1, ]
  return(info)
}

#' @title ParseCSV
#' @aliases CSV spectrum file parser
#' @description  Read a CSV file and parse it to list of spectra with corresponding
#' precursor information
#' @param file character: path of the CSV file
#' @param skip integer: the number of lines of the data file to skip before
#' beginning to read data.
#' @param colSpectra vector of two intergers: columns indices recording spectra info
#' @rdname function-parseSpectra
#' @export
ParseCSV <- function(file,
                     colSpectra = c(1, 2),
                     nameSpectra = c("mz", "intensity"),
                     skip = 0L, ...) {
  dt <- read.csv(file, stringsAsFactors = FALSE, skip = skip)
  return(.GetCsvSpec(dt, colSpectra = colSpectra))
}

.GetCsvSpec <- function(dt,
                        colSpectra = c(1, 2),
                        nameSpectra = c("mz", "intensity")) {
  spec <- dt[, colSpectra]
  colnames(spec) <- nameSpectra
  return(as.matrix(spec))
}

#' @title ParseMZXML
#' @aliases mzXML spectrum file parser
#' @description  Read a mzXML file and parse it to list of spectra with corresponding
#' precursor information
#' @param file character: path of the mzXML file
#' @rdname function-parseSpectra
#' @export
ParseMZXML <- function(file, ...) {
  suppressPackageStartupMessages(require(xcms))
  xraw <- xcms::xcmsRaw(file, includeMSn = TRUE)
  scans <- xraw@msnScanindex
  ms2ce <- xraw@msnCollisionEnergy
  ms2rt <- xraw@msnRt
  ms2mz <- xraw@env$msnMz
  precursormz <- xraw@msnPrecursorMz
  precursorint <- xraw@msnPrecursorIntensity
  ms2int <- xraw@env$msnIntensity
  specAll <- cbind("mz" = ms2mz, "intensity" = ms2int)
  ms2scans <- cbind(scans + 1, c(scans[-1], length(ms2mz)))

  specData <- lapply(seq_along(precursormz), function(idx) {
    spec <- specAll[ms2scans[idx, 1]:ms2scans[idx, 2], , drop = FALSE]
    info <- c("mz" = precursormz[idx],
              'int' = precursorint[idx],
              "ce" = ms2ce[idx],
              "rt" = ms2rt[idx])
    return(list("spec" = spec,
                "info" = info))
  })
  return(specData)
}
