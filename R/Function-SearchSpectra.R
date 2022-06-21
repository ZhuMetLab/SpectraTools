#' @title .TrapezoidalScore
#' @param dataExp numeric. experimental value
#' @param dataRef numeric. referance value
#' @param tolerance numeric vector of two. tolerance range of scoring.
.TrapezoidalScore <- function(dataExp, dataRef, tolerance,
                              type = c("absolute", "percentage")){
  type = match.arg(type)
  if (type == "percentage") {
    deltaRaw <- (dataExp - dataRef) / dataRef * 100
  } else {
    deltaRaw <- dataExp - dataRef
  }
  delta <- abs(deltaRaw)
  tolerance[1] <- tolerance[1]
  tolerance[2] <- tolerance[2]
  delta[delta <= tolerance[1]] <- tolerance[1]
  score <- 1 - ((delta - tolerance[1]) / (tolerance[2] - tolerance[1]))
  score[score < 0] <- 0
  attr(score, "delta") <- deltaRaw
  return(score)
}

.GetRefMZ <- function(metInfo, adductIncluded, adductExcluded, adductFile,
                      classIncluded, classExcluded, ...) {
  if (!is.null(adductFile)) {
    if (!file.exists(adductFile)) {
      stop(paste0("Adduct table file '", adductFile, "' does not exist!\n",
                  "Please check your adduct table file"))
    }
    adductTable <- read.csv(adductFile, stringsAsFactors = FALSE)
    rownames(adductTable) <- adductTable$name
    adducts <- adductTable$name
  } else {
    adducts <- unique(metInfo$adduct)
  }

  if (!is.null(adductIncluded) | !is.null(adductExcluded)) {
    isExcluded <- rep(FALSE, length(adducts))
    isIncluded <- rep(TRUE,  length(adducts))
    if (!is.null(adductExcluded)) {
      isExcluded <- adducts %in% adductExcluded
    }

    if (!is.null(adductIncluded)) {
      adductIncluded <- setdiff(adductIncluded, adductExcluded)
      isIncluded <- adducts %in% adductIncluded
    }
    adducts <- adducts[isIncluded & !isExcluded]
  }

  if (!is.null(classIncluded) | !is.null(classExcluded)) {
    classes <- unique(metInfo$classname)
    isExcluded <- rep(FALSE, length(classes))
    isIncluded <- rep(TRUE,  length(classes))
    if (!is.null(classExcluded)) {
      isExcluded <- classes %in% classExcluded
    }

    if (!is.null(classIncluded)) {
      classIncluded <- setdiff(classIncluded, classExcluded)
      isIncluded <- classes %in% classIncluded
    }
    classes <- classes[isIncluded & !isExcluded]
    metInfo <- metInfo[metInfo$classname %in% classes, , drop = FALSE]
  }

  if (!is.null(adductFile)) {
    adductTable <- adductTable[adducts, , drop = FALSE]
    adductNumber <- length(adducts)
    res <- do.call(rbind, lapply(rownames(metInfo), function(nm) {
      dr <- metInfo[nm, ]
      mz <- (dr$mz * adductTable$nmol + adductTable$massdiff) / abs(adductTable$charge)
      data.frame("specname" = nm,
                 "adduct" = adductTable$name,
                 "mz" = mz,
                 stringsAsFactors = FALSE)
    }))
  } else {
    metInfo <- metInfo[metInfo$adduct %in% adducts, , drop = FALSE]
    res <- data.frame("specname" = rownames(metInfo),
                      "adduct" = metInfo$adduct,
                      "mz" = metInfo$mz,
                      stringsAsFactors = FALSE)
  }

  return(res)
}

.CalibrateRT <- function(refInfo, rtcalRef, rtcalExp) {
  traindata <- merge(rtcalExp, rtcalRef, by = "name", all = FALSE)
  colnames(traindata) <- c("name", "rtexp", "rtref")
  if (nrow(traindata) < 7) {
    warning("The number of RT mapping compounds is less than 7!")
  }
  traindata <- traindata[order(traindata$rtexp), ]
  mapmodel <- loess(rtexp~rtref, data = traindata, span = 0.75, degree = 2)
  refInfo$rt <- round(predict(object = mapmodel, newdata = refInfo$rt), 0)
  return(refInfo)
}
