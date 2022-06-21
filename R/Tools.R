.GetPpmRange <- function(mz, ppm, resDefineAt = 400) {
  mz + c(-1, 1) * max(prod(mz, ppm, 1e-06), prod(resDefineAt,
                                                 ppm, 1e-06))
}

.GetDiffMZppm <- function(mz, resDefineAt = NULL) {
  mzDiff <- diff(mz) / mz[-1] * 1e6
  if (!is.null(resDefineAt)) {
    idx <- which(mz[-1] <= resDefineAt)
    mzDiff[idx] <- mzDiff[idx] * mz[-1][idx] / resDefineAt
  }
  mzDiff
}

.GetPpmDiff <- function(mz, mzref, resDefineAt = NULL) {
  mzdiff <- mz - mzref
  sapply(seq_along(mzdiff), function(idx) {
    mzdiff[idx] / max(mzref[idx], resDefineAt) * 1e6
  })
}

.Col2Numeric <- function(df) {
  if (is.matrix(df)) {
    df <- data.frame(df, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(df)) {
    return(df)
  }
  ncNumeric <- which(sapply(colnames(df), function(nm) {
    all(grepl("^([1-9]+[0-9]*|0)(\\.[\\d]+)?$", df[, nm], perl = TRUE))
  }))

  if (length(ncNumeric) > 0) {
    for (nc in ncNumeric) {
      df[, nc] <- as.numeric(df[, nc])
    }
  }
  return(df)
}

.MakeUniqueNames <- function(name) {
  duplicated <- TRUE
  i <- 2
  while (duplicated) {
    idxDuplicated <- which(duplicated(name))
    if (length(idxDuplicated) > 0) {
      if (i == 2) {
        name[idxDuplicated] <- paste0(name[idxDuplicated], '_', i)
      } else {
        name[idxDuplicated] <- gsub(paste0('_', i - 1), paste0('_', i),
                                    name[idxDuplicated])
      }
      i <- i + 1
    } else {
      duplicated <- FALSE
    }
  }
  return(name)
}

.LoadData <- function(file, keepName = FALSE, env){
  if (missing(env)) env <- new.env()
  b <- load(file, envir = env)
  if (keepName | length( b) > 1) {
    r <- lapply( b, function(b1) env[[ b1]])
    names( r) <- b
    r
  } else {
    env[[b]]
  }
}
