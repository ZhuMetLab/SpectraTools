#' @export
ConsensusSpectra <- function(ms2all,
                             snthreshMSMS = 10,
                             ppmBinMZ = 30,
                             minBinMZ = 0.004,
                             thrMZdiff = 0.3,
                             thrIntensityRel = 0.2,
                             minfracVote = 0.25,
                             ...) {
  ms2sn <- sapply(ms2all, function(spec) {
    int <- spec[, 'intensity']
    max(int) / min(int)
  })
  ms2all <- ms2all[ms2sn > snthreshMSMS]
  if (length(ms2all) == 0) {
    return(NULL)
  }

  # merge fragments within the same m/z bin across spectra
  ms2merged <- .MergeFragments(ms2all, ppmBinMZ, minBinMZ)
  # quanlity control: removing ring effect and low intensity spectra
  ms2merged <- .RemoveRingEffect(ms2merged,
                                 thrMZdiff = thrMZdiff,
                                 thrIntensityRel = thrIntensityRel)
  # get concensus spectra with peak voting
  spec <- .VoteSpectra(ms2merged, ms2all,
                       minfracVote = minfracVote,
                       ppm = ppmBinMZ,
                       minBinMZ = minBinMZ)
  if (nrow(spec) == 0) {
    spec <- NULL
  }
  return(spec)
}

.MergeFragments <- function(ms2all, ppmBinMZ = 30, minBinMZ = 0.004) {
  specall <- do.call(rbind, ms2all)
  specall <- specall[order(specall[, 'mz']), , drop = FALSE]

  idxall <- seq(nrow(specall))

  specmerged <- {}
  while (length(idxall) > 0 ) {
    idx <- tail(idxall, 1)
    mz <- specall[idx, 'mz']
    mzrange <- c(-1, 1) * max(prod(mz, ppmBinMZ, 1e-6), minBinMZ) + mz
    idxrange <- idxall[specall[idxall, 'mz'] >= mzrange[1] &
                         specall[idxall, 'mz'] <= mzrange[2]]
    spectmp <- sapply(c('mz', 'intensity'), function(x) {
      quantile(specall[idxall[idxrange], x], 0.5)
    })
    specmerged <- rbind(specmerged, spectmp)
    idxall <- idxall[-idxrange]
  }
  colnames(specmerged) <- c('mz', 'intensity')
  rownames(specmerged) <- NULL
  specmerged <- specmerged[order(specmerged[, 'mz']), , drop = FALSE]

  return(specmerged)
}

.VoteSpectra <- function(spec, ms2all,
                         minfracVote = 0.25,
                         ppmBinMZ = 30,
                         minBinMZ = 0.004) {
  numSpec = length(ms2all)
  if (numSpec * minfracVote <= 1) {
    minfracVote <- ifelse(numSpec > 1, 1 / (numSpec - 1), 1)
  }

  numContained <- sapply(spec[, 'mz'], function(mz) {
    mzrange <- mz + max(prod(mz, ppmBinMZ, 1e-6), minBinMZ) * c(-1, 1)
    contained <- sapply(ms2all, function(spec) {
      any(spec[, 'mz'] >= mzrange[1] & spec[, 'mz'] <= mzrange[2])
    })
    sum(contained)
  })
  spec[numContained/length(ms2all) >= minfracVote, , drop = FALSE]
}
