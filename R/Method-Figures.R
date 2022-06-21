#' @export
setMethod(
"PlotMirror",
signature = c("MatchScore"),
function(matchScore, title, addname = FALSE, plotPNG = FALSE, plotPDF = TRUE,
         filePlot = title, direction = c("both", "forward", "reverse")) {
  direction <- match.arg(direction)
  matchedFragments <- matchScore@matchedFragments
  info <- matchScore@info
  ruleApplied <- "idRule" %in% colnames(info)
  cnmRef <- c("mz", "intensity", "annotation")
  cnmExp <- c("mzExp", "intensityExp")
  if (!dir.exists(dirname(filePlot))) {
    dir.create(dirname(filePlot), recursive = TRUE)
  }
  shapeValues=c(19, 4, 1)
  names(shapeValues) = c("matched", "unmatched", "precursor")

  for (idx in seq_along(matchedFragments)) {
    matchedFragment <- matchedFragments[[idx]]
    matchedFragment$intensity <- -matchedFragment$intensity/max(matchedFragment$intensity)
    matchedFragment$intensityExp <- matchedFragment$intensityExp/max(matchedFragment$intensityExp)

    mzrg <- range(na.omit(c(matchedFragment$mz, matchedFragment$mzExp)))

    colRef <- intersect(cnmRef, colnames(matchedFragment))
    colExp <- intersect(cnmExp, colnames(matchedFragment))
    rInfo <- info[idx, ]

    exp <- matchedFragment[, colExp, drop = FALSE]
    colnames(exp)[1:2] <- c("mz", "intensity")
    ref <- matchedFragment[, colRef, drop = FALSE]
    if ("annotation" %in% colRef) {
      exp$annotation <- NA
    }
    isMatched <- matchedFragment$intensity * matchedFragment$intensityExp != 0
    ref$match <- exp$match <- "unmatched"
    ref$match[isMatched] <- exp$match[isMatched] <- "matched"
    ref$spectrum <- "reference"
    exp$spectrum <- apply(exp, 1, function(dr) {
      ifelse(dr["match"] == "matched", "matched", "unmatched")
    })
    if (any(matchedFragment$fragPrecursor)) {
      ref$match[matchedFragment$fragPrecursor] <- "precursor"
      exp$match[matchedFragment$fragPrecursor] <- "precursor"
    }

    df <- rbind(ref, exp)
    df <- df[df$intensity != 0, , drop = FALSE]

    # df$clr <- apply(df, 1, function(dr) {
    #   switch(dr['spectrum'],
    #          'reference' = 'red',
    #          'experiment' = {
    #            ifelse(dr['match'] == 'matched', 'blue', 'gray50')
    #          })
    # })

    clsc <- switch(direction,
                   "both" = c("scoreReverse", "scoreForward"),
                   "reverse" = "scoreReverse",
                   "forward" = "scoreForward")
    sc <- paste(gsub("score", "", clsc), round(info[idx, clsc], 3),
                collapse = " | ",
                sep = ": ")
    titleText <- paste(title, rInfo$name, sep = ": ")
    titleText <- ifelse(ruleApplied,
                        paste(titleText, rInfo$idRule, sep = " -> "),
                        titleText)

    p <- ggplot2::ggplot() +
      ggplot2::geom_segment(data = df,
                   ggplot2::aes(x = mz, xend = mz, y = 0, yend = intensity,
                       color = spectrum)) +
      ggplot2::scale_color_manual(values = c("red", "blue", "gray50"),
                         breaks = c("reference", "matched", "unmatched")) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::geom_point(data = df,
                          ggplot2::aes(x = mz, y = intensity,
                     color = spectrum,
                     shape = match)) +
      ggplot2::scale_shape_manual(values = c(19, 4, 1),
                         breaks = c("matched", "unmatched", "precursor")) +
      ggplot2::labs(title = titleText,
           subtitle = paste(info$adduct, sc, sep = " | ")) +
      ggplot2::xlab("m/z (Da)") + ggplot2::ylab("Relative Intensity") +
      ggplot2::theme(title = ggplot2::element_text(size = 16, face = 'bold'),
            plot.subtitle = ggplot2::element_text(size = 12, face = "plain"),
            axis.text = ggplot2::element_text(size = 14),
            axis.title = ggplot2::element_text(size = 16,face = "bold"),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank())
    if (ruleApplied) {
      # require(ggrepel)
      iskeep <- !is.na(df$annotation) & df$match == "matched"
      p <- p + ggrepel::geom_text_repel(data = df[iskeep, , drop = FALSE],
                                        ggplot2::aes(mz, intensity,
                                                     label = df[iskeep, 'annotation'],
                                                     angle = 0),
                                        ylim = c(-1, -0.1),
                                        segment.color = 'steelblue',
                                        segment.alpha = 0.8)
    }

    if (!dir.exists(filePlot)) {
      dir.create(filePlot)
    }
    if (plotPNG) {
      ggplot2::ggsave(file.path(filePlot, paste0(title, "-", idx, ".png")),
             plot = p, width = 8, height = 6, dpi = 150)
    }
    if (plotPDF) {
      ggplot2::ggsave(file.path(filePlot, paste0(title, "-", idx, ".pdf")),
             plot = p, width = 8, height = 6, dpi = 300)
    }

  }
})
# setMethod(
#   "PlotMirror",
#   signature = c("MatchScore"),
#   function(matchScore, title, addname = FALSE, plotPNG = FALSE, plotPDF = TRUE,
#            filePlot = title) {
#     matchedFragments <- matchScore@matchedFragments
#     info <- matchScore@info
#     ruleApplied <- "idRule" %in% colnames(info)
#     cnmRef <- c("mz", "intensity", "annotation")
#     if (!dir.exists(dirname(filePlot))) {
#       dir.create(dirname(filePlot), recursive = TRUE)
#     }
#     # browser()
#     if (plotPDF) {
#       CairoPDF(paste0(filePlot, ".pdf"), width = 8, height = 6)
#     }
#     for (idx in seq_along(matchedFragments)) {
#       matchedFragment <- matchedFragments[[idx]]
#       rInfo <- info[idx, ]
#       mzrg <- range(na.omit(c(matchedFragment$mz, matchedFragment$mzExp)))
#       matchedFragment$intensity <- -matchedFragment$intensity/max(matchedFragment$intensity)
#       matchedFragment$intensityExp <- matchedFragment$intensityExp/max(matchedFragment$intensityExp)
#       if (plotPNG) {
#         dir.create(filePlot)
#         CairoPNG(file.path(filePlot, paste(idx, ".png")), width = 800, height = 600)
#       }
#       plot(matchedFragment[, c("mz", "intensity")],
#            type = "h", col = "red",
#            xlim = mzrg, ylim = c(-1, 1),
#            xlab = "m/z", ylab = "Relative Intensity",
#            main = title, cex.lab = 1.5, cex.axis = 1.2)
#       abline(h = 0)
#       points(matchedFragment[, c("mzExp", "intensityExp")],
#              type = "h", col = "gray50")
#
#       isMatched <- matchedFragment$intensity * matchedFragment$intensityExp != 0
#       topch20 <- isMatched & !matchedFragment$fragPrecursor
#       topch1 <- isMatched & matchedFragment$fragPrecursor
#       expMatched <- matchedFragment[topch20, c("mzExp", "intensityExp"), drop = FALSE]
#       points(expMatched, type = "h", col = "blue")
#       points(expMatched, pch = 20, col = "blue")
#
#       colRef <- na.omit(match(cnmRef, colnames(matchedFragment)))
#       refMatched <- matchedFragment[topch20, colRef, drop = FALSE]
#       points(refMatched[, c("mz", "intensity")], pch = 20, col = "red")
#       if (addname) {
#         text(refMatched$mz, 0, refMatched$annotation, srt = 90)
#       }
#       if (any(topch1)) {
#         points(matchedFragment[topch1, c("mzExp", "intensityExp")],
#                type = "h", col = "blue")
#         points(matchedFragment[topch1, c("mzExp", "intensityExp")],
#                pch = 1, col = "blue")
#         points(matchedFragment[topch1, c("mz", "intensity")],
#                pch = 1, col = "red")
#         legend("bottomright",
#                c("Experiment unmatched", "Experiment matched",
#                  "Reference matched", "Precursor matched"),
#                lty = 1, pch = c(NA, 20, 20, 1), col = c("gray50", "blue", "red", "blue"),
#                box.lty = 0, bg = "transparent", cex = 0.8)
#       } else {
#         legend("bottomright", c("Experiment unmatched", "Experiment", "Reference"),
#                lty = 1, pch = c(NA, 20, 20), col = c("gray50", "blue", "red"),
#                box.lty = 0, bg = "transparent", cex = 0.8)
#       }
#       idname <- ifelse(ruleApplied,
#                        paste(rInfo$name, rInfo$idRule, sep = " -> "),
#                        rInfo$name)
#       legend("topleft",
#              c(idname,
#                paste(c("Reverse", "Forward"),
#                      round(c(rInfo$scoreReverse[1], rInfo$scoreForward[1]), 4),
#                      sep = ": ")),
#                box.lty = 0, bg = "transparent", cex = 0.8)
#       if (plotPNG) {
#         dev.off()
#       }
#     }
#
#     if (plotPDF) {
#       dev.off()
#     }
#     Sys.sleep(1)
#   })
