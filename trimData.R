
trimData <- function (coverageGlobal, plafFile) {
  
  coverageGlobal$MATCH <- paste(coverageGlobal$CHROM, coverageGlobal$POS, sep = "-")
  plafFile$MATCH <- paste(plafFile$CHROM, plafFile$POS, sep = "-")
  ### assertion: take a look at r-package: assertthat
  
  # Two stage of trimming
  #  1. trim them so they have the same sites
  #  2. trim off sites where REF+ALT<5 and PLAF == 0
  
  ### Trim1
  # get index of plaf
  plafIndex <- which(plafFile$MATCH %in% coverageGlobal$MATCH)
  plafFileTrim1 <- plafFile[plafIndex, ]
  # get index of coverage
  coverageIndex <- which(coverageGlobal$MATCH %in% plafFile$MATCH)
  coverageTrim1 <- coverageGlobal[coverageIndex, ]

  
  ### Trim2
  plafFileTrim2 <- plafFileTrim1[(plafFileTrim1$PLAF != 0) & 
                                   (coverageTrim1$refCount + coverageTrim1$altCount >= 5), ]
  coverageTrim2 <- coverageTrim1[(plafFileTrim1$PLAF != 0) & 
                                   (coverageTrim1$refCount + coverageTrim1$altCount >= 5), ]                         
  
  ### write files    
  plafTrim2 <- plafFileTrim2$PLAF
  altTrim2 <- coverageTrim2$altCount
  refTrim2 <- coverageTrim2$refCount
  # obsWSAFtmp <- alttmp/(reftmp + alttmp)
  
  write.table(plafTrim2, file = "tmpPLAF.txt", sep = "\t", quote = F, row.names = F)
  write.table(altTrim2, file = "tmpALT.txt", sep = "\t", quote = F, row.names = F)
  write.table(refTrim2, file = "tmpREF.txt", sep = "\t", quote = F, row.names = F)
  
  return (NULL)
}






