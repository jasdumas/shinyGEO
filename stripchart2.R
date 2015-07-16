###############################################################################
### stripchart2 function to dictate the graphical appearance of gene expression
##############################################################################
stripchart2 <- function(x,y, group.names = NULL, jitter = 0.3, line.off = 0.3, 
                        lwd = 5, col = NULL, main = "", mark = "mean", 
                        ret = FALSE,  AUC = FALSE, subset = rep(TRUE, length(y)), 
                        keep.groups = NULL, ...) {
  x = x[subset]
  y = y[subset]
  s = split(x,y)
  if (is.null(keep.groups)) keep.groups = 1:length(s)
  
  keep = y %in% names(s)[keep.groups]  
  x = x[keep]
  y = y[keep]
  
  keep = (1:length(s)) %in% keep.groups
  s = s[keep]
  if (is.null(group.names)) group.names = names(s)
  
  if (is.null(col)) col = 1:length(s)
  if (length(s) == 2) {
    m = lapply(s,mean)
    fc = round(2**(m[[2]] - m[[1]]), 2)
    
    add = paste("\nFC = ", fc, "", collapse = "")
    
    if (is.null(main)) {
      main = "" 
    } else {
      main = paste(main, add)
    }
  }
  
  #stripchart(s, vertical=TRUE, method = "jitter", jitter = jitter, col = col, pch = 19, group.names=group.names, main = main,  ...)
  stripchart3 <- ggplot( melt(s), aes(x = as.factor(L1), y = value)) + geom_jitter()
  #stripchart3 <- ggplot(try.s, aes(x, y)) + geom_jitter()
  #print(stripchart3)
  return(stripchart3)
  #print(head(melt(s)))
  
  
  if (mark %in% c("mean", "median")) {
    if (mark == "mean") mm = lapply(s,mean, na.rm=TRUE)
    if (mark == "median") mm = lapply(s, median, na.rm=TRUE)
    for (i in 1:length(mm)) {
      lines(c(i-line.off, i+line.off), c(mm[[i]], mm[[i]]), lwd = lwd)
    }
  }
  if (ret) return(s)
}
