GSE = "GSE13"
GPL = "GPL75"
PROBE = "aa253688_s_at"
LOG2 = TRUE
DE.column = "geo_accession"
DE.groups = c("GSM610")

##################################################################
# Analysis settings
# Note: we assume expression is on log scale
# We also match probe directly, rather than gene and then probe
#GSE = "GSE13"
#GPL = "GPL75"
#LOG2 = TRUE
#PROBE = "aa000380_s_at"
#DE.column = "source_name_ch1"
#DE.groups = c("Immature B cells", "Mature B cells")
##################################################################

library(GEOquery)
library(ggplot2)
library(reshape)

###############################################################################
### stripchart2 function to dictate the graphical appearance of gene expression
##############################################################################
stripchart2 <- function(x,y, group.names = NULL, jitter = 0.3, line.off = 0.3, 
                        lwd = 5, col = NULL, main = "", mark = "mean", ...) {
  
  s = split(x,y)
  #print(s)
  
  if (is.null(group.names)) group.names = names(s)
  
  if (is.null(col)) col = 1:length(s)
  add = NULL
  if (length(s) == 2) {
    m = lapply(s,mean, na.rm=TRUE)
    fc = round(2**(m[[2]] - m[[1]]), 2)
    t = t.test(s[[1]], s[[2]])
    p = round(t$p.value, 3)   
    if (p < 0.001) {
      p = "(P < 0.001)"
    } else {
      p = paste0("(P = ", p, ")")
    }
    add = paste("\nFC = ", fc, p, collapse = "")
  } else if (length(s) > 2) {
    #cat("fittin lm...\n")
    l = lm(x~y); l = summary(l)
    p = 1-pf(l$fstatistic[1], l$fstatistic[2], l$fstatistic[3])
    p = round(p, 3)
    if (p < 0.001) {
      add = "\nP < 0.001"
    } else {
      add = paste0("\nP = ", p)
    }
  } 
  
  if (is.null(main)) {
    main = "" 
  } else {
    main = paste(main, add)
  }
  
  #gd
  #stripchart(s, vertical=TRUE, method = "jitter", jitter = jitter, col = col, pch = 19, group.names=group.names, main = main,  ...)
  
  #jd
  m = melt(s, na.rm=TRUE)
  #View(m)
  stripchart3 <- ggplot(m, aes(x = as.factor(L1), y = value, color=L1)) 
  return(stripchart3 + 
           labs(title = main, y = "log2 expression", x="") +
           theme(legend.position="none") +
           scale_x_discrete(labels=group.names) +
           geom_point(position = "jitter", aes(colour = L1)) + 
           scale_colour_manual(values = col) +
           geom_errorbar(stat = "hline", yintercept = "mean", width=0.8,aes(ymax=..y..,ymin=..y..)))
  
  if (mark %in% c("mean", "median")) {
    if (mark == "mean") mm = lapply(s,mean, na.rm=TRUE)
    if (mark == "median") mm = lapply(s, median, na.rm=TRUE)
    for (i in 1:length(mm)) {
      lines(c(i-line.off, i+line.off), c(mm[[i]], mm[[i]]), lwd = lwd)
    }
  }
}


dataInput = getGEO(GSE)
platforms = sapply(dataInput, annotation)
pl.index = match(GPL, platforms)

if (is.na(pl.index)) {
  stop("error: cannot find platform")
}

expr = exprs(dataInput[[pl.index]])
if (LOG2) expr = log2(expr)
clinical = pData(dataInput[[pl.index]])

m = match(PROBE, rownames(expr))
if (is.na(pl.index)) {
  stop("error: cannot find probe")
}

x = expr[m,]
y = as.character(clinical[[DE.column]])

y[!y%in%DE.groups] = NA

stripchart2(x,y)


