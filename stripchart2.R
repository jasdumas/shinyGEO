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
  
  
  stripchart(s, vertical=TRUE, method = "jitter", jitter = jitter, col = col, pch = 19, group.names=group.names, main = main,  ...)
  #try.s <- as.data.frame(s)
  #stripchart3 <- ggplot(try.s, aes(x, y)) + geom_jitter()
  #print(stripchart3)
  
  if (mark %in% c("mean", "median")) {
    if (mark == "mean") mm = lapply(s,mean, na.rm=TRUE)
    if (mark == "median") mm = lapply(s, median, na.rm=TRUE)
    for (i in 1:length(mm)) {
      lines(c(i-line.off, i+line.off), c(mm[[i]], mm[[i]]), lwd = lwd)
    }
  }
}
