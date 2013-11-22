splot.mol.mcia <-
function(x, axis1, axis2, col, pch, gene.nlab) {
  ndata <- length(x$coa)
  co <- x$mcoa$Tco[, c(axis1, axis2)]
  c <- as.numeric(x$mcoa$TC$"T")
  if (length(col) == 1)
    col <- rep(col, ndata)
  if (length(pch) == 1)
    pch <- rep(pch, ndata)
  
  mcoaEnv <- environment(mcoa)
  fakeEnv2 <- new.env(parent = mcoaEnv)
  plotgenes2 <- made4::plotgenes
  environment(plotgenes2) <- fakeEnv2
  s.var <- made4::s.var
  environment(s.var) <- environment(mcoa)
  assign("genes", made4:::genes, fakeEnv2)
  assign("s.var", s.var, fakeEnv2)

  plotgenes2(co, colpoints=0, nlab=gene.nlab, pch=".", axis1=1, axis2=2, sub="variable space")
  
  for (i in 1:ndata) {
    points(co[c %in% i, ], col=col[i], pch=pch[i])
  }
}
