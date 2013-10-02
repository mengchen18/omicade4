\name{omicade4-package}
\alias{omicade4-package}
\alias{omicade4}
\docType{package}
\title{
multiple co-inertia analysis of omics datasets
}
\description{
The main function in the package performing multiple co-inertia analysis on omics datasets
}
\details{
\tabular{ll}{
Package: \tab omicade4\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-04-01\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
Multiple co-inertia analysis (MCIA) is a multivariate 
analysis method that could be used to analyze multiple tables measuring the 
same set of individuals, this package provides a one-stop function for 
MCIA and functions for subsequent analysis especially for multiple 
omics datasets.
}
\author{
Chen Meng, Aedin Culhane, Amin M. Gholami

Maintainer: Chen Meng <chen.meng@tum.de>
}

\references{
  Meng C, Kuster B, Culhane AC and Gholami AM. A multivariate 
  approach to the integration of multi-omics datasets. 
  (Manuscript under preparation) 

  Culhane AC, Thioulouse J, Perriere G, Higgins DG. (2005)
  MADE4: an R package for multivariate analysis of gene
  expression data.Bioinformatics. 21(11):2789-90. 
  
  S. Dray and A.B. Dufour. (2007) The ade4 package: implementing 
  the duality diagram for ecologists. Journal of Statistical 
  Software 22(4):1-20.
}

\keyword{multivariate}
\seealso{
  \code{\link[ade4]{ade4}} and package \code{made4}
}
\examples{
  data(NCI60_4arrays)
  mcoin <- mcia(NCI60_4arrays)
}
