\name{plotVar.mcia}
\alias{plotVar.mcia}
\title{
  Plot variable space of result from \code{\link{mcia}}
}
\description{
  Plot variable space of \code{\link{mcia}} and visualize selected
  variables across datasets, the function is called by \code{plotVar}.
}
\usage{
\method{plotVar}{mcia}(x, var = NA, axes = 1:2, 
  var.col = "red", var.lab = FALSE, bg.var.col = "gray", 
  nlab = 0, sepID.data=NULL, sepID.sep= "\\\.",
  df = NA, layout = NA, ...)
}
\arguments{
  \item{x}{
    An object of class \code{mcia}, the result returned by \code{\link{mcia}}.
}
  \item{var}{
    see \code{\link{plotVar}}
}
  \item{axes}{
    see \code{\link{plotVar}}
}
  \item{var.col}{
    see \code{\link{plotVar}}
}
  \item{var.lab}{
    see \code{\link{plotVar}}
}
  \item{bg.var.col}{
    see \code{\link{plotVar}}
}
  \item{nlab}{
    see \code{\link{plotVar}}
}
  \item{sepID.data}{
    see \code{\link{plotVar}}
}
  \item{sepID.sep}{
    see \code{\link{plotVar}}
}
  \item{df}{
    Integers indicating which dataset should be plotted, the default
    NA means all datasets are plotted.
}
  \item{layout}{
    The layout of multiple plots.
}
  \item{\dots}{
    Other arguments
}
}

\value{
  If \code{var} is not NA, a data frame is return, with rows for variables of
  interest and columns of logical values indicating which data.frames contains which
  variables.
}

\author{
  Chen Meng
}

\seealso{
See Also as \code{\link{plotVar.cia}}, \code{\link{plotVar}}
}
\examples{
  data(NCI60_4arrays)
  mcoin <- mcia(NCI60_4arrays)
  plot.mcia(mcoin, sample.lab=FALSE, df.col=4:7)
  plotVar(mcoin, var=NA, bg.var.col=1:4,  var.lab=TRUE)
  plotVar(mcoin, var=c("SPOPL", "CAPN2", "SNX8"), 
          df=1:4, var.lab=TRUE, var.col=c("red", "green", "blue"))

  data(NCI60_4arrays)
  mcoin <- mcia(NCI60_4arrays)
  plotVar(mcoin, var=c("S100B", "S100A1"), var.lab=TRUE)

  # an example for the usage of sepID.data and sepID.sep
  nci60_mod <- NCI60_4arrays
  rownames(nci60_mod$hgu95) <- paste(rownames(nci60_mod$hgu95), "s1", sep="_")
  mcoin_mod <- mcia(nci60_mod)
  id <- split(rownames(mcoin_mod$mcoa$Tco), mcoin_mod$mcoa$TC$T)
  sapply(id, function(x) head(x))


  plotVar(mcoin_mod, var=c("S100B", "S100A1"), var.lab=TRUE, sepID.data=1:4, sepID.sep = c("\\\.", "\\\.", "\\\.", "_"))
  plotVar(mcoin_mod, var=c("S100B", "S100A1"), var.lab=TRUE, sepID.data=4, sepID.sep="_")
  plotVar(mcoin_mod, var=c("S100B", "S100A1"), var.lab=TRUE, sepID.data=1:3, sepID.sep="\\\.")

}