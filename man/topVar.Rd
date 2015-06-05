\name{topVar}
\alias{topVar}

\title{
  Selecting top weighted variables (genes) from result of MCIA or CIA 
}
\description{
  The user level function calls \code{topVar.mcia} or 
  \code{topVar.cia}. This function provides a method selecting 
  top weighted variables (genes) on an axis (either positive side or negative side or both).
}
\usage{
	topVar(x, axis = 1, end = "both", topN = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  	an object of class \code{mcia} or \code{cia}
}
  \item{axis}{
  	an interger to sepecify which axis to check
}
  \item{end}{
  	which end of the axis to check, could be \code{positive},
  	\code{negative} or \code{both}. Any unambiguous substring can be given.
}
  \item{topN}{
  	An integer. The number of top weighted variable to return.
}
}

\value{
	Returns a data.frame contains selected variables. 
}

\author{
	Chen Meng
}

\examples{
	data(NCI60_4arrays)
	mcoin <- mcia(NCI60_4arrays)
	topVar(mcoin, axis = 1, end = "both", topN = 3)
}

