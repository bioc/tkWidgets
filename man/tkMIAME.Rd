\name{tkMIAME}
\alias{tkMIAME}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Simple Interface to enter experimental design information}
\description{This function provides a widget for users to enter
  experimental design MIAME information.
}
\usage{
tkMIAME()
}

\details{
This widget provides an interface to enter experimental information following
the MIAME (Minimum Information About a Microarray Experiment) standard.

A draft of the latest document (v. 1.1) is
\url{http://www.mged.org/Workgroups/MIAME/miame_1.1.html}

Brazma et al. divide the MIAME into 6 sections
1. Experimental design, 2. Array design, 3. Samples, 4. Hybridizations,
5. Measurements, and 6. Normalization controls
This widget is for the first section.
We ask for the user to enter: experimenter name,
laboratory, contact information, a single-sentence experiment title, an
abstract describing the experiment, URLs. This slot could also include a
formal statistical description of the experimental design (e.g. using
factors). Some of this info is already stored in phenoData or elsewhere.

The function returns a list that is intended for the creation of an
object of class \code{\link[Biobase]{MIAME}}. However, we return a list
so that the function can work independently of the Biobase package.
}
\value{
  A list containing entries:
  
   \item{ExperimentName}{character string}
   \item{LabName}{character string}
   \item{ContactInfo}{character string}
   \item{ExperimentTitle}{character string}
   \item{Description}{character string}
   \item{URL}{character string}
}
\references{``Minimum information about a microarray experiment
  (MIAME)-toward standards for microarray data'', A. Brazma, et al.,
  Nature Genetics, vol. 29 (December 2001), pp. 365-371,
  \url{http://www.mged.org/Workgroups/MIAME/miame_1.1.html}} 
\author{Majnu John}
\seealso{\code{\link[Biobase]{MIAME}}}


\keyword{interface}% __ONLY ONE__ keyword per line
