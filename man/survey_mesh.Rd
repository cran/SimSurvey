% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{survey_mesh}
\alias{survey_mesh}
\title{Sample survey meshes and related items}
\format{
An object of class \code{list} of length 3.
}
\usage{
survey_mesh
}
\description{
@format A list containing the R-INLA survey mesh, the set of triangles in the barrier and the barrier polygons for plotting
}
\details{
An example of a mesh containing barrier information for use with
sim_ays_covar_spde. Also derived from global administrative boundaries
data (http://gadm.org). Details on creation provided in the data-raw
folder of this package in the survey_mesh.R file. Includes the set
of barrier triangles needed to use the barrier approach, barrier
polygons for plotting and the set of triangles in the barrier.
}
\keyword{datasets}
