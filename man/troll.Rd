% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/troll.R
\name{troll}
\alias{troll}
\title{troll}
\usage{
troll(
  name = NULL,
  path = NULL,
  global,
  species,
  climate,
  daily,
  lidar = NULL,
  forest = NULL,
  verbose = TRUE,
  overwrite = TRUE,
  thin = NULL
)
}
\arguments{
\item{name}{char. Model name (if NULL timestamp).}

\item{path}{char. Path to save the simulation outputs, the default is null
corresponding to a simulation in memory without saved intermediary files.}

\item{global}{df. Global parameters.}

\item{species}{df. Species parameters.}

\item{climate}{df. Climate parameters.}

\item{daily}{df. Daily variation parameters.}

\item{lidar}{df. Lidar simulation parameters, if null no computed
(default NULL).}

\item{forest}{df. TROLL with forest input, if null starts from an empty grid
(default NULL).}

\item{verbose}{bool. Show TROLL outputs in the console.}

\item{overwrite}{bool. Overwrite previous outputs.}

\item{thin}{int. Vector of integers corresponding to the iterations to be
kept to reduce output size, default is NULL and corresponds to no
thinning.}
}
\value{
A trollsim object.
}
\description{
Run a TROLL simulation.
}
\examples{
\donttest{
data("TROLLv3_species")
data("TROLLv3_climatedaytime12")
data("TROLLv3_daytimevar")
troll(
  name = "test",
  global = generate_parameters(
    cols = 100, rows = 100,
    iterperyear = 12, nbiter = 12 * 1
  ),
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar
)
}

}
