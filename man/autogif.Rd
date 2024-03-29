% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autogif.R
\name{autogif}
\alias{autogif}
\title{Create an animation from a TROLL simulation}
\usage{
autogif(
  name = NULL,
  path = NULL,
  variables = c("species", "height_ct", "npp_gpp", "height", "lai"),
  global,
  species,
  climate,
  daily,
  forest = NULL,
  verbose = TRUE,
  overwrite = TRUE,
  thin = NULL
)
}
\arguments{
\item{name}{char. Model name (if NULL timestamp).}

\item{path}{char. Path to save the simulation outputs, the default is null
corresponding to a simulation in memory without saved intermediary files
(based on temporary files from \link{option.rcontroll}).}

\item{variables}{char. Variables to build as a gif among 'species',
'height_ct', 'npp_gpp', 'height', or 'lai'.}

\item{global}{df. Global parameters (e.g. \link{TROLLv3_input} or using
\code{\link[=generate_parameters]{generate_parameters()}}).}

\item{species}{df. Species parameters (e.g. \link{TROLLv3_species}).}

\item{climate}{df. Climate parameters (e.g. \link{TROLLv3_climatedaytime12}).}

\item{daily}{df. Daily variation parameters (e.g. \link{TROLLv3_daytimevar}).}

\item{forest}{df. TROLL with forest input, if null starts from an empty grid
(default NULL) (e.g. using \link{TROLLv3_output} with \code{\link[=get_forest]{get_forest()}}).}

\item{verbose}{bool. Show TROLL outputs in the console.}

\item{overwrite}{bool. Overwrite previous outputs.}

\item{thin}{int. Vector of integers corresponding to the iterations to be
kept to reduce output size, default is NULL and corresponds to no thinning.}
}
\value{
A list of \code{gganimate} objects corresponding to chosen outputs.
}
\description{
\code{autogif()} uses \code{ggplot2} and \code{gganimate} to render an animation from a
TROLL simulation. The animation can include a vertical cut in the forest
structure along the X-axis highlighting either tree species (\code{variables = 'species'}), either tree height relative to their maximum height (\code{variables = 'height_ct'}), or tree carbon acquisition with net over growth primary
productivity (\code{variables = 'npp_gpp'}). The animation can also include a top
view of the canopy representing either canopy trees height (\code{variables = 'height'}) or total leaf area index per pixel (\code{variables = 'lai'}).
}
\examples{
\dontrun{
data("TROLLv3_species")
data("TROLLv3_climatedaytime12")
data("TROLLv3_daytimevar")
autogif(
  name = "test", global = generate_parameters(
    cols = 100, rows = 100,
    iterperyear = 12, nbiter = 12 * 100,
    extent_visual = 10
  ),
  species = TROLLv3_species,
  climate = TROLLv3_climatedaytime12,
  daily = TROLLv3_daytimevar,
  verbose = FALSE
)
}

}
\seealso{
\link{autoplot,trollsim-method}
}
