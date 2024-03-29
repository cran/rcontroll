#' @include troll.R
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach foreach %dopar%
#' @importFrom iterators icount
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom dplyr bind_rows
#' @importFrom tidyr unnest
NULL

#' Run a stack of `TROLL` simulations
#'
#' `stack()` run a stack of `TROLL` simulation. The minimal set of input files
#' required for a `TROLL` run include (i) climate data for the focal location
#' (`climate` and `daily`), (ii) functional traits for the list of species at
#' the focal location (`species`), and (iii) global parameters (`global`), i.e.
#' parameters that do not depend on species identity.
#'
#' @param name char. Stack name (if NULL the timestamp will be used).
#' @param simulations char. Simulation names (corrsponding to simulation indexes
#'   in orresponding tables, see example below).
#' @param path char. Path to save the stack of simulation outputs (parent
#'   folder), the default is null corresponding to a simulation in memory
#'   without saved intermediary files (based on temporary files from
#'   [option.rcontroll]).
#' @param global df. Global parameters (e.g. [TROLLv3_input] or using
#'   [generate_parameters()]).
#' @param species df. Species parameters (e.g. [TROLLv3_species]).
#' @param climate df. Climate parameters (e.g. [TROLLv3_climatedaytime12]).
#' @param daily df. Daily variation parameters (e.g. [TROLLv3_daytimevar]).
#' @param lidar df. Lidar simulation parameters (e.g. using [generate_lidar()]),
#'   if null not computed (default NULL).
#' @param forest df. TROLL with forest input, if null starts from an empty grid
#'   (default NULL) (e.g. using [TROLLv3_output] with [get_forest()]).
#' @param load bool. TROLL outputs are loaded in R memory, if not only the path
#'   and name of the stack of simulations is kept in the resulting
#'   [trollstack()] object but the content can be accessed later using the
#'   [load_sim()] method.
#' @param cores int. Number of cores for parallelization, if NULL available
#'   cores - 1 (default NULL). You can use [parallel::detectCores()] to know
#'   available cores on your machine.
#' @param verbose bool. Show TROLL log in the console.
#' @param overwrite bool. Overwrite previous outputs folder and files.
#' @param thin int. Vector of integers corresponding to the iterations to be
#'   kept to reduce output size, default is NULL and corresponds to no thinning.
#'
#' @return A [trollstack()] object.
#'
#' @seealso [troll()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data("TROLLv3_species")
#' data("TROLLv3_climatedaytime12")
#' data("TROLLv3_daytimevar")
#' data("TROLLv3_output")
#' TROLLv3_input_stack <- generate_parameters(
#'   cols = 100, rows = 100,
#'   iterperyear = 12, nbiter = 12 * 1
#' ) %>%
#'   mutate(simulation = list(c("seed50000", "seed500"))) %>%
#'   unnest(simulation)
#' TROLLv3_input_stack[62, 2] <- 500 # Cseedrain
#' stack(
#'   name = "teststack",
#'   simulations = c("seed50000", "seed500"),
#'   global = TROLLv3_input_stack,
#'   species = TROLLv3_species,
#'   climate = TROLLv3_climatedaytime12,
#'   daily = TROLLv3_daytimevar,
#'   load = TRUE,
#'   cores = 2,
#'   verbose = FALSE,
#'   thin = c(1, 5, 10)
#' )
#' }
#'
stack <- function(name = NULL, # nolint
                  simulations,
                  path = NULL,
                  global,
                  species,
                  climate,
                  daily,
                  lidar = NULL,
                  forest = NULL,
                  load = TRUE,
                  cores = NULL,
                  verbose = TRUE,
                  overwrite = TRUE,
                  thin = NULL) {
  # cores
  if (is.null(cores)) {
    cores <- detectCores()
    message("Detect cores was not defined, ", cores, " cores will be used.")
  }
  if ((detectCores()) < cores) {
    cores <- detectCores()
    warning(paste(
      "It seems you attributed more cores than your CPU has!
      Automatic reduction to",
      cores, "cores."
    ))
  }

  # stack name
  if (is.null(name)) {
    name <- paste0(
      "stack_",
      gsub(
        " ", "_",
        timestamp(
          prefix = "",
          suffix = "",
          quiet = TRUE
        )
      )
    )
  }

  # stack path
  tmp <- FALSE
  if (is.null(path)) {
    path <- getOption("rcontroll.tmp")
    tmp <- TRUE
  }
  if (tmp && !load) {
    stop("You can not unactivate the load option if you have not defined a path for your files.") # nolint
  }
  if (name %in% list.dirs(path, full.names = FALSE)[-1]) {
    if (!overwrite) {
      stop("Outputs already exist, use overwrite = TRUE.")
    }
    path_o <- file.path(path, name)
    unlink(path_o, recursive = TRUE)
  } else {
    path_o <- file.path(path, name)
  }
  dir.create(path_o)

  # inputs
  global <- .prep_input(global, simulations)
  species <- .prep_input(species, simulations)
  climate <- .prep_input(climate, simulations)
  daily <- .prep_input(daily, simulations)
  if (!is.null(forest)) {
    forest <- .prep_input(forest, simulations)
  } else {
    forest <- lapply(simulations, function(x) forest)
    names(forest) <- simulations
  }

  if (!is.null(lidar)) {
    lidar <- .prep_input(lidar, simulations)
  } else {
    lidar <- lapply(simulations, function(x) lidar)
    names(lidar) <- simulations
  }
  if (tmp) {
    sim_path <- lapply(simulations, function(x) path_o)
    names(sim_path) <- simulations
  } else {
    sim_path <- lapply(simulations, function(x) path_o)
    names(sim_path) <- simulations
  }

  # stack
  batches <- split(simulations, ceiling(seq_along(simulations) / cores))
  pb <- txtProgressBar(min = 0, max = length(batches), initial = 0, style = 3)
  stack_res <- list()
  for (i in seq_along(batches)) {
    j <- NULL
    cl <- makeCluster(cores, outfile = "")
    registerDoSNOW(cl)
    stack_res_batch <- foreach(
      j = seq_along(batches[[1]]),
      .export = ".troll_child"
    ) %dopar% {
      sim <- batches[[1]][j]
      .troll_child(
        name = sim,
        path = sim_path[[sim]],
        global = global[[sim]],
        species = species[[sim]],
        climate = climate[[sim]],
        daily = daily[[sim]],
        lidar = lidar[[sim]],
        forest = forest[[sim]],
        load = load,
        verbose = verbose,
        overwrite = overwrite,
        thin = thin
      )
    }
    stopCluster(cl)
    stack_res <- c(stack_res, stack_res_batch)
    setTxtProgressBar(pb, i)
    cat("\n")
  }
  close(pb)


  # loading outputs
  stack_res <- trollstack(name = name, path = path_o, mem = FALSE)
  if (load) {
    stack_res <- load_sim(stack_res)
  }
  if (tmp) {
    unlink(path_o)
    stack_res@path <- character()
  }

  return(stack_res)
}

.prep_input <- function(input, simulations) {
  simulation <- NULL

  if ("simulation" %in% colnames(input)) {
    if (!all(as.character(unique(input$simulation)) %in% simulations)) {
      stop("Simulations names in your inputs don't
           match indicated simulations names.")
    }
    input <- split(input, input$simulation)
    input <- lapply(input, select, -simulation)
  } else {
    input <- lapply(simulations, function(x) input)
    names(input) <- simulations
  }
  return(input)
}
