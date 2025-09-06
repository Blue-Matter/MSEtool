# Set up parallel processing 
# if (parallel & !snowfall::sfIsRunning())
#   setup()
# ncpus <- set_parallel(any(unlist(parallel)))

# Set pbapply functions
# .lapply <- define.lapply(silent)
# .sapply <- define.sapply(silent)

# NOTE: future appears a lot slower 
# if (parallel) {
#   ncores <- parallelly::availableCores(logical=FALSE)
#   future::plan('multisession', workers=ncores)
# } else {
#   future::plan()
# }


# Set up parallel processing 
if (parallel & !snowfall::sfIsRunning())
  setup()
ncpus <- set_parallel(any(unlist(parallel)))

# Set pbapply functions 
.lapply <- define.lapply(silent) 
.sapply <- define.sapply(silent)