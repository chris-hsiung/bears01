#' Copy Variables from One Environment to the Global Environment
#'
#' This function copies all variables from a specified source environment 
#' to the global environment.
#'
#' @param source_env An environment from which to copy variables. 
#' This should be a valid R environment.
#'
#' @return Invisible NULL. The function is called for its side effect 
#' of copying variables to the global environment.
#' @export
#'
#' @examples
#' env_example <- new.env()
#' env_example$a <- 1
#' env_example$b <- 2
#' copy_to_global(env_example)
#' print(a)  # Should print 1
#' print(b)  # Should print 2
copy_to_global <- function(source_env) {
  
  # Assertion to check if the input is an environment
  if (!is.environment(source_env)) {
    stop("source_env must be an environment")
  }
  
  var_names <- ls(envir = source_env)
  for (var in var_names) {
    assign(var, get(var, envir = source_env), envir = .GlobalEnv)
  }
  
  invisible(NULL)
}
