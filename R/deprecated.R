deprecate_msg <- function(cl, func_str) {
  # re-write call to switch arguments for deprecated versions
  cl[[1]] <- rlang::sym(func_str)
  pp <- cl[[2]]
  cl[[2]] <- cl[["model"]]
  cl[["model"]] <- pp
  names(cl)[1:3] <- ""
  paste0(
    "The first argument to `",
    func_str,
    "()` should be either a model or a ",
    "workflow. In the future, you can use:\n",
    rlang::expr_text(cl)
  )
}
