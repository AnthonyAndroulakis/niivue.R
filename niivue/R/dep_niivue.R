dep_niivue <- function() {
  # Create the niivue dependency
  niivue_dependency <- htmltools::htmlDependency(
    name = "niivue",
    version = "0.1.0",
    src = c(file = "htmlwidgets"),
    script = c("niivue.umd.js", "niivue.js")
  )

  return(niivue_dependency)
}