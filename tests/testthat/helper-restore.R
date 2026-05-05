with_real_function <- function(name, code) {
  ns <- asNamespace("dauPortalTools")
  original <- get(name, ns)

  local({
    assignInNamespace(name, original, ns)
    on.exit(assignInNamespace(name, original, ns), add = TRUE)
    force(code)
  })
}
