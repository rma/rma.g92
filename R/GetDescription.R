#
# Returns the description associated with some value.
#
# Args:
#   descs:   The table of descriptions (eg, "guyton.descs").
#   name:    The name of the value.
#   null.rm: Whether to an empty string (default) or NULL if no description
#            is found.
#
# Returns:
#   The description associated with the value.
#
# Example:
#   data(guyton.descs)
#   print(GetDescription(guyton.descs, "v_PA"))
#
GetDescription <- function(descs, name, null.rm=TRUE) {
  prefix = substr(name, 1, 2)
  if (prefix == "v_" || prefix == "p_") {
    name <- substring(name, 3)
  }

  desc <- descs[[name]]

  if (null.rm && is.null(desc)) {
    return("")
  } else {
    return(desc)
  }
}
