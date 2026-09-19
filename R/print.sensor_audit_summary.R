#' Print a sensor audit summary
#'
#' @param x An object of class `sensor_audit_summary`.
#' @param ... Additional arguments.
#'
#' @return Invisibly returns the summary object.
#'
#' @export
print.sensor_audit_summary <- function(x, ...) {

  cat("Sensor Leakage Audit Summary\n")
  cat("============================\n\n")

  cat(
    "Temporal findings: ",
    x$temporal_findings,
    "\n",
    sep = ""
  )

  cat(
    "Shared sensors:    ",
    x$shared_sensors,
    "\n",
    sep = ""
  )

  cat(
    "Spatial pairs:     ",
    x$spatial_pairs,
    "\n",
    sep = ""
  )

  invisible(x)
}
