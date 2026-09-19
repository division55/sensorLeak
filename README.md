
# sensorLeak

<!-- badges: start -->

[![R-CMD-check](https://github.com/division55/sensorLeak/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/division55/sensorLeak/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`sensorLeak` provides tools for identifying potential validation and
information leakage risks in machine learning workflows using
environmental sensor data.

The package focuses on leakage risks that can arise from the way
environmental observations are divided into training and testing data,
including temporal ordering, shared sensors, and spatial proximity.

## Installation

You can install the development version of `sensorLeak` from GitHub:

``` r
# install.packages("pak")
pak::pak("division55/sensorLeak")
```

## Example

Create a simple environmental sensor dataset:

``` r
library(sensorLeak)

sensor_data <- data.frame(
  timestamp = as.POSIXct(c(
    "2026-01-01 10:00:00",
    "2026-01-01 12:00:00",
    "2026-01-01 14:00:00",
    "2026-01-01 16:00:00"
  )),
  sensor_id = c("S1", "S1", "S1", "S2"),
  latitude = c(
    13.0827,
    13.0830,
    13.0832,
    13.3000
  ),
  longitude = c(
    80.2707,
    80.2710,
    80.2712,
    80.4000
  ),
  split = c(
    "train",
    "train",
    "test",
    "test"
  )
)
```

Run the audit:

``` r
audit <- sensor_audit(
  sensor_data,
  time = "timestamp",
  sensor = "sensor_id",
  split = "split",
  lat = "latitude",
  lon = "longitude"
)
```

Print the audit:

``` r
audit
```

Summarize the results:

``` r
summary(audit)
```

## Leakage checks

`sensorLeak` currently provides four diagnostic functions:

### Temporal leakage

``` r
check_temporal_leakage(
  sensor_data,
  time = "timestamp",
  split = "split"
)
```

Identifies test observations that occur at or before the end of the
training period.

### Sensor-group overlap

``` r
check_sensor_leakage(
  sensor_data,
  sensor = "sensor_id",
  split = "split"
)
```

Identifies sensors that occur in both training and testing data.

This should be interpreted according to the intended validation
strategy. Sharing a sensor between training and testing data is not
automatically invalid; it can be appropriate when the model is intended
to predict future observations from known sensors.

### Spatial proximity

``` r
check_spatial_leakage(
  sensor_data,
  lat = "latitude",
  lon = "longitude",
  split = "split",
  threshold = 1
)
```

Identifies training/testing observation pairs that are within the
specified geographic distance in kilometres.

Spatial proximity may represent a validation risk when the intended goal
is geographic generalization.

### Overlapping temporal windows

``` r
check_window_leakage(
  data,
  start = "window_start",
  end = "window_end",
  split = "split",
  sensor = "sensor_id"
)
```

Identifies temporal windows that overlap between training and testing
data for the same sensor.

## Important interpretation

`sensorLeak` reports **potential validation and information leakage
risks**. A detected condition is not necessarily an error.

Whether a condition represents leakage depends on the scientific
question and the intended generalization target of the machine learning
workflow.

For example, a model intended to predict future measurements from known
sensors may legitimately contain the same sensors in both training and
testing data.

## Related packages

`sensorLeak` is designed as a diagnostic tool rather than a replacement
for packages that construct spatial or temporal validation schemes.

Users may also consider packages such as `blockCV` and `CAST` when
constructing spatial or space-time cross-validation schemes.

## Development

This package is under active development.

Run the test suite with:

``` r
devtools::test()
```

Run package checks with:

``` r
devtools::check()
```

## License

MIT
