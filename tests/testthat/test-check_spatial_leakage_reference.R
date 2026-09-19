# Independent reference: the original pair-by-pair haversine calculation.
# It is deliberately simple and slow, and lives only in this test file.
reference_spatial_pairs <- function(lat, lon, split, threshold) {

  haversine <- function(lat1, lon1, lat2, lon2) {
    lat1 <- lat1 * pi / 180
    lat2 <- lat2 * pi / 180
    dlat <- lat2 - lat1
    dlon <- (lon2 - lon1) * pi / 180
    a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
    2 * 6371 * asin(sqrt(a))
  }

  out <- list()

  for (i in which(split == "train")) {
    for (j in which(split == "test")) {
      d <- haversine(lat[i], lon[i], lat[j], lon[j])
      if (d <= threshold) {
        out[[length(out) + 1]] <- data.frame(
          train_row = i,
          test_row = j,
          distance_km = d
        )
      }
    }
  }

  if (length(out) == 0) {
    return(data.frame(
      train_row = integer(),
      test_row = integer(),
      distance_km = numeric()
    ))
  }

  do.call(rbind, out)
}

random_sensor_data <- function(n) {
  data.frame(
    latitude = runif(n, -80, 80),
    longitude = runif(n, -170, 170),
    split = rep(c("train", "test"), length.out = n)
  )
}

test_that("results match a brute-force reference calculation", {

  set.seed(123)

  for (threshold in c(0, 1, 500, 5000)) {

    data <- random_sensor_data(80)

    expected <- reference_spatial_pairs(
      data$latitude, data$longitude, data$split, threshold
    )

    result <- check_spatial_leakage(
      data,
      lat = "latitude",
      lon = "longitude",
      split = "split",
      threshold = threshold
    )

    expect_equal(result, expected)
  }
})

test_that("pairs are ordered by train row and then test row", {

  set.seed(456)
  data <- random_sensor_data(60)

  result <- check_spatial_leakage(
    data,
    lat = "latitude",
    lon = "longitude",
    split = "split",
    threshold = 5000
  )

  expect_gt(nrow(result), 1)
  expect_identical(
    order(result$train_row, result$test_row),
    seq_len(nrow(result))
  )
})

test_that("processing in several blocks gives the same result as one block", {

  set.seed(789)
  data <- random_sensor_data(90)

  train_rows <- which(data$split == "train")
  test_rows <- which(data$split == "test")

  one_block <- spatial_pairs_within(
    data$latitude, data$longitude, train_rows, test_rows,
    threshold = 3000
  )

  many_blocks <- spatial_pairs_within(
    data$latitude, data$longitude, train_rows, test_rows,
    threshold = 3000,
    max_cells = 50
  )

  expect_gt(nrow(one_block), 0)
  expect_equal(many_blocks, one_block)
})

test_that("distances are computed in kilometres", {

  data <- data.frame(
    latitude = c(0, 1),
    longitude = c(0, 0),
    split = c("train", "test")
  )

  result <- check_spatial_leakage(
    data,
    lat = "latitude",
    lon = "longitude",
    split = "split",
    threshold = 200
  )

  # One degree of latitude is about 111.19 km on a 6371 km sphere.
  expect_equal(result$distance_km, 6371 * pi / 180, tolerance = 1e-8)
})

test_that("a pair exactly at the threshold is included", {

  data <- data.frame(
    latitude = c(13.0827, 13.2),
    longitude = c(80.2707, 80.3),
    split = c("train", "test")
  )

  distance <- check_spatial_leakage(
    data,
    lat = "latitude",
    lon = "longitude",
    split = "split",
    threshold = 1000
  )$distance_km

  at_threshold <- check_spatial_leakage(
    data,
    lat = "latitude",
    lon = "longitude",
    split = "split",
    threshold = distance
  )

  expect_equal(nrow(at_threshold), 1)
})

test_that("missing train or test observations return an empty result", {

  only_train <- data.frame(
    latitude = c(13.08, 13.09),
    longitude = c(80.27, 80.28),
    split = c("train", "train")
  )

  only_test <- only_train
  only_test$split <- "test"

  for (data in list(only_train, only_test)) {

    result <- check_spatial_leakage(
      data,
      lat = "latitude",
      lon = "longitude",
      split = "split"
    )

    expect_equal(nrow(result), 0)
    expect_named(result, c("train_row", "test_row", "distance_km"))
  }
})
