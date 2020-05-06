doubling_time <- function(N0, d0, ts) {
  N0 * 2 ^ (ts / d0)
}