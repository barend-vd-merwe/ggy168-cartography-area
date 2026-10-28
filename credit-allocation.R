library(torch)
options(scipen = 999)

rounding_error <- 1.0 * (10^(-4))
measure_error <- 1.5

## variables
sb_dist <- torch_tensor(10000.0, requires_grad = TRUE)
sb_dist_km <- torch_tensor(10.0, requires_grad = TRUE)
sb_length <- torch_tensor(34.0, requires_grad = TRUE)
width_mm <- torch_tensor(19.0, requires_grad = TRUE)
length_mm_s1 <- torch_tensor(40.0, requires_grad = TRUE)
length_mm_s2 <- torch_tensor(71.0, requires_grad = TRUE)
length_mm_s3 <- torch_tensor(62.0, requires_grad = TRUE)
length_mm_s4 <- torch_tensor(33.0, requires_grad = TRUE)
c1000 <- torch_tensor(1000.0, requires_grad = TRUE)

## conversion factor (mm to km)
cf <- sb_dist_km / sb_length
cf$retain_grad()

## determine the width (km) of the strips
w <- cf * width_mm
w$retain_grad()

## determine the length (km) of each strip
l1 <- cf * length_mm_s1
l1$retain_grad()

l2 <- cf * length_mm_s2
l2$retain_grad() 

l3 <- cf * length_mm_s3
l3$retain_grad()

l4 <- cf * length_mm_s4
l4$retain_grad()

## determine the surface ara of each strip
a1 <- l1 * w
a1$retain_grad()

a2 <- l2 * w
a2$retain_grad()

a3 <- l3 * w
a3$retain_grad()

a4 <- l4 * w
a4$retain_grad()

## final answer
Y <- a1 + a2 + a3 + a4

Y$backward()

results <- data.frame(nodes = c("sb_length",
                                "width_mm",
                                "length_mm_s1",
                                "length_mm_s2",
                                "length_mm_s3",
                                "length_mm_s4",
                                "w",
                                "l1",
                                "l2",
                                "l3",
                                "l4",
                                "a1",
                                "a2",
                                "a3",
                                "a4",
                                "Y"),
                      fwd = c(as.numeric(sb_length),
                              as.numeric(width_mm),
                              as.numeric(length_mm_s1),
                              as.numeric(length_mm_s2),
                              as.numeric(length_mm_s3),
                              as.numeric(length_mm_s4),
                              as.numeric(w),
                              as.numeric(l1),
                              as.numeric(l2),
                              as.numeric(l3),
                              as.numeric(l4),
                              as.numeric(a1),
                              as.numeric(a2),
                              as.numeric(a3),
                              as.numeric(a4),
                              as.numeric(Y)),
                      part = c(abs(as.numeric(sb_length$grad)),
                               abs(as.numeric(width_mm$grad)),
                               abs(as.numeric(length_mm_s1$grad)),
                               abs(as.numeric(length_mm_s2$grad)),
                               abs(as.numeric(length_mm_s3$grad)),
                               abs(as.numeric(length_mm_s4$grad)),
                               abs(as.numeric(w$grad)),
                               abs(as.numeric(l1$grad)),
                               abs(as.numeric(l2$grad)),
                               abs(as.numeric(l3$grad)),
                               abs(as.numeric(l4$grad)),
                               abs(as.numeric(a1$grad)),
                               abs(as.numeric(a2$grad)),
                               abs(as.numeric(a3$grad)),
                               abs(as.numeric(a4$grad)),
                               1.0),
                      error = c(measure_error,
                                measure_error,
                                measure_error,
                                measure_error,
                                measure_error,
                                measure_error,
                                rounding_error,
                                rounding_error,
                                rounding_error,
                                rounding_error,
                                rounding_error,
                                rounding_error,
                                rounding_error,
                                rounding_error,
                                rounding_error,
                                rounding_error))

fc <- results$error %*% results$part
pc <- 2 * fc
Y
fc
pc

## calculate localised error
if (!is.null(sb_length$grad)) sb_length$grad <- NULL
if (!is.null(width_mm$grad)) width_mm$grad <- NULL
if (!is.null(sb_dist_km$grad)) sb_dist$grad <- NULL
if (!is.null(length_mm_s1$grad)) length_mm_s1$grad <- NULL
if (!is.null(length_mm_s2$grad)) length_mm_s1$grad <- NULL
if (!is.null(length_mm_s3$grad)) length_mm_s1$grad <- NULL
if (!is.null(length_mm_s4$grad)) length_mm_s1$grad <- NULL

n1 <- sb_dist_km / sb_length
n1$retain_grad()

Y <- length_mm_s4 * n1

Y$backward()

results <- data.frame(nodes = c("sb_length",
                                "length_mm_s43",
                                "n1",
                                "Y"),
                      fwd = c(as.numeric(sb_length), 
                              as.numeric(length_mm_s4),
                              as.numeric(n1),
                              as.numeric(Y)),
                      part = c(abs(as.numeric(sb_length$grad)),  
                               abs(as.numeric(length_mm_s4$grad)), 
                               abs(as.numeric(n1$grad)),
                               1.0),
                      error = c(measure_error,
                                measure_error,
                                rounding_error,
                                rounding_error))

fc <- results$error %*% results$part
pc <- 2 * fc
Y
fc
pc

## calculate error for individual strip areas
if (!is.null(cf_km$grad)) cf_km$grad <- NULL
if (!is.null(w$grad)) w$grad <- NULL
if (!is.null(l$grad)) l$grad <- NULL
if (!is.null(sb_length$grad)) sb_length$grad <- NULL
if (!is.null(width_mm$grad)) width_mm$grad <- NULL
if (!is.null(sb_dist_km$grad)) sb_dist$grad <- NULL
if (!is.null(length_mm_s1$grad)) length_mm_s1$grad <- NULL
if (!is.null(length_mm_s2$grad)) length_mm_s1$grad <- NULL
if (!is.null(length_mm_s3$grad)) length_mm_s1$grad <- NULL
if (!is.null(length_mm_s4$grad)) length_mm_s1$grad <- NULL

cf_km <- sb_dist_km / sb_length
cf_km$retain_grad()

w <- cf_km * width_mm
w$retain_grad()

l <- length_mm_s4 * cf_km
l$retain_grad()

Y <- w * l

Y$backward()

results <- data.frame(nodes = c("sb_length",
                                "width_mm",
                                "w",
                                "strip_length",
                                "l",
                                "Y"),
                      fwd = c(as.numeric(sb_length),
                              as.numeric(width_mm),
                              as.numeric(w),
                              as.numeric(length_mm_s4),
                              as.numeric(l),
                              as.numeric(Y)),
                      part = c(abs(as.numeric(sb_length$grad)),
                               abs(as.numeric(width_mm$grad)),
                               abs(as.numeric(w$grad)),
                               abs(as.numeric(length_mm_s4$grad)),
                               abs(as.numeric(l$grad)),
                               1.0),
                      error = c(measure_error,
                                measure_error,
                                rounding_error,
                                measure_error,
                                rounding_error,
                                rounding_error))

fc <- results$error %*% results$part
pc <- fc * 2
Y
fc
pc
