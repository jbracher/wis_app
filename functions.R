read_week_ahead <- function(path){
  dat <- read.csv(path, colClasses = c(location = "character", forecast_date = "Date", target_end_date = "Date"))
  return(subset(dat, grepl("wk ahead cum", target)))
}

# extract the date from a file name in our standardized format
get_date_from_filename <- function(filename){
  as.Date(substr(filename, start = 1, stop = 10))
}

# get the date of the next Monday following after a given date
next_monday <- function(date){
  nm <- rep(NA, length(date))
  for(i in seq_along(date)){
    nm[i] <- date[i] + (0:6)[weekdays(date[i] + (0:6)) == "Monday"]
  }
  return(as.Date(nm, origin = "1970-01-01"))
}

# among a set of forecast dates: choose those which are Mondays and those which are Sundays,
# Saturdays or Fridays if no forecast is available from Monday (or a day closer to Monday)
choose_relevant_dates <- function(dates){
  wds <- weekdays(dates)
  next_mondays <- next_monday(dates)
  relevant_dates <- c()
  for(day in c("Monday", "Sunday", "Saturday", "Friday")){
    relevant_dates <- c(relevant_dates, dates[wds == day &
                                                !(next_mondays %in% relevant_dates) &
                                                !((next_mondays - 1) %in% relevant_dates) &
                                                !((next_mondays - 2) %in% relevant_dates)
                                              ])
  }
  relevant_dates <- as.Date(relevant_dates, origin = "1970-01-01")
  return(as.Date(relevant_dates, origin = "1970-01-01"))
}

# evaluate WIS for a data.frame containing forecasts:
# THIS NEEDS TO BE RE-CHECKED
wis_table <- function(forecasts, truth){
  forecasts <- forecasts[forecasts$type == "quantile", ]
  forecasts_wide <- reshape(forecasts, direction = "wide", timevar = "quantile",
                            v.names = "value", idvar = c("location", "target_end_date", "target"))
  # add truths:
  colnames(truth)[colnames(truth) == "value"] <- "truth"
  forecasts_wide <- merge(forecasts_wide, truth[, c("date", "location", "truth")],
                          by.x = c("target_end_date", "location"),
                          by.y = c("date", "location"))

  coverage_levels <- c(0:9/10, 0.95, 0.98) # median can be treated like the 0% PI

  # get weighted interval widths. Note that this already contains the weighting with alpha/2
  for(coverage in coverage_levels){
    forecasts_wide[, paste0("wgt_iw_", coverage)] <-
      (1 - coverage)/2*(
        forecasts_wide[paste0("value.", 1 - (1 - coverage)/2)] -
          forecasts_wide[paste0("value.", (1 - coverage)/2)]
      )
  }

  # get weighted penalties. Note that this already contains the weighting with alpha/2,
  # which makes the terms simpler
  for(coverage in coverage_levels){
    q_u <- 1 - (1 - coverage)/2
    forecasts_wide[, paste0("wgt_pen_u_", coverage)] <-
      pmax(0, forecasts_wide$truth - forecasts_wide[, paste0("value.", q_u)])

    q_l <- (1 - coverage)/2
    forecasts_wide[, paste0("wgt_pen_l_", coverage)] <-
      pmax(0, forecasts_wide[, paste0("value.", q_l)] - forecasts_wide$truth)
  }

  forecasts_wide$wgt_iw <- rowMeans(forecasts_wide[, grepl("wgt_iw", colnames(forecasts_wide))])
  forecasts_wide$wgt_pen_u <- rowMeans(forecasts_wide[, grepl("wgt_pen_u", colnames(forecasts_wide))])
  forecasts_wide$wgt_pen_l <- rowMeans(forecasts_wide[, grepl("wgt_pen_l", colnames(forecasts_wide))])
  forecasts_wide$wis <- forecasts_wide$wgt_iw + forecasts_wide$wgt_pen_u + forecasts_wide$wgt_pen_l

  return(forecasts_wide)
}

ae_table <- function(forecasts, truth){
  forecasts <- forecasts[forecasts$type == "point", ]
  colnames(truth)[colnames(truth) == "value"] <- "truth"
  forecasts <- merge(forecasts, truth[, c("date", "truth", "location")],
                     by.x = c("target_end_date", "location"),
                     by.y = c("date", "location"))
  forecasts$ae <- abs(forecasts$value - forecasts$truth)
  return(forecasts)
}

# create an empty plot to which forecasts can be added:
empty_plot <- function(xlim, ylim, xlab, ylab){
  plot(NULL, xlim = xlim, ylim = ylim,
       xlab = xlab, ylab = "", axes = FALSE)
  axis(2, las = 1)
  title(ylab = ylab, line = 4)
  all_dates <- seq(from = as.Date("2020-02-01"), to = Sys.Date() + 28, by  =1)
  saturdays <- all_dates[weekdays(all_dates) == "Saturday"]
  axis(1, at = saturdays, labels = as.Date(saturdays, origin = "1970-01-01"))
  box()
}

# add a single prediction interval:
draw_prediction_band <- function(forecasts, target, coverage, col = "lightgrey"){
  if(!coverage %in% c(1:9/10, 0.95, 0.98)) stop("Coverage needs to be from 0.1, 0.2, ..., 0.9, 0.95, 0.98")
  forecasts <- forecasts[forecasts$target == target & forecasts$type == "quantile", ]
  # select points to draw polygon:
  lower <- subset(forecasts, abs(quantile - (1 - coverage)/2) < 0.01)
  lower <- lower[order(lower$target_end_date), ]
  upper <- subset(forecasts, abs(quantile - (1 - (1 - coverage)/2)) < 0.01)
  upper <- upper[order(upper$target_end_date, decreasing = TRUE), ]
  # draw:
  polygon(x = c(lower$target_end_date, upper$target_end_date),
          y = c(lower$value, upper$value), col = col, border = NA)
}

# draw many prediction intervals (resulting in a fanplot)
draw_fanplot <- function(forecasts, target, cols = colorRampPalette(c("deepskyblue4", "lightgrey"))(12)[-1]){
  levels_coverage <- c(1:9/10, 0.95, 0.98)
  for(i in rev(seq_along(levels_coverage))){
    draw_prediction_band(forecasts = forecasts,
                         target = target,
                         coverage = levels_coverage[i],
                         col = cols[i])
  }
}

# add points for point forecasts:
draw_points <- function(forecasts, target, col = "deepskyblue4"){
  forecasts <- forecasts[forecasts$target == target & forecasts$type == "point", ]
  forecasts <- forecasts[order(forecasts$target_end_date), ]
  points(forecasts$target_end_date, forecasts$value, pch = 21, col = col, bg = "white")
}

# add smaller points for truths:
draw_truths <- function(truth){
  truth <- subset(truth, weekdays(truth$date) == "Saturday")
  points(truth$date, truth$value, pch = 20, type = "b")
}

# wrap it all up into one plotting function:
plot_forecast <- function(forecasts, target, truth, xlim = c(as.Date("2020-04-01"), Sys.Date() + 28), ylim){
  empty_plot(xlim = xlim, ylim = ylim, xlab = "target end date", ylab = "cumulative deaths")
  draw_fanplot(forecasts = forecasts, target = target)
  draw_points(forecasts = forecasts, target = target)
  draw_truths(truth = truth)
}

# plotting function for result of wis_table and ae_table:
plot_scores <- function(wis_tab, ae_tab, target, xlim = c(as.Date("2020-04-01"), Sys.Date() + 28), ylim = c(0, 5000)){

  # subset to chosen target:
  wis_tab <- wis_tab[wis_tab$target == target, ]
  ae_tab <- ae_tab[ae_tab$target == target, ]

  empty_plot(xlab = "target end date", xlim = xlim, ylim = ylim, ylab = "WIS")

  lines(wis_tab$target_end_date, wis_tab$wis, type = "h", col = "red", lwd = 2)
  lines(wis_tab$target_end_date, wis_tab$wgt_pen_l + wis_tab$wgt_iw, type = "h", col = "royalblue", lwd = 2)
  lines(wis_tab$target_end_date[wis_tab$wgt_pen_l > 0],
        wis_tab$wgt_pen_l[wis_tab$wgt_pen_l > 0], type = "h", col = "orange", lwd = 2)
  # points(wis_tab$target_end_date, wis_tab$wis, pch = 1, lwd = 2)

  points(ae_tab$target_end_date, ae_tab$ae, pch = 5)

  legend("topright", legend = c("penalty for underprediction",
                                "forecast dispersion",
                                "penalty for overprediction",
                                "absolute error"),
         col = c("red", "royalblue1", "orange", "black"), lwd = c(2, 2, 2, NA, NA),
         bty = "n", cex = 0.9, pch = c(NA, NA, NA, 5))
}

# plotting function to display average scores:
plot_average_scores <- function(wis_tab, ae_tab, target, ylim = c(0, 5000)){

  # subset to chosen target:
  wis_tab <- wis_tab[wis_tab$target == target, ]
  ae_tab <- ae_tab[ae_tab$target == target, ]

  plot(NULL, xlim = c(0, 2), ylim = ylim, axes = FALSE, xlab = "", ylab = "average WIS")
  lines(1, mean(wis_tab$wis, na.rm = TRUE), type = "h", col = "red", lwd = 2)
  lines(1, mean(wis_tab$wgt_pen_l + wis_tab$wgt_iw, na.rm = TRUE), type = "h", col = "royalblue", lwd = 2)
  lines(1, mean(wis_tab$wgt_pen_l, na.rm = TRUE), type = "h", col = "orange", lwd = 2)
  # points(1, mean(wis_tab$wis), pch = 1, lwd = 2)

  points(1, mean(ae_tab$ae, na.rm = TRUE), pch = 5)

  axis(2); box()
}

# plotting function for pit_histogram:
pit_hist <- function(wis_tab, target){

  # subset to chosen target:
  wis_tab <- wis_tab[wis_tab$target == target, ]

  pit_middles <- seq(from = 0.025, to = 0.975, by = 0.05)
  approx_pit <- numeric(nrow(wis_tab))
  for(i in 1:nrow(wis_tab)){
    quantiles <- c(0, wis_tab[i, paste0("value.", 1:19/20)], 100000000)
    quantiles <- unlist(quantiles) + runif(length(quantiles), -0.01, 0.01) # slightly perturb to avoid ties
    approx_pit[i] <- cut(wis_tab$truth[i], breaks = quantiles)
  }
  hist(pit_middles[approx_pit], breaks = 0:10/10, probability = TRUE, xlab = "PIT", main = "")
  box()
  abline(h = 1, lty = "dashed")
  return(invisible(pit_middles[approx_pit]))
}

# plotting function for plot showing coverage of different PIs
coverage_plot <- function(wis_tab, target){
  # subset to chosen target:
  wis_tab <- wis_tab[wis_tab$target == target, ]

  coverage_levels <- c(0:9/10, 0.95, 0.98)
  emp_coverage <- numeric(length(coverage_levels))
  for(i in seq(coverage_levels)){
    emp_coverage[i] <-
      mean(
        wis_tab$truth < wis_tab[, paste0("value.", 1 - (1 - coverage_levels[i])/2)] &
          wis_tab$truth > wis_tab[, paste0("value.", (1 - coverage_levels[i])/2)],
        na.rm = TRUE
      )
  }
  plot(coverage_levels, emp_coverage, type = "l", xlab = "nominal coverage of PI",
       ylab = "empirical coverage", xlim = 0:1, ylim = 0:1)
  abline(0:1, lty = 2)
}
