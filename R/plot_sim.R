
#' Simple plotting functions
#'
#' @description These functions are simple plotting helpers to get some quick
#' visuals of values produced by \code{\link{sim_abundance}},
#' \code{\link{sim_distribution}}, etc.
#'
#' @param sim            Object returned by \code{\link{sim_abundance}},
#'                       \code{\link{sim_distribution}}, etc.
#' @param mat            Name of matrix in \code{sim} list to plot.
#' @param grid           Grid produced by \code{\link{make_grid}}.
#' @param xlab,ylab,zlab Axes labels.
#' @param sum_ages       Sum across these ages
#' @param ages           Subset data to one or more ages.
#' @param lengths        Subset data to one or more length groups.
#' @param years          Subset data to one or more years.
#' @param type           Plot type: "contour" or "heatmap".
#' @param scale          Plot response on "natural" or "log" scale?
#' @param which_year     Subset to specific year
#' @param which_sim      Subset to specific sim
#' @param select_by      Select plot by "age", "length" or "year"?
#' @param plot_by        Plot error surface by "rule" or "samples"?
#' @param surveys        Subset data to one or more surveys.
#' @param quants         Quantile intervals to display on fan plot
#' @param col            Plot color
#' @param which_strat    Which strat values to focus on? (total, length, or age)
#' @param ...            Additional arguments to pass to \code{\link[plotly]{plot_ly}}.
#'
#' @import plotly
#' @importFrom rlang .data
#'
#' @return Returns a plot of class \code{plotly}.
#'
#' @export
#' @rdname plot_trend
plot_trend <- function(sim, sum_ages = sim$ages, col = viridis::viridis(1), ...) {
  if (length(sum_ages) == 1) {
    n <- sim$N[sum_ages, ]
  } else {
    n <- colSums(sim$N[sum_ages, ])
  }
  tot <- data.frame(Year = sim$years, N = n)
  tot$text <- paste0("Year: ", tot$Year, "\nN: ", round(tot$N))
  plot_ly(x = ~Year, y = ~N, text = ~text, hoverinfo = "text",
          type = "scatter", mode = "lines", color = I(col), data = tot, ...)
}

#' @export
#' @rdname plot_trend
plot_surface <- function(sim, mat = "N", xlab = "Age", ylab = "Year", zlab = mat, ...) {
  plot_ly(x = sim$ages, y = sim$years, z = t(sim[[mat]]), type = "surface",
          ...) %>%
    layout(
      scene = list(
        xaxis = list(title = xlab),
        yaxis = list(title = ylab),
        zaxis = list(title = zlab)
      ))
}


#' @export
#' @rdname plot_trend
plot_grid <- function(grid, ...) {

  xax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE,
    ticks = ""
  )
  yax <- c(scaleanchor = "x", xax)

  sf_div <- sf::st_as_sf(grid["division"], as_points = FALSE, merge = TRUE)
  sf_strat <- sf::st_as_sf(grid["strat"], as_points = FALSE, merge = TRUE)
  xyz <- data.frame(grid)

  plot_ly(...) %>%
    add_trace(data = xyz, x = ~x, y = ~y, z = ~depth,
              hoverinfo = "none", type = "heatmap") %>%
    add_sf(data = sf_strat, color = I(NA), stroke = I("white"), span = I(1),
           hoverinfo = "none", showlegend = FALSE) %>%
    add_sf(data = sf_div, color = I(NA), stroke = I("darkgrey"), span = I(4),
           hoverinfo = "none", showlegend = FALSE) %>%
    add_markers(data = xyz, x = ~x, y = ~y, color = I("white"), size = I(0.1),
                text = ~paste("x:", x, "<br>y:", y, "<br>depth:", depth, "<br>cell:", cell,
                              "<br>division:", division, "<br>strat:", strat),
                hoverinfo = "text", type = "heatmap", showlegend = FALSE) %>%
    layout(xaxis = xax, yaxis = yax)

}

#' @export
#' @rdname plot_trend
plot_distribution <- function(sim, ages = sim$ages, years = sim$years,
                              type = "contour", scale = "natural", ...) {

  age <- NULL

  xax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE,
    ticks = ""
  )
  yax <- c(scaleanchor = "x", xax)

  d <- merge(sim$grid_xy, sim$sp_N[age %in% ages & year %in% years, ], by = "cell")
  if (scale == "log") d$N <- log(d$N)
  d$ay <- paste(d$age, d$year, sep = "-")
  split_d <- split(d, d$ay)
  split_d <- lapply(split_d, function(.) {
    stats::xtabs(N ~ y + x, data = ., subset = NULL)
  })
  x <- sort(unique(d$x))
  y <- sort(unique(d$y))

  ## sort the splits
  ay_combos <- expand.grid(age = sort(unique(d$age)), year = sort(unique(d$year)))
  ay_combos$ay <- paste(ay_combos$age, ay_combos$year, sep = "-")
  split_d <- split_d[ay_combos$ay]

  p <- plot_ly(x = ~x, y = ~y, ...)
  steps <- list()
  for (i in seq_along(split_d)) {
    vis <- rep(FALSE, length(split_d))
    vis[i] <- TRUE
    z <- split_d[[i]]
    attr(z, "class") <- attr(z, "call") <- NULL # plotly didn't like the xtabs attributes
    dimnames(z) <- NULL
    p <- p %>% add_trace(type = type,
                         z = z,
                         visible = i == 1,
                         showscale = i == 1,
                         name = names(split_d)[i],
                         colorbar = list(title = "N")) %>%
      layout(xaxis = xax, yaxis = yax)
    steps[[i]] <- list(args = list(list(visible = vis,
                                        showscale = vis)),
                       method = "update",
                       label = names(split_d)[i])
  }

  if (length(ages) > 1 | length(years) > 1) {
    p <- p %>%
      layout(sliders = list(list(
        currentvalue = list(prefix = "Age-Year: "),
        steps = steps
      )))
  }

  p

}


## little helper for scaling the bubbles (using plotly's scale results in a warning)
.scale_between <- function(x, new_min = 0, new_max = 1) {
  (x - min(x)) / (max(x) - min(x)) * (new_max - new_min) + new_min
}

#' @export
#' @rdname plot_trend
plot_survey <- function(sim, which_year = 1, which_sim = 1) {

  xax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE,
    ticks = ""
  )
  yax <- c(scaleanchor = "x", xax)

  sf_strat <- sf::st_as_sf(sim$grid["strat"], as_points = FALSE, merge = TRUE)

  setdet <- sim$setdet
  setdet <- setdet[setdet$year == which_year & setdet$sim == which_sim, ]
  samp <- sim$samp
  samp <- samp[samp$set %in% setdet$set, ]
  samp <- merge(setdet[, c("year", "sim", "set", "x", "y")],
                samp, by = "set", all = TRUE)

  l <- sim$I_at_length[, as.character(which_year)]
  l <- (l / sum(l)) * 100
  true_length <- data.frame(length = as.numeric(names(l)), percent = l)
  a <- sim$I[, as.character(which_year)]
  a <- (a / sum(a)) * 100
  true_age <- data.frame(age = as.numeric(names(a)), percent = a)

  ## impose the same length grouping on the data as chosen for the abundance
  ## at length grouping
  length_group <- get("length_group", envir = environment(sim$sim_length))

  d <- crosstalk::SharedData$new(samp, ~set)

  base <- plot_ly(data = d)

  sp_p <- base %>%
    group_by(set) %>%
    summarise(x = unique(.data$x), y = unique(.data$y), n = sum(!is.na(.data$measured))) %>%
    add_markers(x = ~x, y = ~y, text = ~n,
                color = ~n, name = "n",
                showlegend = FALSE,
                marker = list(size = ~.scale_between(n, 2, 600),
                              sizemode = "area")) %>%
    add_sf(data = sf_strat, color = I(NA), stroke = I("black"),
           hoverinfo = "none", span = I(1), showlegend = FALSE, alpha = 0.1) %>%
    layout(xaxis = xax, yaxis = yax,
           margin = list(t = 0, r = 0, l = 0, b = 0, pad = 0))

  hist_base <- base %>%
    mutate(length = group_lengths(length, length_group)) %>%
    group_by(set) %>%
    filter(!is.na(.data$measured)) %>%
    slice(rep(1:length(.data$measured), each = 2)) %>%
    mutate(lab = rep(c("caught", "sampled"), times = length(.data$measured) / 2))

  lf_p <- hist_base %>%
    filter(.data$measured & .data$lab == "sampled" | .data$lab == "caught") %>%
    add_histogram(x = ~length, color = ~lab, histnorm = "percent",
                  colors = c("#FDE725FF", "#21908CFF"),
                  legendgroup = ~lab) %>%
    add_lines(data = true_length, x = ~length, y = ~percent, color = I("#440154FF"),
              fill = "tozeroy", name = "true", fillcolor = "#44015433",
              legendgroup = "true") %>%
    layout(xaxis = list(title = "Length",
                        range = grDevices::extendrange(range(samp$length, na.rm = TRUE))),
           yaxis = list(title = "Percent", ticksuffix = "%"))

  af_p <- hist_base %>%
    filter(.data$aged & .data$lab == "sampled" | .data$lab == "caught") %>%
    add_histogram(x = ~age, color = ~lab, histnorm = "percent",
                  colors = c("#FDE725FF", "#21908CFF"),
                  legendgroup = ~lab, showlegend = FALSE) %>%
    add_lines(data = true_age, x = ~age, y = ~percent, color = I("#440154FF"),
              fill = "tozeroy", name = "true", fillcolor = "#44015433",
              legendgroup = "true", showlegend = FALSE) %>%
    layout(xaxis = list(title = "Age",
                        range = grDevices::extendrange(range(samp$age, na.rm = TRUE))),
           yaxis = list(title = "Percent", ticksuffix = "%"))

  subplot(sp_p,
          subplot(lf_p, af_p, nrows = 2, titleX = TRUE, titleY = TRUE,
                  margin = 0.1),
          nrows = 1, margin = c(0, 0.2, 0, 0), widths = c(0.75, 0.25),
          titleX = TRUE, titleY = TRUE) %>%
    colorbar(x = 0.52, y = 1) %>%
    layout(legend = list(x = 1, y = 0.95))

}




## helper function for calculating multiple quantiles by group
.ints <- function(d, quants = seq(90, 10, by = -10), by = c("year", "survey")) {
  I_hat <- NULL
  ints <- lapply(quants, function(q) {
    d[, list(prob = paste0(q, "%"),
             lower = stats::quantile(I_hat, prob = (1 - q / 100) / 2),
             upper = stats::quantile(I_hat, prob = 1 - (1 - q / 100) / 2)),
      by = by]
  })
  ints <- data.table::rbindlist(ints)
  ints$prob <- factor(ints$prob, levels = paste0(quants, "%"))
  ints
}

## helper function for making fan plots
.fan <- function(d, x = NULL, xlab = NULL, cols = NULL, ylim = NULL, ...) {
  plot_ly(data = d, x = x) %>%
    add_ribbons(ymin = ~lower, ymax = ~upper, line = list(width = 0),
                color = ~prob, colors = cols,
                showlegend = FALSE) %>%
    add_lines(y = ~I, color = I("black"), name = "True",
              showlegend = FALSE) %>%
    layout(yaxis = list(title = "Abundance index",
                        range = ylim),
           xaxis = list(title = xlab), ...)
}



#' @export
#' @rdname plot_trend
plot_total_strat_fan <- function(sim, surveys = 1:5,
                                 quants = seq(90, 10, by = -10),
                                 ...) {

  survey <- set_den <- NULL

  d <- sim$total_strat_error
  sub_d <- d[survey %in% surveys, ]
  sub_d <- merge(sim$surveys, sub_d, by = "survey", all.y = TRUE)
  true <- unique(sub_d[sim == 1, list(year, set_den, I)])

  ## Calculate a series of quantiles
  ints <- .ints(sub_d, quants = quants, by = c("year", "set_den"))
  ints$lab <- formatC(ints$set_den, format = "fg")
  ints <- merge(ints, true, by = c("year", "set_den"))

  if (length(surveys) > 1) {

    shared_ints <- crosstalk::SharedData$new(ints)

    p <- crosstalk::bscols(
      list(
        htmltools::div(style = htmltools::css(height = "10px")), # small margin
        crosstalk::filter_select("set_den", "Set density", shared_ints, ~set_den, multiple = FALSE)
      ),
      .fan(shared_ints, x = ~year, xlab = "Year",
           cols = viridis::viridis(nlevels(ints$prob)),
           ylim = c(min(ints$lower), max(ints$upper)),
           ...),
      widths = c(3, NA)
    )

  } else {

    p <- .fan(ints, x = ~year, xlab = "Year",
              cols = viridis::viridis(nlevels(ints$prob)),
              ylim = c(min(ints$lower), max(ints$upper)),
              ...)

  }

  p

}



#' @export
#' @rdname plot_trend
plot_length_strat_fan <- function(sim, surveys = 1:5, years = 1:10,
                                  lengths = 1:50, select_by = "year",
                                  quants = seq(90, 10, by = -10),
                                  ...) {

  survey <- set_den <- lengths_cap <- NULL

  d <- sim$length_strat_error
  sub_d <- d[survey %in% surveys & year %in% years & length %in% lengths, ]
  sub_d <- merge(sim$surveys, sub_d, by = "survey", all.y = TRUE)
  true <- unique(sub_d[sim == 1, list(year, length, set_den, lengths_cap, I)])

  ## Calculate a series of quantiles
  ints <- .ints(sub_d, quants = quants, by = c("length", "year", "set_den", "lengths_cap"))
  ints$lab <- paste(formatC(ints$set_den, format = "fg"),
                    ints$lengths_cap, sep = "-")
  ints <- merge(ints, true, by = c("length", "year", "set_den", "lengths_cap"))

  shared_ints <- crosstalk::SharedData$new(ints)

  if (select_by == "year") {
    f <- crosstalk::filter_select(select_by, "Year", shared_ints, ~year, multiple = FALSE)
    x <- ~length
    xlab <- "Length"
  } else {
    f <- crosstalk::filter_select(select_by, "Length", shared_ints, ~length, multiple = FALSE)
    x <- ~year
    xlab <- "Year"
  }

  if (sum(c(length(surveys), length(years), length(lengths)) > 1) > 1) {

    p <- crosstalk::bscols(
      list(
        htmltools::div(style = htmltools::css(height = "10px")), # small margin
        f,
        crosstalk::filter_select("set_den", "Set density", shared_ints, ~set_den, multiple = FALSE),
        crosstalk::filter_select("lengths_cap", "Lengths cap", shared_ints, ~lengths_cap, multiple = FALSE)
      ),
      .fan(shared_ints, x = x, xlab = xlab,
           cols = viridis::viridis(nlevels(ints$prob)),
           ylim = c(min(ints$lower), max(ints$upper)),
           ...),
      widths = c(3, NA)
    )

  } else {

    p <- .fan(ints, x = x, xlab = xlab,
              cols = viridis::viridis(nlevels(ints$prob)),
              ylim = c(min(ints$lower), max(ints$upper)),
              ...)

  }

  p

}



#' @export
#' @rdname plot_trend
plot_age_strat_fan <- function(sim, surveys = 1:5, years = 1:10,
                               ages = 1:10, select_by = "year",
                               quants = seq(90, 10, by = -10),
                               ...) {

  survey <- age <- NULL

  d <- sim$age_strat_error
  sub_d <- d[survey %in% surveys & year %in% years & age %in% ages, ]
  true <- sub_d[sim == 1, list(year, age, survey, I)]

  ## Calculate a series of quantiles
  ints <- .ints(sub_d, quants = quants, by = c("age", "year", "survey"))
  ints <- merge(sim$surveys, ints, by = "survey", all.y = TRUE)
  ints$lab <- paste(formatC(ints$set_den, format = "fg"),
                    ints$lengths_cap, ints$ages_cap, sep = "-")
  ints <- merge(ints, true, by = c("age", "year", "survey"))

  shared_ints <- crosstalk::SharedData$new(ints)

  if (select_by == "year") {
    f <- crosstalk::filter_select(select_by, "Year", shared_ints, ~year, multiple = FALSE)
    x <- ~age
    xlab <- "Age"
  } else {
    f <- crosstalk::filter_select(select_by, "Age", shared_ints, ~age, multiple = FALSE)
    x <- ~year
    xlab <- "Year"
  }

  if (sum(c(length(surveys), length(years), length(ages)) > 1) > 1) {

    p <- crosstalk::bscols(
      list(
        htmltools::div(style = htmltools::css(height = "10px")), # small margin
        f,
        crosstalk::filter_select("set_den", "Set density", shared_ints, ~set_den, multiple = FALSE),
        crosstalk::filter_select("lengths_cap", "Lengths cap", shared_ints, ~lengths_cap, multiple = FALSE),
        crosstalk::filter_select("ages_cap", "Ages cap", shared_ints, ~ages_cap, multiple = FALSE)
      ),
      .fan(shared_ints, x = x, xlab = xlab,
           cols = viridis::viridis(nlevels(ints$prob)),
           ylim = c(min(ints$lower), max(ints$upper)),
           ...),
      widths = c(3, NA)
    )

  } else {

    p <- .fan(ints, x = x, xlab = xlab,
              cols = viridis::viridis(nlevels(ints$prob)),
              ylim = c(min(ints$lower), max(ints$upper)),
              ...)

  }

  p

}



#' @export
#' @rdname plot_trend
plot_error_surface <- function(sim, plot_by = "rule") {

  n_sets <- n_caught <- n_measured <- n_aged <- NULL

  totals <- sim$samp_totals[, list(n_sets = mean(n_sets), n_caught = mean(n_caught),
                                   n_measured = mean(n_measured), n_aged = mean(n_aged)),
                            by = "survey"]
  errors <- merge(sim$surveys, sim$age_strat_error_stats, by = "survey")
  d <- merge(errors, totals, by = "survey")

  d$text <- with(d, paste("<br>set density:", set_den,
                          "<br>lengths cap:", lengths_cap,
                          "<br>ages cap:", ages_cap,
                          "<br><br>N sets:", n_sets,
                          "<br>N lengths:", round(n_measured),
                          "<br>N ages:", round(n_aged)))

  if (plot_by == "rule") {
    split_d <- split(d, d$set_den)
    x <- sort(unique(d$ages_cap))
    y <- sort(unique(d$lengths_cap))
    marker_x <- ~ages_cap
    marker_y <- ~lengths_cap
    xlab <- "Ages cap"
    ylab <- "Lengths cap"
    slab <- "Set density"
  }
  if (plot_by == "samples") {
    split_d <- split(d, d$n_sets)
    x <- marker_x <- ~n_aged
    y <- marker_y <- ~n_measured
    xlab <- "N ages"
    ylab <- "N lengths"
    slab <- "N sets"
  }

  p <- plot_ly(x = x, y = y)
  splits <- vector("list", length(split_d) + 1)
  splits[[1]] <- list(args = list(list(visible = rep(c(TRUE, FALSE, TRUE), length(split_d)),
                                       showscale = c(TRUE, rep(FALSE, (length(split_d) * 3) - 1)))),
                      method = "update",
                      label = "all")
  showscale <- c(TRUE, rep(FALSE, length(split_d) - 1))

  for (i in seq_along(split_d)) {

    vis <- replicate(length(split_d), c(FALSE, FALSE, FALSE), simplify = FALSE)
    vis[[i]] <- c(FALSE, TRUE, TRUE)
    if (plot_by == "rule") {
      z <- stats::xtabs(RMSE ~ ages_cap + lengths_cap, data = split_d[[i]], subset = NULL)
      dimnames(z) <- NULL # plotly didn't like the xtabs attributes
      attr(z, "class") <- NULL
      attr(z, "call") <- NULL
      p <- p %>% add_surface(z = t(z),
                             cmin = min(d$RMSE), cmax = max(d$RMSE),
                             showscale = i == 1,
                             name = names(split_d)[i],
                             colorbar = list(title = "RMSE"),
                             hoverinfo = "skip") %>%
        add_surface(z = t(z),
                    visible = FALSE,
                    showscale = FALSE,
                    name = names(split_d)[i],
                    colorbar = list(title = "RMSE"),
                    hoverinfo = "skip")
    }
    if (plot_by == "samples") {
      p <- p %>% add_mesh(z = ~RMSE,
                          intensity = ~RMSE,
                          data = split_d[[i]],
                          cmin = min(d$RMSE), cmax = max(d$RMSE),
                          showscale = i == 1,
                          name = names(split_d)[i],
                          flatshading = TRUE,
                          colorbar = list(title = "RMSE"),
                          contour = list(show = TRUE, width = 15, color = toRGB("white")),
                          hoverinfo = "skip") %>%
        add_mesh(z = ~RMSE,
                 intensity = ~RMSE,
                 data = split_d[[i]],
                 visible = FALSE,
                 showscale = FALSE,
                 name = names(split_d)[i],
                 flatshading = TRUE,
                 colorbar = list(title = "RMSE"),
                 contour = list(show = TRUE, width = 15, color = toRGB("white")),
                 hoverinfo = "skip")
    }
    p <- p %>%
      add_markers(z = ~RMSE,
                  x = marker_x,
                  y = marker_y,
                  data = split_d[[i]],
                  text = ~text,
                  hoverinfo = "text+z",
                  name = names(split_d)[i],
                  legendgroup = names(split_d)[i],
                  color = I("grey30"),
                  opacity = 0,
                  visible = TRUE,
                  showlegend = FALSE)
    splits[[i + 1]] <- list(args = list(list(visible = unlist(vis),
                                             showscale = unlist(vis))),
                            method = "update",
                            label = names(split_d)[i])

  }


  p %>%
    layout(
      updatemenus = list(
        list(
          y = 0.9,
          x = 0,
          xanchor = "center",
          yanchor = "top",
          buttons = splits
        )
      ),
      annotations = list(
        list(
          text = slab,
          y = 0.9,
          x = 0,
          borderpad = 5,
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE
        )
      ),
      scene = list(
        xaxis = list(title = xlab),
        yaxis = list(title = ylab),
        zaxis = list(title = "RMSE"),
        camera = list(eye = list(x = 1.5, y = 1.5, z = 1.5))
      ))

}


.label_plot <- function(d, text = NULL, name = NULL, ...) {
  plot_ly(data = d) %>%
    add_text(x = 1, y = ~rank, text = text, hoverinfo = "none",
             name = name, ...) %>%
    hide_guides() %>%
    layout(yaxis = list(showticklabels = FALSE,
                        tickvals = d$rank,
                        title = "",
                        range = c(min(c(max(d$rank) + 1, 25)), 0),
                        zeroline = FALSE),
           xaxis = list(title = name,
                        side = "top",
                        zeroline = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        showgrid = FALSE))
}


.rank_plot <- function(d, x = NULL, name = NULL, ...) {
  plot_ly(data = d) %>%
    add_markers(x = x, y = ~rank, color = x, colors = rev(viridis::viridis(100)),
                name = name, ...) %>%
    hide_guides() %>%
    layout(yaxis = list(showticklabels = FALSE,
                        tickvals = d$rank,
                        title = "",
                        range = c(min(c(max(d$rank) + 1, 25)), 0)),
           xaxis = list(title = name,
                        side = "top",
                        ticks = "outside",
                        showline = TRUE,
                        zeroline = FALSE,
                        showgrid = FALSE))
}


#' @export
#' @rdname plot_trend
plot_survey_rank <- function(sim, which_strat = "age") {

  survey <- n_sets <- n_caught <- n_measured <- n_aged <- RMSE <- NULL

  surveys <- switch(which_strat,
                    total = sim$surveys[, list(survey = min(survey)), by = c("set_den")],
                    length = sim$surveys[, list(survey = min(survey)), by = c("set_den", "lengths_cap")],
                    age = sim$surveys,
                    stop("which_strat must be total, length, or age"))
  totals <- switch(which_strat,
                   total = sim$samp_totals[, list(n_sets = mean(n_sets), n_caught = mean(n_caught)),
                                           by = "survey"],
                   length = sim$samp_totals[, list(n_sets = mean(n_sets), n_caught = mean(n_caught),
                                                   n_measured = mean(n_measured)),
                                            by = "survey"],
                   age = sim$samp_totals[, list(n_sets = mean(n_sets), n_caught = mean(n_caught),
                                                n_measured = mean(n_measured), n_aged = mean(n_aged)),
                                         by = "survey"],
                   stop("which_strat must be total, length, or age"))
  errors <- switch(which_strat,
                   total = merge(surveys, sim$total_strat_error_stats, by = "survey"),
                   length = merge(surveys, sim$length_strat_error_stats, by = "survey"),
                   age = merge(surveys, sim$age_strat_error_stats, by = "survey"),
                   stop("which_strat must be total, length, or age"))
  d <- merge(errors, totals, by = "survey")

  d <- d[order(RMSE), ]
  d$rank <- seq(nrow(d))

  sub_d <- d

  a_list <- list()
  a_list$p_sets_lab <- .label_plot(sub_d, text = ~set_den, name = "D<sub>sets</sub>")
  if (which_strat == "length" | which_strat == "age") {
    a_list$p_lens_lab <- .label_plot(sub_d, text = ~lengths_cap, name = "M<sub>lengths</sub>")
  }
  if (which_strat == "age") {
    a_list$p_ages_lab <- .label_plot(sub_d, text = ~ages_cap, name = "M<sub>ages</sub>")
  }

  a <- subplot(a_list, margin = 0.01,
               nrows = 1, titleX = TRUE, shareY = TRUE)

  b_list <- list()
  b_list$p_rmse <- .rank_plot(sub_d, x = ~RMSE, name = "RMSE")
  b_list$p_sets <- .rank_plot(sub_d, x = ~n_sets, name = "N<sub>sets</sub>")
  if (which_strat == "length" | which_strat == "age") {
    b_list$p_measured <- .rank_plot(sub_d, x = ~n_measured, name = "N<sub>measured</sub>")
  }
  if (which_strat == "age") {
    b_list$p_aged <- .rank_plot(sub_d, x = ~n_aged, name = "N<sub>aged</sub>")
  }

  b <- subplot(b_list, nrows = 1, titleX = TRUE, shareY = TRUE)

  suppressWarnings({
    subplot(a, b, titleX = TRUE, shareY = TRUE, nrows = 1, widths = c(0.3, 0.7),
            margin = 0.01) %>%
      layout(margin = list(b = 10, l = 10))
  })

}



