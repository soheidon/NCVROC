# execution_preview_plot.R -- Visualization methods for execution plans and scaling curves

#' Plot Execution Plan Scaling Curves and Benchmark Results
#'
#' Renders base R diagnostic plots of empirical scaling curves, speedup,
#' and parallel efficiency across all benchmarked resource configurations.
#'
#' @param x An object of class `"ncvroc_execution_plan"`.
#' @param type Plot type to display: `"runtime"` (estimated runtime vs resources),
#'   `"speedup"` (observed speedup curve), `"efficiency"` (parallel efficiency decay),
#'   or `"all"` (multi-panel display). Default `"runtime"`.
#' @param ... Additional arguments passed to base plotting functions.
#' @return Invisibly returns `x`.
#' @export
plot.ncvroc_execution_plan <- function(x, type = c("runtime", "speedup", "efficiency", "all"), ...) {
  type <- match.arg(type)
  tb <- x$benchmark_table
  if (!is.data.frame(tb) || nrow(tb) == 0L) {
    message("No benchmark table available in execution plan to plot.")
    return(invisible(x))
  }

  ok_tb <- tb[tb$status == "ok" & is.finite(tb$median_elapsed), , drop = FALSE]
  if (nrow(ok_tb) == 0L) {
    message("No successful benchmarked configurations available to plot.")
    return(invisible(x))
  }

  # Color palette for backends
  backends <- unique(ok_tb$parallel)
  palette_map <- c(
    none    = "#555555",
    threads = "#1b9e77",
    outer   = "#d95f02",
    chunks  = "#7570b3",
    hybrid  = "#e7298a"
  )
  get_color <- function(b) if (b %in% names(palette_map)) palette_map[[b]] else "#333333"

  # Sub-functions for individual panels
  plot_runtime <- function() {
    y_vals <- ok_tb$estimated_full_runtime
    if (all(is.na(y_vals))) y_vals <- ok_tb$median_elapsed
    y_label <- if (all(is.na(ok_tb$estimated_full_runtime))) "Pilot Elapsed (sec)" else "Estimated Runtime (sec)"

    graphics::plot(
      ok_tb$resource_count, y_vals, type = "n",
      xlab = "Allocated Resources (Cores / Workers)",
      ylab = y_label,
      main = paste0("Runtime Scaling: ", x$workflow),
      ...
    )
    graphics::grid()

    for (b in backends) {
      sub_b <- ok_tb[ok_tb$parallel == b, , drop = FALSE]
      sub_b <- sub_b[order(sub_b$resource_count), , drop = FALSE]
      y_b <- if (all(is.na(sub_b$estimated_full_runtime))) sub_b$median_elapsed else sub_b$estimated_full_runtime
      graphics::lines(sub_b$resource_count, y_b, col = get_color(b), lwd = 2)
      graphics::points(sub_b$resource_count, y_b, col = get_color(b), pch = 19, cex = 1.2)
    }

    # Highlight selected plan
    sel <- x$selected_plan
    sel_rows <- ok_tb[ok_tb$parallel == sel$parallel & ok_tb$resource_count == sel$resource_count, , drop = FALSE]
    if (nrow(sel_rows) > 0L) {
      sel_y <- if (all(is.na(sel_rows$estimated_full_runtime))) sel_rows$median_elapsed[[1L]] else sel_rows$estimated_full_runtime[[1L]]
      graphics::points(sel$resource_count, sel_y, col = "red", pch = 1, cex = 2.5, lwd = 2.5)
    }

    graphics::legend(
      "topright", legend = c(backends, "Selected Plan"),
      col = c(vapply(backends, get_color, character(1)), "red"),
      pch = c(rep(19, length(backends)), 1),
      lwd = c(rep(2, length(backends)), 2.5),
      pt.cex = c(rep(1.2, length(backends)), 2.0),
      bg = "white"
    )
  }

  plot_speedup <- function() {
    max_res <- if (any(is.finite(ok_tb$resource_count))) max(ok_tb$resource_count[is.finite(ok_tb$resource_count)]) else 1L
    max_sp <- if (any(is.finite(ok_tb$speedup))) max(ok_tb$speedup[is.finite(ok_tb$speedup)]) else 1.0
    graphics::plot(
      ok_tb$resource_count, ok_tb$speedup, type = "n",
      xlim = c(1, max(1L, max_res)), ylim = c(1, max(max_res, max_sp, 1.0)),
      xlab = "Allocated Resources",
      ylab = "Speedup vs. Serial",
      main = paste0("Speedup Curve: ", x$workflow),
      ...
    )
    graphics::grid()
    graphics::abline(a = 0, b = 1, col = "gray70", lty = 2, lwd = 1.5)

    for (b in backends) {
      sub_b <- ok_tb[ok_tb$parallel == b, , drop = FALSE]
      sub_b <- sub_b[order(sub_b$resource_count), , drop = FALSE]
      graphics::lines(sub_b$resource_count, sub_b$speedup, col = get_color(b), lwd = 2)
      graphics::points(sub_b$resource_count, sub_b$speedup, col = get_color(b), pch = 19, cex = 1.2)
    }

    graphics::legend(
      "topleft", legend = c(backends, "Ideal Linear"),
      col = c(vapply(backends, get_color, character(1)), "gray70"),
      lty = c(rep(1, length(backends)), 2),
      lwd = 2, bg = "white"
    )
  }

  plot_efficiency <- function() {
    graphics::plot(
      ok_tb$resource_count, ok_tb$parallel_efficiency * 100, type = "n",
      ylim = c(0, 110),
      xlab = "Allocated Resources",
      ylab = "Parallel Efficiency (%)",
      main = paste0("Parallel Efficiency: ", x$workflow),
      ...
    )
    graphics::grid()
    graphics::abline(h = 50, col = "red", lty = 3, lwd = 1.2)

    for (b in backends) {
      sub_b <- ok_tb[ok_tb$parallel == b, , drop = FALSE]
      sub_b <- sub_b[order(sub_b$resource_count), , drop = FALSE]
      graphics::lines(sub_b$resource_count, sub_b$parallel_efficiency * 100, col = get_color(b), lwd = 2)
      graphics::points(sub_b$resource_count, sub_b$parallel_efficiency * 100, col = get_color(b), pch = 19, cex = 1.2)
    }

    graphics::legend(
      "bottomleft", legend = c(backends, "50% Threshold"),
      col = c(vapply(backends, get_color, character(1)), "red"),
      lty = c(rep(1, length(backends)), 3),
      lwd = 2, bg = "white"
    )
  }

  if (type == "runtime") {
    plot_runtime()
  } else if (type == "speedup") {
    plot_speedup()
  } else if (type == "efficiency") {
    plot_efficiency()
  } else if (type == "all") {
    old_par <- graphics::par(mfrow = c(1, 3))
    on.exit(graphics::par(old_par), add = TRUE)
    plot_runtime()
    plot_speedup()
    plot_efficiency()
  }

  invisible(x)
}
