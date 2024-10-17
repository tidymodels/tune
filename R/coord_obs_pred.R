

CoordObsPred <-
  ggplot2::ggproto(
    "CoordObsPred",
    ggplot2::CoordFixed,
    setup_panel_params = function(self, scale_x, scale_y, params = list()) {
      # coord limits take precedence over scale limits
      rngs <- range(
        self$limits$x %||% scale_x$get_limits(),
        self$limits$y %||% scale_y$get_limits(),
        na.rm = TRUE
      )
      self$limits$y <- rngs
      self$limits$x <- rngs
      ggplot2::ggproto_parent(ggplot2::CoordFixed, self)$setup_panel_params(scale_x, scale_y, params)
    },
    aspect = function(self, ranges) {
      1 / self$ratio
    }
  )

#' Use same scale for plots of observed vs predicted values
#'
#' For regression models, `coord_obs_pred()` can be used in a ggplot to make the
#' x- and y-axes have the same exact scale along with an aspect ratio of one.
#' @param ratio	Aspect ratio, expressed as `y / x`. Defaults to 1.0.
#' @param xlim,ylim Limits for the x and y axes.
#' @param expand Not currently used.
#' @param clip Should drawing be clipped to the extent of the plot panel? A setting
#' of "on" (the default) means yes, and a setting of "off" means no. In most
#' cases, the default of "on" should not be changed, as setting `clip = "off"`
#' can cause unexpected results. It allows drawing of data points anywhere on
#' the plot, including in the plot margins. If limits are set via `xlim` and
#' `ylim` and some data points fall outside those limits, then those data points
#' may show up in places such as the axes, the legend, the plot title, or the
#' plot margins.
#' @return A `ggproto` object.
#' @examplesIf rlang::is_installed("modeldata")
#' # example code
#' data(solubility_test, package = "modeldata")
#'
#' library(ggplot2)
#' p <- ggplot(solubility_test, aes(x = solubility, y = prediction)) +
#'   geom_abline(lty = 2) +
#'   geom_point(alpha = 0.5)
#'
#' p
#'
#' p + coord_fixed()
#'
#' p + coord_obs_pred()
#' @export
coord_obs_pred <-
  function(ratio = 1,
           xlim = NULL,
           ylim = NULL,
           expand = TRUE,
           clip = "on") {
    ggplot2::ggproto(
      NULL,
      CoordObsPred,
      limits = list(x = xlim, y = ylim),
      ratio = ratio,
      expand = expand,
      clip = clip
    )
  }
