#' @importFrom ggraph geometry circle square ellipsis rectangle is.geometry
#' @export geometry circle square ellipsis rectangle is.geometry
NULL

#' @rdname stat_bezier_segment
StatBezierSegment <- ggplot2::ggproto("StatBezierSegment", ggplot2::Stat,
  required_aes = c("x", "y", "xend", "yend"),
  optional_aes = c("angle", "strength", "angle_end", "strength_end"),
  dropped_aes = c("xend", "yend", "angle", "strength", "angle_end", "strength_end"),
  extra_params = c("na.rm", "n"),
  compute_panel = function(data, scales, n = 100) {
    if (ggforce:::empty_data(data)) return(data)

    segment_length <- sqrt((data$xend - data$x)^2 + (data$yend - data$y)^2)
    data$group <- seq_len(nrow(data))
    data$angle <- data$angle %||% rep(0, nrow(data))
    data$strength <- data$strength %||% segment_length / 2
    data$angle_end <- data$angle_end %||% data$angle
    data$strength_end <- data$strength_end %||% data$strength

    segment_angle <- atan2(data$yend - data$y, data$xend - data$x)
    angle1 <- segment_angle + data$angle * pi / 180
    angle2 <- segment_angle + (180 - data$angle_end) * pi / 180

    data_control <- data.frame(
      x = round(data$x + data$strength * cos(angle1), digits = 12),
      y = round(data$y + data$strength * sin(angle1), digits = 12),
      group = data$group
    )
    data_control_end <- data.frame(
      x = round(data$xend + data$strength_end * cos(angle2), digits = 12),
      y = round(data$yend + data$strength_end * sin(angle2), digits = 12),
      group = data$group
    )
    control_points <- rbind(
      data[, c("x", "y", "group")],
      data_control,
      data_control_end,
      setNames(data[, c("xend", "yend", "group")], c("x", "y", "group"))
    )
    tmp_order <- seq_len(nrow(control_points))
    control_points <- control_points[order(control_points$group, tmp_order), ]

    paths <- ggforce:::getBeziers(
      control_points$x,
      control_points$y,
      control_points$group,
      n
    )
    paths_df <- data.frame(
      x = paths$paths[, 1],
      y = paths$paths[, 2],
      group = paths$pathID,
      index = rep(seq(0, 1, length.out = n), length(unique(paths$pathID)))
    )

    merge(
      paths_df,
      subset(
        data,
        select = -c(x, y, xend, yend, angle, strength, angle_end, strength_end)
      ),
      by = "group"
    )
  },
)

#' Create Bezier Curves between Define Points
#'
#' This stat computes Bezier curves between specified points in a ggplot.
#' It offers control over the curvature and style of the arcs through
#' optional aesthetics, enhancing the visualization of paths or connections
#' in data.
#'
#' @format An object of class \code{ggproto} inheriting from \code{Stat}.
#'
#' @section Aesthetics:
#' \code{StatBezierSegment} understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{x}: Starting x-coordinate of the segment.
#'   \item \strong{y}: Starting y-coordinate of the segment.
#'   \item \strong{xend}: Ending x-coordinate of the segment.
#'   \item \strong{yend}: Ending y-coordinate of the segment.
#'   \item \code{angle}: Angle of the initial tangent vector relative to the segment in degrees, default is 0.
#'   \item \code{strength}: Distance from the start point to the control point, default is half the segment length.
#'   \item \code{angle_end}: Angle of the terminating tangent vector relative to the segment in degrees, default is the same as \code{angle}.
#'   \item \code{strength_end}: Distance from the end point to the control point, default is the same as \code{strength}.
#'}
#'
#' @section Dropped Aesthetics:
#' The following columns are removed from the data frame passed to the compute function:
#' \itemize{
#'   \item \code{xend}
#'   \item \code{yend}
#'   \item \code{angle}
#'   \item \code{strength}
#'   \item \code{angle_end}
#'   \item \code{strength_end}
#' }
#'
#' @section Extra Parameters:
#' The following parameters can be set via \code{params}:
#' \itemize{
#'   \item \code{na.rm}: A logical indicating whether missing values should be removed.
#'   \item \code{n}: The number of interpolated points to create the Bezier curve.
#'}
#'
#' @section Computation:
#' The computation involves the following steps:
#' \enumerate{
#'   \item Compute the length and angle of each segment using the Euclidean distance and \code{atan2}.
#'   \item Set the controls for the Bezier curves, either using provided values or defaults.
#'   \item Calculate control points and define Bezier path using a custom utility to generate paths.
#'   \item Generate a data frame comprising the interpolated path points, where one path is formed per segment.
#'}
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' data <- data.frame(
#'   x = c(1, 2),
#'   y = c(1, 2),
#'   xend = c(3, 4),
#'   yend = c(2, 3)
#' )
#' ggplot(data) +
#'   geom_path(aes(x = x, y = y, group = group), stat = "BezierSegment")
#' }
#'
#' @seealso \code{\link[ggforce]{getBeziers}} for details on Bezier calculations.
#' @seealso \code{\link{geom_marqued_bezier}}.
#' @export
stat_bezier_segment <- function(
  mapping = NULL, data = NULL, geom = "marqued_bezier", position = "identity",
  na.rm = FALSE, show.legend = NA, n = 100, inherit.aes = TRUE, ...
) {
  ggplot2::layer(
    stat = StatBezierSegment, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n = n, ...)
  )
}

#' @rdname geom_marqued_bezier
GeomMarquedPath <- ggplot2::ggproto("GeomMarquedPath", ggplot2::Geom,
  required_aes = c("x", "y"),
  optional_aes = c(
    "alpha", "color", "linetype", "linewidth",
    "label", "label_alpha", "label_color", "label_fill", "label_angle",
    "label_family", "label_fontface", "label_lineheight", "label_size",
    "label_hjust", "label_vjust", "label_nudge", "label_style", "label_width",
    "start_cap", "end_cap"
  ),
  default_aes = ggplot2::aes(
    alpha = NA,
    color = "black",
    linetype = "solid",
    linewidth = 0.5,
    label = NA,
    label_alpha = NA,
    label_color = "black",
    label_fill = NA,
    label_angle = NA,
    label_family = "",
    label_fontface = 1,
    label_lineheight = 1.2,
    label_size = 3.88,
    label_hjust = 0.5,
    label_vjust = "center-ink",
    label_nudge = 0,
    label_style = marquee::classic_style(),
    label_width = NA,
    start_cap = NA,
    end_cap = NA
  ),
  extra_params = c("na.rm", "arrow", "show.label"),
  draw_group = function(
    data,
    panel_params,
    coord,
    arrow = NULL,
    show.label = TRUE,
    na.rm = FALSE
  ) {
    coords <- coord$transform(data, panel_params)
    mask <- .cap_mask(coords)
    capped_data <- data[mask, ]
    path_grob <- ggplot2::GeomPath$draw_panel(
      capped_data,
      panel_params,
      coord,
      na.rm = na.rm
    )
    arrow_grob <- .arrow_grobs(
      arrow,
      capped_data,
      panel_params,
      coord,
      na.rm
    )

    # Avoids https://github.com/r-lib/marquee/issues/47
    if (isFALSE(show.label) || all(trimws(data$label) == "", na.rm = TRUE)) {
      return(grid::gList(path_grob, arrow_grob))
    }

    mid <- round(nrow(capped_data) / 2)
    label_data <- capped_data[mid, ]
    label_angle <- round(
      atan2(
        capped_data$y[mid] - capped_data$y[mid-1],
        capped_data$x[mid] - capped_data$x[mid-1]
      ),
      digits = 2
    )
    label_data$x <- label_data$x + label_data$label_nudge * cos(label_angle + pi / 2)
    label_data$y <- label_data$y + label_data$label_nudge * sin(label_angle + pi / 2)
    if (is.na(label_data$label_angle)) {
      label_data$label_angle <- if (label_angle == round(pi / 2, digits = 2)) {
        label_angle / pi * 180
      } else {
        ((90 + label_angle / pi * 180) %% 180) - 90
      }
    }
    names(label_data) <- sub("label_", "", names(label_data))
    label_grob <- marquee::GeomMarquee$draw_panel(
      label_data,
      panel_params,
      coord,
      na.rm = na.rm,
      size.unit = "mm"
    )

    grid::gList(path_grob, arrow_grob, label_grob)
  }
)

#' Create a Path labelled with Marquee
#'
#' Create paths with optional labels that use marquee styling. The label is
#' automatically positioned at the midpoint of the path, following its
#' orientation. This geometry also decouples the arrow's linestyle from the
#' path's linestyle: in normal geom_path, the arrowhead's linetype follows
#' the dashing or dotting of the path, which is sometimes confusing.
#'
#' @section Aesthetics:
#' \code{StatBezierSegment} understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{x}: The x-coordinate of points defining the path (required).
#'   \item \strong{y}: The y-coordinate of points defining the path (required).
#'   \item \code{alpha}: Transparency level of the path.
#'   \item \code{color}: Color of the path.
#'   \item \code{linetype}: Line type for the path. Note that the linetype for the arrow is always solid for clarity.
#'   \item \code{linewidth}: Line width of the path.
#'   \item \code{label}: Text label for the midpoint of the path.
#'   \item \code{label_alpha}: Transparency level of the label.
#'   \item \code{label_color}: Color of the label.
#'   \item \code{label_fill}: Fill color for the label's background.
#'   \item \code{label_angle}: Angle of the label text, by default automatically aligned with the path direction.
#'   \item \code{label_family}: Font family of the label text.
#'   \item \code{label_fontface}: Font face of the label text (e.g., bold or italic).
#'   \item \code{label_lineheight}: Line height of the label.
#'   \item \code{label_size}: Text size for the label.
#'   \item \code{label_hjust}: Horizontal justification of label text.
#'   \item \code{label_vjust}: Vertical justification of label text.
#'   \item \code{label_nudge}: Numerical value to nudge the label perpendicular to the path direction.
#'   \item \code{label_style}: Style of the marquee label.
#'   \item \code{label_width}: Width of the label's bounding box.
#'   \item \code{start_cap}: Style for the start cap, similar to those in the \code{ggraph} package.
#'   \item \code{end_cap}: Style for the end cap, similar to those in the \code{ggraph} package.
#' }
#'
#' @section Parameters:
#' Geom parameters include:
#' \itemize{
#'   \item \code{na.rm}: Logical indicating whether missing values should be removed.
#'   \item \code{arrow}: Specification for arrow heads created using the \code{grid::arrow} function.
#'   \item \code{show.label}: Logical to control whether the label should be drawn.
#' }
#'
#'
#' @return A ggplot2 layer that draws paths with enhanced features such as arrows with always solid lines and intelligently placed labels.
#'
#' @export
geom_marqued_bezier <- function(
  mapping = NULL, data = NULL, stat = "bezier_segment", ...,
  position = "identity", arrow = NULL, na.rm = FALSE, show.label = TRUE,
  show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMarquedPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      arrow = arrow,
      show.label = show.label,
      ...
    )
  )
}

.cap_mask <- function(coords) {
  mask <- rep(TRUE, nrow(coords))
  if (!all(is.na(coords$start_cap))) {
    cap <- coords$start_cap[!duplicated(coords$group)]
    center <- coords[1, c("x", "y"), drop = TRUE]
    captype <- ggraph:::geo_type(cap)
    mask <- if (captype == "circle") {
      radius <- as.numeric(grid::convertUnit(ggraph:::geo_width(cap), "npc"))
      mask & (coords$x - center$x)^2 + (coords$y - center$y)^2 > radius^2
    } else if (captype == "rect") {
      width <- as.numeric(grid::convertUnit(ggraph:::geo_width(cap), "npc")) / 2
      mask & (abs(coords$x - center$x) > width | abs(coords$y - center$y) > width)
    } else {
      stop("Unsupported cap type: ", captype)
    }
  }
  if (!all(is.na(coords$end_cap))) {
    cap <- coords$end_cap[!duplicated(coords$group)]
    center <- coords[nrow(coords), c("x", "y"), drop = TRUE]
    captype <- ggraph:::geo_type(cap)
    mask <- if (captype == "circle") {
      radius <- as.numeric(grid::convertUnit(ggraph:::geo_width(cap), "npc"))
      mask & (coords$x - center$x)^2 + (coords$y - center$y)^2 > radius^2
    } else if (captype == "rect") {
      width <- as.numeric(grid::convertUnit(ggraph:::geo_width(cap), "npc")) / 2
      mask & (abs(coords$x - center$x) > width | abs(coords$y - center$y) > width)
    } else {
      stop("Unsupported cap type: ", captype)
    }
  }
  mask
}

.arrow_grobs <- function(arrow, data, panel_params, coord, na.rm) {
  if (is.null(arrow)) {
    return(grid::nullGrob())
  }
  data$linetype <- 1
  arrow_list <- list()
  if (arrow$ends %in% c(1, 3)) {
    arrow_list <- c(
      arrow_list,
      list(
        ggplot2::GeomPath$draw_panel(
          data[1:2, ],
          panel_params,
          coord,
          arrow = grid::arrow(
            angle = arrow$angle,
            length = arrow$length,
            ends = "first",
            type = c("open", "closed")[arrow$type]
          ),
          na.rm = na.rm
        )
      )
    )
  }
  if (arrow$ends %in% c(2, 3)) {
    arrow_list <- c(
      arrow_list,
      list(
        ggplot2::GeomPath$draw_panel(
          data[(nrow(data) - 1):nrow(data), ],
          panel_params,
          coord,
          arrow = grid::arrow(
            angle = arrow$angle,
            length = arrow$length,
            ends = "last",
            type = c("open", "closed")[arrow$type]
          ),
          na.rm = na.rm
        )
      )
    )
  }
  do.call(grid::gList, arrow_list)
}
