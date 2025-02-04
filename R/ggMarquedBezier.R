#' Exported objects from ggraph
#'
#' @importFrom ggraph geometry
#' @export
#' @rdname ggraph_exports
#' @name geometry
geometry

#' @importFrom ggraph circle
#' @export
#' @rdname ggraph_exports
#' @name geometry
circle

#' @importFrom ggraph square
#' @export
#' @rdname ggraph_exports
#' @name geometry
square

#' @import ggforce
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

#' Create Bezier Curves Between Two Points
#'
#' This stat computes Bezier curves between specified points in a ggplot.
#' It's parametrixed by x, y, xend, yend, like `geom_segment()`.
#' It offers control over the curvature and style of the arcs through
#' optional aesthetics. Similar in spirit to `ggraph::geom_edge_bezier()`,
#' but parametrized differently.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggforce::stat_bezier
#'
#' @section Aesthetics:
#' `stat_bezier_segment` understands the following aesthetics (required aesthetics are in bold):
#'   - **x**: Starting x-coordinate of the segment.
#'   - **y**: Starting y-coordinate of the segment.
#'   - **xend**: Ending x-coordinate of the segment.
#'   - **yend**: Ending y-coordinate of the segment.
#'   - angle: Angle of the initial tangent vector relative to the segment in degrees, default is 0.
#'   - strength: Distance from the start point to the control point, default is half the segment length.
#'   - angle_end: Angle of the terminating tangent vector relative to the segment in degrees, default is the same as `angle`.
#'   - strength_end: Distance from the end point to the control point, default is the same as `strength`.
#'
#' @section Computed values:
#' The stat calculates the following values:
#'   - x
#'   - y
#'
#' @section Dropped Aesthetics:
#' The following columns are removed from the data frame passed to the compute function:
#'   - xend
#'   - yend
#'   - angle
#'   - strength
#'   - angle_end
#'   - strength_end
#'
#' @section Details:
#' The computation involves the following steps:
#'   - Compute the length and angle of each segment using the Euclidean distance and \code{atan2}.
#'   - Set the controls for the Bezier curves, either using provided values or defaults.
#'   - Calculate control points and define Bezier path using a custom utility to generate paths.
#'   - Generate a data frame comprising the interpolated path points, where one path is formed per segment.
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
#'   geom_path(
#'     aes(x = x, y = y, xend = xend, yend = yend),
#'     stat = "BezierSegment"
#'   )
#' }
#'
#' @seealso [geom_marqued_bezier()]
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

#' @import marquee
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
    arrow_grob <- .arrowhead_grobs(
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
#' the dashing or dotting of the path, which is sometimes ugly.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_path
#' @param show.label Logical to control whether the label should be drawn.
#'
#' @section Aesthetics:
#' `geom_marqued_bezier` understands the following aesthetics (required aesthetics are in bold):
#'   - **x**: The x-coordinate of points defining the path (required).
#'   - **y**: The y-coordinate of points defining the path (required).
#'   - alpha: Transparency level of the path.
#'   - color: Color of the path.
#'   - linetype: Line type for the path. Note that the linetype for the arrow is always solid for clarity.
#'   - linewidth: Line width of the path.
#'   - label: Text label for the midpoint of the path.
#'   - label_alpha: Transparency level of the label.
#'   - label_color: Color of the label.
#'   - label_fill: Fill color for the label's background.
#'   - label_angle: Angle of the label text, by default automatically aligned with the path direction.
#'   - label_family: Font family of the label text.
#'   - label_fontface: Font face of the label text (e.g., bold or italic).
#'   - label_lineheight: Line height of the label.
#'   - label_size: Text size for the label.
#'   - label_hjust: Horizontal justification of label text.
#'   - label_vjust: Vertical justification of label text.
#'   - label_nudge: Numerical value to nudge the label perpendicular to the path direction.
#'   - label_style: Style of the marquee label.
#'   - label_width: Width of the label's bounding box.
#'   - start_cap: Style for the start cap, similar to those in the \code{ggraph} package.
#'   - end_cap: Style for the end cap, similar to those in the \code{ggraph} package.
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

#' Mask Rows Based on Geometric Cap Aesthetics
#'
#' This function generates a logical vector mask for the rows of a data frame
#' containing coordinates (`coords`), excluding those points that fall within
#' the geometric areas defined by the `start_cap` and/or `end_cap` aesthetics.
#'
#' The function is designed to emulate the internal functionality of
#' `ggraph::geom_edges`. Currently, it supports circular and rectangular caps.
#' Points that lie within the specified circular radius or within the
#' rectangular bounds will be excluded, i.e., marked as `FALSE` in the mask.
#'
#' @param coords A data frame containing coordinates with columns `x`, `y`,
#' `start_cap`, `end_cap`, and `group`.
#'
#' @return A logical vector of the same length as the number of rows in `coords`.
#' Each element is `TRUE` if the corresponding point is outside the `start_cap`
#' and `end_cap`, and `FALSE` if it is within one of these caps.
#'
#' @details
#' The function first checks whether the `start_cap` and `end_cap` are
#' provided and not missing. It identifies the cap type using an internal
#' function `ggraph:::geo_type`. Based on the cap type (`circle` or `rect`),
#' it computes the necessary geometric checks:
#'
#' - For circles, it calculates the squared radius using
#'   `grid::convertUnit(ggraph:::geo_width(cap), "npc")`, and applies the
#'   Euclidean distance formula to exclude points within the radius.
#'
#' - For rectangles, it computes half the width for boundary comparison and
#' excludes points within these bounds.
#'
#' @note Only `circle` and `square` cap types are supported currently.
#' The function may be expanded in the future to support additional shapes.
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

#' Disentangle Arrow's Head from Its Path
#'
#' The `.arrow_grobs` function creates arrowheads separately from the segment.
#' It's useful to customize the linetype of the segment without affecting the
#' head.
#'
#' @param arrow `grid::arrow()` specification.
#' @param data,panel_params,coord,na.rm arguments to `Geom$draw_panel()`.
#'
#' @return A grid graphics list (`gList`) containing grob(s) that represent the arrow head(s).
#'
#' The function handles distinct cases for drawing arrowheads at the
#' start ("first"), end ("last"), or both ends of the path ("both"). This
#' is based on the `arrow$ends` parameter, with 1 representing the start, 2
#' the end, and 3 indicating both.
#'
#' @details
#' Call this function from within the `$draw_group()` method of a geom.
.arrowhead_grobs <- function(arrow, data, panel_params, coord, na.rm) {
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
