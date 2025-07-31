plot_network_correlation <- function(
    net,
    coord,
    rho,
    color_ramp = c("darkred", "red", "white", "green", "darkgreen"),
    reference_node = 1,
    display_rho = TRUE,
    display_labels = TRUE,
    display_roi_index = FALSE,
    ...) {
  adj <- as.matrix(net, matrix.type = "adjacency", attrname = NULL)

  ramp <- colorRamp(color_ramp)
  corRange <- c(-1, 1)


  vertex_col <- rgb(ramp((rho[reference_node, ] + (1 - 1e-15)) / 2), max = 255) # maybe +0.999999999
  vertex_cex <- abs(rho[reference_node, ]) + 1

  # ~~~~~~~~~~~~~~~~~~~~~~~~~
  # plot
  # ~~~~~~~~~~~~~~~~~~~~~~~~~
  par()
  plot(
    net,
    coord = coord,
    jitter = FALSE,
    displaylabels = display_labels,
    label.pos = 1,
    label.cex = 0.8,
    label.col = "blue",
    vertex.col = vertex_col,
    vertex.cex = vertex_cex,
    # vertex.border = Vertex.Border ,
    edge.col = rgb(0, 0, 0, 0.1),
    ...
  )

  if (isTRUE(display_rho)) {
    text(
      coord[, 1], 
      coord[, 2],
      pos = 4,
      round(rho[reference_node, ], 2),
      cex = 0.8
    )
  }

  ref_node_col <- 4 # blue
  ref_node_cex <- 5
  points(
    coord[reference_node, 1], 
    coord[reference_node, 2],
    pch = 24,
    cex = ref_node_cex,
    bg = ref_node_col,
    col = ref_node_col
  )
  points(
    coord[reference_node, 1], 
    coord[reference_node, 2],
    pch = 25,
    cex = ref_node_cex,
    bg = ref_node_col,
    col = ref_node_col
  )

  if (isTRUE(display_roi_index)) {
    text(
      coord[, 1], 
      coord[, 2],
      pos = 3,
      1:68,
      cex = 0.8,
      col = "blue"
    )
  }
}