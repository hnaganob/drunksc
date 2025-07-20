# ConvergeVisitCount                                    ---- function ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Note:
# - 



ConvergeVisitCount <- function(path,
                               convergent_count = 100,
                               save_data = FALSE,
                               plot_data = FALSE,
                               adj_code) {
  
  node_list <- sort(unique(path))
  n_nodes <- length(node_list)
  path_length <- length(path)
  
  window_size <- n_nodes * convergent_count
  
  end_iteration <- floor(path_length / window_size)
  
  # create data frames
  # dsc:  drunk santa claus
  # vdsc: very drunk santa claus
  dsc <- NULL
  vdsc <- NULL
  
  vdsc_score <- rep_len(c(1, -1), path_length)
  
  for (i in 1:end_iteration) {
    
    # path_length_i <- i * window_size
    # path_i <- path[1:path_length_i]
    # 
    # # drunk santa claus
    # dsc_i <- as.vector(tabulate(path_i, n_nodes))
    # dsc <- rbind(dsc, dsc_i)
    
    start_path <- (i-1) * window_size + 1
    end_path <- i * window_size
    path_i <- path[start_path:end_path]
    
    dsc_i <- as.vector(tabulate(path_i, n_nodes))
    dsc <- rbind(dsc, dsc_i)
    
    # # very drunk santa claus
    # vdsc_table <- table(
    #   rep_len(c(1, -1), path_length_i),
    #   factor(path_i, seq(n_nodes))
    # )
    # vdsc_i <- as.vector(-diff(vdsc_table))
    # vdsc <- rbind(vdsc, vdsc_i)
    
    vdsc_score_i <- vdsc_score[start_path:end_path]
    
    vdsc_table <- table(vdsc_score_i, factor(path_i, seq(n_nodes)))
    vdsc_i <- as.vector(-diff(vdsc_table))
    vdsc <- rbind(vdsc, vdsc_i)
    
    # if (i %% 1000 == 0) {print(i)}
  }
  
  dsc <- as.data.frame(dsc)
  dsc <- cumsum(dsc)
  rownames(dsc) <- (1:end_iteration) * window_size
  colnames(dsc) <- node_list # make sure every single node has been
                             # visited at least once
  
  vdsc <- as.data.frame(vdsc)
  vdsc <- cumsum(vdsc)
  rownames(vdsc) <- (1:end_iteration) * window_size
  colnames(vdsc) <- node_list
  
  if (save_data) {
    # drunk santa claus
    file_name_dsc <- paste0(
      "cvc_dsc", "&",
      "adj=", adj_code, "&",
      "path_length=", path_length, "&",
      "convergent_count=", convergent_count
    )
    file_name_dsc <- paste0(file_name_dsc, ".csv")
    write.csv(dsc, file_name_dsc, row.names = FALSE)

    # very drunk santa claus
    file_name_vdsc <- paste0(
      "cvc_vdsc", "&",
      "adj=", adj_code, "&",
      "path_length=", path_length, "&",
      "convergent_count=", convergent_count
    )
    file_name_vdsc <- paste0(file_name_vdsc, ".csv")
    write.csv(vdsc, file_name_vdsc, row.names = FALSE)
  }

    if (plot_data) {
    # opar <- par(mfrow = c(2, 1))
    col_line <- rgb(0, 0, 0, 0.2)
    
    ts_dsc <- ts(
      dsc,
      start = window_size,
      end = end_iteration * window_size,
      frequency = 1 / window_size
    )
    ts.plot(ts_dsc / (1:end_iteration), col = col_line, xlab = "")
    abline(h = convergent_count, col = 2, lty = 2)
    title(xlab = "n Steps", ylab = "Normalized DSC Visits")
    
    ts_vdsc <- ts(
      vdsc,
      start = window_size,
      end = end_iteration * window_size,
      frequency = 1 / window_size
    )
    ts.plot(ts_vdsc / (1:end_iteration), col = col_line, xlab = "")
    abline(h = 0, col = 2, lty = 2)
    title(xlab = "n Steps", ylab = "Normalized vDSC Visits")
    # on.exit(par(opar))
  }
  
  return(
    list(
      cvc_dsc = dsc,
      cvc_vdsc = vdsc
    )
  )
}
