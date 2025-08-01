drunk_santa_claus <- function(
    adj,
    n_xmas = 100,
    n_presents = 1000,
    n_santa = 1,
    start_house = 1,
    seed = NULL,
    save_data = FALSE) {

  if (is.null(seed)) {
    set.seed(Sys.time())
  } else {
    set.seed(seed)
  }

  start_house <- rep(start_house, length.out = n_xmas)


  n_houses <- nrow(adj)
  dsc <- vdsc <- matrix(0, nrow = n_xmas, ncol = n_houses)
  colnames(dsc) <- colnames(vdsc) <- colnames(adj)

  # Add progress bar
  pb <- txtProgressBar(min = 0, max = n_xmas, style = 3)

  for (xmas in seq_len(n_xmas)) {
    dsc_temp <- numeric(n_houses)
    vdsc_temp <- numeric(n_houses)

    for (santa in seq_len(n_santa)) {
      path <- random_walk(adj, n_steps = n_presents, start_node = start_house[xmas])

      # drunk santa claus
      dsc_temp <- dsc_temp + tabulate(path, nbins = n_houses)

      # very drunk santa claus
      spins <- rep_len(c(1, -1), n_presents)
      fac <- factor(path, levels = seq_len(n_houses))
      vdsc_temp <- vdsc_temp + tapply(spins, fac, sum, default = 0)
    }

    dsc[xmas, ] <- dsc_temp
    vdsc[xmas, ] <- vdsc_temp

    setTxtProgressBar(pb, xmas) # update progress

  }

  close(pb) # close the progress bar


  return(list(dsc = dsc, vdsc = vdsc))
}
