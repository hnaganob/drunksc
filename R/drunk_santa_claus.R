drunk_santa_claus <- function(
    adj,
    n_xmas = 100,
    n_presents = 1000,
    n_santa = 1,
    start_house = NULL,
    seed = NULL,
    save_data = FALSE) {

  if (is.null(seed)) {
    set.seed(as.integer(Sys.time()))
  } else {
    set.seed(seed)
  }

  n_houses <- nrow(adj)

  # start_house ----
  # start_house <- rep(start_house, length.out = n_xmas)
  if (is.null(start_house)) {
    start_house <- rep(seq_len(n_houses), length.out = n_xmas)
  } else {
    start_house <- rep(start_house, length.out = n_xmas)
  }

  # drunk santa claus ----

  # prepare template
  dsc <- vdsc <- matrix(0, nrow = n_xmas, ncol = n_houses)
  colnames(dsc) <- colnames(vdsc) <- colnames(adj)

  # add progress bar
  pb <- txtProgressBar(min = 0, max = n_xmas, style = 3)

  for (xmas in seq_len(n_xmas)) {
    dsc_temp <- numeric(n_houses)
    vdsc_temp <- numeric(n_houses)

    start_node <- start_house[xmas]

    for (santa in seq_len(n_santa)) {
      # start_node <- start_house_santa[xmas]
      path <- random_walk(adj, n_steps = n_presents, start_node = start_node)

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
