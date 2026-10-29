
.CalcCatchAtSizeFleet <- function(key, selectivity, sel_mode = c("length", "age"),
                                  landings_N, discards_N, useCpp = TRUE) {
  sel_mode <- match.arg(sel_mode)

  d_key <- dim(key)
  d_N   <- dim(landings_N)

  nSim   <- d_N[1]
  nClass <- d_key[3]
  nYear  <- d_N[3]
  nArea  <- d_N[4]

  key_nSim <- d_key[1]
  sel_ok   <- TRUE
  if (sel_mode == "length") {
    d_sel  <- dim(selectivity)
    sel_nSim <- d_sel[1]
    sel_ok <- d_sel[3] == nYear && d_sel[4] == nArea &&
      sel_nSim %in% c(1L, nSim)
  }

  dims_ok <- d_key[4] == nYear && d_key[5] == nArea &&
    key_nSim %in% c(1L, nSim) &&
    dim(discards_N)[3] == nYear && dim(discards_N)[4] == nArea &&
    sel_ok

  if (!dims_ok) {
    cli::cli_abort(
      "Dimension mismatch in {.fn .CalcCatchAtSizeFleet}: `key`/`selectivity` are not aligned with `landings_N`/`discards_N`.",
      .internal = TRUE
    )
  }

  if (useCpp) {
    .CatchAtSizeFleetCpp(key, selectivity, sel_mode, landings_N, discards_N,
                        d_key, d_N, nSim, nClass, nYear, nArea)
  } else {
    .CatchAtSizeFleetFastR(key, selectivity, sel_mode, landings_N, discards_N,
                          d_key, nSim, nClass, nYear, nArea, key_nSim)
  }
}

.CatchAtSizeFleetCpp <- function(key, selectivity, sel_mode, landings_N, discards_N,
                                 d_key, d_N, nSim, nClass, nYear, nArea) {
  classes <- as.numeric(dimnames(key)$Class)
  dn_out  <- list(Sim   = dimnames(landings_N)$Sim,
                  Class = classes,
                  Year  = dimnames(landings_N)$Year,
                  Area  = dimnames(landings_N)$Area)

  sel_mode_length <- identical(sel_mode, "length")

  res <- CalcCatchAtSizeFleet_(
    key             = key,
    key_dim         = as.integer(d_key),
    selectivity     = if (sel_mode_length) selectivity else numeric(0),
    sel_dim         = if (sel_mode_length) as.integer(dim(selectivity)) else rep(1L, 4L),
    sel_mode_length = sel_mode_length,
    landings_N      = landings_N,
    discards_N      = discards_N,
    N_dim           = as.integer(d_N)
  )

  out_dim <- c(nSim, nClass, nYear, nArea)
  LAS <- array(res$LAS, dim = out_dim, dimnames = dn_out)
  DAS <- array(res$DAS, dim = out_dim, dimnames = dn_out)

  list(LAS = LAS, DAS = DAS)
}

.CatchAtSizeFleetFastR <- function(key, selectivity, sel_mode, landings_N, discards_N,
                                   d_key, nSim, nClass, nYear, nArea, key_nSim) {
  nAge <- d_key[2]

  classes <- as.numeric(dimnames(key)$Class)
  dn_out  <- list(Sim   = dimnames(landings_N)$Sim,
                  Class = classes,
                  Year  = dimnames(landings_N)$Year,
                  Area  = dimnames(landings_N)$Area)

  LAS <- array(0, dim = c(nSim, nClass, nYear, nArea), dimnames = dn_out)
  DAS <- array(0, dim = c(nSim, nClass, nYear, nArea), dimnames = dn_out)

  key_raw <- key;         dimnames(key_raw) <- NULL
  Nl_raw  <- landings_N;  dimnames(Nl_raw)  <- NULL
  Nd_raw  <- discards_N;  dimnames(Nd_raw)  <- NULL

  sim_idx_key <- if (key_nSim == 1L) rep.int(1L, nSim) else seq_len(nSim)

  sel_mode_length <- identical(sel_mode, "length")
  if (sel_mode_length) {
    sel_raw <- selectivity; dimnames(sel_raw) <- NULL
    sel_nSim <- dim(selectivity)[1]
    sim_idx_sel <- if (sel_nSim == 1L) rep.int(1L, nSim) else seq_len(nSim)
    sim_rep_age <- rep.int(seq_len(nSim), nAge)
  }

  eps <- .Machine$double.eps

  for (ar in seq_len(nArea)) {
    for (yr in seq_len(nYear)) {

      key_slice <- key_raw[sim_idx_key, , , yr, ar, drop = FALSE]
      dim(key_slice) <- c(nSim * nAge, nClass)

      if (sel_mode_length) {
        sel_slice <- sel_raw[sim_idx_sel, , yr, ar, drop = FALSE]
        dim(sel_slice) <- c(nSim, nClass)
        sel_expanded <- sel_slice[sim_rep_age, , drop = FALSE]

        weighted <- key_slice * sel_expanded
        denom    <- rowSums(weighted)
        denom[denom == 0] <- eps
        cond_key <- weighted / denom
        cond_key[!is.finite(cond_key)] <- 0
      } else {
        cond_key <- key_slice
      }

      Nl_vec <- as.vector(Nl_raw[, , yr, ar, drop = FALSE])
      Nd_vec <- as.vector(Nd_raw[, , yr, ar, drop = FALSE])

      contrib_l <- cond_key * Nl_vec
      contrib_d <- cond_key * Nd_vec
      dim(contrib_l) <- c(nSim, nAge, nClass)
      dim(contrib_d) <- c(nSim, nAge, nClass)

      LAS[, , yr, ar] <- .SumMiddleDim(contrib_l, nSim, nAge, nClass)
      DAS[, , yr, ar] <- .SumMiddleDim(contrib_d, nSim, nAge, nClass)
    }
  }

  list(LAS = LAS, DAS = DAS)
}

.SumMiddleDim <- function(arr, d1, d2, d3) {
  perm <- aperm(arr, c(2L, 1L, 3L))
  dim(perm) <- c(d2, d1 * d3)
  summed <- colSums(perm)
  dim(summed) <- c(d1, d3)
  summed
}
