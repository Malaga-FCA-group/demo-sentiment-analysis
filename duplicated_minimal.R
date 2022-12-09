remove_duplicated <- function(M) {

  equals <- fcaR:::.equal_sets(M)
  n <- nrow(M)
  idx <- which(Matrix::colSums(equals) > 1)

  mark_to_remove <- rep(FALSE, ncol(M))

  if (length(idx) > 0) {

    for (i in idx) {

      j <- which(equals[, i] > 0)

      mark_to_remove[j[j > i]] <- TRUE

    }

  }

  M <- M[, which(!mark_to_remove)]
  if (!inherits(M, "Matrix")) {

    M <- Matrix(M, nrow = n, sparse = TRUE)

  }

  return(M)

}


minimal_elements <- function(S) {

  if (is.numeric(S)) {

    S <- Matrix(S, sparse = TRUE)

  }

  subsets <- fcaR:::.subset(S)

  idx <- which(colSums(subsets) == 1)

  return(remove_duplicated(Matrix(S[, idx], sparse = TRUE)))

}

maximal_elements <- function(S) {

  if (is.numeric(S)) {

    S <- Matrix(S, sparse = TRUE)

  }

  subsets <- fcaR:::.subset(S)

  idx <- which(rowSums(subsets) == 1)

  return(remove_duplicated(Matrix(S[, idx], sparse = TRUE)))

}


remove_duplicated_idx <- function(M) {

  equals <- fcaR:::.equal_sets(M)
  n <- nrow(M)
  idx <- which(Matrix::colSums(equals) > 1)

  mark_to_remove <- rep(FALSE, ncol(M))

  if (length(idx) > 0) {

    for (i in idx) {

      j <- which(equals[, i] > 0)

      mark_to_remove[j[j > i]] <- TRUE

    }

  }

  return(setdiff(seq(ncol(M)), which(mark_to_remove)))


}


minimal_elements_idx <- function(S) {

  if (is.numeric(S)) {

    S <- Matrix(S, sparse = TRUE)

  }

  subsets <- fcaR:::.subset(S)

  idx <- which(colSums(subsets) == 1)
  if (length(idx) > 1) {

    id2 <- remove_duplicated_idx(Matrix(S[, idx], sparse = TRUE))

  } else id2 <- 1

  return(idx[id2])

}

maximal_elements_idx <- function(S) {

  if (is.numeric(S)) {

    S <- Matrix(S, sparse = TRUE)

  }

  subsets <- fcaR:::.subset(S)

  idx <- which(rowSums(subsets) == 1)

  if (length(idx) > 1) {

    id2 <- remove_duplicated_idx(Matrix(S[, idx], sparse = TRUE))

  } else id2 <- 1

  return(idx[id2])

}
