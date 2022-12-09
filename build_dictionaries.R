build_dictionaries <- function(dataset,
                               sentiments,
                               quantile_support = 0.1,
                               normalize = TRUE,
                               norm_modality = "L2") {

  # Find concept lattice
  fc <- FormalContext$new(dataset)
  fc$find_concepts()
  concepts <- fc$concepts

  # Select concepts with minimum support
  supp_threshold <- quantile(concepts$support(),
                             quantile_support,
                             na.rm = TRUE)
  idx_supp <- which(concepts$support() > supp_threshold)

  # Polarity of each concept, according to its support
  CLOSED <- concepts$intents()
  EXTENTS <- concepts$extents()

  imp_class <- 2*(as.numeric(as.factor(sentiments[fc$objects])) - 1) - 1


  imps_cl2 <- (t(EXTENTS) %*% imp_class) / colSums(EXTENTS)
  imps_cl2[!is.finite(imps_cl2)] <- 0

  imps_cl <- factor(sign(imps_cl2),
                    levels = c(-1, 0, +1),
                    labels = c("negative", "neutral", "positive"))

  ## ----att_polarity_all---------------------

  INTENTS2 <- concepts$intents()
  n_concepts_all <- concepts$size()
  CL2 <- imps_cl

  att_polarity_all <- list()
  att <- fc$attributes

  for (sentiment in c("positive", "negative")) {

    M <- INTENTS2[, which(CL2 == sentiment)]
    att_polarity_all[[sentiment]] <- (Matrix::rowSums(M) / ncol(M)) %>%
      set_names(att)

  }

  att_polarity_all <- t(do.call(rbind, att_polarity_all))
  if (normalize)
    att_polarity_all <- norm_by_rows(
      att_polarity_all,
      modality = norm_modality)


  ## ----att_polarity_maximal---------------------

  INTENTS3 <- concepts$intents()[, -c(1, concepts$size())]
  n_concepts_maximal <- concepts$size()
  CL3 <- imps_cl[-c(1, concepts$size())]

  att_polarity_maximal <- list()
  att <- fc$attributes

  for (sentiment in c("positive", "negative")) {

    M <- INTENTS3[, which(CL3 == sentiment)]
    M <- maximal_elements(M)
    att_polarity_maximal[[sentiment]] <- (Matrix::rowSums(M) / ncol(M)) %>%
      set_names(att)

  }

  att_polarity_maximal <- t(do.call(rbind, att_polarity_maximal))
  if (normalize)
    att_polarity_maximal <- norm_by_rows(
      att_polarity_maximal,
      modality = norm_modality)

  ## ----att_polarity_support---------------------

  INTENTS2 <- concepts$intents()[, idx_supp]
  n_concepts_support <- length(idx_supp)

  CL2 <- imps_cl[idx_supp]

  att_polarity_support <- list()
  att <- fc$attributes

  for (sentiment in c("positive", "negative")) {

    M <- INTENTS2[, which(CL2 == sentiment)]
    att_polarity_support[[sentiment]] <- (Matrix::rowSums(M) / ncol(M)) %>%
      set_names(att)

  }

  att_polarity_support <- t(do.call(rbind, att_polarity_support))
  if (normalize)
    att_polarity_support <- norm_by_rows(
      att_polarity_support,
      modality = norm_modality)

  ## ----att_polarity_support_maximal---------------------

  INTENTS2 <- concepts$intents()[, idx_supp]
  n_concepts_support_maximal <- length(idx_supp)
  CL2 <- imps_cl[idx_supp]

  att_polarity_support_maximal <- list()
  att <- fc$attributes

  for (sentiment in c("positive", "negative")) {

    M <- INTENTS2[, which(CL2 == sentiment)]
    M <- maximal_elements(M)
    att_polarity_support_maximal[[sentiment]] <- (Matrix::rowSums(M) / ncol(M)) %>%
      set_names(att)

  }

  att_polarity_support_maximal <- t(do.call(rbind, att_polarity_support_maximal))
  if (normalize)
    att_polarity_support_maximal <- norm_by_rows(
      att_polarity_support_maximal,
      modality = norm_modality)

  ## ----build_dic, echo = TRUE------------

  d_all <- SentimentDictionaryWeighted(
    words = rownames(att_polarity_all),
    scores = att_polarity_all[, 1] - att_polarity_all[, 2])

  d_maximal <- SentimentDictionaryWeighted(
    words = rownames(att_polarity_maximal),
    scores = att_polarity_maximal[, 1] - att_polarity_maximal[, 2])

  d_supp <- SentimentDictionaryWeighted(
    words = rownames(att_polarity_support),
    scores = att_polarity_support[, 1] - att_polarity_support[, 2])

  d_supp_max <- SentimentDictionaryWeighted(
    words = rownames(att_polarity_support_maximal),
    scores = att_polarity_support_maximal[, 1] - att_polarity_support_maximal[, 2])


  ## ----positivenegative_words-------------
  scores_all <- (att_polarity_all[, 1] - att_polarity_all[, 2]) / (att_polarity_all[, 1] + att_polarity_all[, 2])
  d_all_bin <- SentimentDictionaryBinary(
    positiveWords = rownames(att_polarity_all)[scores_all > 0],
    negativeWords = rownames(att_polarity_all)[scores_all < 0])

  scores_maximal <- (att_polarity_maximal[, 1] - att_polarity_maximal[, 2]) / (att_polarity_maximal[, 1] + att_polarity_maximal[, 2])
  d_maximal_bin <- SentimentDictionaryBinary(
    positiveWords = rownames(att_polarity_maximal)[scores_maximal > 0],
    negativeWords = rownames(att_polarity_maximal)[scores_maximal < 0])

  scores_supp <- (att_polarity_support[, 1] - att_polarity_support[, 2]) / (att_polarity_support[, 1] + att_polarity_support[, 2])

  d_supp_bin <- SentimentDictionaryBinary(
    positiveWords = rownames(att_polarity_support)[scores_supp > 0],
    negativeWords = rownames(att_polarity_support)[scores_supp < 0])

  scores_supp_max <- (att_polarity_support_maximal[, 1] - att_polarity_support_maximal[, 2]) / (att_polarity_support_maximal[, 1] + att_polarity_support_maximal[, 2])

  d_supp_max_bin <- SentimentDictionaryBinary(
    positiveWords = rownames(att_polarity_support_maximal)[scores_supp_max > 0],
    negativeWords = rownames(att_polarity_support_maximal)[scores_supp_max < 0])


  return(list(

    polarity = list(
      all = att_polarity_all,
      maximal = att_polarity_maximal,
      supp = att_polarity_support,
      supp_maximal = att_polarity_support_maximal
    ),

    sizes = list(
      all = n_concepts_all,
      maximal = n_concepts_maximal,
      supp = n_concepts_support,
      supp_maximal = n_concepts_support_maximal
    ),

    dictionaries = list(

      binary = list(
        all = d_all_bin,
        maximal = d_maximal_bin,
        supp = d_supp_bin,
        supp_maximal = d_supp_max_bin
      ),

      weighted = list(
        all = d_all,
        maximal = d_maximal,
        supp = d_supp,
        supp_maximal = d_supp_max
      )

    )

  ))

}
