check_dictionaries <- function(testset, sentiments, dict_list) {

  dict_list |>
    lapply(\(d) check_dict(testset, sentiments, d))

}

check_dict <- function(testset, sentiments, dict) {

  if (inherits(dict, "SentimentDictionaryBinary")) {

    res_all <- analyzeSentiment(
      testset,
      rules = list("LM" = list(ruleSentiment,
                               dict)),
      stemming = FALSE)

    rownames(res_all) <- rownames(testset)
    vv <- res_all$LM
    names(vv) <- rownames(res_all)

  } else {

    if (inherits(dict, "SentimentDictionaryWeighted")) {

      res_all <- analyzeSentiment(
        testset,
        rules = list("LM" = list(ruleLinearModel,
                                 dict)),
        stemming = FALSE)

      rownames(res_all) <- rownames(testset)
      vv <- res_all$LM
      names(vv) <- rownames(res_all)

    } else {

      stop("Not valid dictionary type.")

    }

  }

  sents1 <- c("negative", "positive")
  cl <- convertToBinaryResponse(as.vector(vv))
  names(cl) <- names(vv)
  cl <- forcats::fct_expand(cl, sents1)
  ref <- as_factor(sentiments[names(cl)]) %>%
    forcats::fct_expand(levels(cl))

  LL <- permute_factors(
    true = ref,
    pred = cl)

  cl <- LL$val

  return(list(pred = cl, ref = ref))

}

check_standard_dictionaries <- function(testset, sentiments) {

  res1 <- analyzeSentiment(testset,
                           stemming = FALSE)

  # QDAP
  cl_QDAP <- convertToBinaryResponse(res1$SentimentQDAP)

  # GI
  cl_GI <- convertToBinaryResponse(res1$SentimentGI)

  # HE
  cl_HE <- convertToBinaryResponse(res1$SentimentHE)

  # LM
  cl_LM <- convertToBinaryResponse(res1$SentimentLM)

  return(list(
    QDAP = list(pred = cl_QDAP,
                ref = as.factor(sentiments[rownames(testset)])),
    GI = list(pred = cl_GI,
              ref = as.factor(sentiments[rownames(testset)])),
    HE = list(pred = cl_HE,
              ref = as.factor(sentiments[rownames(testset)])),
    LM = list(pred = cl_LM,
              ref = as.factor(sentiments[rownames(testset)]))
  ))

}
