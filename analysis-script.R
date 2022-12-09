## ----ImportData----------
tweet_data <- read_csv(
  file.path(code_folder, "tweet-train.csv"),
  show_col_types = FALSE)
texts <- tweet_data$text
names(texts) <- tweet_data$textID
sentiments <- tweet_data$sentiment
names(sentiments) <- tweet_data$textID

# Remove neutral texts
idx <- sentiments != "neutral"
texts <- texts[idx]
sentiments <- sentiments[idx]

## ----Lexicon-----------

prep_fun <- \(x) x |> tolower() |> process_text()
# If the user wants not to use stemming, use
# tok_fun <- tokenizers::tokenize_words
# instead of
tok_fun <- tokenizers::tokenize_word_stems

it_train <- itoken(texts,
                   preprocessor = prep_fun,
                   tokenizer = tok_fun,
                   ids = names(texts),
                   progressbar = FALSE)

vocab <- create_vocabulary(
  it_train,
  ngram = c(1L, 1L),
  stopwords = c(tm::stopwords(),
                tm::stopwords("SMART"),
                quanteda::stopwords()))

vocab <- prune_vocabulary(vocab,
                          term_count_min = params$term_count_min,
                          doc_proportion_max = params$doc_proportion_max,
                          doc_count_min = params$doc_count_min)
len <- vocab$term |> stringr::str_length()
ii <- which(len < 3) # Minimal length of terms is 3.
vocab <- vocab[-ii, ]

gram_vectorizer = vocab_vectorizer(vocab)


## ----DocumentTermMatrix------------------

DTM <- create_dtm(it_train, gram_vectorizer)
DTM[DTM > 0] <- 1
dtm <- as.matrix(DTM)

dtm <- unique(dtm)

## ----ExperimentConfiguration----------

nfolds <- as.numeric(params$nfolds)
quantile_stab <- as.numeric(params$quantile_stability)
neutral_threshold <- as.numeric(params$neutral_threshold)
quantile_support <- as.numeric(params$quantile_support)

## ----MakeFolds------------------------

set.seed(12345L)
folds <- sample(seq(nfolds),
                size = nrow(dtm),
                replace = TRUE)

for (ii in seq(nfolds)) {

  ## ----DataForFold_i---------------------------

  cat("Fold", ii, "\n")

  LL <- extract_fold(dtm,
                     sentiments[rownames(dtm)],
                     which(folds == ii))

  dataset <- LL$trainset
  testset <- LL$testset
  trainclasses <- LL$trainclasses
  testclasses <- LL$testclasses

  TEST <- as.DocumentTermMatrix(
    slam::as.simple_triplet_matrix(testset),
    weighting = weightBin)

  ## ----BuildDictionaries-------------
  DICT <- build_dictionaries(
    dataset = dataset,
    sentiments = sentiments,
    quantile_support = quantile_support,
    normalize = params$normalize)

  ## ----CheckSentiment------

  RES_weighted <- check_dictionaries(
    TEST,
    sentiments = sentiments[rownames(TEST)],
    dict_list = DICT$dictionaries$weighted)

  ## ----CheckWithStandardDictionaries------------
  RES_std <- check_standard_dictionaries(
    testset = TEST,
    sentiments = sentiments[rownames(TEST)])

  ## ----CheckOtherDictionaries-------------------
  ## Remove the suffix _stemmed to check the proposal without stemming.
  RES_other <- check_dictionaries(
    TEST,
    sentiments = sentiments[rownames(TEST)],
    dict_list = list(
      SentiWN_weight = SentiWordNet_weighted_stemmed,
      Subjectivity = Subjectivity_binary_stemmed,
      Opinion = opinion_binary_stemmed))


  ## ----ConcatenateResults----------

  RESULTS <- c(RES_std,
               RES_other,
               RES_weighted)

  if (ii == 1) {

    FINAL <- RESULTS
    POLARITY <- DICT$polarity
    DICTIONARIES <- data.frame(
      "Dictionary" = names(DICT$dictionaries$weighted),
      "LatticeSize" = unlist(DICT$sizes),
      "Positive" = sapply(DICT$dictionaries$binary, \(x) length(x$positiveWords)),
      "Negative" = sapply(DICT$dictionaries$binary, \(x) length(x$negativeWords)),
      "Weighted" = sapply(DICT$dictionaries$weighted, \(x) length(x$words)))

  } else {

    FINAL <- lapply(seq_along(RESULTS),
                    \(j) list(pred = c(FINAL[[j]]$pred, RESULTS[[j]]$pred),
                              ref = c(FINAL[[j]]$ref, RESULTS[[j]]$ref))) |>
      set_names(names(RESULTS))

    POLARITY <- lapply(names(POLARITY),
                       \(nm) POLARITY[[nm]] + DICT$polarity[[nm]]) |>
      set_names(names(DICT$polarity))

    DICTIONARIES <- rbind(
      DICTIONARIES,
      data.frame(
        "Dictionary" = names(DICT$dictionaries$weighted),
        "LatticeSize" = unlist(DICT$sizes),
        "Positive" = sapply(DICT$dictionaries$binary, \(x) length(x$positiveWords)),
        "Negative" = sapply(DICT$dictionaries$binary, \(x) length(x$negativeWords)),
        "Weighted" = sapply(DICT$dictionaries$weighted, \(x) length(x$words))))

  }

}


## ----AnalyzeResults------------------------

final_results <- FINAL |>
  lapply(\(x) analyze_results(x$pred, x$ref))

ALL_RES <- final_results |>
  sapply(\(x) c(x$conftable$overall["Accuracy"],
                x$conftable$byClass[c("Sensitivity", "Specificity")],
                AUC = x$auc)) |> t() |> round(4)

## ----ShowAndSave-----------------------

ALL_RES |> knitr::kable()

DICTIONARIES <- DICTIONARIES |>
  group_by(Dictionary) |>
  summarise(Positive = mean(Positive),
            Negative = mean(Negative),
            Weighted = mean(Weighted),
            LatticeSize = mean(LatticeSize))

