## ----init------------------------------
## The user will have to install all these packages if not
## already installed.
library(SentimentAnalysis)
library(tidytext)
library(tidyverse)
library(textstem)
library(fcaR)
library(tm)
library(quanteda)
library(tokenizers)
library(text2vec)
library(glue)
library(here)
library(permute)
library(ggplot2)
library(stringr)
library(forcats)


# Import auxiliary functions
code_folder <- here::here()

source(here("extract_fold.R"))
source(here("auxiliary_functions.R"))
source(here("build_dictionaries.R"))
source(here("check_dictionaries.R"))
source(here("analyze_results.R"))


# Load dictionaries
# Stemmed
opinion_binary_stemmed <- readRDS(
  file = here("dictionaries",
              "opinion_binary_stemmed.RDS"))

Subjectivity_binary_stemmed <- readRDS(
  file = here("dictionaries",
              "subjectivity_binary_stemmed.RDS"))

SentiWordNet_weighted_stemmed <- readRDS(
  file = here("dictionaries",
              "SentiWordNet_weighted_stemmed.RDS"))

# Not stemmed
opinion_binary <- readRDS(
  file = here("dictionaries",
              "opinion_binary.RDS"))

Subjectivity_binary <- readRDS(
  file = here("dictionaries",
              "subjectivity_binary.RDS"))

SentiWordNet_weighted <- readRDS(
  file = here("dictionaries",
              "SentiWordNet_weighted.RDS"))

## ----Parameters--------------------------

support <- c(0.1, 0.2, 0.4, 0.6)
doc_count_min <- c(50, 100, 150, 200)

AGGREGATE <- NULL
DICTS <- NULL

for (s in support) {

  for (d in doc_count_min) {

    glue::glue(
      "minsupp = {s}; doc_count_min = {d}"
    ) |> cat()
    cat("\n")

    params <- list(
      nfolds = 4,
      quantile_support = s,
      normalize = TRUE,
      norm_modality = "L1",
      term_count_min = 50, #50
      doc_proportion_max = 0.5, #0.5
      doc_count_min = d) #150

    # Launch analysis for this set of parameters
    source(file.path(code_folder, "analysis-script.R"))

    AGGR <- data.frame("Support" = s,
                       "Terms" = ncol(dtm),
                       "Dictionary" = rownames(ALL_RES),
                       ALL_RES)

    # Aggregate all the results
    AGGREGATE <- rbind(AGGREGATE,
                       AGGR)

    DICTS <- rbind(DICTS,
                   data.frame(
                     "Support" = s,
                     DICTIONARIES))

  }

}

## -----Results------------------------------
ORIGINAL_AGGREGATE <- AGGREGATE
AGGREGATE$Dictionary[AGGREGATE$Dictionary == "SentiWN_weight"] <- "SentiWordNet"

# Table
final_dicts <- c("QDAP", "GI", "LM",
                 "SentiWordNet", "Subjectivity",
                 "Opinion", "all", "maximal",
                 "supp", "supp_maximal")
std_dicts <- c("QDAP", "GI", "LM",
               "SentiWordNet", "Subjectivity",
               "Opinion")
nosupp_dicts <- c("all", "maximal")

AGGREGATE$Support[AGGREGATE$Dictionary %in% c(std_dicts, nosupp_dicts)] <- 1
TABLE1 <- AGGREGATE |>
  select(-Terms) |>
  filter(Dictionary %in% final_dicts) |>
  group_by(Dictionary, Support) |>
  summarise(Accuracy = mean(Accuracy) |> round(3),
            Sensitivity = mean(Sensitivity) |> round(3),
            Specificity = mean(Specificity) |> round(3),
            AUC = mean(AUC) |> round(3),
            .groups = "keep")

TABLE1[c(6, 2, 3, 7, 8, 5, 1, 4, 9:16), ] |>
  knitr::kable(format = "latex",
               booktabs = TRUE)

# Influence of minsupp
## On accuracy
AGGREGATE |>
  filter(Dictionary %in% c("all", "maximal", "supp", "supp_maximal"))  |>
  group_by(Dictionary, Terms, Support) |>
  summarise(Accuracy = mean(Accuracy) |> round(3),
            Sensitivity = mean(Sensitivity) |> round(3),
            Specificity = mean(Specificity) |> round(3),
            AUC = mean(AUC) |> round(3),
            .groups = "keep") |>
  pivot_wider(
    id_cols = c(Dictionary, Support),
    names_from = Terms,
    values_from = AUC
  ) |>
  knitr::kable(format = "latex",
               booktabs = TRUE)

## On number of concepts to compute
DICTS |>
  filter(Dictionary %in% c("all", "supp"))  |>
  group_by(Dictionary, Weighted, Support) |>
  summarise(LatticeSize = mean(LatticeSize) |> round(3),
            .groups = "keep") |>
  pivot_wider(
    id_cols = c(Dictionary, Support),
    names_from = Weighted,
    values_from = LatticeSize
  ) |>
  knitr::kable(format = "latex",
               booktabs = TRUE)

# AUC vs DTM size
df <- AGGREGATE |>
  filter(Dictionary %in% final_dicts) |>
  filter(Support >= 0.6) |>
  select(-Support, Dictionary, Terms, AUC)

df$Dictionary[df$Dictionary == "supp_maximal"] <- "supp+maximal"
df <- df |>
  mutate(Dictionary = as.factor(Dictionary)) |>
  mutate(Dictionary = forcats::fct_reorder(Dictionary, AUC, mean, .desc = TRUE))

cbbPalette <- c("#c5b0d5", "#8c510a", "#01665e",
                "#bf812d", "#d01c8b", "#C0B442",
                "#0072B2", "#D55E00", "#80cdc1", "#35978f")
df |>
  ggplot(aes(x = Terms, y = AUC, color = Dictionary)) +
  geom_line(aes(linetype = Dictionary)) +
  geom_point(aes(shape = Dictionary), size = 3) +
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4)) +
  theme_minimal() +
  scale_color_manual(values = cbbPalette)

# Dictionary sizes
DICTS |>
  group_by(Dictionary, Weighted) |>
  summarise(Positive = mean(Positive),
            Negative = mean(Negative)) |>
  rename(Total = Weighted) |>
  mutate(Value = paste0(Positive, " / ",
                        Negative)) |>
  select(Dictionary, Total, Value) |>
  pivot_wider(id_cols = Dictionary,
              names_from = Total,
              values_from = Value) |>
  knitr::kable(format = "latex",
               booktabs = TRUE)

# Main positive and negative terms
# This has to be done anter running only one of the configurations
main_terms <- function(pol, q = 0.95) {

  pos <- data.frame(weight = pol[, "positive"],
                    term = rownames(pol))
  qp <- quantile(pos$weight, q)
  pos_words <- pos |>
    filter(weight >= qp) |>
    arrange(desc(weight)) |>
    pull()

  neg <- data.frame(weight = pol[, "negative"],
                    term = rownames(pol))
  qn <- quantile(neg$weight, q)
  neg_words <- neg |>
    filter(weight >= qn) |>
    arrange(desc(weight)) |>
    pull()

  m <- length(pos_words)
  n <- length(neg_words)
  N <- max(c(m, n))
  res <- matrix("", nrow = N, ncol = 2)
  colnames(res) <- c("Positive", "Negative")

  res[seq(m), 1] <- pos_words
  res[seq(n), 2] <- neg_words


  return(res)

}

posneg_terms <- POLARITY |>
  lapply(\(x) main_terms(x, 0.9))

N <- posneg_terms |> sapply(nrow) |> max()
M <- matrix("", ncol = 2 * length(posneg_terms), nrow = N)
for (i in seq_along(posneg_terms)) {

  A <- posneg_terms[[i]]
  M[seq(nrow(A)), seq(2 * i - 1, 2 * i)] <- A

}

M
