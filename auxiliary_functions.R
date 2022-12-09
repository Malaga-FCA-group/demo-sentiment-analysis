process_text <- function(text, remove_hash = TRUE) {

  regex_urls <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
  regex_mentions <- "@\\w+"
  regex_numbers <- "[0-9]*"
  regex_punct <- "[^\\w\\s]"
  text <- text %>%
    str_remove_all(pattern = regex_urls) %>%
    str_remove_all(pattern = regex_mentions) %>%
    str_replace_all("’", "'") %>%
    str_remove_all("\\b[a-zA-Z0-9\\']\\b") %>%
    str_replace_all("á", "a") %>%
    str_replace_all("é", "e") %>%
    str_replace_all("í", "i") %>%
    str_replace_all("ó", "o") %>%
    str_replace_all("ú", "u") %>%
    str_remove_all(regex_numbers) %>%
    str_remove_all(regex_punct)

  if (remove_hash) {

    text <- text %>%
      str_replace_all("#", " ") %>%
      str_replace_all("\\s+", " ")

  }

  return(text)

}

permute_factors <- function(true, pred) {

  L <- levels(true)
  L2 <- levels(pred)
  true <- as.numeric(true)
  pred <- as.numeric(pred)

  k <- max(c(pred, true), na.rm = TRUE)
  perms <- rbind(seq(k), allPerms(seq(k)))

  res <- vector(mode = "numeric", length = nrow(perms))
  for (i in seq(nrow(perms))) {

    ii <- perms[i, ]
    res[i] <- sum(ii[pred] == true, na.rm = TRUE)

  }

  i <- which.max(res)
  ii <- perms[i, ]

  k1 <- max(pred, na.rm = TRUE)
  res <- ii[pred] %>%
    factor(labels = L[seq(k1)]) %>%
    fct_expand(L)

  return(list(lev = L, val = res))

}

norm_by_rows <- function(A, modality = c("L2", "L1")) {

  modality <- match.arg(modality)

  if (modality == "L2") {

    B <- A^2 |> apply(1, sum) |> sqrt()
    A <- A / B

    return(A)

  }

  if (modality == "L1") {

    B <- A |> abs() |> apply(1, sum)
    A <- A / B

    return(A)

  }

  return(A)

}
