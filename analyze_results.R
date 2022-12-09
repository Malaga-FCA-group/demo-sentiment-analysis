analyze_results <- function(pred, ref) {

  return(list(
  conftable = caret::confusionMatrix(
    reference = ref,
    data = pred),
  auc = glmnet:::auc(ref, pred),
  f1 = ModelMetrics::f1Score(ref, pred)
  ))

}
