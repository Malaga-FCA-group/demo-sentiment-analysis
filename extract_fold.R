extract_fold <- function(dataset, classes,
                         fold_ids) {

  trainset <- dataset[-fold_ids, ]
  testset <- dataset[fold_ids, ]
  trainclasses <- classes[-fold_ids]
  testclasses <- classes[fold_ids]

  return(list(
    trainset = trainset,
    testset = testset,
    trainclasses = trainclasses,
    testclasses = testclasses
  ))

}
