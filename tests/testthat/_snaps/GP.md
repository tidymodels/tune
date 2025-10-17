# GP fit - svm - failure

    Code
      svm_gp <- tune:::fit_gp(collect_metrics(svm_results), pset = svm_set, metric = "accuracy",
      control = control_bayes(verbose = TRUE))
    Message
      (x) GP has a LOO R² of -6.1% and is unreliable.

---

    Code
      svm_scores <- tune:::pred_gp(svm_gp, pset = svm_set, size = 20, current = curr,
        control = ctrl)
    Message
      i Generating a candidate as far away from existing points as possible.

# GP scoring with failed model

    Code
      svm_gp <- tune:::fit_gp(collect_metrics(svm_results), pset = svm_set, metric = "accuracy",
      control = ctrl)
    Message
      (x) GP has a LOO R² of -6.1% and is unreliable.

---

    Code
      svm_scores <- tune:::pred_gp(svm_gp, pset = svm_set, size = 20, current = curr,
        control = ctrl)
    Message
      i Generating a candidate as far away from existing points as possible.

# GP fit - knn

    Code
      set.seed(1)
      knn_scores <- tune:::pred_gp(knn_gp, pset = knn_set, size = 20, current = mutate(
        knn_mtr, .iter = 0), control = ctrl)
    Message
      i Generating 20 candidates

