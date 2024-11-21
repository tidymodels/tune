# bad inputs

    Code
      filter_parameters(collect_metrics(svm_reg_results), parameters = tibble::tibble(
        `%^*#` = 1))
    Condition
      Error in `filter_parameters()`:
      ! `collect_metrics(svm_reg_results)` should have class <tune_results>; a tibble was passed.

---

    Code
      filter_parameters(svm_reg_results, parameters = tibble::tibble(soup = 1))
    Condition
      Error in `filter_by_join()`:
      ! There are no columns in `parameters` that match with "svm_reg_results".

---

    Code
      filter_parameters(svm_reg_results, tibble::tibble(soup = 1))
    Condition
      Error in `filter_parameters()`:
      ! An element passed to `...` is a data frame rather than a filter expression.
      i Did you forget to name the `parameters` argument?

---

    Code
      filter_parameters(svm_reg_results, cost < 1, tibble::tibble(soup = 1))
    Condition
      Error in `filter_parameters()`:
      ! An element passed to `...` is a data frame rather than a filter expression.
      i Did you forget to name the `parameters` argument?

---

    Code
      filter_parameters(svm_reg_results, parameters = tibble::tibble(`%^*#` = 1 / 3))
    Condition
      Error in `filter_by_join()`:
      ! No parameter combinations were selected using your subset.

---

    Code
      filter_parameters(svm_reg_results, parameters = tibble::tibble(`%^*#` = 1,
        soup = 2))
    Condition
      Warning:
      The column `soup` passed in `parameters` is not needed and will be ignored.
    Output
      # Tuning results
      # 10-fold cross-validation repeated 5 times 
      # A tibble: 50 x 6
         splits           id      id2    .metrics          .notes   .predictions      
         <list>           <chr>   <chr>  <list>            <list>   <list>            
       1 <split [711/80]> Repeat1 Fold01 <tibble [30 x 7]> <tibble> <tibble [800 x 7]>
       2 <split [712/79]> Repeat1 Fold02 <tibble [30 x 7]> <tibble> <tibble [790 x 7]>
       3 <split [712/79]> Repeat1 Fold03 <tibble [30 x 7]> <tibble> <tibble [790 x 7]>
       4 <split [712/79]> Repeat1 Fold04 <tibble [30 x 7]> <tibble> <tibble [790 x 7]>
       5 <split [712/79]> Repeat1 Fold05 <tibble [30 x 7]> <tibble> <tibble [790 x 7]>
       6 <split [712/79]> Repeat1 Fold06 <tibble [30 x 7]> <tibble> <tibble [790 x 7]>
       7 <split [712/79]> Repeat1 Fold07 <tibble [30 x 7]> <tibble> <tibble [790 x 7]>
       8 <split [712/79]> Repeat1 Fold08 <tibble [30 x 7]> <tibble> <tibble [790 x 7]>
       9 <split [712/79]> Repeat1 Fold09 <tibble [30 x 7]> <tibble> <tibble [790 x 7]>
      10 <split [712/79]> Repeat1 Fold10 <tibble [30 x 7]> <tibble> <tibble [790 x 7]>
      # i 40 more rows

---

    Code
      res <- filter_parameters(svm_reg_results, parameters = tibble::tibble(`%^*#` = 1,
        soup = 2, boop = 3))
    Condition
      Warning:
      The columns `soup` and `boop` passed in `parameters` are not needed and will be ignored.

---

    Code
      res <- filter_parameters(svm_reg_results, parameters = tibble::tibble(`%^*#` = 1,
        soup = 2, boop = 3, loop = 4))
    Condition
      Warning:
      The columns `soup`, `boop`, and `loop` passed in `parameters` are not needed and will be ignored.

