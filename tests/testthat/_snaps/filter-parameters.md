# bad inputs

    Code
      filter_parameters(collect_metrics(svm_reg_results), parameters = tibble::tibble(
        `%^*#` = 1))
    Condition
      Error in `filter_parameters()`:
      ! collect_metrics should have class 'tune_results'.
      * svm_reg_results should have class 'tune_results'.

---

    Code
      filter_parameters(svm_reg_results, parameters = tibble::tibble(soup = 1))
    Condition
      Error in `filter_by_join()`:
      ! There are no columns in 'parameters' that match with svm_reg_results

---

    Code
      filter_parameters(svm_reg_results, tibble::tibble(soup = 1))
    Condition
      Error in `dplyr::filter()`:
      ! Problem while computing `..1 = tibble::tibble(soup = 1)`.
      x Input `..1$soup` must be a logical vector, not a double.

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
      There are unneeded columns in `parameters` that were ignored: 'soup'
    Output
      # Tuning results
      # 10-fold cross-validation repeated 5 times 
      # A tibble: 50 x 6
         splits           id      id2    .metrics          .notes           .predict~1
         <list>           <chr>   <chr>  <list>            <list>           <list>    
       1 <split [711/80]> Repeat1 Fold01 <tibble [30 x 7]> <tibble [0 x 1]> <tibble>  
       2 <split [712/79]> Repeat1 Fold02 <tibble [30 x 7]> <tibble [0 x 1]> <tibble>  
       3 <split [712/79]> Repeat1 Fold03 <tibble [30 x 7]> <tibble [0 x 1]> <tibble>  
       4 <split [712/79]> Repeat1 Fold04 <tibble [30 x 7]> <tibble [0 x 1]> <tibble>  
       5 <split [712/79]> Repeat1 Fold05 <tibble [30 x 7]> <tibble [0 x 1]> <tibble>  
       6 <split [712/79]> Repeat1 Fold06 <tibble [30 x 7]> <tibble [0 x 1]> <tibble>  
       7 <split [712/79]> Repeat1 Fold07 <tibble [30 x 7]> <tibble [0 x 1]> <tibble>  
       8 <split [712/79]> Repeat1 Fold08 <tibble [30 x 7]> <tibble [0 x 1]> <tibble>  
       9 <split [712/79]> Repeat1 Fold09 <tibble [30 x 7]> <tibble [0 x 1]> <tibble>  
      10 <split [712/79]> Repeat1 Fold10 <tibble [30 x 7]> <tibble [0 x 1]> <tibble>  
      # ... with 40 more rows, and abbreviated variable name 1: .predictions

