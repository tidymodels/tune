# multi-predict bare

    Code
      c5_multi %>% tidyr::unnest(.pred)
    Output
      # A tibble: 75 x 2
         trees .pred_class
         <dbl> <fct>      
       1    20 WS         
       2    30 WS         
       3    40 WS         
       4    20 PS         
       5    30 PS         
       6    40 WS         
       7    20 PS         
       8    30 PS         
       9    40 PS         
      10    20 PS         
      # ... with 65 more rows

# multi-predict grid

    Code
      c5_search %>% collect_metrics()
    Output
      # A tibble: 100 x 7
         trees .metric .estimator  mean     n std_err .config               
         <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                 
       1     1 roc_auc binary     0.797     2 0.0108  Preprocessor1_Model001
       2     2 roc_auc binary     0.792     2 0.00333 Preprocessor1_Model002
       3     3 roc_auc binary     0.790     2 0.00454 Preprocessor1_Model003
       4     4 roc_auc binary     0.805     2 0.00522 Preprocessor1_Model004
       5     5 roc_auc binary     0.814     2 0.00666 Preprocessor1_Model005
       6     6 roc_auc binary     0.824     2 0.0146  Preprocessor1_Model006
       7     7 roc_auc binary     0.820     2 0.00877 Preprocessor1_Model007
       8     8 roc_auc binary     0.831     2 0.00386 Preprocessor1_Model008
       9     9 roc_auc binary     0.834     2 0.00338 Preprocessor1_Model009
      10    10 roc_auc binary     0.842     2 0.0109  Preprocessor1_Model010
      # ... with 90 more rows

