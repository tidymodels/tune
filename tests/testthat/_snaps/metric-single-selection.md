# selecting a metric

    Code
      choose_metric(ames_grid_search, "rmse")
    Output
      # A tibble: 1 x 3
        metric class          direction
        <chr>  <chr>          <chr>    
      1 rmse   numeric_metric minimize 

---

    Code
      choose_metric(ames_grid_search, NULL)
    Condition
      Warning:
      No value of `metric` was given; "rmse" will be used.
    Output
      # A tibble: 1 x 3
        metric class          direction
        <chr>  <chr>          <chr>    
      1 rmse   numeric_metric minimize 

---

    Code
      choose_metric(ames_grid_search, "potato")
    Condition
      Error:
      ! "potato" was not in the metric set. Please choose from: "rmse" and "rsq".

---

    Code
      choose_metric(ames_grid_search, c("rmse", "ccc"))
    Condition
      Warning:
      2 metrics were given; "rmse" will be used.
    Output
      # A tibble: 1 x 3
        metric class          direction
        <chr>  <chr>          <chr>    
      1 rmse   numeric_metric minimize 

