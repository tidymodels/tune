# set configurations - single element

    Code
      tune:::get_config_key(tibble::tibble(polynimial_degree = 1:2), wflow_pre)
    Condition
      Error:
      ! Some parameters are tagged for tuning but are not in the grid: `degree`

---

    Code
      tune:::get_config_key(tibble::tibble(degree = 1:3), wflow_pre)
    Output
      # A tibble: 3 x 2
        degree .config        
         <int> <chr>          
      1      1 pre1_mod0_post0
      2      2 pre2_mod0_post0
      3      3 pre3_mod0_post0

---

    Code
      tune:::get_config_key(tibble::tibble(degree = 1), wflow_pre)
    Output
      # A tibble: 1 x 2
        degree .config        
         <dbl> <chr>          
      1      1 pre1_mod0_post0

---

    Code
      tune:::get_config_key(tibble::tibble(min_n = 10:12), wflow_mod)
    Output
      # A tibble: 3 x 2
        min_n .config        
        <int> <chr>          
      1    10 pre0_mod1_post0
      2    11 pre0_mod2_post0
      3    12 pre0_mod3_post0

---

    Code
      tune:::get_config_key(tibble::tibble(cut = seq(0, 1, length.out = 5)),
      wflow_post)
    Output
      # A tibble: 5 x 2
          cut .config        
        <dbl> <chr>          
      1  0    pre0_mod0_post1
      2  0.25 pre0_mod0_post2
      3  0.5  pre0_mod0_post3
      4  0.75 pre0_mod0_post4
      5  1    pre0_mod0_post5

# set configurations - two elements

    Code
      tune:::get_config_key(tidyr::crossing(degree = 1:3, neighbors = 1:2), wflow_1)
    Output
      # A tibble: 6 x 3
        neighbors degree .config        
            <int>  <int> <chr>          
      1         1      1 pre1_mod1_post0
      2         2      1 pre1_mod2_post0
      3         1      2 pre2_mod1_post0
      4         2      2 pre2_mod2_post0
      5         1      3 pre3_mod1_post0
      6         2      3 pre3_mod2_post0

---

    Code
      tune:::get_config_key(slice(tidyr::crossing(min_n = 1:3, cut = (1:5) / 5), -1),
      wflow_2)
    Output
      # A tibble: 14 x 3
         min_n   cut .config        
         <int> <dbl> <chr>          
       1     1   0.4 pre0_mod1_post2
       2     1   0.6 pre0_mod1_post3
       3     1   0.8 pre0_mod1_post4
       4     1   1   pre0_mod1_post5
       5     2   0.2 pre0_mod2_post1
       6     2   0.4 pre0_mod2_post2
       7     2   0.6 pre0_mod2_post3
       8     2   0.8 pre0_mod2_post4
       9     2   1   pre0_mod2_post5
      10     3   0.2 pre0_mod3_post1
      11     3   0.4 pre0_mod3_post2
      12     3   0.6 pre0_mod3_post3
      13     3   0.8 pre0_mod3_post4
      14     3   1   pre0_mod3_post5

---

    Code
      tune:::get_config_key(grid, wflow_3)
    Output
      # A tibble: 10 x 3
         degree   cut .config         
          <int> <dbl> <chr>           
       1      1 0     pre1_mod0_post01
       2      1 0.333 pre1_mod0_post04
       3      1 0.444 pre1_mod0_post05
       4      1 0.778 pre1_mod0_post08
       5      2 0.111 pre2_mod0_post02
       6      2 0.556 pre2_mod0_post06
       7      2 0.889 pre2_mod0_post09
       8      3 0.222 pre3_mod0_post03
       9      3 0.667 pre3_mod0_post07
      10      3 1     pre3_mod0_post10

# set configurations - all or none

    Code
      tune:::get_config_key(tibble(), wflow_none)
    Output
      # A tibble: 0 x 1
      # i 1 variable: .config <chr>

---

    Code
      tune:::get_config_key(grid, wflow_all)
    Output
      # A tibble: 5 x 4
        min_n degree   cut .config        
        <int>  <int> <dbl> <chr>          
      1    21      1  1    pre1_mod3_post5
      2    30      1  0.25 pre1_mod4_post2
      3     2      2  0    pre2_mod1_post1
      4    40      2  0.5  pre2_mod5_post3
      5    11      3  0.75 pre3_mod2_post4

