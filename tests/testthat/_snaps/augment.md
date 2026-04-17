# augment fit_resamples

    Code
      names(aug_1)
    Output
      [1] "Class"        ".pred_class"  ".pred_Class1" ".pred_Class2" "A"           
      [6] "B"           

---

    Code
      augment(fit_1, hey = "you")
    Condition
      Error in `augment()`:
      ! `...` must be empty.
      x Problematic argument:
      * hey = "you"

---

    Code
      aug_2 <- augment(fit_2)
    Condition
      Warning:
      The original data had 791 rows but there were 593 hold-out predictions.

---

    Code
      names(aug_2)
    Output
      [1] "Class"        ".pred_class"  ".pred_Class1" ".pred_Class2" "A"           
      [6] "B"           

# augment tune_grid

    Code
      names(aug_1)
    Output
       [1] "mpg"    ".pred"  ".resid" "cyl"    "disp"   "hp"     "drat"   "wt"    
       [9] "qsec"   "vs"     "am"     "gear"   "carb"  

---

    Code
      augment(fit_1, parameters = list(cost = 3))
    Condition
      Error in `augment()`:
      ! `parameters` should be a single row data frame.

---

    Code
      augment(fit_1, parameters = data.frame(cost = 3:4))
    Condition
      Error in `augment()`:
      ! `parameters` should be a single row data frame.

---

    Code
      augment(fit_1, cost = 4)
    Condition
      Error in `augment()`:
      ! `...` must be empty.
      x Problematic argument:
      * cost = 4

---

    Code
      names(aug_3)
    Output
       [1] "mpg"    ".pred"  ".resid" "cyl"    "disp"   "hp"     "drat"   "wt"    
       [9] "qsec"   "vs"     "am"     "gear"   "carb"  

# augment last_fit

    Code
      names(aug_1)
    Output
      [1] "Class"        ".pred_class"  ".pred_Class1" ".pred_Class2" "A"           
      [6] "B"           

---

    Code
      augment(fit_1, potato = TRUE)
    Condition
      Error in `augment()`:
      ! `...` must be empty.
      x Problematic argument:
      * potato = TRUE

