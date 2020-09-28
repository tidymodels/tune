---
name: "Bug Report"
about: Submit a bug report to help us improve tidymodels
---

### Tips for a helpful bug report: 

 * Please include a **minimal reproducible example,** a reprex, to demonstrate the bug. If you've never heard of a reprex before, start by reading "[What is a reprex](https://github.com/tidyverse/reprex#what-is-a-reprex)" and following the advice there. If we can't reproduce a bug, we can't fit it. 

 * Here is a good example bug report: [#46](https://github.com/tidymodels/tune/issues/46)
 
 * We don't want you to use confidential data; you can use a dataset from [modeldata](https://modeldata.tidymodels.org/reference/index.html), blind the data, or simulate other data to demonstrate your bug. The functions [`caret::twoClassSim()`](https://www.rdocumentation.org/packages/caret/versions/6.0-84/topics/SLC14_1) or [`caret::SLC14_1()`](https://www.rdocumentation.org/packages/caret/versions/6.0-84/topics/SLC14_1) might be good tools to simulate data for you. 

 * Unless the problem is explicitly about parallel processing, please run sequentially. _Even if_ it is about parallel processing, please make sure that it runs sequentially first.

 * Use `set.seed()` to ensure any randomness in your code is reproducible.

 * Please check <https://stackoverflow.com/> or <https://community.rstudio.com/> to see if someone has already reported the same problem (see: [Yihui's Rule](https://yihui.name/en/2017/08/so-gh-email/)). 

 * You might need to install these packages to create a reproducible example or share session info:

```r
install.packages(c("reprex", "sessioninfo"), repos = "http://cran.r-project.org")
```

When you are ready to file the bug 🐛 report, please delete everything above this line:
< -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

## The problem

I'm having trouble with ...

## Reproducible example

```r
## copy your code to the clipboard and run:
reprex::reprex(si = TRUE)
```

