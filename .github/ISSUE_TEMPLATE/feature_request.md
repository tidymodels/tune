---
name: "Feature Request"
about: Suggest a change or new feature in tidymodels
---

### Tips for a helpful feature request: 

 * See our [contributing guidelines](https://github.com/tidyverse/tune/blob/master/.github/CONTRIBUTING.md) and [development guide](https://www.tidymodels.org/contribute/) for more on design goals and how to contribute.

 * Please include a **minimal reproducible example,** a reprex, to demonstrate your feature idea when appropriate. If you've never heard of a reprex before, start by reading "[What is a reprex](https://github.com/tidyverse/reprex#what-is-a-reprex)" and following the advice there. A reproducible example can be effective at demonstrating the case for a new feature. 

 * We don't want you to use confidential data; you can use a dataset from [modeldata](https://modeldata.tidymodels.org/reference/index.html), blind the data, or simulate other data to demonstrate your bug. The functions [`caret::twoClassSim()`](https://www.rdocumentation.org/packages/caret/versions/6.0-84/topics/SLC14_1) or [`caret::SLC14_1()`](https://www.rdocumentation.org/packages/caret/versions/6.0-84/topics/SLC14_1) might be good tools to simulate data for you. 

 * Unless the feature is explicitly about parallel processing, please run sequentially. _Even if_ it is about parallel processing, please make sure that it runs sequentially first.

 * Use `set.seed()` to ensure any randomness in your code is reproducible.

 * Please check <https://stackoverflow.com/> or <https://community.rstudio.com/> to see if someone has already asked the same question (see: [Yihui's Rule](https://yihui.name/en/2017/08/so-gh-email/)). 

 * You might need to install these packages to create a reproducible example or share session info:

```r
install.packages(c("reprex", "sessioninfo"), repos = "http://cran.r-project.org")
```

When you are ready to file the feature ✨ request, please delete everything above this line:
< -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

## Feature

In situations when ...

