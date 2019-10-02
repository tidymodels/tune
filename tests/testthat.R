library(testthat)
library(tune)

if (requireNamespace("xml2")) {
  test_check("tune", reporter = MultiReporter$new(reporters = list(JunitReporter$new(file = "test-results.xml"), CheckReporter$new())))
} else {
  test_check("tune")
}
