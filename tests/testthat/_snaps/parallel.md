# no parallelism

    Code
      tune:::choose_framework(verbose = TRUE)
    Message
      mirai is not active.
      future is active with 1 worker.
      Too few workers for parallel processing.
    Output
      [1] "sequential"

# enable future parallelism

    Code
      tune:::choose_framework(verbose = TRUE)
    Message
      mirai is not active.
      future is active with 1 worker.
      Too few workers for parallel processing.
    Output
      [1] "sequential"

---

    Code
      tune:::choose_framework(verbose = TRUE)
    Message
      mirai is not active.
      future is active with 2 workers.
      future will be used for parallel processing}.
    Output
      [1] "future"

---

    Code
      tune:::choose_framework(control = ctrl_no, verbose = TRUE)
    Output
      [1] "sequential"

---

    Code
      tune:::choose_framework(control = ctrl_java, verbose = TRUE)
    Message
      These packages cannot be used with explicit parallel processing: rJava.
    Output
      [1] "sequential"

---

    Code
      tune:::choose_framework(verbose = TRUE)
    Message
      mirai is not active.
      future is active with 1 worker.
      Too few workers for parallel processing.
    Output
      [1] "sequential"

---

    Code
      tune:::choose_framework(verbose = TRUE)
    Message
      mirai is not active.
      future is active with 1 worker.
      Too few workers for parallel processing.
    Output
      [1] "sequential"

# enable mirai parallelism

    Code
      tune:::choose_framework(verbose = TRUE)
    Message
      mirai is not active.
      future is active with 1 worker.
      Too few workers for parallel processing.
    Output
      [1] "sequential"

---

    Code
      tune:::choose_framework(verbose = TRUE)
    Message
      mirai is active with 2 workers.
      future is active with 1 worker.
      mirai will be used for parallel processing}.
    Output
      [1] "mirai"

---

    Code
      tune:::choose_framework(control = ctrl_no, verbose = TRUE)
    Output
      [1] "sequential"

---

    Code
      tune:::choose_framework(control = ctrl_java, verbose = TRUE)
    Message
      These packages cannot be used with explicit parallel processing: rJava.
    Output
      [1] "sequential"

---

    Code
      tune:::choose_framework(verbose = TRUE)
    Message
      mirai is active with 1 worker.
      future is active with 1 worker.
      Too few workers for parallel processing.
    Output
      [1] "sequential"

---

    Code
      tune:::choose_framework(verbose = TRUE)
    Message
      mirai is not active.
      future is active with 1 worker.
      Too few workers for parallel processing.
    Output
      [1] "sequential"

# break parallelism tie

    Code
      tune:::choose_framework(verbose = TRUE)
    Message
      mirai is active with 2 workers.
      future is active with 2 workers.
      Multiple workers exist for both mirai and future; falling back to the default of mirai.
    Output
      [1] "mirai"

# loop execution code

    Code
      tune:::loop_call("resamples", "sequential", list())
    Output
      lapply(resamples, .loop_over_all_stages, grid = grid, static = static)

---

    Code
      tune:::loop_call("resamples", "sequential", list(a = quote(a)))
    Output
      lapply(resamples, .loop_over_all_stages, grid = grid, static = static, 
          a = a)

---

    Code
      tune:::loop_call("everything", "sequential", list())
    Output
      lapply(inds, loop_over_all_stages2, resamples = resamples, grid = candidates, 
          static = static)

---

    Code
      tune:::loop_call("everything", "sequential", list(a = quote(a)))
    Output
      lapply(inds, loop_over_all_stages2, resamples = resamples, grid = candidates, 
          static = static, a = a)

---

    Code
      tune:::loop_call("resamples", "future", list())
    Output
      future.apply::future_lapply(resamples, .loop_over_all_stages, 
          grid = grid, static = static, future.label = "tune-grid-%d", 
          future.stdout = TRUE, future.seed = TRUE, future.globals = c("grid", 
          "static"))

---

    Code
      tune:::loop_call("resamples", "future", list(a = quote(a)))
    Output
      future.apply::future_lapply(resamples, .loop_over_all_stages, 
          grid = grid, static = static, a = a, future.label = "tune-grid-%d", 
          future.stdout = TRUE, future.seed = TRUE, future.globals = c("grid", 
          "static", "a"))

---

    Code
      tune:::loop_call("everything", "future", list())
    Output
      future.apply::future_lapply(inds, loop_over_all_stages2, resamples = resamples, 
          grid = candidates, static = static, future.label = "tune-grid-%d", 
          future.stdout = TRUE, future.seed = TRUE, future.globals = c("resamples", 
          "grid", "static"))

---

    Code
      tune:::loop_call("everything", "future", list(a = quote(a)))
    Output
      future.apply::future_lapply(inds, loop_over_all_stages2, resamples = resamples, 
          grid = candidates, static = static, a = a, future.label = "tune-grid-%d", 
          future.stdout = TRUE, future.seed = TRUE, future.globals = c("resamples", 
          "grid", "static", "a"))

---

    Code
      tune:::loop_call("resamples", "mirai", list())
    Output
      eval_mirai(resamples, .loop_over_all_stages, .args = list(grid = grid, 
          static = static))

---

    Code
      tune:::loop_call("resamples", "mirai", list(a = quote(a)))
    Output
      eval_mirai(resamples, .loop_over_all_stages, .args = list(grid = grid, 
          static = static, a = a))

---

    Code
      tune:::loop_call("everything", "mirai", list())
    Output
      eval_mirai(inds, loop_over_all_stages2, .args = list(resamples = resamples, 
          grid = candidates, static = static))

---

    Code
      tune:::loop_call("everything", "mirai", list(a = quote(a)))
    Output
      eval_mirai(inds, loop_over_all_stages2, .args = list(resamples = resamples, 
          grid = candidates, static = static, a = a))

