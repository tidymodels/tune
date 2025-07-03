# loop execution code

    Code
      tune:::loop_call("resamples", "sequential", list())
    Output
      lapply(resamples, loop_over_all_stages, grid = grid, static = static)

---

    Code
      tune:::loop_call("resamples", "sequential", list(a = quote(a)))
    Output
      lapply(resamples, loop_over_all_stages, grid = grid, static = static, 
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
      future.apply::future_lapply(resamples, loop_over_all_stages, 
          grid = grid, static = static, future.label = "tune-grid-%d", 
          future.stdout = TRUE, future.seed = TRUE, future.packages = load_pkgs)

---

    Code
      tune:::loop_call("resamples", "future", list(a = quote(a)))
    Output
      future.apply::future_lapply(resamples, loop_over_all_stages, 
          grid = grid, static = static, a = a, future.label = "tune-grid-%d", 
          future.stdout = TRUE, future.seed = TRUE, future.packages = load_pkgs)

---

    Code
      tune:::loop_call("everything", "future", list())
    Output
      future.apply::future_lapply(inds, loop_over_all_stages2, resamples = resamples, 
          grid = candidates, static = static, future.label = "tune-grid-%d", 
          future.stdout = TRUE, future.seed = TRUE, future.packages = load_pkgs)

---

    Code
      tune:::loop_call("everything", "future", list(a = quote(a)))
    Output
      future.apply::future_lapply(inds, loop_over_all_stages2, resamples = resamples, 
          grid = candidates, static = static, a = a, future.label = "tune-grid-%d", 
          future.stdout = TRUE, future.seed = TRUE, future.packages = load_pkgs)

---

    Code
      tune:::loop_call("resamples", "mirai", list())
    Output
      eval_mirai(resamples, loop_over_all_stages, .args = list(grid = grid, 
          static = static))

---

    Code
      tune:::loop_call("resamples", "mirai", list(a = quote(a)))
    Output
      eval_mirai(resamples, loop_over_all_stages, .args = list(grid = grid, 
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

