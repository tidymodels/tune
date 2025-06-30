# no parallelism

    Code
      tune:::choose_framework(verbose = TRUE)
    Message
      Neither mirai or future are loaded
    Output
      [1] "sequential"

# enable future parallelism

    Code
      tune:::choose_framework(verbose = TRUE)
    Message
      mirai is not loaded
      future is loaded with 1 worker
    Output
      [1] "sequential"

---

    Code
      tune:::choose_framework(verbose = TRUE)
    Message
      mirai is not loaded
      future is loaded with 2 workers
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
      mirai is not loaded
      future is loaded with 1 worker
    Output
      [1] "sequential"

---

    Code
      tune:::choose_framework(verbose = TRUE)
    Message
      Neither mirai or future are loaded
    Output
      [1] "sequential"

# enable mirai parallelism

    Code
      tune:::choose_framework(verbose = TRUE)
    Message
      mirai is loaded with 0 workers
      future is not loaded
    Output
      [1] "sequential"

---

    Code
      tune:::choose_framework(verbose = TRUE)
    Message
      mirai is loaded with 2 workers
      future is not loaded
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

# break parallelism tie

    Code
      tune:::choose_framework(verbose = TRUE)
    Message
      mirai is loaded with 2 workers
      future is loaded with 2 workers
      Multiple workers exist for both mirai and future; falling back to the default of mirai.
    Output
      [1] "mirai"

