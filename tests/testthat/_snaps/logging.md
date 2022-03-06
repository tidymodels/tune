# low-level messages

    Code
      tune:::siren("a", "werewolf")
    Error <simpleError>
      'arg' should be one of "warning", "go", "danger", "success", "info"

---

    Code
      tune:::siren("bat", "info")
    Message <simpleMessage>
      i bat

---

    Code
      tune:::siren("bat", "go")
    Message <simpleMessage>
      > bat

---

    Code
      tune:::siren("bat", "danger")
    Message <simpleMessage>
      x bat

---

    Code
      tune:::siren("bat", "warning")
    Message <simpleMessage>
      ! bat

---

    Code
      tune:::siren("bat", "success")
    Message <simpleMessage>
      v bat

# tune_log

    Code
      tune:::tune_log(ctrl_t, rs, task = "cube", type = "go")
    Message <simpleMessage>
      > Fold01: cube
    Output
      NULL

---

    Code
      tune:::tune_log(ctrl_t, NULL, task = "cube", type = "go")
    Message <simpleMessage>
      > cube
    Output
      NULL

---

    Code
      tune:::tune_log(ctrl_t, rs, task = "cube", type = "success")
    Message <simpleMessage>
      v Fold01: cube
    Output
      NULL

# log issues

    Code
      expect_equal(tune:::log_problems(note_1, ctrl_f, rs, "toledo", res_1, bad_only = FALSE),
      dplyr::bind_rows(note_1, note_2))
    Message <simpleMessage>
      x Fold01: toledo: Error in log("a"): non-numeric argument to mathematical function

---

    Code
      expect_equal(tune:::log_problems(note_1, ctrl_f, rs, "toledo", res_3, bad_only = FALSE),
      dplyr::bind_rows(note_1, note_3))
    Message <simpleMessage>
      ! Fold01: toledo: NaNs produced

# catch and log issues

    Code
      out_1 <- tune:::catch_and_log(log("a"), ctrl_f, rs, "toledo", bad_only = FALSE,
      notes = null)
    Message <simpleMessage>
      x Fold01: toledo: Error in log("a"): non-numeric argument to mathematical function

---

    Code
      out_3 <- tune:::catch_and_log(log(-1), ctrl_f, rs, "toledo", bad_only = FALSE,
      notes = null)
    Message <simpleMessage>
      ! Fold01: toledo: NaNs produced

---

    Code
      out_5 <- tune:::catch_and_log(log("a"), ctrl_f, NULL, "toledo", bad_only = FALSE,
      notes = null)
    Message <simpleMessage>
      x toledo: Error in log("a"): non-numeric argument to mathematical function

---

    Code
      out_6 <- tune:::catch_and_log(log(-1), ctrl_f, NULL, "toledo", bad_only = FALSE,
      notes = null)
    Message <simpleMessage>
      ! toledo: NaNs produced

# logging iterations

    Code
      tune:::log_best(ctrl_t, 10, sc_1)
    Message <simpleMessage>
      
      -- Iteration 10 ----------------------------------------------------------------
      
      i Current best:		0.8=7 (@iter 2)
    Output
      NULL

# logging search info

    Code
      tune:::check_and_log_flow(ctrl_t, tb_1 %>% mutate(.mean = .mean * NA))
    Message <simpleMessage>
      x Skipping to next iteration
    Error <simpleError>
      no loop for break/next, jumping to top level

---

    Code
      tune:::check_and_log_flow(ctrl_t, tb_1 %>% mutate(.mean = .mean * NA) %>% slice(
        1))
    Message <simpleMessage>
      x Halting search
    Error <simpleError>
      no loop for break/next, jumping to top level

# current results

    Code
      tune:::log_progress(ctrl_t, tb_2, maximize = FALSE, objective = "a")
    Message <simpleMessage>
      (x) Newest results:	a=4 (+/-0.4)

---

    Code
      tune:::log_progress(ctrl_t, tb_2, maximize = TRUE, objective = "b")
    Message <simpleMessage>
      <3 Newest results:	b=8 (+/-0.8)

---

    Code
      tune:::log_progress(ctrl_t, tb_2, maximize = TRUE, objective = "a")
    Message <simpleMessage>
      <3 Newest results:	a=4 (+/-0.4)

# show parameters

    Code
      tune:::param_msg(ctrl_t, iris[1, 4:5])
    Message <simpleMessage>
      i Petal.Width=0.2, Species=setosa

# acquisition functions

    Code
      tune:::acq_summarizer(ctrl_t, 1, conf_bound(I))
    Message <simpleMessage>
      i Kappa value: 1

---

    Code
      tune:::acq_summarizer(ctrl_t, 1, exp_improve(I))
    Message <simpleMessage>
      i Trade-off value: 1

---

    Code
      tune:::acq_summarizer(ctrl_t, 1, prob_improve(I))
    Message <simpleMessage>
      i Trade-off value: 1

