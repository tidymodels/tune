# mis-specified extract function

    Code
      res_extract_warning <- fit_resamples(wf, boots, control = control_resamples(
        extract = raise_warning))
    Message
      ! Bootstrap1: preprocessor 1/1, model 1/1 (extracts): AHHH
      ! Bootstrap2: preprocessor 1/1, model 1/1 (extracts): AHHH
      ! Bootstrap3: preprocessor 1/1, model 1/1 (extracts): AHHH

---

    Code
      res_extract_error <- fit_resamples(wf, boots, control = control_resamples(
        extract = raise_error))
    Message
      x Bootstrap1: preprocessor 1/1, model 1/1 (extracts): Error in extractor(object): AHHH
      x Bootstrap2: preprocessor 1/1, model 1/1 (extracts): Error in extractor(object): AHHH
      x Bootstrap3: preprocessor 1/1, model 1/1 (extracts): Error in extractor(object): AHHH

---

    Code
      res_extract_both <- fit_resamples(wf, boots, control = control_resamples(
        extract = raise_both))
    Message
      ! Bootstrap1: preprocessor 1/1, model 1/1 (extracts): AH
      x Bootstrap1: preprocessor 1/1, model 1/1 (extracts): Error in extractor(object): AHHH
      ! Bootstrap2: preprocessor 1/1, model 1/1 (extracts): AH
      x Bootstrap2: preprocessor 1/1, model 1/1 (extracts): Error in extractor(object): AHHH
      ! Bootstrap3: preprocessor 1/1, model 1/1 (extracts): AH
      x Bootstrap3: preprocessor 1/1, model 1/1 (extracts): Error in extractor(object): AHHH

---

    Code
      res_extract_error_once <- fit_resamples(wf, boots, control = control_resamples(
        extract = raise_error_once))
    Message
      x Bootstrap1: preprocessor 1/1, model 1/1 (extracts): Error in extractor(object): oh no

---

    Code
      res_extract_warning
    Output
      # Resampling results
      # Bootstrap sampling 
      # A tibble: 3 x 5
        splits          id         .metrics         .notes           .extracts       
        <list>          <chr>      <list>           <list>           <list>          
      1 <split [32/13]> Bootstrap1 <tibble [2 x 4]> <tibble [1 x 4]> <tibble [1 x 2]>
      2 <split [32/17]> Bootstrap2 <tibble [2 x 4]> <tibble [1 x 4]> <tibble [1 x 2]>
      3 <split [32/11]> Bootstrap3 <tibble [2 x 4]> <tibble [1 x 4]> <tibble [1 x 2]>
      
      There were issues with some computations:
      
        - Warning(s) x3: AHHH
      
      Run `show_notes(.Last.tune.result)` for more information.

---

    Code
      res_extract_error
    Output
      # Resampling results
      # Bootstrap sampling 
      # A tibble: 3 x 5
        splits          id         .metrics         .notes           .extracts       
        <list>          <chr>      <list>           <list>           <list>          
      1 <split [32/13]> Bootstrap1 <tibble [2 x 4]> <tibble [1 x 4]> <tibble [1 x 2]>
      2 <split [32/17]> Bootstrap2 <tibble [2 x 4]> <tibble [1 x 4]> <tibble [1 x 2]>
      3 <split [32/11]> Bootstrap3 <tibble [2 x 4]> <tibble [1 x 4]> <tibble [1 x 2]>
      
      There were issues with some computations:
      
        - Error(s) x3: Error in extractor(object): AHHH
      
      Run `show_notes(.Last.tune.result)` for more information.

---

    Code
      res_extract_both
    Output
      # Resampling results
      # Bootstrap sampling 
      # A tibble: 3 x 5
        splits          id         .metrics         .notes           .extracts       
        <list>          <chr>      <list>           <list>           <list>          
      1 <split [32/13]> Bootstrap1 <tibble [2 x 4]> <tibble [2 x 4]> <tibble [1 x 2]>
      2 <split [32/17]> Bootstrap2 <tibble [2 x 4]> <tibble [2 x 4]> <tibble [1 x 2]>
      3 <split [32/11]> Bootstrap3 <tibble [2 x 4]> <tibble [2 x 4]> <tibble [1 x 2]>
      
      There were issues with some computations:
      
        - Error(s) x3: Error in extractor(object): AHHH
        - Warning(s) x3: AH
      
      Run `show_notes(.Last.tune.result)` for more information.

---

    Code
      res_extract_error_once
    Output
      # Resampling results
      # Bootstrap sampling 
      # A tibble: 3 x 5
        splits          id         .metrics         .notes           .extracts       
        <list>          <chr>      <list>           <list>           <list>          
      1 <split [32/13]> Bootstrap1 <tibble [2 x 4]> <tibble [1 x 4]> <tibble [1 x 2]>
      2 <split [32/17]> Bootstrap2 <tibble [2 x 4]> <tibble [0 x 4]> <tibble [1 x 2]>
      3 <split [32/11]> Bootstrap3 <tibble [2 x 4]> <tibble [0 x 4]> <tibble [1 x 2]>
      
      There were issues with some computations:
      
        - Error(s) x1: Error in extractor(object): oh no
      
      Run `show_notes(.Last.tune.result)` for more information.

---

    Code
      res_extract_warning$.notes[[1]]
    Output
      # A tibble: 1 x 4
        location                               type    note  trace              
        <chr>                                  <chr>   <chr> <list>             
      1 preprocessor 1/1, model 1/1 (extracts) warning AHHH  <rlng_trc [46 x 5]>

---

    Code
      res_extract_error$.extracts[[1]]$.extracts[[1]]
    Output
      [1] "Error in extractor(object): AHHH\n"
      attr(,"class")
      [1] "try-error"
      attr(,"condition")
      <simpleError in extractor(object): AHHH>

---

    Code
      res_extract_error$.notes[[1]]
    Output
      # A tibble: 1 x 4
        location                               type  note                   trace     
        <chr>                                  <chr> <chr>                  <list>    
      1 preprocessor 1/1, model 1/1 (extracts) error Error in extractor(ob~ <rlng_trc>

---

    Code
      res_extract_both$.extracts[[1]]$.extracts[[1]]
    Output
      [1] "Error in extractor(object): AHHH\n"
      attr(,"class")
      [1] "try-error"
      attr(,"condition")
      <simpleError in extractor(object): AHHH>

---

    Code
      res_extract_error_once$.extracts[[1]]$.extracts[[1]]
    Output
      [1] "Error in extractor(object): oh no\n"
      attr(,"class")
      [1] "try-error"
      attr(,"condition")
      <simpleError in extractor(object): oh no>

