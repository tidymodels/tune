# check for no arguments pass to ...

    Code
      empty_ellipses(a = 5)
    Condition
      Warning:
      The `...` are not used in this function but 1 object was passed: "a"

---

    Code
      empty_ellipses(a = 5, b = 1)
    Condition
      Warning:
      The `...` are not used in this function but 2 objects were passed: "a" and "b"

---

    Code
      empty_ellipses(5)
    Condition
      Warning:
      The `...` are not used in this function but 1 unnamed object was passed.

---

    Code
      empty_ellipses(1, 5)
    Condition
      Warning:
      The `...` are not used in this function but 2 unnamed objects were passed.

---

    Code
      empty_ellipses(1, second = 2)
    Condition
      Warning:
      The `...` are not used in this function but 2 objects were passed.

