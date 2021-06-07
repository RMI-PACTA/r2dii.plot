# with more than one value of some columns errors gracefully

    `scenario_source` must have a single value. It has: 1, 2.

---

    `sector` must have a single value. It has: 1, 2.

---

    `region` must have a single value. It has: 1, 2.

# with bad metric errors gracefully

    Can't find values to recode as 'portfolio'.

# with multiple distinct values in some columns errors gracefully

    `sector` must have a single value. It has: 1, 2.

---

    `technology` must have a single value. It has: 1, 2.

---

    `region` must have a single value. It has: 1, 2.

---

    `scenario_source` must have a single value. It has: 1, 2.

# if `normalize` isn't length-1 errors gracefully

    `normalize` must be of length 1, not 2.

# if `normalize` isn't logical errors gracefully

    is.logical(normalize) is not TRUE

