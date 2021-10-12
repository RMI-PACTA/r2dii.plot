# if with bad `sector` errors gracefully

    Each value of `sector` must be one of these:
    power, automotive, oil&gas, fossil fuels.
    x You passed: bad.
    i Do you need to see valid values in this dataset?:
    technology_colours

---

    Each value of `sector` must be one of these:
    power, automotive, oil&gas, fossil fuels.
    x You passed: bad.
    i Do you need to see valid values in this dataset?:
    technology_colours

# if with bad `technologies` errors gracefully

    Each value of `technologies` must be one of these:
    ice_hdv, ice, hybrid_hdv, hybrid, fuelcell, electric_hdv, electric.
    x You passed: bad.
    i Do you need to see valid values in this dataset?:
    filter(technology_colours, .data$sector == some_sector)

---

    Each value of `technologies` must be one of these:
    ice_hdv, ice, hybrid_hdv, hybrid, fuelcell, electric_hdv, electric.
    x You passed: bad.
    i Do you need to see valid values in this dataset?:
    filter(technology_colours, .data$sector == some_sector)

