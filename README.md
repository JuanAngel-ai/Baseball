# How similar will they throw to me?
### Spatial analysis of pitches thrown to Aaron Judge by Kevin Gausman and José Berríos

---

## Overview

This project introduces a spatial statistics approach to a classic sabermetrics question: **if you were a batter, how similar are the pitches of two different pitchers thrown to you?**

To answer this, we analyze every pitch thrown to Aaron Judge by Kevin Gausman and José Berríos and apply **Lee's L statistic** a bivariate spatial correlation coefficient to measure how similarly both pitchers distribute their pitches across the strike zone.

---

## Visualizations

![Pitch location heatmap](images/pitch_heatmap.png)
*Proportion of pitches thrown to each zone of the strike area. The white box represents Aaron Judge's rulebook strike zone.*

---

## Methodology

1. **Data collection** — pitch-level Statcast data retrieved from Baseball Savant via the `baseballr` package (seasons 2016–2025)
2. **Grid construction** — the strike zone area is divided into a 0.5 ft × 0.5 ft grid; each cell gets a pitch proportion for each pitcher
3. **Spatial weights** — queen contiguity neighbors built with `spdep::cell2nb()`
4. **Lee's L test** — `spdep::lee.mc()` with 9,999 Monte Carlo permutations, one-tailed (alternative = "greater")

---

## Results

| Statistic | Value |
|---|---|
| Lee's L | 0.52458 |
| Observed rank | 10,000 / 10,000 |
| p-value | 0.0001 |
| Alternative | greater |
| Simulations | 9,999 + 1 |

---

## Repo structure

```
├── README.md
├── analysis.qmd        # Main Quarto document
├── references.bib      # Bibliography
└── images/             # Saved plots for README preview
```

---

## Requirements

```r
install.packages(c(
  "baseballr",
  "tidyverse",
  "spdep",
  "DescTools",
  "ggtext",
  "knitr",
  "gt",
  "scales"
))
```

Data is fetched directly from Baseball Savant at runtime — no local data files needed.

---

## Further work

- Extend the analysis to more pitchers and batters
- Link pitch location zones to batting outcomes (hits, strikeouts, xBA)
- Explore why pitchers target specific zones based on batter tendencies
- Apply the same spatial framework to other sports

---

## Author

**Juan Ángel Pérez Córcoles**

---

## Data source

Baseball Savant / MLB Statcast — [baseballsavant.mlb.com](https://baseballsavant.mlb.com)
