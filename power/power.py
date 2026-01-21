#!/usr/bin/env python3
"""
Generate the minimum absolute percentage-point shift (Δ, in pp) needed to achieve ≥80% power
(after BH correction) for contrasts of each patient-variable level vs control.

Assumptions:
  • Two-sided tests with BH-adjusted α per category:
      - Sociodemographic: α_eff = 0.05 / 10 = 0.005
      - Medical:          α_eff = 0.05 / 7  ≈ 0.007142857
  • n per level (control vs variable) = 6 surgeries × 2 sexes × 50 reps = 600
  • Replicates are independent within each surgery–sex–variable combination → ICC = 0
  • Base proportions p0 ∈ {0.10, 0.20, …, 0.90}
  • Δ search grid: 0.1 pp to 20.0 pp in 0.1 pp increments

Outputs CSV to ./power.csv with columns:
  Base p0,Medical (m=7),Sociodemographic (m=10)
"""

import numpy as np
from math import sqrt
from scipy.stats import norm

OUTFILE = "./analysis/power/power.csv"

# Parameters
alpha_socio = 0.05 / 10.0      # 0.005
alpha_med = 0.05 / 7.0         # ≈0.007142857
n_per_level = 600.0            # per group (control and variable each)
p0_vals = np.round(np.linspace(0.10, 0.90, 9), 2)

# Δ grid in percentage points
min_pp = 0.1
max_pp = 20.0
res_pp = 0.1
deltas_pp = np.arange(min_pp, max_pp + 1e-9, res_pp)
deltas = deltas_pp / 100.0  # convert to proportion

def power_two_proportions(p0: float, p1: float, alpha: float, n: float) -> float:
    """Two-sided z-test power for independent proportions (no clustering)."""
    z_alpha2 = norm.ppf(1 - alpha / 2.0)
    var = p0 * (1 - p0) / n + p1 * (1 - p1) / n
    if var <= 0:
        return 1.0 if abs(p1 - p0) > 0 else 0.5
    delta = abs(p1 - p0)
    lambda_nc = delta / sqrt(var)
    return norm.cdf(-z_alpha2 - lambda_nc) + (1 - norm.cdf(z_alpha2 - lambda_nc))

rows = []
for p0 in p0_vals:
    # Medical category (α_eff ≈ 0.00714)
    med_min = None
    for d in deltas:
        p1 = min(max(p0 + d, 1e-12), 1 - 1e-12)
        pw = power_two_proportions(p0, p1, alpha_med, n_per_level)
        if pw >= 0.80:
            med_min = round(d * 100.0, 1)
            break

    # Sociodemographic category (α_eff = 0.005)
    socio_min = None
    for d in deltas:
        p1 = min(max(p0 + d, 1e-12), 1 - 1e-12)
        pw = power_two_proportions(p0, p1, alpha_socio, n_per_level)
        if pw >= 0.80:
            socio_min = round(d * 100.0, 1)
            break

    rows.append((f"{p0:.2f}", med_min, socio_min))

# Write CSV
with open(OUTFILE, "w", encoding="utf-8") as f:
    f.write("Base p0,Medical (m=7),Sociodemographic (m=10)\n")
    for base, med_min, socio_min in rows:
        med_str = "" if med_min is None else f"{med_min:.1f}"
        socio_str = "" if socio_min is None else f"{socio_min:.1f}"
        f.write(f"{base},{med_str},{socio_str}\n")