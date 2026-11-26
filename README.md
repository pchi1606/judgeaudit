# judgeaudit 0.1.0

Design and analysis of human-gold-standard audits of an
LLM-as-judge / AI annotator.

**What it does not do:** it never calls an LLM, uses no network
and no API keys. It works offline on human gold labels and judge
labels you already have.

## Installation

judgeaudit is **not on CRAN** and has not been released anywhere.
This is a local development prototype. To try the development
version from a local source checkout:

```r
# from the package root, in a terminal:
#   R CMD INSTALL .
# or, inside R, with the working directory at the package root:
install.packages(".", repos = NULL, type = "source")
```

## Functions

- `judge_design()` — how many human labels you need: required
  positives, negatives and total audit n for target CI widths on
  sensitivity / specificity (and an explicitly approximate kappa
  target).
- `judge_audit()` — agreement analysis: confusion matrix,
  observed agreement, Cohen's kappa with CI, sensitivity /
  specificity with Wilson CIs, human vs judge prevalence, and a
  kappa-paradox warning at extreme prevalence.
- `judge_adjust()` — consequence: Rogan-Gladen
  misclassification-adjusted prevalence with a delta-method CI
  propagating uncertainty in sensitivity, specificity and
  apparent prevalence. Returns `valid = FALSE` with `NA` when
  Se + Sp <= 1.

## Example

```r
d <- judge_design(0.85, 0.90, prevalence = 0.30,
                  halfwidth_se = 0.05, halfwidth_sp = 0.05)
d$total_n                       # required audit size
h <- c(1,1,0,0,1,0,1,0,0,1); j <- c(1,0,0,0,1,0,1,1,0,1)
a <- judge_audit(h, j)          # kappa, Se, Sp with CIs
adj <- judge_adjust(0.325, 0.85, 0.90, n_main = 2000,
                    n_audit_pos = 196, n_audit_neg = 139)
adj$adjusted                    # Rogan-Gladen adjusted prevalence
```

The audit counts 196 / 139 in the last call are the design's
*required* positives / negatives (`d$n_pos` / `d$n_neg`) for the
two half-width targets. They are not the *expected* counts at
the required total: at `d$total_n` = 654 and prevalence 0.30 a
random audit sample is expected to contain 197 positives and
457 negatives.

## Validation status

Simulation validation runs in two arms, both against
pre-stated bars (coverage in 0.93-0.97; mean sensitivity CI
width within +/-10% of the 0.100 target), 1000 reps each, true
prevalence 0.30, Se 0.85, Sp 0.90, main N = 2000, design audit
n = 654. Stratified arm (audit cell sizes fixed at the design
expectations 197 / 457; seed 20261123): coverage = 0.9520,
mean width = 0.0989 (ratio 0.989) — both bars pass; a fresh-seed
retest (seed 20261126) gives coverage 0.9500, ratio 0.989.
Unstratified arm (audit drawn as one random sample of the
design total, the sampling model `judge_design()` plans for;
seed 20261125): coverage = 0.9520, mean width = 0.0992
(ratio 0.992) — both bars pass. The simulation scripts
(`validation/run_validation.R`,
`validation/run_validation_unstratified.R`,
`validation/run_fresh_retest.R`) are the auditable source of
every number above. Scope: a single well-behaved scenario,
away from boundaries — coverage near prevalence 0 or 1, at
small audit sizes, or with Se + Sp close to 1 is NOT validated.
Standalone test runner: 41/41 checks pass; testthat edition-3
suite: 92 expectations, 0 failures (also under R CMD check).
R CMD check --as-cran on the final build: 0 errors, 1 warning
+ 2 notes, all environmental or informational (VM locale;
pandoc absence; incoming-feasibility listing) - see
`validation/check_summary.txt`. Full record:
`validation/validation_results.txt`,
`validation/validation_results_unstratified.txt` and
`validation/` (local-only audit record; that folder is
excluded from the built package, so these numbers are
verifiable from this source tree, not from the installed
package).

## Status

Local prototype only. Not submitted anywhere. Maintainer
email: prabhu.evals@gmail.com (set 2026-11-24).
