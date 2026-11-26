# judgeaudit 0.1.0

* First submission-preparation release (2026-11-25): version
  bumped from the 0.0.1 local prototype; maintainer email set
  to prabhu.evals@gmail.com (the placeholder carried through
  the 0.0.1 cycle is closed); ORCID recorded in Authors@R;
  `inst/CITATION` and README version references aligned to
  0.1.0. No statistical behaviour changed.

# judgeaudit 0.0.1

* Initial local prototype (not released, not on CRAN).
* `judge_design()`: audit sample-size planning for target CI
  widths on sensitivity / specificity (kappa target labelled
  approximate).
* `judge_audit()`: agreement analysis — Cohen's kappa, Wilson
  CIs for sensitivity / specificity, kappa-paradox warning.
* `judge_adjust()`: Rogan-Gladen misclassification-adjusted
  prevalence with delta-method CI.
* Gemini UI seat fix cycle (2026-11-24): `target_kappa_halfwidth`
  is now validated as a single non-NA number; kappa CI clamped
  to [-1, 1] with the SE approximation documented as potentially
  understating uncertainty; Rogan-Gladen CI bounds clamped to
  [0, 1] and both note branches state the truncation, clamping,
  and independence assumption; count arguments require whole
  numbers; `judge_audit()` warns when a class is absent
  (sensitivity/specificity undefined) and its print method
  handles NA kappa explicitly; the kappa-paradox check now
  considers both margins; validation gains an unstratified arm
  matching the sampling model `judge_design()` plans for;
  edge-case and print-method tests added. The maintainer email
  in DESCRIPTION remains a placeholder pending the author's
  decision (carried blocker).
* Standards pass (2026-11-23): Authors@R metadata, testthat
  edition-3 test layout, S3 print methods registered for all
  three result classes, shared internal validation helpers,
  references added to all help pages, `inst/CITATION`,
  `.Rbuildignore` excluding `validation/` and `battery/` from
  the built tarball. No statistical behaviour changed for valid
  inputs; `judge_audit()` now validates `conf_level` like the
  other two functions (invalid values error instead of
  producing a meaningless interval).
