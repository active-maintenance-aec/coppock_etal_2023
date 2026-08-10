# Maintained reproduction of Coppock, Gross, Porter, Thorson and Wood (2023)


- [What this repository is](#what-this-repository-is)
  - [Folder layout](#folder-layout)
  - [How to reproduce](#how-to-reproduce)
- [The paper](#the-paper)
- [Does the deposited archive run?](#does-the-deposited-archive-run)
  - [Do the deposited objects follow from the deposited
    data?](#do-the-deposited-objects-follow-from-the-deposited-data)
- [Errata](#errata)
  - [Corrections the rewrite makes to the deposited
    code](#corrections-the-rewrite-makes-to-the-deposited-code)
  - [The persistence percentages are pooled one way in the main text and
    another in the
    appendix](#the-persistence-percentages-are-pooled-one-way-in-the-main-text-and-another-in-the-appendix)
  - [Two further quantities with no
    counterpart](#two-further-quantities-with-no-counterpart)
- [The ground truth](#the-ground-truth)
  - [Coverage, float by float](#coverage-float-by-float)
- [The extraction and the two
  instruments](#the-extraction-and-the-two-instruments)
- [The maintained rewrite](#the-maintained-rewrite)
- [Figure verification](#figure-verification)
- [Rewrite verification](#rewrite-verification)
- [R environment](#r-environment)

*Drafted by Claude Opus 5 under the supervision of Alex Coppock.*

## What this repository is

This repository re-runs the analysis behind a published article from its
deposited data, in current R, and lays every number the article prints
against what that analysis produces. It holds three things: a script
that fetches and verifies the authors’ replication archive, a rewrite of
the analysis using current packages, and a ground truth that records,
claim by claim, whether the published number reproduces.

|  |  |
|----|----|
| Article | Coppock A, Gross K, Porter E, Thorson E and Wood TJ (2023). “Conceptual Replication of Four Key Findings about Factual Corrections and Misinformation during the 2020 US Election: Evidence from Panel-Survey Experiments.” *British Journal of Political Science*. <https://doi.org/10.1017/S0007123422000631> |
| Replication archive | <https://doi.org/10.7910/DVN/WQXZUP> |
| Pre-analysis plan | <https://osf.io/2hnv5> |

### Folder layout

    download_original.R    fetches and verifies the deposited archive
    run_all.R              runs everything, in order
    original/              the deposited archive (not redistributed; fetched on demand)
    original_manifest.csv  every deposited file, its size and its published checksum
    maintained/            the rewrite; maintained/output/ holds every number it produces
    ground_truth/          the extraction, the comparison and the gates
    errata.qmd             corrections to the published article

### How to reproduce

Download the repository, open `coppock_etal_2023.Rproj`, and run:

``` r
source("run_all.R")
```

`run_all.R` fetches the deposited archive from Harvard Dataverse,
verifies every file against its published checksum and byte size, fits
every model, writes every figure and table, runs the deposited scripts
in a scratch copy, rebuilds the ground truth and re-verifies the
archive. Nothing writes into `original/`.

## The paper

Eight panel-survey experiments fielded on Lucid and Mechanical Turk
between September and November 2020, with 17,629 respondents across 24
experiments built on 21 distinct PolitiFact fact-checks. Each respondent
saw three topics; for each topic they were assigned to pure control,
misinformation, or misinformation followed by a fact-check. Belief
certainty in the false claim is the primary outcome, measured again
after at least a week.

The headline estimates are a meta-analytic misinformation effect of 4.3
points and a correction effect of -10.5 points on the 100-point belief
certainty scale.

## Does the deposited archive run?

The deposit is fourteen scripts, six data files and thirty-four saved
model objects, in a directory tree of 63 files. Every script was run in
a scratch copy, twice: once as the archive ships, and once stripped to
data plus code so that a script which passes only because the deposit
ships the object it reads is visible as a failure.

| pass               | clean | error |
|:-------------------|------:|------:|
| as_shipped         |    12 |     2 |
| stripped           |     7 |     7 |
| stripped_after_fit |    10 |     3 |

Deposited scripts by pass and outcome.

Two scripts fail in every pass, for reasons unrelated to the analysis.
`appendix_figure_3.R` asks the graphics device for a font family that is
not installed, and `appendix_figure_9.R` reads
`data/clean/all_panels_long.rds`, a path the deposit does not contain:
its copy of that file sits at `data/all_panels_long.rds`.

Stripping the copy to data plus code is what the second pass adds, and
it changes the answer: 7 of 14 scripts fail without the deposited model
objects, against 2 as shipped. Every one of the additional failures is a
`readRDS` of an object `fit_all_models.R` builds. Running
`fit_all_models.R` first and then the analysis scripts, which is the
third pass in the table above, leaves 3 failures. Two are the pair that
fail regardless. The third is `figure_5.R`, and it is a finding of its
own: it reads `fitted_models/meta_trait_dic_w1_all_adj`, a deposited
file with no extension, while the deposit’s own code writes that object
as `meta_trait_dic_w1_all_adj.rds`, which the archive does not contain.
The object survives only as a deposited artifact; nothing in the deposit
can regenerate it under the name the figure reads.

**The deposit writes nothing except model objects.** Grepping the
fourteen scripts for uncommented calls that touch a file returns
thirty-four `write_rds()` lines, all in `fit_all_models.R`, and nothing
else: no `ggsave`, no `write_csv`, no `sink`. Every figure is drawn to
the active device and every table is printed to the console as LaTeX. So
there is no deposited artifact to diff against, and a checksum result on
`original/` is a statement about the download rather than about the
analysis.

Running the deposit in place would not be harmless. Measured by
modification time in a scratch copy, `fit_all_models.R` overwrites 32
deposited model objects and adds 3 more, and the figure scripts leave an
`Rplots.pdf` behind.

### Do the deposited objects follow from the deposited data?

Every model the deposit saves was refitted from the survey data and
compared against the saved object.

| objects | estimates agree to 1e-9 | estimates disagree | cells missing on one side |
|---:|---:|---:|---:|
| 34 | 23 | 11 | 200 |

Deposited model objects against a refit from the deposited data.

| object | rows_deposited | rows_rewrite | estimate | archive_specification |
|:---|---:|---:|---:|---:|
| ft_targets_ates_w1_PID_adj | 138 | 132 | 28.7025088 | 0 |
| ates_w1_p3_adj | 6 | 6 | 12.8732396 | 0 |
| persistence_meta | 30 | 30 | 7.4147056 | NA |
| ates_w3_p3_adj | 6 | 6 | 7.0610233 | 0 |
| ft_targets_ates_w1_all_adj | 46 | 44 | 4.6340926 | 0 |
| ft_targets_meta_w1_PID_adj | 6 | 6 | 2.9569308 | NA |
| meta_trait_cates_w2_p2_adj | 18 | 18 | 2.0977864 | NA |
| iv_p3_adj | 6 | 6 | 1.4617805 | 0 |
| ft_targets_meta_w1_all_adj | 2 | 2 | 0.6069452 | NA |
| meta_trait_cates_w1_all_adj | 36 | 36 | 0.4848515 | NA |
| meta_trait_dic_w1_all_adj | 18 | 18 | 0.2771084 | NA |

The objects where the refit and the deposit disagree, and what the
deposit’s own specification gives.

The `archive_specification` column is the same comparison run under the
deposit’s own model formulas rather than the corrected ones, and where
it returns zero the disagreement above it is entirely the correction.
The two thermometer objects are the exception and are discussed under
the errata below.

Two properties of the comparison are worth stating because a single
summary number hides both. The rewrite reverses the confidence interval
the deposit prints backwards for every negated contrast, so a maximum
taken over all six numeric columns at once is the width of the widest
interval in the object and says nothing about agreement. And a cell one
side computes while the other returns as missing is invisible to any
difference, which is exactly how the political knowledge estimates move:
the two `estimatr` versions disagree about which HC2 standard errors are
estimable, so the pooling drops a different study.

## Errata

Four errors in the published article are corrected in
[`coppock_etal_2023_errata.pdf`](coppock_etal_2023_errata.pdf), whose
values are computed at render time from this repository’s output. None
of them changes a conclusion. In summary, and numbered as the note
numbers them: **1.** the reported total sample size is 17,681 where the
article’s own appendix table sums to 17,629; **2.** the count of
experiments in which misinformation significantly reduced accuracy is
given as twelve where the deposited estimates give 13; **3.** two
different appendix floats are both captioned Table 1; and **4.** the
four sentences reporting how much of the correction effect survives to a
later wave report a figure of the order a fixed-effect pool gives where
the appendix gives a random-effects pool of the same ratios, 48.9 per
cent at Wave 2 and 34.3 per cent at Wave 3.

### Corrections the rewrite makes to the deposited code

Four coding errors in the deposit are corrected in `maintained/`. Each
is unambiguous, in the sense that the code plainly does something other
than what the surrounding code says it is doing.

**The thermometer models are fitted on the wrong sample.** The deposit’s
loop over targets reads

``` r
filter(all_panels_long, fc == fc, topic_short_2 == topic_short_2)
```

Inside `filter()` the bare names resolve to the columns, so both
conditions compare a column with itself and neither subsets anything:
every target model is fitted on all eight panels pooled, including
respondents who were never shown the claim. The loop extracts `fc` and
`topic_short_2` one row at a time in order to subset, so the intent is
not in doubt. Applying the subset costs one of the twenty-three target
rows, because Panel 3 names Barack Obama as a target and never asked the
Barack Obama thermometer; that row is recorded in
`maintained/output/fits/ft_targets_unfittable.csv`.

This moves Figure 6. The published overall labels are -0.19 and 0.18;
the corrected estimates are -0.74 and 0.8. All eight of the figure’s
labels change, and with them two sentences of prose: the introduction’s
“smaller than one-quarter of a point” and the Results section’s “smaller
than half a point” are both true of the published figure and false of
the corrected one.

There is a second thing wrong with Figure 6 and it is not the rewrite’s
doing. Six of its eight labels are exactly what the deposited objects
give. The two Overall labels are not: the deposit returns -0.23 and 0.19
where the page prints -0.19 and 0.18. The figure script reads those two
labels straight out of `ft_targets_meta_w1_all_adj.rds` and rounds them
to two decimals, so the published pair cannot be recovered from the
archive as deposited. The cause is not established.

These stay findings here rather than errata. The published cells
reproduce from the deposit exactly, so what is wrong with them is a
matter of method rather than of arithmetic, and correcting a column of
cells the deposit reproduces exactly is a different act from correcting
a misstated number. The substantive claim is untouched either way: a
correction effect of 0.8 points on a 100-point feeling thermometer is as
close to nothing as 0.18 is.

**The adjusted three-wave models regress the wrong outcomes.** In the
deposit, `ates_w1_p3_adj` and `ates_w2_p3_adj` both regress
`outcome_w3`, `ates_w3_p3_adj` regresses `outcome_w2`, and `iv_p3_adj`
instruments `outcome_w2`. The unadjusted block immediately above pairs
each wave with its own outcome, and so does the covariate-adjusted
conditional-effect block below, so this is a copy-and-paste slip. Two
published artifacts show it. Appendix Table 7’s `w1` row holds the wave
3 estimate and its unlabelled row holds the wave 2 estimate, which is
why 7 of its 21 cells reproduce where the other six regression tables
reproduce completely. And Figure 4’s bottom-left panel draws its Wave 1
and Wave 2 points at the same height, because they are the same
estimate.

**Every negated confidence interval prints backwards.** The deposit’s
`filter_and_flip()` negates the estimate and both interval bounds for
the misinformation contrast without reversing their order, so those
cells print as `[upper, lower]`. 144 of the 492 published appendix cells
are printed this way. The interval itself is right; only the order is
wrong. The rewrite swaps the bounds back.

**The significance column of the appendix regression tables is not a
comparison of numbers.** The deposit’s table script reshapes each fit
with `reshape2::melt`, and where a fit carries a character column
outside `id.vars` every melted value becomes a string. The surviving
`if_else(p.value < 0.05, "* ", " ")` then compares two strings, so
`"0.0144"` sorts before `"0.05"` and earns a star while `"1.28e-10"`
sorts after it and does not. The effect is that the most significant
estimates in the table are the ones printed without a marker: the
published tables carry 205 stars where 256 cells have p \< 0.05. This is
deterministic, it is visible on the page once you know to look for it,
and it is a property of the presentation rather than of any estimate, so
it stays a finding here rather than an erratum.

### The persistence percentages are pooled one way in the main text and another in the appendix

The article states three times that fact-check effects persist “at 66
per cent” of their original magnitude after one week, once at the
sharper “66.4 per cent”, and “at 50 per cent” after more than two weeks.
The appendix reports the same quantity and gives different numbers.

The quantity the pre-analysis plan defines, and the one the appendix’s
own persistence tables print in their `ratio` rows, is the
per-experiment ratio of the later-wave coefficient to the Wave 1
coefficient, pooled across experiments by meta-analysis. Pooled with
random effects, which is what `metafor` does by default and what the
appendix’s meta-analysis rows carry, it is 48.9 per cent at Wave 2 and
34.3 per cent at Wave 3 with covariates, and 54.4 and 38.9 per cent
without them. Pooled with fixed effects, the same ratios give 64.9 and
70.8 per cent at Wave 2 and 52.2 and 63.3 per cent at Wave 3. The ratio
of the pooled Wave 2 effect to the pooled Wave 1 effect, a different
derivation of the same idea, gives 46.9 per cent, and the corresponding
Wave 3 figure is 39.7 per cent.

The published percentages are of the order the fixed-effect pool gives
and not of the order the appendix’s own rows give, so a reader of the
introduction takes away a materially different number from a reader of
the appendix. No combination of adjustment, estimator and sample returns
66.4 or 50 exactly. The article should have used random effects
throughout, and the four sentences carrying these figures are corrected
on that basis in `errata.qmd`. The Wave 3 correction is larger than the
pooling alone would make it, because the ratio behind appendix Table 7
also instruments the wrong outcome; see the three-wave finding above.

### Two further quantities with no counterpart

The appendix states that attitudinal outcomes were observed for 22
political figures and groups. The deposited data carry 14 feeling
thermometer columns. And appendix Table 9 reports a mean adjusted
r-squared for each of ten covariates across “our 21 experiments,
resulting in 210 separate linear models”; the deposit ships no code for
that table, and the article elsewhere describes 24 experiments. The
rewrite adds no script for it, since writing one would be estimating
something the deposited archive never did.

## The ground truth

`ground_truth/coppock_etal_2023_ground_truth.csv` carries one row per
published claim. `value_paper` is the string the article prints and is
the only column anyone typed; `value_script` is read out of the deposit
and `value_rewrite` out of `maintained/output/`, both by script. A value
agrees when the computed number, printed to the page’s own precision,
gives the same digits.

| claim_type   | does not reproduce | no verdict | reproduces | does not hold | holds |
|:-------------|-------------------:|-----------:|-----------:|--------------:|------:|
| definitional |                  3 |          9 |         21 |             0 |     0 |
| descriptive  |                  0 |          2 |          0 |             3 |    15 |
| pipeline     |                 21 |         11 |        130 |             0 |     0 |
| structural   |                  1 |         12 |         12 |             0 |     0 |
| transcribed  |                  0 |         14 |          0 |             0 |     0 |

Published claims by type and verdict.

| defect_locus   | rows |
|:---------------|-----:|
| archive        |   26 |
| environment    |    3 |
| paper_internal |    8 |
| unresolved     |    2 |

Where the fault lies, on every row that is not a clean match.

Every row that is not a clean match carries a `defect_locus`, and no
clean match carries one. The five values are `paper_internal` (the
article disagrees with its own tables or its own data), `archive` (the
deposit cannot support the claim), `environment` (R or a package moved
underneath it), `rewrite` (ours) and `unresolved` (cause not
established).

### Coverage, float by float

| float | printed numbers | covered | fraction | plotted quantities | counts verified |
|:---|---:|---:|---:|---:|---:|
| Appendix Figure 1 | 0 | 0 | NA | 36 | 1 |
| Appendix Figure 2 | 0 | 0 | NA | 72 | 1 |
| Appendix Figure 3 | 0 | 0 | NA | 21 | 1 |
| Appendix Figure 7 | 0 | 0 | NA | 48 | 1 |
| Appendix Figure 8 | 0 | 0 | NA | 108 | 1 |
| Appendix Figure 9 | 12 | 1 | 0.083 | 12 | 1 |
| Appendix Figures 4 to 6 | 0 | 0 | NA | 816 | 1 |
| Appendix regression table 1 | 400 | 400 | 1.000 | 400 | 0 |
| Appendix regression table 2 | 400 | 400 | 1.000 | 400 | 0 |
| Appendix regression table 3 | 400 | 400 | 1.000 | 400 | 0 |
| Appendix regression table 4 | 300 | 300 | 1.000 | 300 | 0 |
| Appendix regression table 5 | 300 | 300 | 1.000 | 300 | 0 |
| Appendix regression table 6 | 84 | 84 | 1.000 | 84 | 0 |
| Appendix regression table 7 | 84 | 28 | 0.333 | 84 | 0 |
| Appendix Table 1 (sample composition) | 206 | 9 | 0.044 | 206 | 1 |
| Appendix Table 8 | 72 | 0 | 0.000 | 72 | 0 |
| Appendix Table 9 | 10 | 1 | 0.100 | 10 | 0 |
| Figure 1 | 0 | 0 | NA | 72 | 2 |
| Figure 2 | 72 | 72 | 1.000 | 72 | 0 |
| Figure 3 | 68 | 0 | 0.000 | 104 | 1 |
| Figure 4 | 0 | 1 | NA | 30 | 1 |
| Figure 5 | 36 | 36 | 1.000 | 36 | 0 |
| Figure 6 | 8 | 8 | 1.000 | 8 | 0 |

Coverage per published float. Printed numbers are what a float puts on
its own face; plotted quantities are what it draws.

Seven of the appendix’s floats and two of the article’s print no numbers
at all. For those the checkable published quantity is the count of what
they plot, and that count is verified; the fraction is left empty rather
than reported as zero.

Two floats are covered thinly and both are worth stating plainly.
**Figure 3 is the largest gap in this repository.** It labels every
effect significantly different from zero and every one of its eight
meta-analytic summaries, roughly 68 printed numbers, and none of those
labels was transcribed. What is verified is that the figure plots 96
conditional effect estimates, and that those estimates are the
covariate-adjusted cells of appendix Tables 2 and 3, every one of which
reproduces. **Appendix Table 1** prints 206 numbers, of which the eight
panel sizes are checked individually and the rest are checked as counts
of non-empty cells.

The seven appendix regression tables are the bulk of the published
record: 492 cells, each printing an estimate, a standard error and two
interval bounds, so 1968 published numbers. All of them are parsed
positionally from the published PDF into
`ground_truth/published_appendix_values.csv` and compared cell by cell.

## The extraction and the two instruments

`ground_truth/published_claims.csv` is the extraction: 254 rows, one per
numeric claim in the article and its appendix, each classified by hand
as `pipeline`, `descriptive`, `definitional`, `structural` or
`transcribed`, and each carrying the string the page prints and the
precision it prints it at. Spelled-out numbers were swept for separately
from digits, since no token scan sees “eight panel experiments”, “twelve
out of the twenty-four opportunities” or “thirteen days”; 20 of the
extraction’s quoted sentences state their number in words.

Two instruments read the same pipeline output by separate paths.
`ground_truth/build_ground_truth.R` builds the comparison table.
`maintained/in_text_claims.R` carries the article’s own sentence in a
block comment beside code that recomputes the number, and prints 219
claims in the form `CLAIM <id> = <value> || <label>`. It reads
`maintained/output/` and the extraction, never the ground truth, so the
two derivations are independent and a disagreement between them is a
finding.

The build asserts all of the following and stops if any fails.

- Every `pipeline` and `descriptive` claim has a block and a row.
- The number of printed claims equals the number of rows requiring one,
  and the two sets of ids are equal in both directions.
- Every value the two instruments both produce agrees exactly.
- Re-rendering `value_paper` from its own number at its own recorded
  precision returns the string the extraction stores, which is what
  catches a precision that is right about the value and wrong about the
  digits.
- Every adverse row carries a `defect_locus` and no clean match does.
- Every published float has at least one row.

Each of the first, third and fourth was tested by breaking it: deleting
a block fails the count, perturbing a printed value fails the
cross-check, and changing one claim’s recorded precision fails the
string check.

## The maintained rewrite

`maintained/` is 22 scripts. `fit_all_models.R` fits every model from
the deposited survey data and writes each family as a tidy CSV under
`maintained/output/fits/`, so every number a figure or table prints can
be traced to a file a reader can open. Nothing downstream reads the
deposit’s saved objects.

The substitutions the rewrite makes are the usual ones for code of this
age: the native pipe for `magrittr`, `pivot_longer`/`pivot_wider` for
`reshape2::melt`/`dcast`, `reframe(tidy(...))` for `do(tidy(...))`,
`pick(everything())` for `cur_data()`, `write_csv()` for `xtable`
printed to the console, `linewidth` for the deprecated `size` aesthetic,
and `position_dodge()` for `ggstance::position_dodgev()`. The Roboto
font dependency in the traffic figure is dropped. `plyr` is not loaded
at all, which removes the masking hazard the deposit works around by
loading it first.

## Figure verification

Every rendered figure was laid beside the published one. Numbers alone
cannot catch a transposed axis or a mislabelled series, and three of
this article’s six figures print their estimates on the face of the
plot, which makes them published tables in disguise.

![Figure 2 as the rewrite draws it. All seventy-two conditional means
reproduce the published labels
exactly.](maintained/output/figure_2_raw_outcomes_by_treatment.png)

![Figure 5 as the rewrite draws it. Thirty-four of thirty-six labels
reproduce; the two political knowledge correction labels move, which is
environment drift rather than a defect in either
analysis.](maintained/output/figure_5_heterogeneity_by_trait.png)

![Figure 6 as the rewrite draws it, with the thermometer models fitted
on the panel that carried each claim. All eight labels differ from the
published ones.](maintained/output/figure_6_ft_targets.png)

![Figure 4 as the rewrite draws it. In the published version the Wave 1
and Wave 2 points of the bottom-left panel sit at the same height,
because the deposit fits both on the Wave 3
outcome.](maintained/output/figure_4_persistence.png)

## Rewrite verification

`run_all.R` was run to completion twice from clean sessions and the
whole tree diffed. Every CSV is byte-identical between runs. The figure
PDFs differ, because a PDF records the time it was written.

`original/` is verified twice per run: `download_original.R` is sourced
first, which checks every file against the MD5 Dataverse serves for
`?format=original` and against its deposited byte size, and refuses to
continue if `original/` holds any file the manifest does not list,
dotfiles included. It is sourced again as the last step, because the
first pass proves only that the archive was intact when the run began.
All 63 files match, and 0 carry a published checksum that disagrees with
the bytes Dataverse serves. Checksums for files this repository writes
are not quoted anywhere: a PDF records its write time, so its hash
changes on every run.

## R environment

| component | version |
|:----------|:--------|
| R         | 4.6.0   |
| tidyverse | 2.0.0   |
| estimatr  | 1.0.6   |
| metafor   | 5.0.1   |
| ggplot2   | 4.0.3   |
| broom     | 1.0.13  |

The rewrite keeps the current sampler and the current packages
throughout. Nothing in this analysis draws at random: no script calls
`sample()`, `rnorm()` or a bootstrap, so the results are bit-identical
at any seed and the sampler change in R 3.6 is not in play.
