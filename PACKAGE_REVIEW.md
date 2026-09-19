# chRonostatistics 1.0.0: function and architecture review

This document is a review aid for maintainers and contributors. It explains
what the package currently does, how the implementation is organised, where
the background papers informed the examples, and which design questions should
be settled before expanding the API.

## Package purpose

`chRonostatistics` treats ordered process observations as a time-structured
stream. The central object is not a fitted forecasting model: it is a validated
regular series whose lagged differences can be examined at several time scales.
The package is intentionally descriptive and engineering-oriented.

The workflow is:

```text
raw table / tsibble
        |
        v
chrono_data()  -> validated time, value, segment, index
        |
        v
chrono_variogram()  -> V(j), pair counts, lag eligibility
        |
        +--> chrono_integral_variogram()
        |
        +--> chrono_components()  -> V(0), V(1), interval, cycle, sill
                                      |
                                      v
                              chrono_control_limits()
```

## Public functions

| Function | Role | Main design point |
|---|---|---|
| `chrono_data()` | Validate and standardise source data | Sorts time, rejects duplicates and small irregularities, starts new segments at missing values or large gaps, and preserves separate value/time units plus specification metadata. A `tsibble` index can be inferred when `tsibble` is installed. |
| `chrono_variogram()` | Compute the direct chronological semivariogram | For lag `j`, computes `sum((x[i+j] - x[i])^2) / (2 * N[j])`; respects continuous segments by default and records pair counts. |
| `chrono_integral_variogram()` | Accumulate the variogram | Supports a physical trapezoid convention and a Pitard-style discrete convention so the implementation can reproduce either interpretation explicitly. |
| `chrono_components()` | Estimate engineering components | Uses the first three eligible lags for the default Pitard endpoint solution or an AICc-selected prefix for the modern alternative, obtains the one-step process contribution, estimates a tail sill, and optionally compares a pre-cycle peak with a selected cycle lag. |
| `chrono_control_limits()` | Build progressive descriptive limits | Uses `V(1)` as the Pitard linear-root interval term and `V(1)-V(0)` as the non-overlapping RSS term. These are engineering limits, not prediction intervals. |
| `simulate_chronostatistics()` | Generate a small controlled example | Combines baseline, positive trend, AR(1) movement, small sinusoidal cycles, measurement error and optional deterministic events. Latent columns reconcile to `value` when observations are present. |
| `simulate_plant_process()` | Generate a richer plant table | Produces ore domains, feed characteristics, equipment condition, downtime, throughput, density, delayed recovery/product quality, sensor error and sparse delayed laboratory results. |

The package also provides `print()`, `summary()`, `plot()` and
`ggplot2::autoplot()` methods for the main classes.

## How the package was built

The implementation uses a small R-package surface:

- `R/data-validation.R` owns input validation and metadata.
- `R/variogram.R` owns direct lag calculations.
- `R/integral-variogram.R` owns numerical accumulation conventions.
- `R/components.R` turns variogram outputs into a one-row engineering record.
- `R/control-limits.R` turns that record into progressive limits.
- `R/simulation.R` contains reproducible focused and plant-process data
  generators.
- `R/plot.R` and `R/methods.R` provide user-facing inspection methods.
- `R/utils.R` contains small internal helpers used across these modules.

The computational core is mostly base R. `ggplot2` is used for plotting and
`rlang` supports tidy column references. The optional `tsibble` integration is
deliberately late-bound with `requireNamespace()`; it does not make tidyverts a
runtime requirement. `fable`, `feasts` and `fabletools` remain compatible
workflow companions rather than package dependencies.

Documentation is maintained in roxygen comments, generated `.Rd` help files,
the README, and the vignette. Tests are in `tests/testthat/`. Reproducibility
comes from explicit seeds and the project records its development environment
with `renv`.

## Release metadata, exclusions and licensing

Version 1.0.0 identifies Matt Clauson as author, maintainer and copyright
holder, with Clauson Geometallurgy Consultancy as the affiliation. The public
repository and issue tracker use
<https://github.com/ClausonGeomet/chRonostatistics-r>.

The public repository is assembled from a reviewed allowlist. Local research
material, validation fixtures, development caches, environment files, build
products and prior repository history are excluded from public commits and
source packages. The public examples and figures are generated from original,
seeded simulations; no observations or figures from cited publications are
redistributed.

The package is MIT-licensed and does not redistribute dependency source. Its
direct runtime dependencies, `ggplot2` and `rlang`, are MIT-licensed. Among the
separately installed suggested/build dependencies, `rmarkdown` and `testthat`
are MIT-licensed, `knitr` is GPL, and `tsibble` is GPL-3. `renv.lock` records
dependency metadata; it does not bundle dependency code.

## Statistical and engineering assumptions

1. A positional lag represents a common elapsed interval within each segment.
   This is why small irregular departures are rejected rather than silently
   treated as regular.
2. Missing values and shutdowns are boundaries, not zeroes and not valid pairs.
3. The first local integral fit is an extrapolation to zero lag. It is a
   diagnostic estimate of sampling/measurement variation, not a directly
   observed ordinate.
4. The selected cycle lag is supplied by the analyst. Automatic peak-picking
   would confuse genuine cycles, aliasing, operating changes and finite-sample
   roughness.
5. The sill is a descriptive long-range benchmark. It is not assumed to be a
   universal stationary variance for every future operating state.
6. Progressive limits depend on the chosen centre, sigma multiplier and
   convention. They should be compared with specifications and process
   knowledge, not labelled as model-based prediction intervals by default.

## What the cited methodology contributed

The cited Minnitt and Pitard (2008) paper describes 4-hourly iron-ore
assays, a mean around 66.1% Fe, standard deviation around 0.68% Fe, and visible
short and long cyclic structures around 17 and 208 lags. It emphasises moving
averages before variography, preserving chronological order, distinguishing
`V[0]`, `V[1]`, cyclic variation and long-range structure, and avoiding
over-correction of process trends.

The README and vignette now use these scales only as a synthetic teaching
scenario. The paper's data, figures and copyrighted PDF are not copied into
the package. The generated figures are original plots from the package's own
seeded simulations.

## Review questions before the next API expansion

- Should a future multi-key helper return a list of `chrono_data` objects or a
  key-preserving collection class?
- Should irregular source data gain an explicit regularisation helper, or
  should that remain in `tsibble`, `tidyr` and domain-specific preprocessing?
- Should `chrono_components()` report uncertainty intervals, or should the
  package remain intentionally descriptive until a validated inferential
  method is specified?
- Should plant simulation parameters become configurable through a structured
  control object instead of an expanding function signature?
- Should a future `autoplot()` method show moving averages and selected cycle
  annotations by default, or keep those choices explicit?

## Token and model efficiency for ongoing development

For AI-assisted maintenance, use a staged context strategy:

1. Start with the specific function, tests and relevant background excerpts;
   do not load the entire repository into every prompt.
2. Use a lower reasoning setting/model for mechanical documentation edits and
   static checks, and reserve the strongest coding/reasoning model for API,
   statistical or licensing decisions.
3. Ask for structured artifacts (a short plan, changed-file list, and test
   evidence) instead of repeated prose summaries.
4. Reuse cached context and pass file paths rather than copying large source
   blocks into prompts.
5. Measure quality and token usage together; shorter output is not an
   improvement if it omits assumptions, acceptance criteria or test evidence.

OpenAI's current model guidance likewise recommends starting with a balanced
reasoning setting, testing lower effort for latency-sensitive work, and
measuring token usage and end-to-end latency rather than assuming that maximum
reasoning is always better:
<https://developers.openai.com/api/docs/guides/latest-model>.

## Suggested review commands

```r
devtools::load_all()
testthat::test_local()
devtools::document()
devtools::check()
```

The README figure assets can be regenerated with:

```sh
R --vanilla --slave -f tools/readme-figures.R
```
