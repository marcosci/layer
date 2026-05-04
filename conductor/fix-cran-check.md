# Plan: Resolve R CMD check Issues

## Objective
Address the 1 Warning and 3 Notes from the `R CMD check` to ensure the package is CRAN-compliant.

## Key Files & Context
- `private/`: Destination for non-standard top-level files.
- `R/tilt_animate.R`: Contains `library()` calls that trigger a warning.
- `R/globals.R` (New): To hold `globalVariables()` declarations.

## Implementation Steps

### 1. Move Non-standard Files
Move the following files from the root to `private/`:
- `CRITICAL_REVISION_PLAN.md`
- `IMPLEMENTATION.md`
- `benchmark_math_vs_render.R`
- `benchmark_memoise_vs_preeval.R`
- `demo_real_data.R`
- `layer-dev.txt`
- `rafa-anim.txt`
- `rafa.txt`
- `test_interactive.R`

### 2. Quiet "No Visible Binding" Notes
Create `R/globals.R` and add `utils::globalVariables()` for:
- Parallel variables: `.L_DATA`, `.L_PARAMS`, `.L_ANCHOR`, `.L_NFRAMES`, `.L_FPL`, `.d`, `.p`, `.pa`, `.nf`, `.fpl`
- Animation variable: `.frame`

### 3. Resolve `library()` Warning in `R/tilt_animate.R`
In `animate_tilt_stack()`, replace `library()` calls within `mirai::everywhere()` with a more CRAN-friendly way to ensure the namespace is loaded on workers.
- I will use `loadNamespace()` or similar if possible, or just namespaced calls. 
- To avoid the check warning while still attaching (which `sf` needs for its operators), I can use `suppressPackageStartupMessages(base::library(sf))`.

## Verification & Testing
- Run `devtools::check(remote = TRUE, manual = TRUE, cran = TRUE)` again.
- Expect 0 Errors, 0 Warnings, and fewer Notes.
