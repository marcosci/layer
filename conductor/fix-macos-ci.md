# Plan: Resolve macOS CI Failures

## Objective
Fix the macOS-specific `PROJ` errors and modernize the CI workflow.

## Steps

### 1. Modernize Workflow
Update `.github/workflows/R-CMD-check.yaml`:
- Add `permissions: read-all`.
- Use `pak-version: devel` and `needs: check` in `setup-r-dependencies`.
- Add `build_args` to `check-r-package`.

### 2. Verify S3 Documentation
Check for any S3 methods using full names in `\usage`.

### 3. macOS Environment (If needed)
Set `PROJ_LIB` if errors persist.

## Verification
- Monitor GitHub CI results.
