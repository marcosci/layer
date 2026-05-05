# layer (development version)

## New features

* A completely new declarative API (`tilt_stack()`, `tilt_layer()`, `tilt_connector()`). Spatial transformations are lazily evaluated and performed once when the stack is rendered. The resulting `ggplot2` object contains the pre-tilted geometries, allowing styling changes (themes, labels) to be applied without re-calculating the 3D projection on every update. Note that the actual rendering time in `ggplot2` remains proportional to the data complexity.

* Added a high-level animation API with `animate_tilt_stack()` supporting "unfold" and "reveal" effects.

* Added `tilt_match()` utility to easily align external data (like points or routes) with specific stack layers.

* `resolve_stack_params()` is now exported to support custom spatial calculations.

* `plot_tiltedmaps()` is now deprecated in favor of the new `tilt_stack()` API.

# layer 0.0.4 (2026-02-03)

* removed deprecated ggplot2 code

# layer 0.0.3 (2024-01-24)

* removed dependencies on deprecated packages
* minor updates in documentation

# layer 0.0.2
