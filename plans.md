# TODOs

- Fix forecast race during sim restore: ensure forecast runs only after restore completes and the input signature matches; gate `begin_forecast()` by a restore flag and signature; dedupe triggers per-sim; reset results on signature change.
- Fix UN reset button in the Population modal: make “Reset to UN” clear custom caches and re-render the table reliably.
- Style “Projection Results” in the sidebar as a standard hyperlink color consistent with other links.
