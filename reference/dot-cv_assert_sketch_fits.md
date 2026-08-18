# Refuse a sketch that cannot be smaller than the data (audit \#13).

clustoCell refuses this itself, correctly and in 0.1 s – but from inside
a callr child, so the agent pays a full worker spawn to deliver it
wrapped in worker-failure framing. The wording below deliberately echoes
clustoCell's own guidance ("at least half the total" under 10,000 cells)
so the early message and the late one cannot give different advice.

## Usage

``` r
.cv_assert_sketch_fits(store, args, data_arg = "data")
```
