# Build the Lincoln & Guba (1985) trustworthiness mapping

Maps pakhom's architectural commitments to the four classic
naturalistic-inquiry criteria from Lincoln & Guba 1985:

- **Credibility** (~internal validity) – T0.1 quote verification,
  methodology-rules injection, framework grounding.

- **Dependability** (~reliability) – audit log + AC9 stamping +
  parent_run_id soft-lock (T1.5).

- **Confirmability** (~objectivity) – reflexivity scaffold (Olmos-Vega
  AMEE 149) + Phase 52 articulation requirement.

- **Transferability** (~external validity) – AC4 output stamping,
  cross-run comparison, QDPX export.

Each criterion's "evidence" field cites the specific decisions logged in
this run so a reviewer can independently verify.

## Usage

``` r
.tr_build_lincoln_guba(rd)
```
