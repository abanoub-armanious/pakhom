# Load memos from a run output directory back into Memo objects

Reads every `memos/*.md` file in `run_dir` and parses it via
[`markdown_to_memo`](https://abanoub-armanious.github.io/pakhom/reference/markdown_to_memo.md).
Used by
[`run_mode1()`](https://abanoub-armanious.github.io/pakhom/reference/run_mode1.md)
on the resume path so a previously persisted memo set survives across
interrupted runs.

## Usage

``` r
load_memos(run_dir)
```

## Arguments

- run_dir:

  Path to the run output directory.

## Value

List of `Memo` objects (zero-length when no memos exist or the memos
directory is missing).
