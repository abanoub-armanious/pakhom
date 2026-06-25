## Submission

This is a new submission of pakhom 1.0.1.

## R CMD check results

0 errors | 0 warnings | 2 notes

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Abanoub J. Armanious <armaniousabanoub@gmail.com>'
  New submission.

* The same NOTE flags some URLs as possibly invalid:
  - The GitHub repository (https://github.com/abanoub-armanious/pakhom and
    its /issues page) and the pkgdown documentation site
    (https://abanoub-armanious.github.io/pakhom/) resolve once the repository
    is public and the pkgdown site is deployed (both done before submission).
  - https://www.linkedin.com/in/abanoubarmanious/ returns HTTP 999, and the
    Reddit help-centre policy page returns HTTP 403, to automated clients.
    These are anti-bot responses, not broken links.

* The second NOTE ("checking for future file timestamps") is environmental
  (the check machine had no network access to the time-stamping service) and
  is unrelated to the package.

## Test environments

* local: macOS, R release
* GitHub Actions (CI): R release on Ubuntu, macOS, and Windows; plus R-devel
  and R-oldrel on Ubuntu.

## Reverse dependencies

None (new submission).

## Note for the reviewer

The analysis pipeline requires an OpenAI or Anthropic API key and makes paid
API calls, so examples that would call a provider are not run. The
statistical, report-rendering, and quote-provenance layers are pure R and are
exercised offline by the test suite (more than 4,900 expectations, all AI calls mocked).
