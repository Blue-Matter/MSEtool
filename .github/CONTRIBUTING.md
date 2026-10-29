# Contributing to MSEtool

This outlines how to propose a change to MSEtool.

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file.
This generally means you'll need to edit [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`, not a `.Rd` file.
You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, first file an issue and make sure someone from the team agrees that it's needed.
If you've found a bug, file an issue that illustrates the bug with a minimal
[reprex](https://reprex.tidyverse.org/), ideally using one of the example operating models (`SingleStockOM`, `MultiStockOM`, `SeasonalSpatialOM`, `ComplexOM`).
This will also help you write a unit test.

## Branches

* `master` is the current CRAN release. Don't open pull requests against it.
* `dev` is the development branch and the default branch. All pull requests target `dev`.
* Pull requests are squash-merged, so each one becomes a single commit on `dev`, titled with the PR title.

Name your branch with a prefix that describes the change:

| Prefix | For |
|---|---|
| `fix/` | Bug fixes, with the issue number when there is one, e.g. `fix/142-ss3-ricker-steepness` |
| `feature/` | New user-facing functionality |
| `docs/` | Documentation, vignettes, README |
| `chore/` | CI, GitHub files, and other changes that don't affect package code |

Keep each pull request to one logical change.

## Pull request process

* If you have write access to the repository, clone it and create your branch there.
  Otherwise, fork the repository and clone your fork: `usethis::create_from_github("Blue-Matter/MSEtool", fork = TRUE)`.

* Install the development dependencies with `devtools::install_dev_deps()`.
  Some suggested packages are not on CRAN:
  `install.packages(c("r4ss", "spict"), repos = c("https://r4ss.r-universe.dev", "https://flr.r-universe.dev", "https://cloud.r-project.org"))`.

* Create a branch from an up-to-date `dev`, e.g. `usethis::pr_init("fix/142-ss3-ricker-steepness")`.

* Make your changes, commit, and create a PR with `usethis::pr_push()`.
  Check that the base branch is `dev`.
  The title of your PR should briefly describe the change in the imperative, e.g. `Fix steepness conversion for Ricker SRR in ImportSS()`.
  The body should contain `Fixes #issue-number` if there is a related issue.

* If you changed roxygen comments, run `devtools::document()` and commit the updated `man/` and `NAMESPACE` files. Don't edit those files by hand.

* For user-facing changes, add a bullet to the top section of `NEWS.md` (just below the first heading).

* The R-CMD-check workflow must pass on macOS, Windows, and Linux before a PR can be merged.

## Code style

Follow the style of the surrounding code:

* Exported functions and S4 classes and slots use `UpperCamelCase`, e.g. `Simulate()`, `ImportSS()`, `Stock@NaturalMortality`.
* Internal (non-exported) functions are prefixed with a dot, e.g. `.CalcSurvival()`.
* Local variables use `camelCase`, e.g. `nSim`, `stockNames`.
* For formatting, follow the [tidyverse style guide](https://style.tidyverse.org): `<-` for assignment, 2-space indentation, spaces after commas and around operators, and lines of 80 characters or fewer where practical.
* Don't restyle code that has nothing to do with your PR.
* Files named `zz_*.R` contain the legacy (MSEtool 3.x) code. Only change them to fix bugs.

## Documentation and tests

* Documentation uses [roxygen2](https://roxygen2.r-lib.org/) with [Markdown syntax](https://roxygen2.r-lib.org/articles/rd-formatting.html).
  Exported functions need a title, description, `@param` for every argument, `@return`, and an example.
  Don't refer to internal (dot-prefixed) functions in the documentation of exported functions.

* Tests use [testthat](https://testthat.r-lib.org/) (3rd edition) and live in `tests/testthat/`.
  Add or update tests for bug fixes and new functionality.
  The full test suite is slow; while developing, run the test files related to your change with `testthat::test_file()`.
  Tests that take more than a few seconds should call `testthat::skip_on_cran()`.
