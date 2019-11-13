## traduire

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![Build Status](https://travis-ci.com/reside-ic/traduire.svg?branch=master)](https://travis-ci.com/reside-ic/traduire)
[![codecov.io](https://codecov.io/github/reside-ic/traduire/coverage.svg?branch=master)](https://codecov.io/github/reside-ic/traduire?branch=master)
<!-- badges: end -->

The `traduire` R package provides a wrapper around the [`i18next` JavaScript library](https://i18next.com).  It presents an alternative interface to R's built-in internationalisation functions, with a focus on the ability to change the target language within a session.  Currently the package presents only a stripped down interface to the underlying library, though this may expand in future.

## Installation

Install with [drat](https://cran.r-project.org/package=drat) from our [repository](https://mrc-ide.github.io/drat/)

```r
drat:::add("mrc-ide")
install.packages("traduire")
```

Or, install directly from GitHub:

```
remotes::install_github("reside-ic/traduire")
```

## License

MIT © Imperial College of Science, Technology and Medicine
