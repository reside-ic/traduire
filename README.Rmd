## traduire

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![Build Status](https://travis-ci.com/reside-ic/traduire.svg?branch=master)](https://travis-ci.com/reside-ic/traduire)
[![codecov.io](https://codecov.io/github/reside-ic/traduire/coverage.svg?branch=master)](https://codecov.io/github/reside-ic/traduire?branch=master)
<!-- badges: end -->

The `traduire` R package provides a wrapper around the [`i18next` JavaScript library](https://www.i18next.com).  It presents an alternative interface to R's built-in internationalisation functions, with a focus on the ability to change the target language within a session.  Currently the package presents only a stripped down interface to the underlying library, though this may expand in future.

[R's built-in internationalisation](https://cran.r-project.org/doc/manuals/R-exts.html#Internationalization) is focussed on translating messages, warnings and errors transparently for package authors, within a session that does not change language.  The translation is completely transparent, as one writes:

```
warning("spline: first and last y values differ - using y[1] for both")
```

(for example, in `stats::spline`), and the text used will vary depending on the locale (the environment variable `LC_MESSAGES`) to show, in a French locale:

```
Warning message:
In stats::spline(1:10, 1:10, method = "periodic") :
  spline : la première et la dernière valeurs de y diffèrent - y[1] utilisé pour les deux
```

It is harder to generate translations in contexts other than messages, warnings and errors - for example the axes of a plot, the output of a [knitr](https://cran.r-project.org/package=knitr) document, or the results of a [web service](https://cran.r-project.org/package=plumber).  It is also not straightforward to change the language of translation during a session (e.g., to produce knitr output in two different languages, or to respond to a query in a language that changes at the client's request).

For this, we wanted the ability to internationalise strings on the fly, changing languages within a session, and decoupled from messages.  When used within a package, the interface is lightweight.

For an introduction to the package, see [the vignette](https://reside-ic.github.io/traduire/articles/traduire.html), which shows how to internationalise strings, including strings requiring interpolation (substitution of variables), and for a more complete example that implements an internationalised "hello-world-as-a-service", showing how to change languages dynamically.

Note that this package is designed only to help with tooling your package or application to allow it to be translated; the translations of content must still be carried out by hand.

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
