R Markdown Slide Utilities
================

<!-- README.md is generated from README.Rmd. Edit this file, not that one -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)  
<!-- badges: end -->

R Markdown Slide Utilities contains functions you can use to add custom
content to RMarkdown slide decks, including:

-   Image builds from a series of PNG files

-   Summary charts from a Google Form survey

-   Emojis

-   Customized SVGs from DiagrammeR

  

# Installation

`slideutils` is on GitHub as well as R-universe. You can install it
with:

    options(repos = c(ajlyons = 'https://ajlyons.r-universe.dev',
                     CRAN = 'https://cloud.r-project.org'))
    install.packages('slideutils')

OR

    remotes::install_github("ucanr-igis/slideutils")

  

To see if its working:

    library(slideutils)

# History

These utilities were originally part of the
[`wrkshputils`](https://github.com/ucanr-igis/wrkshputils) package. They
were split off into their own package mostly to make `wrkshputils` and
because these functions only apply to slide content creators.

# TODO

that collectively represent a slide build, and i) make white pixels
transparent (so they can be overlaid), and ii) generate HTML tags that
can be inserted into a R Markdown HTML slide deck to animate the build.

Create HTML pages containing hints and tips, populated by a Google
Sheet, that can be embedded in a R Notebook as hints and solutions.
Option to create bitly links.
