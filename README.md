
<!-- README.md is generated from README.Rmd. Please edit that file -->

# r2dii.plot <a href='https://github.com/2DegreesInvesting/r2dii.plot'><img src='https://imgur.com/A5ASZPE.png' align='right' height='43' /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Codecov test
coverage](https://codecov.io/gh/2DegreesInvesting/r2dii.plot/branch/master/graph/badge.svg)](https://codecov.io/gh/2DegreesInvesting/r2dii.plot?branch=master)
[![R-CMD-check](https://github.com/2DegreesInvesting/r2dii.plot/workflows/R-CMD-check/badge.svg)](https://github.com/2DegreesInvesting/r2dii.plot/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/r2dii.plot)](https://CRAN.R-project.org/package=r2dii.plot)
<!-- badges: end -->

``` r
intro <- system.file("intro.Rmd", package = "r2dii.plot")
```

The goal of r2dii.plot is to help you plot 2DII data in an informative,

beautiful, and easy way.

\#\# Installation

You can install the development version of r2dii.plot from

[GitHub](%5Bhttps://github.com/2DegreesInvesting/r2dii.plot)\](<https://github.com/2DegreesInvesting/r2dii.plot>))
with:

\`\`\`r

\# install.packages(“devtools”)

devtools::install\_github(“2DegreesInvesting/r2dii.plot”)

\`\`\`

\#\# Example

The r2dii.plot package is designed to work smoothly with other “r2dii”
packages

–
[r2dii.data](%5Bhttps://2degreesinvesting.github.io/r2dii.data/),\](<https://2degreesinvesting.github.io/r2dii.data/>),)

[r2dii.match](%5Bhttps://2degreesinvesting.github.io/r2dii.match/),\](<https://2degreesinvesting.github.io/r2dii.match/>),)
and

[r2dii.analysis](%5Bhttps://2degreesinvesting.github.io/r2dii.analysis/).\](<https://2degreesinvesting.github.io/r2dii.analysis/>).)
It also

plays well with the popular packages
[dplyr](%5Bhttps://www.tidyverse.org/)\](<https://www.tidyverse.org/>))
and

[ggplot2](%5Bhttps://ggplot2.tidyverse.org/),\](<https://ggplot2.tidyverse.org/>),)
which help you customize your plots.

\`\`\`{r setup}

library(dplyr, warn.conflicts = FALSE)

library(ggplot2, warn.conflicts = FALSE)

library(r2dii.plot)

\`\`\`

Your data typically comes from the output of two functions in the
r2dii.analysis

package:

[\`target\_sda()\`](%5Bhttps://2degreesinvesting.github.io/r2dii.analysis/reference/target_sda.html)\](<https://2degreesinvesting.github.io/r2dii.analysis/reference/target_sda.html>))

and

[\`target\_market\_share()\`](%5Bhttps://2degreesinvesting.github.io/r2dii.analysis/reference/target_market_share.html).\](<https://2degreesinvesting.github.io/r2dii.analysis/reference/target_market_share.html>).)

Here you’ll use two example datasets that come with r2dii.plot.

\`\`\`{r}

sda

market\_share

\`\`\`

r2dii.plot currently supports three kinds of plots:
\`plot\_timeline\*()\`,

\`plot\_techmix\*()\`, and \`plot\_trajectory\*()\`. Each plot has
specific requirements

about the main input – passed to the first argument \`data\`. To meet
those

requirements we currently provide two experimental sets of functions

([API](%5Bhttps://en.wikipedia.org/wiki/API)s)\](<https://en.wikipedia.org/wiki/API)s>))
– “X” and “Y”. Both APIs can help

you get the same basic plots, which you can further customization with
ggplot2.

Their difference difference is not in what you can do but in how you can
do it:

\* With the “X” API you meet the \`data\` requirements mainly with

\`dplyr::filter()\`, and with “internal magic” based on the known
structure of

r2dii data. This API should be best for users who already use dplyr or
want to

learn it.

\* With the “Y” API you could meet the \`data\` requirements with dplyr
but you can

also use dedicated “preparation” functions (\`prep\_\*Y()\`),, and with
explicit

arguments to both the preparation and and plotting functions. This API
should

best for users who do not use dplyr or care about it.

Users and developers may have different preferences. The tables below
compare

the X and Y APIs across a number of criteria relevant to them.

\`\`\`{r echo=FALSE}

users &lt;- tibble::tribble(

\~Criteria, \~\`Thin API “X”\`, \~\`Thin API, “Y”\`,

“Required knowledge of dplyr and ggplot2”, “More”, “Less”,

“Customization possible”, “Limitless with dplyr and ggplot2”, “Limitless
with dplyr, ggplot2, and r2dii.plot”,

“Integration with other R workflows”, “More”, “Less”,

)

caption &lt;- “The X and Y APIs compared from a user’s perspective.”

knitr::kable(users, caption = caption)

devs &lt;- tibble::tribble(

\~Criteria, \~\`Thin API “X”\`, \~\`Thin API, “Y”\`,

“Maintenance burden”, “Less”, “More”,

“Easy to extend”, “More”, “Less”,

)

caption &lt;- “The X and Y APIs compared from a developer’s
perspective.”

knitr::kable(devs, caption = caption)

\`\`\`

To make the comparison concrete consider this small example. Notice the

resulting plot is the same but the toolkit is different.

\* “X” API

\`\`\`{r}

data &lt;- market\_share

prep &lt;- filter(

data,

sector == “power”,

technology == “renewablescap”,

region == “global”,

scenario\_source == “demo\_2020”,

year &lt;= 2025

)

plot\_trajectoryX(prep) +

labs (title = “Trajectory plot with the thin ‘X’ API”)

\`\`\`

\* “Y” API

\`\`\`{r}

data &lt;- market\_share

prep &lt;- prep\_trajectoryY(

data,

sector\_filter = “power”,

technology\_filter = “renewablescap”,

region\_filter = “global”,

scenario\_source\_filter = “demo\_2020”,

value = “production”

)

scenario\_specs &lt;- dplyr::tibble(

scenario = c(“sds”, “sps”, “cps”),

label = c(“SDS”, “STEPS”, “CPS”)

)

main\_line\_metric &lt;- dplyr::tibble(metric = “projected”, label =
“Portfolio”)

additional\_line\_metrics &lt;- dplyr::tibble(

metric = “corporate\_economy”,

label = “Corporate Economy”

)

plot\_trajectoryY(

prep,

scenario\_specs\_good\_to\_bad = scenario\_specs,

main\_line\_metric = main\_line\_metric,

additional\_line\_metrics = additional\_line\_metrics

) +

labs (title = “Trajectory plot with the thick ‘Y’ API”)

\`\`\`

For full examples see the dedicated articles \[r2dii.plot

X\]([https://2degreesinvesting.github.io/r2dii.plot/articles/articles/r2dii-plot-X.html)](https://2degreesinvesting.github.io/r2dii.plot/articles/articles/r2dii-plot-X.html))

and \[r2dii.plot

Y\]([https://2degreesinvesting.github.io/r2dii.plot/articles/articles/r2dii-plot-Y.html).](https://2degreesinvesting.github.io/r2dii.plot/articles/articles/r2dii-plot-Y.html).)
