# Unifed

The unifed distribution is the only exponential dispersion family
containing the exponential distribution. This R package contains
function for working with this distribution.

An R vignette introducing the unifed distribution and examples of how
to use this package can be found at
[https://oquijano.gitlab.io/unifed/](https://oquijano.gitlab.io/unifed/).


## Installation

If you do not have `devtools` installed run
`install.packages("devtools")` in your R console.

If you are using windows you also need to install
[Rtools](https://cran.r-project.org/bin/windows/Rtools/).


Then the package can be installed directly from this repository by
running the following in R

```{r}
devtools::install_git("https://gitlab.com/oquijano/unifed")
```


## Side effects

Note that this package rewrites the `summary` method from the `glm`
class. This will not break any code you have that uses the `summary`
function since it is simply a wrapper around `stats::summary.glm` that
sets the dispersion parameter equal to 1 when working with a unifed
GLM.



<!--  LocalWords:  cumulant roundings Unifed unifed
 -->
