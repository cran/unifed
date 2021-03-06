# Unifed

The unifed distribution is the only exponential dispersion family
containing the exponential distribution. This repository contains an R
package for working with this
distribution. [stan](https://mc-stan.org/) functions for the unifed
distribution are also provided.

An R vignette introducing the unifed distribution and examples of how
to use this package can be found at
[https://oquijano.gitlab.io/unifed/](https://oquijano.gitlab.io/unifed/).

## Important Note

The function `unifed.kappa.prime.inverse` in version 1.0 of the
package returns values with the wrong sign when its argument is less
than 0.1.

Please update to the current version to avoid this issue.


## Installation

You can install this package directly from CRAN by typing this in your R console.

```{r}
install.packages("unifed")
```


## Side effects

Note that this package rewrites the `summary` method from the `glm`
class. This will not break any code you have that uses the `summary`
function since it is simply a wrapper around `stats::summary.glm` that
sets the dispersion parameter equal to 1 when working with a unifed
GLM.



<!--  LocalWords:  cumulant roundings Unifed unifed
 -->
