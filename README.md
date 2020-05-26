# STA141C Final Project

In this final project, you are going to work on bag of little bootstraps algorithm.

There is a package at https://github.com/ucdavis-sta141c-sq-2020/blblm
In this package, I have implemented the bag of little bootstraps for linear regression model.

You could install it with
```r
devtools::install_github("ucdavis-sta141c-sq-2020/blblm")
```
and test it out.


Your job is improved my package to various ways.


1. In the current implementation, only one CPU is used in the algorithm. Make it possible to use more than one CPUs. Note that you should let users to decide if they want to use parallelization.

1. Functions are written in pure R, it is possible, for example, to convert the function `lm1` to c++ code.

1. Write tests and documentations

1. More models? Logistic regression? GLM?

1. You should also write a few pages Rmarkdown documentation to explain your work. One recommendation way is to put the documentation as a [vignette](https://r-pkgs.org/vignettes.html). (If you want to use `tidyverse` in the the vignettes, run `usethis::use_package("tidyverse", type = "suggest")` to add `tidyverse` in the suggest field of DESCRIPTION.)

## How to start?

The easiest way to start the project is to [fork](https://help.github.com/en/github/getting-started-with-github/fork-a-repo) my package then use RStudio to clone from your personal repo.

However, your could also start a new package from scratch.

## Grading

Your grade will be determined by the amount of work that you have made and how well they are implemented.

- (60%) the code: 
    - both correctness and efficiency
    - code style: You want your code to be clean and well documented. Just imagine another people will be taking charge of the maintenance of your app. (Hint: make use of `styler`)
- (40%) miscellaneous
    - tests
    - documentations
    - pass `devtools::check()` etc.
    - the vignette


## How to submit?

To be announced.

Due: 6/10/2020 11:59pm
