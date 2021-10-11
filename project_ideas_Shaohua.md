strings\_Shaohua
================
Shaohua Fang
10/05/2021

  - [Project idea](#project-idea)
      - [Working title](#working-title)
      - [Brief summary](#brief-summary)
      - [Data](#data)
      - [Analysis](#analysis)
      - [Session info](#session-info)

``` r
##Set knitr options (show both code and output, show output w/o leading #, make figures smaller, hold figures until after chunk)
knitr::opts_chunk$set(echo=TRUE, include=TRUE, comment=NA, fig.height=3, fig.width=4.2, fig.show="hold")
```

# Project idea

## Working title

Structural change and L2 sentence processing

## Brief summary

This project is to examine how L2 learners would use the information
encoded in a verb and the relationship between this verb and its
adjacent nouns for structural ambiguity resolution during sentence
comprehension. For example, verbs such as ‘see’ tend to take either a
noun phrase or a sentence as their complements, but verbs such as
‘visit’ likely take either a noun phrase or nothing as their
complements. L2 learners may differ from native speakers in their
processing profiles, not only because L2 learners may use such
information to a lesser extent compared to native speakers, but because
they may be negatively affected by their L1.

## Data

The data were collected as part of a combination of several unrelated
self-paced reading experiments. As a plan, the reaction time data would
be modeled as a function of a set of categorical variables and
continuous variables. Data cleaning would involve the removal of
apparent outliers for data points, and participants who failed to pay
enough attention during the experimentation would be excluded as well
based on certain cut-off points that are widely accepted in the field.
Data were collected from 75 Chinese learners of English. It remains to
be seen whether I will include some corpus data to check the type of
complements each verb used in the experiment.

## Analysis

The end goal is to come up with a conclusion to the research questions
based on the statistical analyses. For now, the attempted analysis is
linear mixed-effects models. There are at least two hypotheses to be
tested - 1) whether L2 learners differ from native speakers in their
processing profiles. 2) whether L2 learners do better as their
proficiency increases. I am not quite sure how I may include any
predictive analysis in my case? One predictive analysis I am considering
is to analyzed the data with Bayesian mixed-effect models instead, which
to my understating may be counted as a predictive model.

## Session info

``` r
sessionInfo()
```

``` 
R version 3.6.2 (2019-12-12)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS Catalina 10.15.7

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

loaded via a namespace (and not attached):
 [1] compiler_3.6.2    magrittr_2.0.1    tools_3.6.2       htmltools_0.5.1.1
 [5] yaml_2.2.1        stringi_1.5.3     rmarkdown_2.4     knitr_1.30       
 [9] stringr_1.4.0     xfun_0.18         digest_0.6.27     rlang_0.4.11     
[13] evaluate_0.14    
```
