1st_progress_report_Shaohua
================
Shaohua Fang
10/28/2021

-   [1st progress report](#1st-progress-report)
    -   [Overview](#overview)

``` r
##Set knitr options (show both code and output, show output w/o leading #, make figures smaller, hold figures until after chunk)
knitr::opts_chunk$set(echo=TRUE, include=TRUE, comment=NA, fig.height=3, fig.width=4.2, fig.show="hold")
```

# 1st progress report

## Overview

My project will include data from a series of experiments where English
native speakers and Chinese learners of English did a self-paced reading
(SPR) task in which people controlled the pace of word/phrase
presentation for sentences and answer a meaning-based question, and
additionally did an acceptability judgement task (AJT) probing how well
they understood individual sentences used for SPR. By now, the SPR data
from 135 participants have been processed for data wrangling,
visualization, and statistical modeling. As for AJT data, they are being
processed. In addition to the experimental data, the project plans to
include some data from the COCA corpus with the aim of setting up a
baseline for how verbs (e.g., remember, accept) in individual
experimental sentences are statistically biased towards the type of
complement - NP vs Clause. So far, I have been exploring how to extract
each instance containing these verbs and how to tag the kind of
complement each verb takes.

Since the data aim for a journal publication, I therefore will publicly
share 18% of the data (1000/5517) for the project in this class, but the
presentation will be based on the data in its entirety. Note that the
data have been anonymized.
