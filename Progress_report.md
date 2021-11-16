1st_progress_report_Shaohua
================
Shaohua Fang
10/28/2021

-   [1st progress report](#1st-progress-report)
    -   [Overview](#overview)
-   [2nd progress report](#2nd-progress-report)
    -   [Significance of this study
        (added)](#significance-of-this-study-added)
    -   [Overview](#overview-1)

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
share 18% of the data (1000/5517) for the project in this class (Link:
<https://github.com/Data-Sci-2021/Structural-Change-L2-Sentence-Processing/tree/main/Data_processing/SPR>),
but the presentation will be based on the data in its entirety. Note
that the data have been anonymized.

# 2nd progress report

## Significance of this study (added)

Native speakers (NSs) and non-native speakers (NNSs) have been found to
experience garden-path effects when reading a temporarily structurally
ambiguous sentence \[1, 2\]. The extent to which NNSs differ from NSs in
their use of various types of linguistic information for structural
reanalysis remains a topic of debate \[3, 4\]. Verb bias as a type of
fine-grained lexical information has been demonstrated to influence
reanalysis difficulty in both NSs and NNSs \[5, 6\]. When reading a
sentence with a DO (direct object) - biased verb and a sentence with SC
(sentential complement) - biased verb, both NSs and NNSs were slow to
read the disambiguating regions compared to similar regions in the
unambiguous control sentences \[5, 7\]. This suggests that NSs and NNSs
are sensitive to verb bias information for ambiguity resolution.

The current investigation, however, addresses the influence from a verb
pertaining to its structural properties rather than to its lexical
properties on L2 ambiguity resolution. According to the structural
change theory \[8\], reanalysis is particularly challenging when it
involves a major rearrangement of thematic structure. For example,
sentences like (1) involve NP/S ambiguities since they can take either a
NP or a sentence (S) as the complement; sentences like (2) involve NP/Z
ambiguities since they can take either a NP as the complement or no
(Zero) complement. This theory predicts that NP/Z reanalysis should be
more difficult than NP/S reanalysis because NP/Z reanalysis relative to
NP/S reanalysis requires the NP following the verb to be moved out of
its thematic domain. Sturt et al. \[9\] experimentally confirmed this
prediction among NSs. This study aims to examine whether NNSs are
sensitive to this structural property, thus directly testing this theory
in NNSs.

**References:**

Dussias, P. E., & Scaltz, T. R. C. (2008). Spanish–English L2 speakers’
use of subcategorization bias information in the resolution of temporary
ambiguity during second language reading. *Acta psychologica, 128*(3),
501-513.

Garnsey, S. M., Pearlmutter, N. J., Myers, E., & Lotocky, M. A. (1997).
The contributions of verb bias and plausibility to the comprehension of
temporarily ambiguous sentences. *Journal of memory and language,
37*(1), 58-93.

Hopp, H. (2015). Individual differences in the second language
processing of object–subject ambiguities. *Applied Psycholinguistics,
36*(2), 129-173.

Jegerski, J. (2012). The processing of subject–object ambiguities in
native and near-native Mexican Spanish. *Bilingualism: Language and
Cognition, 15*(4), 721-735.

Juffs, A. (2004). Representation, processing and working memory in a
second language. *Transactions of the Philological Society, 102*(2),
199-225.

Van Gompel, R. P., Pickering, M. J., & Traxler, M. J. (2001). Reanalysis
in sentence processing: Evidence against current constraint-based and
two-stage models. *Journal of Memory and Language, 45*(2), 225-258.

Qian, Z., Lee, E. K., Lu, D. H. Y., & Garnsey, S. M. (2019). Native and
non-native (L1-Mandarin) speakers of English differ in online use of
verb-based cues about sentence structure. *Bilingualism: Language and
Cognition, 22*(5), 897-911.

Pritchett, B. L. (1992). *Grammatical competence and parsing
performance*. University of Chicago Press.

Sturt, P., Pickering, M. J., & Crocker, M. W. (1999). Structural change
and reanalysis difficulty in language comprehension. *Journal of Memory
and Language, 40*(1), 136-150.

## Overview

I have done the analysis for my experiments for AJT (Link:
<https://github.com/Data-Sci-2021/Structural-Change-L2-Sentence-Processing/tree/main/Data_Analysis/AJT/L1_L2_Data>)
and SPR (Link:
<https://github.com/Data-Sci-2021/Structural-Change-L2-Sentence-Processing/tree/main/Data_Analysis/SPR>).

I am still working on the wrangling and analysis of the corpus data.
