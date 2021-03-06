Final_report
================
Shaohua Fang
12/15/2021

-   [Background](#background)
-   [Method](#method)
-   [Results and Discussion](#results-and-discussion)
-   [Reflection](#reflection)
-   [References](#references)

## Background

Native speakers (NSs) and non-native speakers (NNSs) have been found to
experience garden-path effects when reading a temporarily structurally
ambiguous sentence (Dussias & Scaltz,2008; Garnsey et al., 1997). The
extent to which NNSs differ from NSs in their use of various types of
linguistic information for structural reanalysis remains a topic of
debate (Hopp, 2015; Jegerski, 2012). Verb bias as a type of fine-grained
lexical information has been demonstrated to influence reanalysis
difficulty in both NSs and NNSs (Juffs, 2004; Van Gompel et al., 2001).
When reading a sentence with a DO (direct object) - biased verb and a
sentence with SC (sentential complement) biased verb, as in (1a) and
(1b), both NSs and NNSs were slow to read the disambiguating regions
compared to similar regions in the unambiguous control sentences (Juffs,
2004; Qian et al., 2019). This suggests that NSs and NNSs are sensitive
to verb bias information for ambiguity resolution.

1.  (a). The professor forgot the theory revealed the underlying
    mechanism. (DO-bias) <br/> (b). The professor proved the theory
    revealed the underlying mechanism. (SC-bias)

Verb bias which has been widely investigated in previous studies
essentially is a non-structural factor. The current investigation,
however, addresses the influence from a verb pertaining to its
structural properties on L2 ambiguity resolution. According to the
structural change theory (Pritchett, 1992), reanalysis is particularly
challenging when it involves a major rearrangement of thematic
structure. For example, sentences like (1) involve NP/S ambiguities
since they can take either a NP or a sentence (S) as the complement;
sentences like (2) involve NP/Z ambiguities since they can take either a
NP as the complement or no (Zero) complement. This theory predicts that
NP/Z reanalysis should be more difficult than NP/S reanalysis because
NP/Z reanalysis relative to NP/S reanalysis requires the NP following
the verb to be moved out of its thematic domain. Sturt et al.??(1999)
experimentally confirmed this prediction among NSs. This study aims to
examine whether NNSs are sensitive to this structural property, thus
directly testing this theory in NNSs.Specifically, three research
questions are raised as follows.

(1). Is processing difficulty at disambiguation greater in NP/Z
ambiguity than in NP/S ambiguity for L1 and L2 learners? <br/> (2). Do
L1 and L2 learners equally show sensitivity to verb-related structural
properties during L2 sentence processing? <br/> (3). Does proficiency
modulate learners??? sensitivity to verb properties?

## Method

24 English native speakers and 65 Chinese learners of English read
English sentences segment by segment in a self-paced reading (SPR) task
with a 2 (Complement type: NP/S vs.??NP/Z) \* 2 (Ambiguity: ambiguous
vs.??unambiguous) factorial design. Experimental sentences are adapted
from Sturt et al.??(1999). (2a) & (2b) are for the *ambiguous* condition.
For the *unambiguous* condition, sentences like (2c) are created from
(2a) by adding a complementizer *that* following the verb, and sentences
like (2d) from (2b) by adding a comma following the verb. Sentences were
checked for their plausibility with an acceptability judgement by a
subset of participants who participated in the SPR task. To ensure that
the verbs are biased toward the NP reading, I also checked the
statistical distribution of the kind of complement the verbs prefer in
the COCA corpus.

2.  (a). The Australian woman / saw the famous doctor / had been
    drinking / quite a lot.<br/> (b). Before the woman / visited the
    famous doctor / had been drinking / quite a lot.<br/> (c). The
    Australian woman saw that the famous doctor had been drinking quite
    a lot.<br/> (d). Before the woman visited, the famous doctor had
    been drinking quite a lot.<br/>

## Results and Discussion

In this section, I reported on the results for each experimental task.
First, as for the COCA, a subset of text data was extracted and POS
tagged. The tagged data were submitted for further processing by which
any verb in its past tense following either that or a noun phrase+ a
verb was labeled as S-biased and otherwise NP-biased. I acknowledged
that instances such as ???The professor accepted that theory may be
falsely counted as S-biased but in fact they should be NP-biased. COCA
data were visualized in Figure 1, which indicates that these verbs are
strongly NP-biased.

![Figure
1](/Users/shaohuafang/Desktop/from_mac/Pittcourses/DataScience2021/final_project/Structural-Change-L2-Sentence-Processing/Data_Analysis/Corpus/Corpus_data_files/figure-gfm/barplot_forCOCA-1.png)

Roughly going through the data set, I found that such instances were in
small number. Future investigation with a large sample of data will go
with dependency parsing for determining the type of complement bias. As
for the acceptability judgement task (AJT), ratings were z-score
transformed by group and condition to mitigate scale bias. AJT was
visualized in Figure 2. Linear mixed-effects models were fit for the
z-score transformed ratings. The random-effect structure was kept
maximal by adding the by-subject and by-item intercept and the
by-subject random slope for Complement Type and by-item slope for Group.
The fixed effect for Complement Type was sum coded. The random effect
structure was simplified only when models converged. The result showed
that ratings did not significantly differ by condition (*??*=-0.05,
*SE*=0.09, *p*=.554) and language group (*??*=-0.04, *SE*=0.06,
*p*=.543), thus ruling out plausibility as a potential confound.

![Figure
2](/Users/shaohuafang/Desktop/from_mac/Pittcourses/DataScience2021/final_project/Structural-Change-L2-Sentence-Processing/Data_Analysis/AJT/L1_L2_Data/L1_L2_AJT_files/figure-gfm/barplot_forAJT-1.png)
For the self-paced reading task,the data were trimmed as follows prior
to statistical analyses. First, participants whose accuracy on the
comprehension question lower than 80 % were excluded. Reaction times
(RTs) beyond 2.5 SDs from the mean were removed. RTs were then log
transformed to approach normal distribution. The log-transformed RTs
were residualized to adjust for the variability in word length and
individual reading speed. Data across regions from L1 and L2 speakers
were plotted in Figure 3 and 4 respectively. Only region 3, *critical
region*, was analyzed with linear mixed-effects models. For the native
speakers, the random effect structure was also kept maximal by adding
the by-item and by-subject intercept, and by-subject random slope for
Ambiguity and Complement type.Fixed effects for complement type and
ambiguity were sum coded. For L2 speakers, the random effect structure
was identical as above except that the by-item random slope for Language
Proficiency was additionally added. For the L1, there are main effects
of Ambiguity and Complement Type such that unambiguous sentences were
read faster their ambiguous counterparts (*??*=0.224, *SE*=0.039,
*p*\<.0001),and NP/S sentences were read more quickly than NP/Z
sentences (*??*=-0.124, *SE*=0.039, *p*=.0016). Critically, there was an
interaction between Complement Type and Ambiguity such that NP/S
sentences were read faster than NP/Z sentences only in the ambiguous
condition (*??*=-0.194, *SE*=0.055, *p*=.0005).For the L2, main effects
of Ambiguity and Complemet Type were also present. Similarly, Ambiguity
and Complemet Type interacte such that NP/S sentences were read faster
than NP/Z sentences only in the ambiguous condition (*??*=-0.117,
*SE*=0.042, *p*=.0054).Proficiency modulated only the overall RTs but
not the extent to which learners were sensitive to structural properties
(*??*=0.044, *SE*=0.018, *p*=.0137). As such, both the L1 and L2 speakers
were sensitive to the structural properties pertaining to verbs, and
proficiency did not modulate learners??? sensitivity.

![Figure
3](/Users/shaohuafang/Desktop/from_mac/Pittcourses/DataScience2021/final_project/Structural-Change-L2-Sentence-Processing/Data_Analysis/SPR/SPR_Analysis_files/figure-gfm/lineplot_L1-1.png)

![Figure
4](/Users/shaohuafang/Desktop/from_mac/Pittcourses/DataScience2021/final_project/Structural-Change-L2-Sentence-Processing/Data_Analysis/SPR/SPR_Analysis_files/figure-gfm/lineplot_L2-1.png)

## Reflection

Overall, it???s not an easy task for me to wrap up all these analysis
procedures, especially for the COCA one which involves the flexible use
of regular expressions. As said, it would be necessary to practice
dependency parsing on the COCA data in the future work. But aspects of
the analysis processes are really rewarding, i.e., data wrangling,
statistical modeling, R Markdown.

## References

Dussias, P. E., & Scaltz, T. R. C. (2008). Spanish???English L2 speakers???
use of subcategorization bias information in the resolution of temporary
ambiguity during second language reading. *Acta psychologica, 128*(3),
501-513.

Garnsey, S. M., Pearlmutter, N. J., Myers, E., & Lotocky, M. A. (1997).
The contributions of verb bias and plausibility to the comprehension of
temporarily ambiguous sentences. *Journal of memory and language,
37*(1), 58-93.

Hopp, H. (2015). Individual differences in the second language
processing of object???subject ambiguities. *Applied Psycholinguistics,
36*(2), 129-173.

Jegerski, J. (2012). The processing of subject???object ambiguities in
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
