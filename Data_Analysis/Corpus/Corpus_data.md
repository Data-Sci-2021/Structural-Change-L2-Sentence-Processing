Corpus data
================
Shaohua Fang
17 November 2021

``` r
# set options
options(stringsAsFactors = F)          # no automatic data transformation
options("scipen" = 100, "digits" = 12) # suppress math annotation
# activate packages
library(quanteda)
```

    ## Package version: 3.1.0
    ## Unicode version: 13.0
    ## ICU version: 69.1

    ## Parallel computing: 16 of 16 threads used.

    ## See https://quanteda.io for tutorials and examples.

``` r
library(gutenbergr)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
    ## ✓ readr   2.0.2     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(flextable)
```

    ## 
    ## Attaching package: 'flextable'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     compose

``` r
library(readtext)
library(quanteda)
```

``` r
# load packages
library(igraph)
```

    ## 
    ## Attaching package: 'igraph'

    ## The following object is masked from 'package:flextable':
    ## 
    ##     compose

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     as_data_frame, groups, union

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     compose, simplify

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     crossing

    ## The following object is masked from 'package:tibble':
    ## 
    ##     as_data_frame

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

``` r
library(tm)
```

    ## Loading required package: NLP

    ## 
    ## Attaching package: 'NLP'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     annotate

    ## The following objects are masked from 'package:quanteda':
    ## 
    ##     meta, meta<-

    ## 
    ## Attaching package: 'tm'

    ## The following object is masked from 'package:quanteda':
    ## 
    ##     stopwords

``` r
library(NLP)
library(openNLP)
library(openNLPdata)
library(coreNLP)
library(koRpus)
```

    ## Loading required package: sylly

    ## For information on available language packages for 'koRpus', run
    ## 
    ##   available.koRpus.lang()
    ## 
    ## and see ?install.koRpus.lang()

    ## 
    ## Attaching package: 'koRpus'

    ## The following object is masked from 'package:tm':
    ## 
    ##     readTagged

    ## The following object is masked from 'package:readr':
    ## 
    ##     tokenize

    ## The following objects are masked from 'package:quanteda':
    ## 
    ##     tokens, types

``` r
library(koRpus.lang.en)
library(phrasemachine)
```

    ## phrasemachine: Simple Phrase Extraction
    ## Version 1.1.2 created on 2017-05-29.
    ## copyright (c) 2016, Matthew J. Denny, Abram Handler, Brendan O'Connor.
    ## Type help('phrasemachine') or
    ## vignette('getting_started_with_phrasemachine') to get started.
    ## Development website: https://github.com/slanglab/phrasemachine

``` r
library(flextable)
# load function for pos-tagging objects in R
source("https://slcladal.github.io/rscripts/POStagObject.r") 
# syntax tree drawing function
source("https://slcladal.github.io/rscripts/parsetgraph.R")
```

## reading in COCA in txt.

``` r
my_data <- readLines("2012_acad.txt")
```

``` r
# clean data
my_data <- my_data %>%
 str_squish() 
```

``` r
POStag <- function(object){
  require("stringr")
  require("NLP")
  require("openNLP")
  require("openNLPdata")
  # define paths to corpus files
  corpus.tmp <- object
  # define sentence annotator
  sent_token_annotator <- openNLP::Maxent_Sent_Token_Annotator()
  # define word annotator
  word_token_annotator <- openNLP::Maxent_Word_Token_Annotator()
  # define pos annotator
  pos_tag_annotator <- openNLP::Maxent_POS_Tag_Annotator(language = "en", probs = FALSE, 
    # WARNING: YOU NEED TO INCLUDE YOUR OWN PATH HERE!                                            
    model = "/Library/Frameworks/R.framework/Versions/4.1/Resources/library/openNLPdata/models/en-pos-maxent.bin")
  # convert all file content to strings
  Corpus <- lapply(corpus.tmp, function(x){
    x <- as.String(x)  }  )
  # loop over file contents
  lapply(Corpus, function(x){
    y1 <- NLP::annotate(x, list(sent_token_annotator, word_token_annotator))
    y2<- NLP::annotate(x, pos_tag_annotator, y1)
    y2w <- subset(y2, type == "word")
    tags <- sapply(y2w$features, '[[', "POS")
    r1 <- sprintf("%s/%s", x[y2w], tags)
    r2 <- paste(r1, collapse = " ")
    return(r2)  }  )
  }
```

## pos tagging data

``` r
textpos <- POStag(object = my_data)
tagged <- flatten_chr(textpos)
```

## This is to extract the designated verbs in the past tense.

``` r
kwic_natural <- kwic(x = tagged, pattern = c("accepted","maintained","recalled","heard","confirmed","forgot","mentioned","found","announced","discovered","noticed","saw","acknowldged","remembered","read","revealed","doubted","negotiated","polished","scratched","packed","typed","built","painted","debated","lost","investigated","watched","knitted","visited","questioned","attacked","invaded","edited","washed","followed","typed"), window = 10) 
```

    ## Warning: 'kwic.character()' is deprecated. Use 'tokens()' first.

## This is to create columns POS for the verbs and one word following them by using regex

``` r
AA <- kwic_natural %>% 
  as.data.frame() %>% 
  mutate(POS = str_extract(post, "[A-Z]+"),
         FollWord = str_extract(post, "\\S+ / \\S+"))
head(AA)
```

    ##   docname  from    to                                       pre      keyword
    ## 1   text1    17    17             JJ # / # This / DT study / NN investigated
    ## 2   text1  7106  7106      NNS across / IN time / NN were / VBD        found
    ## 3   text1  7390  7390 . Significant / JJ gains / NNS were / VBD        found
    ## 4   text1  7607  7607       JJ gains / NNS were / VBD also / RB        found
    ## 5   text1  9090  9090         . A / DT one-way / JJ ANOVA / NNP     revealed
    ## 6   text1 11358 11358   : Perceived / JJ utility / NN was / VBD        found
    ##                                       post      pattern POS  FollWord
    ## 1 / VBD the / DT relationship / NN among / investigated VBD  the / DT
    ## 2       / VBN for / IN confidence / NN ( /        found VBN  for / IN
    ## 3          / VBN for / IN amount / NN of /        found VBN  for / IN
    ## 4        / VBN for / IN ratings / NNS of /        found VBN  for / IN
    ## 5     / VBD that / IN these / DT changes /     revealed VBD that / IN
    ## 6             / VBN to / TO be / VB much /        found VBN   to / TO

## This is to filter for the verbs in the VBN POS and in the not sentence-final position

## I’d want another two subsequent words that follow FollWord to be shown so that further analyses can be done on them. Have no idea how to get them.

``` r
BB <- AA %>% 
  as.data.frame() %>% 
  mutate(POS = str_extract(post, "[A-Z]+"),
         FollWord = str_extract(post, "\\S+ / \\S+")) %>%
  filter(POS=="VBN",
         FollWord!=". / .")
```

``` r
kwic_natural %>%
  # convert to data frame
  as.data.frame() %>%
  # create new CleanPost where tags and words are merged
  dplyr::mutate(CleanPost = stringr::str_replace_all(post, " {0,}/ {0,}", "/")) %>%
  # extract tag of concordanced word
  dplyr::mutate(PosKeyword = stringr::str_replace(CleanPost, "/([:alnum:]{2,}) .*", "\\1")) %>%
  # extract first element after pos-tag of keyword
  dplyr::mutate(FirstWord = stringr::str_remove(CleanPost, ".*? "),
                FirstWord = stringr::str_remove_all(FirstWord, " .*")) %>%
  # extract second element after pos-tag of keyword
  dplyr::mutate(SecWord = stringr::str_remove(CleanPost, ".*? "),
                SecWord = stringr::str_remove(SecWord, ".*? "),
                SecWord = stringr::str_remove_all(SecWord, " .*")) %>%
  # extract third element after pos-tag of keyword
  dplyr::mutate(ThirdWord = stringr::str_remove(CleanPost, ".*? "),
                ThirdWord = stringr::str_remove(ThirdWord, ".*? "),
                ThirdWord = stringr::str_remove(ThirdWord, ".*? "),
                ThirdWord = stringr::str_remove_all(ThirdWord, " .*")) -> BB
# inspect results
head(BB, 20)
```

    ##    docname  from    to                                        pre      keyword
    ## 1    text1    17    17              JJ # / # This / DT study / NN investigated
    ## 2    text1  7106  7106       NNS across / IN time / NN were / VBD        found
    ## 3    text1  7390  7390  . Significant / JJ gains / NNS were / VBD        found
    ## 4    text1  7607  7607        JJ gains / NNS were / VBD also / RB        found
    ## 5    text1  9090  9090          . A / DT one-way / JJ ANOVA / NNP     revealed
    ## 6    text1 11358 11358    : Perceived / JJ utility / NN was / VBD        found
    ## 7    text1 12056 12056 JJ domain / NN activities / NNS were / VBD        built
    ## 8    text2   623   623 NNP Risk / NNP Behavior / NNP Survey / NNP     revealed
    ## 9    text2  1579  1579      CD % / NN indicating / VBG they / PRP          saw
    ## 10   text2  8859  8859  NN coefficient / NN noted / VBD here / RB     revealed
    ## 11   text2 11038 11038     IN aggression / NN were / VBD not / RB        found
    ## 12   text2 11619 11619        IN the / DT current / JJ study / NN     revealed
    ## 13   text3  6462  6462         NNS of / IN service / NN are / VBP   maintained
    ## 14   text4  2461  2461     JJ results / NNS have / VBP been / VBN      noticed
    ## 15   text4  5717  5717           NN and / CC has / VBZ been / VBN   discovered
    ## 16   text4  5990  5990    NNP results / NNS are / VBP being / VBG      noticed
    ## 17   text5  2914  2914             RB the / DT diet / NN is / VBZ     followed
    ## 18   text6   521   521        NNP contract / NN in / IN 2004 / CD          saw
    ## 19   text6  1243  1243          PRP is / VBZ now / RB widely / RB     accepted
    ## 20   text6  1534  1534       IN patient / JJ safety / NN is / VBZ   maintained
    ##                                          post      pattern
    ## 1    / VBD the / DT relationship / NN among / investigated
    ## 2          / VBN for / IN confidence / NN ( /        found
    ## 3             / VBN for / IN amount / NN of /        found
    ## 4           / VBN for / IN ratings / NNS of /        found
    ## 5        / VBD that / IN these / DT changes /     revealed
    ## 6                / VBN to / TO be / VB much /        found
    ## 7          / VBN into / IN students / NNS ' /        built
    ## 8    / VBD a / DT significant / JJ decrease /     revealed
    ## 9  / VBD peers / NNS being / VBG threatened /          saw
    ## 10    / VBN that / IN the / DT relationship /     revealed
    ## 11            / VBN in / IN this / DT study /        found
    ## 12           / VBD that / IN , / , although /     revealed
    ## 13             / VBN at / IN all / DT times /   maintained
    ## 14       / VBN . / . Laboratories / NNP can /      noticed
    ## 15               / VBN to / TO be / VB more /   discovered
    ## 16         / VBN in / IN samples / NNS then /      noticed
    ## 17          / VBN the / DT more / RBR rapid /     followed
    ## 18       / VBD a / DT realisation / NN that /          saw
    ## 19     / VBN that / IN introducing / VBG an /     accepted
    ## 20           / VBN . / . These / DT include /   maintained
    ##                               CleanPost PosKeyword FirstWord          SecWord
    ## 1    /VBD the/DT relationship/NN among/        VBD    the/DT  relationship/NN
    ## 2          /VBN for/IN confidence/NN (/        VBN    for/IN    confidence/NN
    ## 3             /VBN for/IN amount/NN of/        VBN    for/IN        amount/NN
    ## 4           /VBN for/IN ratings/NNS of/        VBN    for/IN      ratings/NNS
    ## 5        /VBD that/IN these/DT changes/        VBD   that/IN         these/DT
    ## 6                /VBN to/TO be/VB much/        VBN     to/TO            be/VB
    ## 7          /VBN into/IN students/NNS '/        VBN   into/IN     students/NNS
    ## 8    /VBD a/DT significant/JJ decrease/        VBD      a/DT   significant/JJ
    ## 9  /VBD peers/NNS being/VBG threatened/        VBD peers/NNS        being/VBG
    ## 10    /VBN that/IN the/DT relationship/        VBN   that/IN           the/DT
    ## 11            /VBN in/IN this/DT study/        VBN     in/IN          this/DT
    ## 12           /VBD that/IN ,/, although/        VBD   that/IN              ,/,
    ## 13             /VBN at/IN all/DT times/        VBN     at/IN           all/DT
    ## 14       /VBN ./. Laboratories/NNP can/        VBN       ./. Laboratories/NNP
    ## 15               /VBN to/TO be/VB more/        VBN     to/TO            be/VB
    ## 16         /VBN in/IN samples/NNS then/        VBN     in/IN      samples/NNS
    ## 17          /VBN the/DT more/RBR rapid/        VBN    the/DT         more/RBR
    ## 18       /VBD a/DT realisation/NN that/        VBD      a/DT   realisation/NN
    ## 19     /VBN that/IN introducing/VBG an/        VBN   that/IN  introducing/VBG
    ## 20           /VBN ./. These/DT include/        VBN       ./.         These/DT
    ##        ThirdWord
    ## 1         among/
    ## 2             (/
    ## 3            of/
    ## 4            of/
    ## 5       changes/
    ## 6          much/
    ## 7             '/
    ## 8      decrease/
    ## 9    threatened/
    ## 10 relationship/
    ## 11        study/
    ## 12     although/
    ## 13        times/
    ## 14          can/
    ## 15         more/
    ## 16         then/
    ## 17        rapid/
    ## 18         that/
    ## 19           an/
    ## 20      include/

``` r
BB <- BB %>% filter(PosKeyword=="VBN")
S1 <- BB %>% filter(str_detect(FirstWord,"that")) # S-biased
S2 <- BB %>% filter(str_detect(FirstWord,"NN|NNS|NNP"), str_detect(SecWord,"VBN"))  # S-biased
S3 <- bind_rows(S1,S2)
S3$bias <- "S-bias"
CC <- left_join(BB,S3, by="docname")
CC$bias[is.na(CC$bias)] <- "NP-biased" # decide the instances categorized as NP-biased
CC$bias <- as.factor(CC$bias)
```

## Plot

``` r
ggplot(data = CC) + 
  geom_bar(mapping = aes(x = bias, y = ..prop.., group = 1), stat = "count")
```

![](Corpus_data_files/figure-gfm/barplot_forCOCA-1.png)<!-- -->
