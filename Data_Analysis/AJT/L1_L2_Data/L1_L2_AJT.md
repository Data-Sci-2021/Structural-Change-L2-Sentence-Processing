Structural change in L2 sentence processing - AJT
================
Shaohua Fang
Updated December 08 2021

## Load R packages

# AJT data

## Load pre-processed L1 AJT data

``` r
if(file.exists("L1_AJT_Processed.csv")){
  AJT_L1_Processed <- read.csv("L1_AJT_Processed.csv", strip.white=TRUE, header=T, stringsAsFactors=F)
} 
```

## Load pre-processed L2 AJT data

``` r
if(file.exists("L2_AJT_All.csv")){
  AJT_L2_Processed <- read.csv("L2_AJT_All.csv", strip.white=TRUE, header=T, stringsAsFactors=F)
} 
```

## Add language group

``` r
AJT_L1_Processed <- AJT_L1_Processed %>% 
   mutate(Language = "L1_English")
AJT_L2_Processed <- AJT_L2_Processed %>% 
   mutate(Language= "L2_English")
```

## Combined processed L1 and L2 AJT data

``` r
Combined_L1_L2_AJT <- bind_rows(AJT_L1_Processed,AJT_L2_Processed)
length(unique(Combined_L1_L2_AJT$Subject)) # 172 L1(37)+L2(135) participants 
```

    ## [1] 172

## Z-score transformation

``` r
# to account for the fact that people may perceive the scale differently, z-score transformation is applied by language group and individual subjects 
Combined_L1_L2_AJT <- Combined_L1_L2_AJT %>% group_by(Subject,Language) %>% mutate(zrating=scale(rating))
```

## Filter Exp4

``` r
# Filter exp 4
Exp4 <- Combined_L1_L2_AJT %>% filter(str_detect(condition,"NP."))
```

``` r
# Split the "condition" column 
Part <- Exp4 %>% filter(str_detect(condition,"Exp4."))
Part <- Part %>%
  separate(condition, c("Exp4", "NP", "Complement_type", "List", "Version", "Itemnum"))
Part <- Part %>% select(-Exp4) # get rid of Exp4 column


Part1 <- Exp4 %>% filter(!str_detect(condition,"Exp4."))
Part1 <- Part1 %>%
  separate(condition, c("NP", "Complement_type", "List", "Version", "Itemnum"))

# Combined Part and Part 1
Part <- Part %>% select(Subject,Complement_type,Itemnum, percent_corr, rating, zrating, Language)
Part1 <- Part1 %>% select(Subject,Complement_type,Itemnum, percent_corr, rating, zrating, Language)

Updated_L1_L2_AJT <- rbind(Part,Part1)

Updated_L1_L2_AJT$Language <- as.factor(Updated_L1_L2_AJT$Language)

length(unique(Updated_L1_L2_AJT$Subject)) # 109 L1+L2, because some of the L2 participants didn't do the task - they instead did exp5.
```

    ## [1] 109

## 

``` r
# Change column name 
names(Updated_L1_L2_AJT)[names(Updated_L1_L2_AJT) == "percent_corr"] <- "LexTALE_score"
names(Updated_L1_L2_AJT)[names(Updated_L1_L2_AJT) == "Language"] <- "group" 
```

## Centering and standardize LexTALE_score

``` r
Updated_L1_L2_AJT <- Updated_L1_L2_AJT %>% mutate(cen=center(LexTALE_score)) %>% mutate(nor=scale(cen))
Updated_L1_L2_AJT$nor <- as.numeric(Updated_L1_L2_AJT$nor)
```

``` r
# Data type conversion
Updated_L1_L2_AJT$Subject <- as.factor(Updated_L1_L2_AJT$Subject)
Updated_L1_L2_AJT$Complement_type <- as.factor(Updated_L1_L2_AJT$Complement_type)
Updated_L1_L2_AJT$Itemnum <- as.factor(Updated_L1_L2_AJT$Itemnum)
Updated_L1_L2_AJT$LexTALE_score <- as.numeric(Updated_L1_L2_AJT$LexTALE_score)
Updated_L1_L2_AJT$zrating <- as.numeric(Updated_L1_L2_AJT$zrating)
Updated_L1_L2_AJT$group <- as.factor(Updated_L1_L2_AJT$group)
Updated_L1_L2_AJT$cen <- as.numeric(Updated_L1_L2_AJT$cen)
Updated_L1_L2_AJT$nor <- as.numeric(Updated_L1_L2_AJT$nor)
```

``` r
# Rename factor level 
levels(Updated_L1_L2_AJT$Complement_type)[levels(Updated_L1_L2_AJT$Complement_type)=="S"] <- "NP/S"
levels(Updated_L1_L2_AJT$Complement_type)[levels(Updated_L1_L2_AJT$Complement_type)=="Z"] <- "NP/Z"
```

## Plot by condition and group

``` r
#Summarize by Subject, not observations. This gives you a mean proportion of errors *by Subject*.
AJT_bysubject<- Updated_L1_L2_AJT %>% group_by(group,Complement_type,Subject) %>% summarise_at("zrating", list(mean=mean, sd=sd, min=min, max=max), na.rm=TRUE)

# note the standard errors below are calculated over participant means, not observation means!!
# also note that the loading RMisc causes conflicts with tidyverse dyplyr, so load Rmisc first.
summary_AJTstats <- summarySE(AJT_bysubject, measurevar="mean", groupvars=c("Complement_type","group"), na.rm=T)

# Error bars reflect standard errors calculated over subject means
ggplot(summary_AJTstats, aes(x=group, y=mean,fill=Complement_type)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+ 
  xlab("group") + ylab("zrating") + theme(plot.title = element_text(hjust = 0.5))
```

![](L1_L2_AJT_files/figure-gfm/barplot_forAJT-1.png)<!-- -->

## Regression modeling

``` r
contrasts(Updated_L1_L2_AJT$Complement_type) <- c(-0.5,0.5)
contrasts(Updated_L1_L2_AJT$group) <- c(-0.5,0.5)

Model_AJT<-lmer(zrating~ Complement_type*group+(1|Itemnum)
               +(Complement_type|Subject), control=lmerControl(optimizer="bobyqa"), data=Updated_L1_L2_AJT, REML=F)

summary(Model_AJT)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
    ##   method [lmerModLmerTest]
    ## Formula: 
    ## zrating ~ Complement_type * group + (1 | Itemnum) + (Complement_type |  
    ##     Subject)
    ##    Data: Updated_L1_L2_AJT
    ## Control: lmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   2368.2   2413.2  -1175.1   2350.2     1081 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5095 -0.1515  0.2434  0.5524  2.9203 
    ## 
    ## Random effects:
    ##  Groups   Name             Variance Std.Dev. Corr 
    ##  Subject  (Intercept)      0.058540 0.24195       
    ##           Complement_type1 0.008406 0.09168  -0.01
    ##  Itemnum  (Intercept)      0.096467 0.31059       
    ##  Residual                  0.423250 0.65058       
    ## Number of obs: 1090, groups:  Subject, 109; Itemnum, 60
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error        df t value Pr(>|t|)    
    ## (Intercept)               0.37490    0.05139  88.88030   7.295 1.19e-10 ***
    ## Complement_type1         -0.05405    0.09084  60.69231  -0.595    0.554    
    ## group1                   -0.03923    0.06433 105.19769  -0.610    0.543    
    ## Complement_type1:group1   0.10700    0.08542 103.43612   1.253    0.213    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cmpl_1 group1
    ## Cmplmnt_ty1  0.000              
    ## group1      -0.201  0.000       
    ## Cmplmnt_1:1  0.000 -0.151 -0.001

## Filter a sample of data from AJT for the project of ???Data Science??? class

``` r
# Filter the first 1000 rows of the data
nrow(Updated_L1_L2_AJT) # 4690
```

    ## [1] 4690

``` r
AJT_Sample <- Updated_L1_L2_AJT[1:1000,]

# Anoymize the subjects 
AJT_Sample$Subject <- as.factor(AJT_Sample$Subject)
AJT_Sample$Subject <- as.numeric(AJT_Sample$Subject)
AJT_Sample$Subject <- as.factor(AJT_Sample$Subject)

1000/4690
```

    ## [1] 0.2132196

``` r
# Read out this sample in csv. 
write.csv(AJT_Sample,"AJT_Sample_for_DS_class.csv", row.names = FALSE)
```
