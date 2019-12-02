# 2019-11-25 - Hidden Markov Model for C. elegans behavior

After digging into the data initially, I have some questions.

1. The behavior codes are a bit ambiguous (or, at least it is not obvious to me what some of them are meant to be). 
    - The column `beh1_name` has both character and numerical categorical identifiers:
    ```{r}
    > unique(worms.data$beh1_name)
 [1] "NaN"    "rev"    "oparc"  "pir"    "line"   "pauses" "om"     "loop"  
 [9] "clarc"  "93"     "83"     "94"     "92"     "91"     "82"     "84"    
[17] "81" 
    ```