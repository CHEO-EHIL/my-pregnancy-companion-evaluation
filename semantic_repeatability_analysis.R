# libraries
library(tidyverse)
library(readxl)


#### Semantic repeatability ----

# read data
df <- read.csv("data/semantic_repeatability_summary.csv")

# summarize
summary(df$semantic_repeatability_score)

# look at distribution
hist(df$semantic_repeatability_score)

# get logits
df$logit_semantic_repeatability_score <- qlogis(df$semantic_repeatability_score)

# summarize and plot logits
summary(df$logit_semantic_repeatability_score)
hist(df$logit_semantic_repeatability_score)

# one-sided test
t.test(df$logit_semantic_repeatability_score, alternative = "greater", mu = qlogis(0.90))

# backtransformed 95% confidence interval
two_sided <- t.test(df$logit_semantic_repeatability_score)
plogis(two_sided$conf.int[1]) # lower bound
plogis(two_sided$estimate) # estimate
plogis(two_sided$conf.int[2]) # upper bound


#### Tool call repeatability ----

# read data
df_tool <- read.csv("data/tool_repeatability_summary.csv")

# subset
df_tool |> dplyr::select(query_id, prop_modal_set) -> df_tool_subset

# merge
df_merge <- merge(df, df_tool_subset, by = "query_id")

# add T/F column
df_merge$perfect_tool_call <- ifelse(df_merge$prop_modal_set == 1, TRUE, FALSE)

# split
df_T <- df_merge[df_merge$perfect_tool_call,]
df_F <- df_merge[!df_merge$perfect_tool_call,]

# get summaries
summary(df_T$semantic_repeatability_score)
summary(df_F$semantic_repeatability_score)
