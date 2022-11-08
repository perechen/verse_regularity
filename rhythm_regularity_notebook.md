Rhythm regularity
================
Artjoms Šeļa
2022-11-08

-   <a href="#setup" id="toc-setup">Setup</a>
-   <a href="#data-preparation" id="toc-data-preparation">Data
    preparation</a>
    -   <a href="#ruscorpora" id="toc-ruscorpora">Ruscorpora</a>
        -   <a href="#sample-by-meter" id="toc-sample-by-meter">Sample by meter</a>
            -   <a href="#cleaning" id="toc-cleaning">Cleaning</a>
            -   <a href="#getting-our-sample" id="toc-getting-our-sample">Getting our
                sample</a>
    -   <a href="#prose-handling" id="toc-prose-handling">Prose handling</a>
        -   <a href="#sample-construction" id="toc-sample-construction">Sample
            construction</a>
            -   <a href="#paragraph-sampling" id="toc-paragraph-sampling">Paragraph
                sampling</a>
            -   <a href="#quasi-poem-construction"
                id="toc-quasi-poem-construction">Quasi-poem construction</a>
-   <a href="#rhythm-entropy" id="toc-rhythm-entropy">Rhythm entropy</a>
    -   <a href="#extracting-intervals" id="toc-extracting-intervals">Extracting
        intervals</a>
    -   <a href="#entropy-calculation" id="toc-entropy-calculation">Entropy
        calculation</a>
-   <a href="#analysis" id="toc-analysis">Analysis</a>
    -   <a href="#entropy-distributions-figure-2"
        id="toc-entropy-distributions-figure-2">Entropy distributions (Figure
        2)</a>
    -   <a href="#hill-numbers" id="toc-hill-numbers">Hill numbers</a>
    -   <a href="#formal-modeling" id="toc-formal-modeling">Formal modeling</a>
        -   <a href="#preparations" id="toc-preparations">Preparations</a>
        -   <a href="#models" id="toc-models">Models</a>
    -   <a href="#umap-projection" id="toc-umap-projection">UMAP projection</a>
        -   <a href="#k-means" id="toc-k-means">k-means</a>

**DISCLAIMER**. NCR is not an open source corpus, so we cannot provide
it here. We demonstrate our handling of data, but the code in the
**Setup** section is not executable autonomously. Jump to **Rhythm
entropy** for fully reproducible analysis.

# Setup

``` r
library(tidyverse)
library(paletteer)
library(tidytext)
theme_set(theme_minimal())


## for regularity calculations
source("src/calculate_regularity.R")
```

# Data preparation

We use two datasets:

– NCR: National Corpus of Russian language, poetry corpus;  
– 19th century narrative fiction (dataverse)

## Ruscorpora

Assume we have stress annotations: files with postfix `accented.txt`
holding individual poems/texts.

``` r
ncr_path = "../../digital-poetics/"
## ruscorp metadata
ruscorp <- read_tsv(paste0(ncr_path,"ruscorpora_new.tsv"))

## get all text paths
rusc_texts <- paste0(ncr_path, "ruscorp_stressed/", ruscorp$path, ".txt")

## get texts
texts <- lapply(rusc_texts,read_file)

## prep a tibble (one row = one text)
texts_df <- tibble(path=ruscorp$path, text=texts  %>% unlist())

## join a trim the table
ruscorp <- ruscorp %>%
  left_join(texts_df,by="path") %>% 
  select(path,start,author,source,header,meter, feet, clausula,text) %>%
  rename(id=path, year=start,pub_title=source,title=header) %>%
  mutate(doc_id=paste0("rusc-",id),
         path=rusc_texts) %>% 
  select(-id)
```

Now, to rhythmically annotated (scanned) poems.

``` r
## path to accented texts
acc_path = paste0(str_remove(ruscorp$path,"\\.txt$"), ".accented.txt")  # fixing stihi ru paths

## read texts to tibble
acc_texts <- tibble(doc_id = ruscorp$doc_id,
                    acc_text = sapply(acc_path, read_file))

## join to the main table
ruscorp <- ruscorp %>% left_join(acc_texts,by="doc_id")

## save
saveRDS(ruscorp,"data/ruscorp_full.rds")
```

### Sample by meter

#### Cleaning

Often forms used in a poem get quite heterogeneous and metrical
annotation – complex and long. We use only clear-cut cases for classic
meter (enough data) and assume that summary definitions for accentual
and less regular forms (dolniks etc.) are trustworthy (the usual form of
these cases is –
`something something iamb, trochee, accentual: Dolnik`). We avoid
further heterometry by using only explicit `: Dolink[end-of-string]`
cases.

Gentle cleaning of metrical labels.

``` r
ruscorp <- readRDS("data/ruscorp_full.rds")

ruscorp <- ruscorp %>%
  # Дк = dolnik
  mutate(meter_tidy = ifelse(str_detect(meter,": Дк$"), "Дк", meter),
         # Тк == taktovik
         meter_tidy = ifelse(str_detect(meter,": Тк$"), "Тк", meter_tidy),
         # Ак == accentual
         meter_tidy = ifelse(str_detect(meter,": Ак$"), "Ак", meter_tidy),
         # Вл == verse libre (free verse)
         meter_tidy = ifelse(str_detect(meter,": Вл"), "Вл", meter_tidy))
```

#### Getting our sample

As you see meter counts has a long tail of weird forms, only few
dominate the distribution

``` r
meters_c <- ruscorp %>% 
  count(meter_tidy,sort=T) %>% 
  mutate(rank = row_number())

ggplot(meters_c %>% filter(rank <= 100), aes(rank, n,group=1)) +
  geom_line(size=1) +
  labs(title="Meters frequency distribution (first 100 ranks)")
```

![](rhythm_regularity_notebook_files/figure-gfm/meters%20distribution-1.png)<!-- -->

Build labels, define subset of meters, sample 700 poems per each meter;
save.

``` r
labs <- c(rep("classic-meter",5), "Accentual, 1-2", "Accentual, 1-3", "Accentual", "Free verse")
## target subset of meters
m_subset <- tibble(meter_tidy=c("Я", "Х", "Д", "Ан", "Аф", "Дк", "Тк", "Ак", "Вл"),type=labs) %>% 
  left_join(meters_c,by="meter_tidy")

set.seed(98910)
s <- ruscorp %>% 
  right_join(m_subset,by="meter_tidy") %>%
  group_by(meter_tidy) %>% 
  sample_n(700)

saveRDS(s,"data/m_sample.rds")
```

## Prose handling

– first construct \~2k word samples, 1 per available book – do the
stress annotation (`ru_accent_poet` module) – grab baseline prose at
paragraphs level (sample 700) – construct quasi-poems (\~700) – record
text + binary

NB!

– measure length (in lines) for a poem (rusc sample) – measure average
line length for a poem (rusc sample)

Read prose corpus:

``` r
f <- list.files("../data/prose/texts/",full.names = T)
p_df <- tibble(file = f, text=lapply(f, read_lines)) %>% unnest(text)

# tally 10 first lines
# remove Ps that are smaller than 100 chars
p_df <- p_df %>%
  group_by(file) %>%
  filter(nchar(text) > 0) %>%
  mutate(ord = row_number()) %>%
  filter(ord > 10,
         nchar(text) > 100)

#saveRDS(p_df,"data/prose_df.rds")
p_df <- readRDS("data/prose_df.rds")
```

Sample consecutive paragraphs to the chunks of length of \~2000 words. 1
text = 1 chunk of 2k words.

``` r
## 2k words chunks

temp <- p_df %>%
  ungroup() %>% 
  mutate(p_id = paste0("pr-",as.character(row_number()))) %>% ## add ids
  group_by(p_id) %>% 
  mutate(n = lengths(str_extract_all(text, "\\s"))) ## approx number of words

## determine chunks
temp <- temp %>%
  group_by(file) %>% 
  mutate(csum = cumsum(n),
         chunk = ceiling(csum/2000))
  

## chunk selection
set.seed(189110)
v <- temp %>%
  select(file,chunk) %>%
  unique() %>% 
  group_by(file) %>% 
  sample_n(1)

s_df <- v %>%
  left_join(temp,by=c("file","chunk"))  %>% ## only selected chunks
  group_by(file) %>% 
  summarize(text = paste(text, collapse = "\n")) %>% 
  mutate(sample=paste0("prs-",row_number()))

## write and clean (python module breaks for weird reasons, e.g. when encounters dashes in specific positions, etc.)

path="../data/prose/prose_samples/"
for(i in 1:nrow(s_df)) {

  p = paste0(path, s_df$sample[i],".txt")
  
  ## cleaning
  t <- s_df$text[i] %>% str_remove_all("'") %>%
    str_replace_all("-", " - ") %>% 
    str_replace_all("/", "")
  
  write_lines(t,file=p)
}
```

Call python stress detection script to folder of prose samples.

``` r
## tensorflow will throw some errors, just ignore them. Detection is not running on GPU currently.

library(reticulate)
py_install("ru_accent_poet --ignore-installed certifi", pip = TRUE)
py_run_file("src/stress_detection.py")
```

### Sample construction

#### Paragraph sampling

``` r
## read accented prose

f <- list.files("../data/prose/prose_samples/")

f_a <- f[str_detect(f,"accented")]

pr_df <- tibble(doc_id = f_a,
       acc_text = sapply(paste0("../data/prose/prose_samples/",f_a), read_lines)) %>% unnest(cols=acc_text)
```

``` r
## prose baseline, 1 text = 2 paragraphs
set.seed(18911)
pr_baseline <- pr_df %>%
  ungroup() %>% 
  mutate(p_id = paste0("pr-",row_number())) %>% 
  group_by(doc_id) %>% 
  mutate(n=max(row_number())) %>% 
  filter(n > 2) %>% sample_n(2)

saveRDS(pr_baseline,"data/baseline_prose.rds")
```

#### Quasi-poem construction

Binarize prose rhythm, keep spaces intact.

``` r
## full texts first
pr_full <- pr_df %>%
#  mutate(p_id = paste0("pr-",row_number())) %>% 
  group_by(doc_id) %>% 
  summarize(acc_text=paste(acc_text,collapse ="\n"))

## binarize and clean
pr_bin <- to_binary(pr_full,
                    document = "doc_id",
                    text="acc_text",
                    preserve_spaces = T) %>%
  mutate(stress_pattern=str_remove(stress_pattern, "^ *"),               stress_pattern=str_remove(stress_pattern, " *$"),
         stress_pattern=str_replace_all(stress_pattern, " {2,10}", " "))

knitr::kable(head(pr_bin,5))
```

| row_id | doc_id             | stress_pattern                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|-------:|:-------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|      1 | prs-1.accented.txt | 0 0 010 10 0 001 001 0 01 0 010 10 0100 0 0 01 01 001 0 0 10 0 0010 100 01 010 0 0 10 010 10 0100 0010 01 0 100 10 01 0 01 0 10 010 00010 0 0 10 10 100 10 0 01 0 001 0 0010 01 010 0 100 10 0010 01 0 001 0 0100 0 10 01 0010 001 0 0 0 01 0 0 010 01 00100 010 0 0 0 01 0 01 0 10 0 10 01 01 010 01 010 100 0 0100 10 01 000100 0 010 01 00010 1000 0 010 0 100 00100 10 0 0 0 0 10 10 01 0 10 0 0 0 0 010 01 0 00100 0 0 01 0 0 0 0 0 10 10 010 0 10 01 010 0 10 0 001 010 100 0 0 10 01 01000 01 01 10 10 010 010 010 010 0 0 0 0 10 0 00100 0 10 010 0 00100 100 100 0 0010 0 0 10 10 01 10 01 0 0 0100 0 01 0 0 0 001 0 0010 01 01 0010 0 010 010 100 0 0 0 10 10 10 001000 001 0010 01 0 0 010 0 0 0 0 0 0 10 0 10 0 0 001 0 0 0 0 01 0 100 10 10 010 010 010 010 0 10 0 0 0010 0100 |
|      2 | prs-1.accented.txt | 010 100 0 010 01 0 0010 01 01 001 0 10 010 010 0001                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|      3 | prs-1.accented.txt | 0 0 0 10 01 01 01 0 01 0 010 0 10 0 100 01 10 010 010 10 01 0100 10 0 0 0 01 010 10 0 0 001 10 0010 01 0 0 010 0 01 0 00 100                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|      4 | prs-1.accented.txt | 0 0 10 10 01 0010 0 0010 01 01 0 0 01 10 1000 0 0 01 0010 0 10 0 100 10 0 10 100 0010 0 0 0 0 000100 0 0010 0 10 10 00010 0100 0 0 01 0 010 01 01 10 0 10 010 10 01 01 10 10 010 0 010 0100 0 01 001 0 01 0 0 010 10 01 001 01000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|      5 | prs-1.accented.txt | 0 001 0 01 001 0 100 0 0010 0 0 01 10 0 10 01 01 10 0 001 01 0                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |

Get empirical distributions of line lengths and syllables per line.

``` r
ru <- ruscorp %>%
  ungroup() %>% 
  mutate(doc_id = paste("rusc-",row_number()))

## all vowels. In Russian 1 vowel == 1 syllable
vowels <-  c("а", "е","ё", "у", "о", "ю", "я", "и", "ы", "э")
syl_pattern <- paste(vowels, collapse="|")
## will take time

## unnest poetic lines
ru_unnested <- ru %>% 
  mutate(acc_text=str_split(acc_text, "\n")) %>% 
  unnest(cols=c(acc_text))

## count line length in syllables  + number of lines in a poem
ru_counts <- ru_unnested %>% 
  mutate(n_syl=str_extract_all(tolower(acc_text),syl_pattern) %>% lengths()) %>% 
  group_by(doc_id) %>% 
  filter(nchar(acc_text) > 0) %>% 
  mutate(n_lines = max(row_number()))

lines_pool <- ru_counts$n_syl
lines_pool <- lines_pool[lines_pool != 0] # fixing 0-length line (prob in other languages)
length_pool <- ru_counts %>% select(doc_id,n_lines) %>% unique()

p_len <- ggplot(length_pool,aes(n_lines)) + geom_histogram(bins=100) + scale_x_log10() + labs(title="Syllables per line")

p_lin <- tibble(n_syl=lines_pool) %>% ggplot(aes(n_syl)) + geom_histogram(bins = 100) + scale_x_log10() + labs(title="Lines per poem")

library(patchwork)
p_lin + p_len
```

![](rhythm_regularity_notebook_files/figure-gfm/prepare%20poetry%20for%20quasi-1.png)<!-- -->
Split by words, calculate syllable cumulative sum (to chunk
consecutively)

``` r
# calculate n sylables in words and cumulative sum
pr_sim <- pr_bin %>% 
  mutate(stress_pattern=str_split(stress_pattern, " ")) %>%
  unnest(cols=stress_pattern) %>% 
  group_by(doc_id) %>% 
  mutate(nsyl_w = nchar(stress_pattern),
         nsyl_cum = cumsum(nsyl_w))

## syllables total
total <- pr_sim %>% 
  group_by(doc_id) %>%
  summarize(total_syl=sum(nsyl_w))

pr_split <- pr_sim %>% group_by(doc_id) %>% group_split()

knitr::kable(head(pr_split[[1]],10))
```

| row_id | doc_id             | stress_pattern | nsyl_w | nsyl_cum |
|-------:|:-------------------|:---------------|-------:|---------:|
|      1 | prs-1.accented.txt | 0              |      1 |        1 |
|      1 | prs-1.accented.txt | 0              |      1 |        2 |
|      1 | prs-1.accented.txt | 010            |      3 |        5 |
|      1 | prs-1.accented.txt | 10             |      2 |        7 |
|      1 | prs-1.accented.txt | 0              |      1 |        8 |
|      1 | prs-1.accented.txt | 001            |      3 |       11 |
|      1 | prs-1.accented.txt | 001            |      3 |       14 |
|      1 | prs-1.accented.txt | 0              |      1 |       15 |
|      1 | prs-1.accented.txt | 01             |      2 |       17 |
|      1 | prs-1.accented.txt | 0              |      1 |       18 |

Each chunk of prose we cut to as many quasi poems as we can fit there;
for each poem we determine lengths in lines and length of line drawing
from RNC distribution.

``` r
## new vector for dataframes with quasi poems constructed from each text

quasi_p_list <- vector(length = length(pr_split),mode="list")


set.seed(76513)

# For each large prose chunk
for(doc in 1:length(pr_split)) {

## vars  
x <- pr_split[[doc]]
d <- total$total_syl[doc]
orig_id <- x$doc_id %>% unique()

#print(orig_id)
## lists for GROWING vectors (don't know how large and too lazy to estimate)
p_list <- NULL
ll_list <- NULL

## Cut as many poems as possible, stop when total available syllables are exceeded 
## def not more than 5m poems
for(i in 1:5000000) {
  ## get poem size
  p <- sample(length_pool$n_lines,1)
  ## get line length
  ll <- sample(lines_pool,1)
  
  ## save numbers
  p_list <- c(p_list,p)
  ll_list <- c(ll_list,ll)
  
  ## if larger than total syllable count, break
  if(sum(p_list*ll_list) > d) {
    break
  }
  
  
  
}

## get lines according to poem sizes
lls <- rep(ll_list,p_list)

## quasi poem IDs mathcing poem sizes
ids <- paste0("quasip-",1:length(p_list),"-",doc)
ids_c <- rep(ids,p_list)

## cut <<approximately>>, each line length - step in cumulative sum
parts = x %>% 
  mutate(part = cut(x$nsyl_cum, cumsum(c(0,lls)))) %>%
  group_by(part) %>%
  mutate(quasi_id = cur_group_id())

## how many words per line, back to lines
rows_per_part = parts %>%
  group_by(quasi_id) %>% 
  summarise(n = max(row_number()))

## updated IDS length (too many syllables for the last poem)
upd_ids <- ids_c[1:nrow(rows_per_part)]

## now we add poem ids to lines
rows_per_part <- rows_per_part %>% mutate(qp_id = upd_ids)

## uncounting it back to the long table
qpoem_ids <- rows_per_part %>% uncount(n)

## providing poem IDs back, summarizing by quasi lines
v=parts %>%
  ungroup()  %>% 
  mutate(qp_id=qpoem_ids$qp_id) %>%
  group_by(qp_id,quasi_id) %>%
  summarize(stress_pattern=paste(stress_pattern,collapse = ""),
            doc_id=orig_id)

quasi_p_list[[doc]] <- v
## median line length is 9

}
```

So let’s see what we get: this is one quasi poem of 14 lines cut
approximately to 11 syllables per line (both dimension were determined
randomly based on RNC distributions)—approximately, because it respects
the words and does not cut through words.

``` r
knitr::kable(quasi_p_list[[1]] %>% filter(qp_id=="quasip-1-1"))
```

| qp_id      | quasi_id | stress_pattern   | doc_id             |
|:-----------|---------:|:-----------------|:-------------------|
| quasip-1-1 |        1 | 00010100001      | prs-1.accented.txt |
| quasip-1-1 |        2 | 001001001010     | prs-1.accented.txt |
| quasip-1-1 |        3 | 010000010100100  | prs-1.accented.txt |
| quasip-1-1 |        4 | 100001010001     | prs-1.accented.txt |
| quasip-1-1 |        5 | 010001001010     | prs-1.accented.txt |
| quasip-1-1 |        6 | 0100001001010010 | prs-1.accented.txt |
| quasip-1-1 |        7 | 01001010010      | prs-1.accented.txt |
| quasip-1-1 |        8 | 00010001010100   | prs-1.accented.txt |
| quasip-1-1 |        9 | 10001000100010   | prs-1.accented.txt |
| quasip-1-1 |       10 | 01010010010      | prs-1.accented.txt |
| quasip-1-1 |       11 | 001001000100100  | prs-1.accented.txt |
| quasip-1-1 |       12 | 0100100100010    | prs-1.accented.txt |
| quasip-1-1 |       13 | 00010001001      | prs-1.accented.txt |
| quasip-1-1 |       14 | 00100010000010   | prs-1.accented.txt |

In total, we have \~8.5 k poems. Now saving as data frame.

``` r
quasi_df <- quasi_p_list %>% bind_rows()

## ~8.5k poems out of 500 texts
quasi_df$qp_id %>% unique() %>% length()
```

    ## [1] 8570

``` r
saveRDS(quasi_df,"data/quasi_df.rds")

quasi_df
```

    ## # A tibble: 213,728 × 4
    ## # Groups:   qp_id [8,570]
    ##    qp_id      quasi_id stress_pattern   doc_id            
    ##    <chr>         <int> <chr>            <chr>             
    ##  1 quasip-1-1        1 00010100001      prs-1.accented.txt
    ##  2 quasip-1-1        2 001001001010     prs-1.accented.txt
    ##  3 quasip-1-1        3 010000010100100  prs-1.accented.txt
    ##  4 quasip-1-1        4 100001010001     prs-1.accented.txt
    ##  5 quasip-1-1        5 010001001010     prs-1.accented.txt
    ##  6 quasip-1-1        6 0100001001010010 prs-1.accented.txt
    ##  7 quasip-1-1        7 01001010010      prs-1.accented.txt
    ##  8 quasip-1-1        8 00010001010100   prs-1.accented.txt
    ##  9 quasip-1-1        9 10001000100010   prs-1.accented.txt
    ## 10 quasip-1-1       10 01010010010      prs-1.accented.txt
    ## # … with 213,718 more rows

# Rhythm entropy

**DISCLAIMER!** Code above should be autonomously executable, it has all
data needed for this.

## Extracting intervals

Read and combine corpora

``` r
## already binarized

quasi_prose <- readRDS("data/quasi_df.rds") 


## sample 2 quasi poems from each prose text
set.seed(138113)
sq <- quasi_prose %>% 
  group_by(doc_id) %>%
  count(qp_id) %>%
  mutate(nr = max(row_number())) %>%
  filter(nr >= 2) %>% 
  group_by(doc_id) %>% 
  sample_n(2)

## filter main table
quasi_prose <- quasi_prose %>%
  filter(qp_id %in% sq$qp_id) %>% 
  ungroup() %>% 
  mutate(doc_id=qp_id) %>% 
  mutate(row_id = row_number()) %>% 
  select(row_id,doc_id,stress_pattern)

baseline_prose <- readRDS("data/baseline_prose.rds") %>%
  ungroup() %>% 
  select(p_id,acc_text) %>% 
  rename(doc_id=p_id)

ru_poetry <- readRDS("data/m_sample.rds") %>%
  mutate(paste0("rusc-",doc_id))

ru_sample <- ru_poetry %>%
  ungroup() %>% 
  select(doc_id,acc_text,type)
```

Binarize baseline prose and RNC (quasi poems are already binarized)

``` r
corpus <- ru_sample %>%
  bind_rows(baseline_prose)

v <- to_binary(corpus,document="doc_id",text="acc_text")

## bind with quasi prose, fix row ids
v2 <- v %>%
  bind_rows(quasi_prose) %>%
  ungroup() %>% 
  mutate(row_id = row_number(),
         line_length = nchar(stress_pattern)) 

ll <- v2 %>%
  group_by(doc_id) %>%
  summarize(mean_ll = mean(line_length))



## extracts intervals: leaves anacrusis, drops clausula, "11" cases count as interval of length 0
i <- extract_intervals(v2, no_anacrusis = F, no_clausula = T)

### checks 
s1 <- i$doc_id %>% unique() %>% sample(50)
sch <- i %>% filter(doc_id %in% s1)
####
```

## Entropy calculation

``` r
## counts likelihood of intervals in each text
int_count <- rhythm_inequality(i, raw_values = T,drop_rare = T)


## entropy for each text
entr <- int_count %>%
  group_by(doc_id) %>%
  summarise(entropy = -sum(log2(n)*n))


entr_meta <- entr %>%
  # poetry info
  left_join(ru_poetry %>% ungroup() %>% select(doc_id,meter_tidy,type),by="doc_id") %>% 
  # subset type info
  mutate(type=ifelse(is.na(type), str_extract(doc_id,"^.*?-"),type)) %>% 
  # mean line lengths info
  left_join(ll,by="doc_id") %>% 
  mutate(meter_tidy=ifelse(is.na(meter_tidy), type,  meter_tidy))

saveRDS(entr_meta,"data/entr_fin.rds")

knitr::kable(head(entr_meta,10))
```

| doc_id   |  entropy | meter_tidy | type | mean_ll |
|:---------|---------:|:-----------|:-----|--------:|
| pr-10024 | 2.436060 | pr-        | pr-  |     401 |
| pr-1004  | 1.500000 | pr-        | pr-  |      23 |
| pr-10052 | 1.459148 | pr-        | pr-  |      30 |
| pr-10059 | 2.246439 | pr-        | pr-  |      31 |
| pr-10070 | 2.440942 | pr-        | pr-  |     233 |
| pr-10073 | 2.415922 | pr-        | pr-  |      53 |
| pr-10101 | 2.023465 | pr-        | pr-  |      73 |
| pr-10103 | 2.199688 | pr-        | pr-  |      50 |
| pr-10127 | 1.985228 | pr-        | pr-  |      47 |
| pr-10130 | 1.885507 | pr-        | pr-  |     102 |

# Analysis

## Entropy distributions (Figure 2)

Distribution of entropy values in each of our subsets

``` r
library(paletteer)

## translation labels
trans <- entr_meta %>%
  select(type,meter_tidy) %>%
  unique() %>% 
  mutate(meter_en=c("Baseline prose", "Chunked prose", "Iamb", "Dactyl","Trochee","Free verse", "Accentual: 1-3", "Amphibrach", "Anapest", "Accentual: 1-2", "Accentual"))

# if needed
entr_meta <- readRDS("data/entr_fin.rds")

entr_meta %>%
#  mutate(meter_tidy=ifelse(is.na(meter_tidy), type,  meter_tidy)) %>%
  group_by(meter_tidy) %>% 
  mutate(e_median = median(entropy)) %>% left_join(trans %>% select(-type),by="meter_tidy") %>% 
  ggplot(aes(reorder(meter_en,e_median), entropy,fill=type)) + geom_boxplot(outlier.size = 0.5,alpha=0.8) + scale_fill_paletteer_d("basetheme::clean") + labs(x=NULL,y="Entropy") +
  theme(
    text=element_text(family="gotham"),
    axis.text.x = element_text(angle=40,hjust=1)
  ) + guides(fill="none")
```

![](rhythm_regularity_notebook_files/figure-gfm/entropy%20distribution-1.png)<!-- -->

``` r
ggsave("plots/Fig2_regularity.png",width = 8,height = 5)
```

Corresponding data:

``` r
tb1 <- entr_meta %>%
#  mutate(meter_tidy=ifelse(is.na(meter_tidy), type,  meter_tidy)) %>%
  group_by(meter_tidy) %>% 
  summarize(lower_CI=quantile(entropy,0.025),
            upper_CI=quantile(entropy,0.975),
            entropy_median = median(entropy)) %>% 
  arrange(-entropy_median) %>% 
  select(meter_tidy,entropy_median, lower_CI,upper_CI)

knitr::kable(tb1,digits = 3)
```

| meter_tidy | entropy_median | lower_CI | upper_CI |
|:-----------|---------------:|---------:|---------:|
| pr-        |          2.223 |    1.571 |    2.584 |
| quasip-    |          2.067 |    1.315 |    2.397 |
| Ак         |          2.041 |    1.478 |    2.380 |
| Вл         |          1.998 |    1.454 |    2.391 |
| Тк         |          1.823 |    1.196 |    2.282 |
| Х          |          1.739 |    1.371 |    2.139 |
| Дк         |          1.621 |    0.889 |    2.189 |
| Аф         |          1.426 |    0.924 |    1.927 |
| Я          |          1.372 |    0.886 |    1.824 |
| Д          |          1.202 |    0.443 |    1.881 |
| Ан         |          1.053 |    0.337 |    1.708 |

## Hill numbers

``` r
## intervals with meta
i_met <- i %>%
  left_join(entr_meta, by="doc_id")

## labels
tib <- i_met %>% ungroup() %>% select(meter_tidy,type) %>% unique()
## doc ids
s <- i_met %>% ungroup() %>% select(doc_id, meter_tidy) %>% unique()

n_samples <- 1000
## to save
hills_list <- vector(mode="list",length=n_samples)

q=seq(0,4,by=0.25)
## replace 1 to almost 1
q[5] <- 0.99999

set.seed(13112)

## take sample of poems and calculate hills for each sample
for(n in 1:n_samples) {

## sample docs per group 
current_s <- s %>% group_by(meter_tidy) %>% sample_n(100,replace=F)

## filter 
si <- i_met %>% ungroup() %>%  filter(doc_id %in% current_s$doc_id)


## interval probabilities in a sample
type_probs <- rhythm_inequality(si,
                                drop_rare = F,
                                raw_values = T,
                                document = "meter_tidy")
#abund_df <- int_count %>% 
 # left_join(entr_meta ,by="doc_id") %>% 

## hills for a sample
intervals_hills <- type_probs %>% 
  group_by(meter_tidy) %>% 
  summarize(hills=hill(n,q=q),
         order=list(q)) %>% 
  select(hills,order,meter_tidy) %>% 
  unnest(cols=c(hills,order)) %>% 
  mutate(s=n)

## save to list
hills_list[[n]] <- intervals_hills

}

## combine, calculate CIs
hills_sum <- hills_list %>%
  bind_rows() %>%
  group_by(meter_tidy,order) %>% 
  summarise(lower=quantile(hills,0.025),upper=quantile(hills,0.975),hills=mean(hills))

saveRDS(hills_sum,"data/hills_sum.rds")
```

``` r
## if needed
hills_sum <- readRDS("data/hills_sum.rds")

cp <- paletteer_d("basetheme::clean")[1:7]

hills_sum %>% 
  left_join(entr_meta,by="meter_tidy") %>% 
  ggplot(aes(order,hills,group=meter_tidy)) +
  geom_line(aes(color=type)) +
  geom_ribbon(aes(ymin=lower,ymax=upper,fill=type),alpha=0.2) +
  theme_minimal() +
  scale_color_manual(labels=c("Accentual", "Accentual, 1-2", "Accentual, 1-3", "Classic meters", "Free verse", "Baseline prose", "Chunked prose"), values=cp) +
  scale_fill_paletteer_d("basetheme::clean") +
  labs(x="Diversity order", y="Hill number") + guides(fill="none") +
  theme(text=element_text(family="gotham"))
```

![](rhythm_regularity_notebook_files/figure-gfm/hill%20plot-1.png)<!-- -->

``` r
ggsave("plots/FigB_hill_numbers.png",width = 7,height = 3)
```

## Formal modeling

Let’s assume that our types are - or at least should be - ordered
“stages” of regularity (C). We don’t think difference , say, from
classical meters to accentual 1-2 is the same as the difference between
accentual and prose. We can model the “stage” of regularity as a
monotonous effect. We will also stratify entropy (R) by average line
length in syllables (S) - as it has an obvious effect on entropy
(smaller lines = smaller variation in intervals).

*R* ∼ *m**o*(*C*) \* *l**o**g*(*S*)

Flash-forward. Modeling categories as monotonic effect does not yield
better predictions, our final model was:

*R* ∼ *C* \* (*S*+*I*(*S*<sup>2</sup>))

### Preparations

``` r
library(brms)
library(tidybayes)
library(modelr)


reg_stage <- c("classic-meter", "Accentual, 1-2", "Accentual, 1-3", "Accentual", "Free verse", "pr-")

# no prose
np_reg_stage <- c("classic-meter", "Accentual, 1-2", "Accentual, 1-3", "Accentual", "Free verse", "quasip-")

d <- entr_meta %>% filter(type != "quasip-") %>% # filter quasi poems - we don't know where they stand on the regularity side
  mutate(RO=factor(type,levels=reg_stage,ordered = T),
         logS=log(mean_ll),
         R=entropy)

d_no_pr <- entr_meta %>% filter(type != "pr-") %>% # filter quasi poems - we don't know where they stand on the regularity side
  mutate(RO=factor(type,levels=np_reg_stage,ordered = T),
         logS=log(mean_ll),
         S=mean_ll,
         R=entropy)


# entropy ~ length check
ggplot(d, aes(log(mean_ll),entropy)) + geom_point(size=0.5,alpha=0.5) + facet_wrap(~type,scales="free_x") + geom_smooth(method="gam")
```

![](rhythm_regularity_notebook_files/figure-gfm/prepare%20data-1.png)<!-- -->

``` r
# meters ~ length relationship check
#d_no_pr %>% filter(type != "pr-") %>% ggplot(aes(log(mean_ll),entropy)) + geom_point(size=0.5,alpha=0.5) + facet_wrap(~meter_tidy,scales = "free_x") + geom_smooth(method="gam")
```

For classic meters that are governed by strong organizational
principles, length of line barely has an influence on entropy scores.
For free verse and quasi prose, entropy naturally increases with the
increase in line length.

### Models

``` r
## basic with monotonous
m1 <- brm(R ~ mo(RO),
          data=d,
          chains=4,
          iter = 2000,
          cores=4,
          file="models/m1_mo")

## monotonous interaction with size
m2i <- brm(R ~ mo(RO)*logS,
          data=d,
          chains=4,
          iter = 2000,
          cores=4,
          file="models/m2i")

## categorical
m0 <- brm(R ~ type,
          data=d,
          chains=4,
          iter = 2000,
          cores=4,
          file="models/m0")

## categorical with interaction 
m0i <- brm(R ~ type*logS,
          data=d,
          chains=4,
          iter = 2000,
          cores=4,
          file="models/m0i")

## fine-grained meters with interaction

m3i <- brm(R ~ meter_tidy*logS,
          data=d,
          chains=4,
          iter = 2000,
          cores=4,
          file="models/m3i")

## meters & quadratic term for size
m3iq <- brm(R ~ meter_tidy*(logS + I(logS^2)),
          data=d,
          chains=4,
          iter = 2000,
          cores=4,
          file="models/m3iq")

## NO PROSE MODELS (does not make sense since all prose = 1 paragraph = 1 line). We don't have any observation for poems that would have this unreasonably long lines. Also does not make sense to fit monotonic effects for C when using fine-grained meters, since we don't assume any "stages" of regularity between e.g. iamb or trochee

# quadratic
m3iqnp <- brm(R ~ meter_tidy*(logS + I(logS^2)),
          data=d_no_pr,
          chains=4,
          iter = 2000,
          cores=4,
          file="models/m3iqnp")

# not quadratic
m3inp <- brm(R ~ meter_tidy*logS,
          data=d_no_pr,
          chains=4,
          iter = 2000,
          cores=4,
          file="models/m3inp")



## by meter type, no monotonic effects,quadratic better loo
loo_res <- loo(m0,m1,m0i,m2i,m3i,m3iq)

## quadratic term better loo
loo_np <- loo(m3inp,m3iqnp)

loo_res$diffs
```

    ##      elpd_diff se_diff
    ## m3iq     0.0       0.0
    ## m3i    -24.6       8.9
    ## m0i  -1144.6      48.0
    ## m2i  -1146.2      48.3
    ## m0   -1274.8      48.6
    ## m1   -1278.2      48.6

``` r
loo_np$diffs
```

    ##        elpd_diff se_diff
    ## m3iqnp   0.0       0.0  
    ## m3inp  -39.8      13.1

We see that modeling it as a monotonic effect does not bring much
(model’s predictions are a bit worse), so we will stick with
categorical.

``` r
## mean length
ml <- d_no_pr$logS %>% mean()

set.seed(1989)

pred_mean=d_no_pr %>%
  data_grid(meter_tidy,logS=ml) %>%
  add_epred_draws(m3iqnp,ndraws = 1000) %>% 
  left_join(trans) %>% 
  group_by(meter_en) %>% 
  summarize(lower = quantile(.epred,0.025),
            upper = quantile(.epred,0.975),
            entropy = mean(.epred)) %>% 
  arrange(entropy)

post1 <- pred_mean %>% ggplot(aes(reorder(meter_en,entropy),entropy)) + 
  geom_jitter(data=d_no_pr %>% left_join(trans) %>%  mutate(meter_en = factor(meter_en,levels=pred_mean$meter_en)),aes(meter_en,entropy,color=type), alpha=0.1,shape=16,height = 0) +
  geom_point(size=1) +
  geom_errorbar(aes(ymin=lower,ymax=upper),width=0.4) + scale_color_paletteer_d("basetheme::clean") + 
  theme(text=element_text(family="gotham"),
        axis.text.x = element_text(angle=30,hjust = 1)) + labs(x=NULL,y="Entropy") + guides(color="none")
```

``` r
set.seed(1989)
pr_l=d_no_pr %>%
  data_grid(meter_tidy,logS=seq(1,3,by=0.05)) %>%
  add_epred_draws(m3iqnp,ndraws = 1000)

### manual order of subsets
vt <- sort(trans$meter_en)
vt
```

    ##  [1] "Accentual"      "Accentual: 1-2" "Accentual: 1-3" "Amphibrach"    
    ##  [5] "Anapest"        "Baseline prose" "Chunked prose"  "Dactyl"        
    ##  [9] "Free verse"     "Iamb"           "Trochee"

``` r
vtt <- vt[c(1:3,6,10,11,8,5,4,7,9)]

p_ll <- pr_l %>%
  left_join(trans) %>% 
  mutate(meter_en=factor(meter_en,levels=vtt)) %>% 
  group_by(meter_en,type,logS) %>%
  summarize(lower = quantile(.epred,0.025),
            upper = quantile(.epred,0.975),
            mean=mean(.epred)) %>% 
  ggplot(aes(exp(logS),mean,group=meter_en)) +
  geom_line(aes(color=type)) +
  geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.2) + 
  facet_wrap(~meter_en,ncol = 2) + 
  scale_color_paletteer_d("basetheme::clean") + labs(x="Average number of syllables in line",y="Estimated mean") + guides(color="none") +
  geom_vline(aes(xintercept=exp(ml)),color="lightgrey",linetype=2) + 
  theme(text=element_text(family="gotham"),
        axis.title = element_text(size=8),
        axis.text.y = element_text(size=5))
```

``` r
library(patchwork)

layout <- "
AAAABB
"
post1 + p_ll + plot_layout(widths = c(2, 1))
```

![](rhythm_regularity_notebook_files/figure-gfm/plot%20combination-1.png)<!-- -->

``` r
ggsave("plots/FigC_posterior_preds.png",width = 8,height = 5)
```

## UMAP projection

``` r
library(umap)

### document-feature matrix
i_wide <- int_count %>%
  left_join(entr_meta %>% select(doc_id,meter_tidy),by="doc_id") %>% 
  filter(meter_tidy %in% c("Х","Я","Д","Ан", "Аф","pr-","Дк")) %>% 
  select(-meter_tidy) %>% 
#  left_join(entr_meta %>% select(doc_id,type),by="doc_id") %>% 
#  left_join(tib,by="type") %>% 
  pivot_wider(names_from = no_stress,values_from = n,values_fill = 0)

## simple matrix
matrix = i_wide[,-1] %>%
  as.matrix() %>% 
  scale()
rownames(matrix) = i_wide$doc_id

set.seed(1810123)
intervals_u = umap(matrix,preserve.seed = F)

pr_df <- tibble(x = intervals_u$layout[,1], 
                y = intervals_u$layout[,2],
                doc_id=rownames(intervals_u$layout)) %>% 
  left_join(entr_meta %>%  ungroup(),by="doc_id") #%>%

saveRDS(pr_df,"data/projection_df.rds")
```

Plotting

``` r
## if needed
pr_df <- readRDS("data/projection_df.rds")

pr_df %>% 
  left_join(trans,by="meter_tidy") %>% 
#  filter(meter_tidy %in% c("Х","Я","Д","Ан", "Аф")) %>% 
  ggplot(aes(x,y)) + geom_point(aes(color=meter_en),size=0.5) + theme_void() + scale_color_paletteer_d("basetheme::clean") + theme(legend.title = element_blank()) + guides(colour = guide_legend(override.aes = list(size=2)))
```

![](rhythm_regularity_notebook_files/figure-gfm/umap%20plot-1.png)<!-- -->

``` r
ggsave("plots/Fig3_umap.png",width = 8,height = 5)

#entr_meta %>% filter(str_detect(doc_id,"rusc"))
```

### k-means

K-means clustering with the data for UMAP and for 5 basic meters
separately

``` r
k_mat <- matrix
d=tibble(doc_id=rownames(k_mat)) %>% left_join(entr_meta,by="doc_id")
k_mat <- scale(k_mat)
rownames(k_mat) <- d$meter_tidy

k_met <- k_mat[str_detect(rownames(k_mat),"^X|^Аф|^Ан|^Я|^Д$"),]
#set.seed(1989)
k_means_met <- kmeans(k_met, centers = 5, nstart = 50)

#print(k_means_met)

## ARI ~ 0.6 for 5 basic meters
mclust::adjustedRandIndex(k_means_met$cluster,rownames(k_met))
```

    ## [1] 0.5991209

``` r
#r <- paste0(1:2800,"1", rownames(k_met)) 
#k_met2 <- k_met
#rownames(k_met2) <- r
#factoextra::fviz_cluster(k_means_met,k_met2)
```
