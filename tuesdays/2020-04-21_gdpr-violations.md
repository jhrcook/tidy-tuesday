GDPR Violations
================
Joshua Cook
4/21/2020

## Data

``` r
gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')
```

    #> Parsed with column specification:
    #> cols(
    #>   id = col_double(),
    #>   picture = col_character(),
    #>   name = col_character(),
    #>   price = col_double(),
    #>   authority = col_character(),
    #>   date = col_character(),
    #>   controller = col_character(),
    #>   article_violated = col_character(),
    #>   type = col_character(),
    #>   source = col_character(),
    #>   summary = col_character()
    #> )

``` r
gdpr_violations %<>%
    janitor::clean_names() %>% 
    select(-picture, -source) %>%
    mutate(date = ifelse(date == "01/01/1970", NA_character_, date),
           date = lubridate::mdy(date)) %>%
    rename(country = name)
```

``` r
gdpr_violations
```

    #> # A tibble: 250 x 9
    #>       id country  price authority date       controller article_violated type 
    #>    <dbl> <chr>    <dbl> <chr>     <date>     <chr>      <chr>            <chr>
    #>  1     1 Poland    9380 Polish N… 2019-10-18 Polish Ma… Art. 28 GDPR     Non-…
    #>  2     2 Romania   2500 Romanian… 2019-10-17 UTTIS IND… Art. 12 GDPR|Ar… Info…
    #>  3     3 Spain    60000 Spanish … 2019-10-16 Xfera Mov… Art. 5 GDPR|Art… Non-…
    #>  4     4 Spain     8000 Spanish … 2019-10-16 Iberdrola… Art. 31 GDPR     Fail…
    #>  5     5 Romania 150000 Romanian… 2019-10-09 Raiffeise… Art. 32 GDPR     Fail…
    #>  6     6 Romania  20000 Romanian… 2019-10-09 Vreau Cre… Art. 32 GDPR|Ar… Fail…
    #>  7     7 Greece  200000 Hellenic… 2019-10-07 Telecommu… Art. 5 (1) c) G… Fail…
    #>  8     8 Greece  200000 Hellenic… 2019-10-07 Telecommu… Art. 21 (3) GDP… Fail…
    #>  9     9 Spain    30000 Spanish … 2019-10-01 Vueling A… Art. 5 GDPR|Art… Non-…
    #> 10    10 Romania   9000 Romanian… 2019-09-26 Inteligo … Art. 5 (1) a) G… Non-…
    #> # … with 240 more rows, and 1 more variable: summary <chr>

## EDA

``` r
gdpr_violations %>%
    mutate(country = fct_reorder(country, -price, sum)) %>%
    ggplot(aes(x = price, y = country)) +
    geom_col(aes(fill = price), position = "stack") +
    scale_fill_distiller(type = "seq", palette = "RdPu", guide = NULL) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(x = "GDPR fine",
         y = "Country",
         title = "Total GDPR fines accrued per country")
```

![](2020-01-21_gdpr-violations_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
gdpr_violations %>%
    mutate(lbl = paste0(country, "___", id),
           lbl = fct_reorder(lbl, -price)) %>%
    top_n(n = 20, wt = price) %>%
    ggplot(aes(x = price, y = lbl)) +
    geom_col(aes(fill = price), position = "stack") +
    scale_fill_distiller(type = "seq", palette = "RdPu", guide = NULL) +
    scale_y_discrete(labels = function(x) { str_remove(x, "___.*$") }) +
    scale_x_continuous(expand = c(0, 700000)) +
    labs(x = "GDPR fine",
         y = "Individual fines labeled by country",
         title = "Top 20 fines from GDPR violations")
```

![](2020-01-21_gdpr-violations_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
gdpr_violations %>%
    filter(!is.na(date)) %>%
    mutate(price_log = log10(price)) %>%
    ggplot(aes(x = date, y = price_log)) +
    geom_point(aes(color = country)) +
    scale_x_date() +
    scale_y_continuous(labels = function(x) { 10^x }) +
    labs(x = "date of fine",
         y = "price of fine (log-scale)",
         title = "GDPR fines over time")
```

![](2020-01-21_gdpr-violations_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Text Analysis

[Introduction to
tidytext](https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html)

``` r
library(tidytext)
```

``` r
gdpr_words <- gdpr_violations %>%
    mutate(summary = str_replace(summary, "\\&\\#8217\\;", "'")) %>%
    select(id, summary) %>%
    unnest_tokens(output = "word", input = summary)
```

``` r
gdpr_words %>% 
    anti_join(stop_words, by = "word") %>%
    count(word, sort = TRUE) %>%
    top_n(20, wt = n) %>%
    mutate(word = fct_inorder(word)) %>%
    ggplot(aes(x = n, y = word)) +
    geom_col(aes(fill = n)) +
    scale_fill_distiller(type = "seq", palette = "GnBu", guide = NULL, direction = 1) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(x = "word count",
         y = NULL,
         title = "The most frequent words used in GDPR violoation summaries")
```

![](2020-01-21_gdpr-violations_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

### Topic Modeling

``` r
library(topicmodels)
library(ldatuning)
```

``` r
gdpr_word_counts <- gdpr_words %>%
    count(id, word) %>%
    anti_join(stop_words, by = "word")
```

``` r
gdpr_dtm <- cast_dtm(gdpr_word_counts, document = id, term = word, value = n)
gdpr_dtm
```

    #> <<DocumentTermMatrix (documents: 243, terms: 1575)>>
    #> Non-/sparse entries: 4564/378161
    #> Sparsity           : 99%
    #> Maximal term length: 23
    #> Weighting          : term frequency (tf)

``` r
gdpr_lda_ksearch <- FindTopicsNumber(
    gdpr_dtm,
    topics = seq(from = 2, to = 100, by = 3),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 0),
    mc.cores = 4L,
    verbose = TRUE
)
```

    #> fit models... done.
    #> calculate metrics:
    #>   Griffiths2004... done.
    #>   CaoJuan2009... done.
    #>   Arun2010... done.
    #>   Deveaud2014... done.

``` r
as_tibble(gdpr_lda_ksearch)
```

    #> # A tibble: 33 x 5
    #>    topics Griffiths2004 CaoJuan2009 Arun2010 Deveaud2014
    #>     <dbl>         <dbl>       <dbl>    <dbl>       <dbl>
    #>  1     98       -34530.       0.242     83.0       0.353
    #>  2     95       -34475.       0.226     81.5       0.368
    #>  3     92       -34408.       0.225     82.5       0.378
    #>  4     89       -34386.       0.224     83.8       0.389
    #>  5     86       -34235.       0.208     82.6       0.409
    #>  6     83       -34237.       0.222     83.7       0.414
    #>  7     80       -34215.       0.204     84.1       0.428
    #>  8     77       -34313.       0.216     86.2       0.433
    #>  9     74       -34190.       0.197     85.8       0.453
    #> 10     71       -34131.       0.206     86.1       0.462
    #> # … with 23 more rows

``` r
maximize_metric <- c("Deveaud2014", "Griffiths2004")

gdpr_lda_ksearch %>%
    pivot_longer(-topics, names_to = "metric", values_to = "value") %>%
    group_by(metric) %>%
    mutate(value = scales::rescale(value, to = c(0, 1))) %>%
    ungroup() %>%
    mutate(max_or_min = ifelse(metric %in% maximize_metric, 
                               "Maximize", "Minimize")) %>%
    ggplot(aes(x = topics, y = value, color = metric, group = metric)) +
    facet_wrap(max_or_min ~ ., ncol = 1) +
    geom_line(size = 1, alpha = 0.5) +
    geom_point(size = 3) +
    scale_color_brewer(type = "qual", palette = "Set2") +
    theme(strip.text = element_text(face = "bold")) +
    labs(x = "number of topics (k)",
         y = NULL,
         title = "LDA Topic Model fit metrics over different number of topics")
```

![](2020-01-21_gdpr-violations_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
gdpr_lda_ksearch2 <- FindTopicsNumber(
    gdpr_dtm,
    topics = seq(from = 2, to = 25, by = 1),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 0),
    mc.cores = 4L,
    verbose = TRUE
)
```

    #> fit models... done.
    #> calculate metrics:
    #>   Griffiths2004... done.
    #>   CaoJuan2009... done.
    #>   Arun2010... done.
    #>   Deveaud2014... done.

``` r
gdpr_lda_ksearch2 %>%
    pivot_longer(-topics, names_to = "metric", values_to = "value") %>%
    group_by(metric) %>%
    mutate(value = scales::rescale(value, to = c(0, 1))) %>%
    ungroup() %>%
    mutate(max_or_min = ifelse(metric %in% maximize_metric, 
                               "Maximize", "Minimize")) %>%
    ggplot(aes(x = topics, y = value, color = metric, group = metric)) +
    facet_wrap(max_or_min ~ ., ncol = 1) +
    geom_line(size = 1, alpha = 0.5) +
    geom_point(size = 3) +
    geom_text(aes(label = topics), color = "grey25") +
    scale_color_brewer(type = "qual", palette = "Set2") +
    theme(strip.text = element_text(face = "bold")) +
    labs(x = "number of topics (k)",
         y = NULL,
         title = "LDA Topic Model fit metrics over different number of topics")
```

![](2020-01-21_gdpr-violations_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
gdpr_lda_k <- 13
gdpr_lda <- topicmodels::LDA(gdpr_dtm, k = gdpr_lda_k, control = list(seed = 0))
gdpr_lda
```

    #> A LDA_VEM topic model with 13 topics.

``` r
glance(gdpr_lda)
```

    #> # A tibble: 1 x 3
    #>    iter terms  alpha
    #>   <int> <int>  <dbl>
    #> 1    50  5443 0.0217

``` r
perplexity(gdpr_lda)
```

    #> [1] 191.9304

``` r
gdpr_lda_tidy <- tidy(gdpr_lda)
gdpr_lda_tidy
```

    #> # A tibble: 20,475 x 3
    #>    topic term       beta
    #>    <int> <chr>     <dbl>
    #>  1     1 40.000 2.16e- 3
    #>  2     2 40.000 3.64e-65
    #>  3     3 40.000 3.03e-65
    #>  4     4 40.000 3.41e-65
    #>  5     5 40.000 3.76e-65
    #>  6     6 40.000 3.12e-65
    #>  7     7 40.000 4.18e-65
    #>  8     8 40.000 3.43e-65
    #>  9     9 40.000 2.59e-65
    #> 10    10 40.000 4.15e-65
    #> # … with 20,465 more rows

``` r
gdpr_lda_tidy %>%
    group_by(topic) %>%
    top_n(5, wt = beta) %>%
    ungroup() %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(x = beta, y = term)) +
    facet_wrap(~ topic, scales = "free_y") +
    geom_col() +
    scale_y_reordered()
```

![](2020-01-21_gdpr-violations_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
set.seed(0)
topic_pal <- randomcoloR::distinctColorPalette(gdpr_lda_k)
names(topic_pal) <- seq(1, gdpr_lda_k)

gdpr_lda_tidy %>%
    group_by(topic) %>%
    top_n(5, wt = beta) %>%
    ungroup() %>%
    mutate(term = fct_reorder(term, beta, sum),
           topic = factor(topic)) %>%
    ggplot(aes(x = beta, y = term)) +
    geom_col(aes(fill = topic)) +
    scale_fill_manual(values = topic_pal) +
    theme(panel.grid.major.y = element_blank()) +
    labs(x = "LDA topic model beta",
         y = "GDPR terms",
         title = "Latent Dirichlet Allocation topic model for GDPR summaries")
```

![](2020-01-21_gdpr-violations_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
gdpr_lda_gamma <- tidy(gdpr_lda, matrix = "gamma")
gdpr_lda_gamma
```

    #> # A tibble: 3,159 x 3
    #>    document topic    gamma
    #>    <chr>    <int>    <dbl>
    #>  1 1            1 0.990   
    #>  2 2            1 0.000931
    #>  3 3            1 0.00262 
    #>  4 4            1 0.00112 
    #>  5 5            1 0.00125 
    #>  6 6            1 0.00152 
    #>  7 7            1 0.00234 
    #>  8 8            1 0.00234 
    #>  9 9            1 0.00119 
    #> 10 10           1 0.00152 
    #> # … with 3,149 more rows

``` r
gdpr_lda_gamma_df <- gdpr_lda_gamma %>%
    pivot_wider(document, names_from = topic, values_from = gamma) %>%
    as.data.frame() %>%
    column_to_rownames("document")

pheatmap::pheatmap(gdpr_lda_gamma_df, 
                   border_color = NA,
                   show_rownames = FALSE,
                   angle_col = 0,
                   main = "Clustering documents by topic")
```

![](2020-01-21_gdpr-violations_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
gdpr_fines_topics <- gdpr_lda_gamma %>%
    group_by(document) %>%
    top_n(1, wt = gamma) %>%
    ungroup()

gdpr_fines_topics %>%
    count(topic)
```

    #> # A tibble: 13 x 2
    #>    topic     n
    #>    <int> <int>
    #>  1     1    26
    #>  2     2    17
    #>  3     3    17
    #>  4     4     9
    #>  5     5    22
    #>  6     6    20
    #>  7     7    15
    #>  8     8    17
    #>  9     9    29
    #> 10    10     9
    #> 11    11    19
    #> 12    12    24
    #> 13    13    19
