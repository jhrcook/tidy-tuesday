\#TidyTuesdays
================
Joshua Cook
4/7/2020

# Tidy Tuesday

[`#TidyTuesday`](https://github.com/rfordatascience/tidytuesday) is a
tradition in R where every Tuesday, we practice our data analysis skills
on a new “toy” data set.

## Log

**April 21, 2020 - GDPR Violations**

[data](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-04-21/readme.md)
| [analysis](2020-04-21_gdpr-violations.md)

I used the ‘tidytext’ and ‘topicmodels’ packages to group the GDPR fines
based on summaries about the violations.

![](./2020-04-21_gdpr-violations_files/figure-gfm/unnamed-chunk-18-1.png)

**April 14, 2020 - Best Rap Artists**

[data](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-04-14/readme.md)
| [analysis](2020-04-14_best-rap-artists.md)

I built a graph of the songs, artists, and critics using Rap song
rankings.

![](./2020-04-14_best-rap-artists_files/figure-gfm/unnamed-chunk-15-1.png)

**April 7, 2020 - Tour de France**

[data](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-04-07/readme.md)
| [analysis](2020-04-07_tour-de-france.md)

There was quite a lot of data and it took me too long to sort through it
all. Next time, I will focus more on asking a single simple question
rather than trying to understand every aspect of the data.

![](./2020-04-07_tour-de-france_files/figure-gfm/unnamed-chunk-4-1.png)

**March 31, 2020 - Beer Production**

[data](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-03-31/readme.md)
| [analysis](2020-03-31_beer-production.md)

I analyzed the number of breweries at varies size categoires and found a
jump of very small microbreweries to higher capacity in 2018 and 2019.

![](./2020-03-31_beer-production_files/figure-gfm/unnamed-chunk-9-1.png)

**March 24, 2020 - Traumatic Brain Injury**

[data](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-03-24/readme.md)
| [analysis](2020-03-24_traumatic-brain-injury.md)

The data was a bit more limiting because we only had summary statistics
for categorical variables, but I was able to use PCA to identify some
interesting properties of the TBI sustained by the different age groups.

![](./2020-03-24_traumatic-brain-injury_files/figure-gfm/unnamed-chunk-10-1.png)

**March 10, 2020 - College Tuition, Diversity, and Pay**

[data](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-03-10/readme.md)
| [analysis](2020-03-10_college-tuition-diversity-and-pay.md)

I tried to do some classic linear modeling and mixed effects modeling,
but the dat didn’t really require it. Still, I got some practice with
this method and read plenty about it online during the process.

![](./2020-03-10_college-tuition-diversity-and-pay_files/figure-gfm/unnamed-chunk-8-1.png)

**March 3, 2020 - Hockey Goals**

[data](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-03-03/readme.md)
| [analysis](2020-03-03_hockey-goals.md)

I got some practice build regression models for count data by building
Poisson, Negative Binomial, and Zero-Inflated Poisson regression models
for estimating the effect of various game parameters on the goals scored
by Alex Ovechkin.

![](./2020-03-03_hockey-goals_files/figure-gfm/unnamed-chunk-12-1.png)

**January 21, 2020 - Spotify Songs**

[data](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-01-21/readme.md)
| [analysis](2020-01-21_spotify-songs.md)

I used a random forrest model to predict the genre of a playlist using
musical features of their songs. (I want try to further tune this model,
though.)

![](./2020-01-21_spotify-songs_files/figure-gfm/unnamed-chunk-13-1.png)
