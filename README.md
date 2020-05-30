# tidy-exercises

This is a collection of [Tidyverse](https://www.tidyverse.org/) exercises I've
carried out. I started doing this to keep my data science skills sharp, and to
learn new stuff by studying data. I try to keep up with the
[TidyTuesday](https://github.com/rfordatascience/tidytuesday) exercises, though
occasionally I wrangle data from other sources.

# History

Here's a history of the exercises, along with a brief description of lessons learned.

- 2019-11-08: [US PhD's Awarded](https://github.com/zdelrosario/tidy-exercises/blob/master/2019/2019-02-19-usphds/eda.md)
  + Some disciplines have an *astounding* number of sub-disciplines, especially the Life Sciences.
  + As an engineer, I was very surprised to see that the vast majority of PhD's awarded per-year in the US go to Social and Life scientists, with Engineering, CS, and Math/Stats far fewer in number.
- 2019-11-24: [Dept. of Education Outcomes Data](https://github.com/zdelrosario/tidy-exercises/blob/master/2019/2019-11-22-ed-data/outcomes.md)
  + A large *count* of Title IV funding tends to go to Business Admin majors, but more *dollars* of Title IV funding go (unsurprisingly) to medical professionals.
  + Most engineering degrees seem to have similar earnings; the outliers are Naval/Marine architecture degree holders (who earn more), and Environment-related degree holders (who earn less).
  + Computer Technologies/Technician degrees seem to be a racket; these graduates end up with higher debt, but make a bit less than other degree holders.
- 2019-12-03: [Philadelphia Parking Violations](https://github.com/zdelrosario/tidy-exercises/blob/master/2019/2019-12-03-phily-tickets/proc.md)
  + In Philadelpha, incidents of parking violations follow a clear weekly trend, with the greatest violations on Thursdays, and far fewer on the weekends.
  + National holidays lead to far fewer violations as people stay home, but it seems that strong snowstorms have the same effect in this Northeastern city. Having grown up in PA I know what snowstorms are like, but I didn't realize they had a similar effect on traffic violations as national holidays.
- 2019-12-10: [UNDOC Homicide Data](https://github.com/zdelrosario/tidy-exercises/blob/master/2019/2019-12-10-news-plots/proc.md)
  + US general homicide rates are much higher than other developed nations; about 3x that of Canada
  + US gun homicide rates are similarly high; the US *firearm homicide* rate is higher than the *general homicide* rate of many nations of similar wealth
  + Firearm homicide data are fairly sparse
- 2019-12-25: [Christmas songs on the Top-100](https://github.com/zdelrosario/tidy-exercises/blob/master/2019/2019-12-24-hot100/proc.md)
  + Christmas songs on the Top-100 were common from around 1958 through the early 60's. They have become rarer since.
  + A few Christmas songs have enjoyed "revivals":
    - Some artists re-chart, such as Nat King Cole's "The Christmas Song" charted decades apart (first in 1960, recently in 2017)
	- Others were re-interpreted, such as "Please Come Home for Christmas" first by Charlie Brown (1961), then by The Eagles (1979)
  + Christmas songs tend to use words with positive sentiment, and tend to be lyrically simple: If a Christmas song has 100 words in its lyrics, it will tend to repeat the same 18 words
- 2019-12-31: [IMDB TV Shows](https://github.com/zdelrosario/tidy-exercises/blob/master/2019/2019-12-31-imdb/proc.md)
  + TV shows need time to "get good" as measured by their average rating score, which tends to peak around season 5.
  + TV shows also seem to be renewed at different rates across genres. Shows of the Action, Adventure, Comedy, Crime, Mystery, Romance, and Family genre seem to be more reliably renewed than others.
- 2020-01-11: [Australia Fires](https://github.com/zdelrosario/tidy-exercises/blob/master/2020/2020-01-10-australia-fires/proc.md)
  + The Australia fires are dangerously close to Canberra
  + The temperature in Canberra seemed to be a bit colder than other Australian cities leading up to the fires.
  + The rainfall in July was also very low, which probably contributed to blaze.
- 2020-01-19: [Popular passwords](https://github.com/zdelrosario/tidy-exercises/blob/master/2020/2020-01-14-passwords/proc.md)
  + Some funny popular passwords are `trustno1`, `rush2112`, and `passw0rd`
  + There is a 'spike' of length-six passwords with small offline time-to-crack. These are fairly diverse in category, though are mostly people's names, cool-macho words, or sport-related.
  + There is a 'band' of easily-cracked passwords that are simple numerical sequences. These tend to be exceptionally bad passwords, because they have a great deal of structure. Though [Tommy Tutone](https://www.youtube.com/watch?v=6WTdTwcmxyo) also shows up here!
- 2020-01-20: [School Shootings](https://github.com/zdelrosario/tidy-exercises/blob/master/2020/2020-01-14-shootings/proc.md)
  + There has been an *alarming* upward trend in school shootings, starting around 2016; since then there have been over 20 a year.
  + School shootings tend to occur less frequently on a Monday, and more frequently on Fridays.
- 2020-01-21: [Spotify Data](https://github.com/zdelrosario/tidy-exercises/blob/master/2020/2020-01-21-spotify/proc.md)
  + Top-popularity artists tend to produce a body of work that is rather homogeneous, as quantified by Spotify's song characteristics
  + Variability among top artists tends to be very little in terms of instrumentalness (there's a lot of singing at the top)
  + Variability among top artists tends to be wide in "valence" (positive sentiment) and the use of acoustic instruments
    - This variability is least among rappers, and greatest among rock artists
- 2020-01-28: [SF Trees](https://github.com/zdelrosario/tidy-exercises/blob/master/2020/2020-01-28-sf-trees/proc.md)
  + There's a long row of American Ash trees on Turk Blvd. near Arbol Lane. This is a strange place for American Ash, whose natural range is the [Eastern US](https://en.wikipedia.org/wiki/Fraxinus_americana).
- 2020-02-18: [CO2 Food Emissions](https://github.com/zdelrosario/tidy-exercises/blob/master/2020/2020-02-18-food-co2/proc.md)
  + Lamb, goat, and beef products produce the most CO2 emissions among food products
  + There's great variety in the rate of production of milk, wheat, and rice among countries of the world
  + Argentina, Australia, Albania, New Zealand, and Iceland are the top
    producers of CO2 emissions by food (the US is not far behind)
- 2020-02-25: [Vaccination Rates](https://github.com/zdelrosario/tidy-exercises/blob/master/2020/2020-02-25-vaccines/proc.md)
  + There is **huge** variation in vaccination rates across schools within the same state; schools in California range between near-zero and near 100% vaccination!
  + Median MMR and overall vaccination rates tend to be high, though Arkansas lags in MMR and Idaho lags in overall
- 2020-04-09: [ABET-Accredited Engineering Programs](https://github.com/zdelrosario/tidy-exercises/blob/master/2020/2020-04-09-us-engr/proc.md)
  + There are `370` US institutiosn with ABET-accredited engineering programs in the US, as of 2020-04-09
  + A large majority are in California, Texas, and New York
  + Boston, LA, NYC, Philadelphia, and the District of Columbia have the most, with 4 each
- 2020-05-30: [NYT COVID-19 Data](https://github.com/zdelrosario/tidy-exercises/blob/master/2020/2020-05-30-nyt-covid-counties/proc.md)
  + The NYT publishes county-level covid-19 data; this allows for a detailed look at the outbreak across the country
  + The bulk of counties started reporting cases around March 03
  + As of 2020-05-30, the following states case increase has slowed: AL, HI, KS, ME, MA, MI, MT, NV, NJ, NY, VT, WA. All other states seem to be increasing in case count
  + Some counties are extreme outliers in terms of large case counts per capita. Many of these are small (~10,000) to medium-counties (~50,000), often with outbreaks in prisons
