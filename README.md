# eventsVis
A tool for analyzing and visualizing time series descrete events.

This tool uses the [shiny framework](https://shiny.rstudio.com/) for visualizing events.
In order to run it, you need to have [R](https://mran.microsoft.com/download) and preferably [Rstudio](https://www.rstudio.com/products/rstudio/download/).

This tool allows you to inspect events on a timeline, or to inspect the distribution of events across sessions.
It also allows you to query your dataset easily using SQL.

You can see a live example of the tool on ShinyApps:
[https://omrimendels.shinyapps.io/eventsvis/]




#### Timeline (based on Google Timeline chart and R's googleVis package):
![Timeline](https://github.com/omri374/eventsVis/raw/master/img/timeline.png)

In addition, it allows you to group adjacent similar events together (to ignore the effect of multiple events calls that are actually one event in reality).


#### Events distributions accross sessions:
![Distributions](https://github.com/omri374/eventsVis/raw/master/img/distributions.png)

#### Consecutive events analysis
Which events occur after others?

![Consecutive](https://github.com/omri374/eventsVis/raw/master/img/consecutive.png)


#### SQL querying: 
![SQL](https://github.com/omri374/eventsVis/raw/master/img/sql.png)



### Run the app locally:
Option 1: Open the project in R studio, open the server.R file and click on the play button. 

Option 2: Install the 'shiny' package and call


    R -e "shiny::runApp('~/eventsVis')"

or change '~/eventsVis' to the app path.


### Additional visualizations and interaction ideas are mostly welcome.
