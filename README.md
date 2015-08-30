## Overview

This is the code I used for an analysis of effects of severe weather events in the US
for the Coursera class "Reproducible Research" (part of the JHU Data Science Specialization).

The analysis is performed in the file project2.Rmd (here I have kept `eval=FALSE` since I cached
some time-consuming computations). Most of the work in the analysis went into cleaning the data to match
the standardized event types according to the [Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf).

A direction worth exploring in this data set is to see how the database evolved overtime, for example
it seems that the range of events reported at the beginning (around the 1950s) was more restrictive and this
might bias the analysis; also, in term of property damage, the analysis does not take into account inflation.