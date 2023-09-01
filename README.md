# California COVID Event Timeline

The purpose of this project is to create an interactive timeline of events and epidemiologic trends during California's COVID response. 

The timeline data is publicly available information from CDPH and various news sources. Attribution for specific news items is found in the timeline events data CSV file.

The live timeline is visible here: <https://brianerly.shinyapps.io/ca_covid_tl/>. It is optimized for customizeable export, and includes the ability to upload new data.

## Known Issues

- The "Notes" tab probably needs an edit or mild rewrite
- There are some typos and dispreferred language in the timeline itself
- It'd be interesting to have a whole ~Diamond Princess~ set of events, but that's not implemented yet.
- A few parts of the shiny app should really be functions but are repeated code
- The shiny app code could really use a few more comments!
- The data compiler outputs data into the main root folder, rather than the shiny app folder. 

## Attribution
Created by Brian Erly MD MPH
<bkerly.github.io>
