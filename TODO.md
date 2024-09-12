- [X] End Year default should be 2100.
Tim said it should be 2024 in the PPT slides he sent.

- [X] When loading initial population, the R session gives this warning:
  Warning in html && nzchar(params[["text"]]) :
    'length(x) = 3 > 1' in coercion to 'logical(1)'

Fixed

- [X] Screen with the initial population: the table with broader age groups covers the title of the pyramid, see below. Could the table be moved a little lower?

This might be possible but at the moment the slider is too large with such a big range of dates and it looks very weird.

- [X] Also in that tab, the default year should be the first projected year instead of 1949.

Fixed

- [X] Could the year be also a part of the graph, say in the title? That's because when you download the picture as png, the year info is lost. Or/and the year could be a part of the file name, so that people can save multiple years after one another without overwriting the previous file. The same for the data download.

The filename contains now the country / year and other customizable properties in the file name.

- [X] Remove the "Editable" text from the pop up in the PopulationProjection.

- [X] Add footer of the UN in the bottom.

- [X] Rename columns of customize data.

- [X] Eliminate the negative value label in X label of pyramid of starting population.

- [X] When passing from population to TFR, put a pop up saying: If you'd like to provide TFR, make sure to provide it here, etc...

- [X] Label the confidence intervals (90% / 95%) on the right for all results.

95% label placed in all labels.

- [X] Add the uncertainty intervals as a toggle on / off for the "Deaths and Births". The idea is to have another legend for uncertainty interval and they can toggle on/off in the legend in plotly. This should happen in all plots that have uncertainty intervals.

All CIs are now toggle on / off

- [X] YADR / OADR plot is only plotting from the starting year (for example 2023) but we want to see everything before the starting year. Should have a plot from the first year and last year. Contact Hana for this.

- [X] Pop and Aging / Life Expectancy and CDR / TFR by CDR / plot should have dots instead of lines. Also, make sure you sort it well becuase years are not plotted on an axis. Hana needs to check that this calculation is right because there is a lot of wiggly patterns.

I just changed this to points instead of lines and looks much better. Share this with Patrick / Tim to check if they're alright with it.

- [X] Add name of the country + starting / ending year in the analysis tab. Put it above the menu.

- [X] Specify the units in the X / Y axis (years, thousands, millions, percentage, etc..)

- [X] Fix 2021 starting year problem.

- [X] Add check that end year should be higher than starting year

- [X] Add download button to customize tabs

- [X] On the customize tabs, highlight the data needs to be exactly like the example data. Need to highlight the metric of the value (thousands, millions, etc..)

- [X] Refactor the app

TODO:

- [X] Make sure the app renders well in different OS and browsers
- [X] For population customize, the title should only reflect the minimum year (base year).
- [X] For population customize, the CSV file doesn't have an extension.
- [X] For population customize, the filename should only reflect the base year instead of the range of years.
- [X] Patrick wants for the title of the customize tab to be refreshed when a new file is uploaded and for the name and base year to change. This is impossible because we don't know the year they could be uploaded. Instead, you should change the title to "Country in Base Year" and don't specify it.
- [X] The upload button doesn't complete the loading bar in customize population.
- [X] TFR customize filename, same thing. Ending extension not present.
- [X] Update TFR customize name to reflect the entire TFR series, not only between 2023 and 2100.
- [X] Add the country name to the plot title.
- [X] When exporting results, remove the row.names from all files.
- [X] Change title of tabs: "CDR and Life Expectancy" and "CBR and TFR".
- [X] Change "Calculate" button to "Run Population Projection"
- [X] Change TFR plot name to "Total Fertility Rate: Chile, 1950-2100" in the TFR page.
- [X] Change projected population pyramid plot to "Population by age and sex: Chile, 2024"
- [X] Change age group plot title to "Population by broad age groups: Chile, 1950-2100".
- [X] Change title of Pop Over Time to "Population Age 40-59: Chile, 1950-2100"
- [X] Change TFR title of projcted TFR to "Total Fertility Rate: Chile, 1950-2100". Ask Patrick / Tim whether they want TFR or Live Births Per Women.
- [X] Change title to "Population growth rates for broad age groups: Chile, 1950-2100".
- [X] Change title name to 'Total Fertility Rate: Chile, 1950-2100'.
- [X] Change title to 'Population growth rates for broad age groups: Chile, 1950-2100'.
- [X] Change title to 'Annual number of births: Chile, 1950-2100'.
- [X] Change title to 'Annual number of deaths: Chile, 1950-2100'.
- [X] Change y label to Number of births (thousands) and Number of deaths (thousands).
- [X] Change title to 'Crude Birth Rate: Chile, 1950-2100'.
- [X] Change title to 'Crude Death Rate: Chile, 1950-2100'.
- [X] Change y label to Births per 1,000 population and Deaths per 1,000 population
- [X] Change title to 'Young-age dependency ratio (Age <20 / Age 20-64): Chile, 1950-2100'.
- [X] Change y label to "Persons age <20 per 100 persons age 20-64” in YADR".
- [X] Change title to 'Old-age dependency ratio (Age 65+ / Age 20-64): Chile, 1950-2100'.
- [X] Change y label to "Persons age 65+ per 100 persons age 20-64".
- [X] Change title to "Population size and percent of population 65+: Chile, 1950-2100".
- [X] Change title to "Crude death rate and life expectancy at birth: Chile, 1950-2100"
- [X] Change title to "Crude birth rate and total fer:lity rate: Chile, 1950-2100"
- [X] For me is not clear the instruction for skipping or specifying the TFR page. The option should be something like: "Use median WPP22 TFR or define your own values".
- [X] Use Projection instead of Forecast on all plots.
- [X] Change CI to PI (Prediction Intervals)
- [X] Change upload statement in pop up window to "Upload: starting population by sex and single year of age with an open age interval of 100+"
- [X] We need a way to download all pyramid population data.  In the current version, we can only download data for 1 year at a time.
- [X] Reduce the rounding of decimals on the tool tips when showing rates, ratios, percentage, number of children to something like 2 decimals.
- [X] Can you change the line type between projection and UN estimate? For the color blind people.
- [X] Remind Tim that Patrick wanted the possibility to skip the TFR section.
- [X] Confirm lower / upper bound for age group tool tip corresponds with the data. See example of chile, 60+, 2023-2100
- [X] For the plots it would be nice to add systematically the units in the axis label: per 1000 persons, and for TFR (live births per woman). See for example https://population.un.org/wpp/Graphs/DemographicProfiles/Line/156. JC: This means you need to add "per 1000 persons" instead of the parenthesis and for TFR simply write "Live Births Per Woman".

- [ ] The term 'colour' seems to be appearing inside each of the ribbons of the PI tooltip, why?
- [ ] the customize window seems to not allow scrolling at first time, is this because of the table not being scrollable?
- [ ] Plt title cuts in different devices. Would be great to have some sort of automatic title wrapper for different scren

- [ ] Multiply YADR by 100 (hana will do it)
- [ ] Multiply OADR by 100 (hana will do it)
- [ ] Include methodological links for interested users.
- [ ] For the count data, and deal with the scaling issue between small and largest countries like China and India we use a few conditions in R to check what scaling factor to use and unit label (eg billion, million, thousand, etc) based on the max value. JC: I don't think this might be doable a this point because we're fixing the unit label to thousands for now.
- [ ] For the scatter plots it might be nice to have the option to switch on/off the labeling of some years like event 10 or 20 years depending the length of years plotted. JC: Do they mean to change the label of the X axis to have labels every 10 or 20 years instead of fixed?
- [ ] Apps time out very quickly.
- [ ] For the shiny plots it would be good to add some footnote to remind the user whether the figures refer to end of year or mid-year. Hana needs to tell me whether the results are for end of year or mid-year.
- [ ] Change "Data Portal" in the UN logo to "Online Population Projection". How to do that? No idea :D since this is in the HTML file. Perhaps when you read the HTML and place it in the app you could replace that.

Mobile changes:
- [ ] Footer section takes too much space vertically
- [ ] Summary stats panel for input pop has issues with display of 3 columns vertically - they get stacked into 1 column vertically
- [ ] One small enhancement for pop pyramids would be to add on the right y-axis the year of birth to track generations
- [ ] Another nice tweak would be to add an option for pop pyramids to have animation, ie  flipping thru years automatically from whatever start year.

Version 2.0:

- [ ] Change legend to be horizontal for better responsive ness in small screens
- [ ] Force the max/min-width of untheme to be 100% when in mobile phones
- [ ] For TFR customize data do you think you could add a third option for user to specify a target value for the end of their projection, and whether to use a linear or logistic decline function.  For the target value it could be either entered manually (simpler I presume) or defined using a slider (in this case you are able to better control the valid range from 0.5 to 10).

- [ ] What should the app do if the user changes the projected TFR and uploads it again? Does it change anything? What if the user adds more years than the downloaded TFR? The TFR customize should have some parsing rules and fail accordingly. See the files in https://mail.google.com/mail/u/0/#inbox/FMfcgzGwJSCRWXLhZQFxSZWcqBkTlWXt for some tests.

- [ ] Add option to download all years from a single button for plots with have widget to select a single year.
- [ ] In Pop Pyramid export plot, remove the negative values for males from the exported data.



- [ ] Need to fix the feedback that Patrick sent


- UI for life expectancy by sex (Hana to provide get_wpp_e0)
- You need to understand the output of get_wpp_e0 to set up the UI interface for updating e0
- run_forecast will have a slot for e0


- UI for migration (Hana to provide get_wpp_mig)
- You need to understand the output of get_wpp_mig to set up the UI interface for updating migration
- run_forecast will have a slot for mig


- e0_by_time data frame will now be in the output of run_forecast, need to create a plot to show it
- mig_by_time data frame will now be in the output of run_forecast, need to create a plot to show it


- Need to change the initial year of the forecast. Set it t 1970 for now to avoid weird errors (tested this with Hana)


- Need to add widget for changing between country and region aggregation. Hana to provide get_wpp_region to show it in the UI.


- Need to install the private version of wppp2024 package on the server so that it overrides the public one. Need to think how to automate this. Hana will give access to this repository so that I can install it on the server.
