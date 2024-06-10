# query-hoydedata-norgeibilder-api
Some R script to query Kartverket's Nordeibilder and Høydedata APIs for project metadata

The [Høydedata API](https://hoydedata.no/LaserInnsyn2/dok/webtjenester.pdf) and [Norgeibilder API](https://norgeibilder.no/dok/webtjenester.pdf) are REST APIs that you can use to get project-specific information about LiDAR and orthophoto missions. To get an overview of project coverage and overlap, I query the APIs to get all project metadata and write out to geosjon file for further analysis. Downstream analysis could be (for example) identifying areas with multiple time points of LiDAR missions for change analysis.
