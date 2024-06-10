
#### Import libraries ------------------------------------------------------------------
library(httr2)
library(stringr)
library(tidyverse)
library(jsonlite)
library(sf)
library(leaflet)
library(geojsonsf)

#### Tetsting ----------------------------------------------------------------------------
# The LIDAR and Orthophot API's look identical except for the first part of the URL string
# So will test on the LiDAR API and then implement a global function which works for both

# https://hoydedata.no/LaserInnsyn2/dok/webtjenester.pdf
# https://norgeibilder.no/dok/webtjenester.pdf

# Get project names
req <- request("https://hoydedata.no/laserservices/rest/projectMetadata.ashx")
response <- req_perform(req)
print(response)
resp_header(response, "content-type")
projectsString <- resp_body_string(response)[1]
projects <- tibble(name = str_match_all(projectsString, "\"([^\"]+)\"")[[1]][,2]) %>%
  filter(!name %in% c('Success', 'ErrorMessage', 'ProjectList', 'ProjectMetadata')) %>%
  mutate(nameFormatted = str_replace_all(name, ' ', '%20'))

# Some example API calls - testing
#https://hoydedata.no/laserservices/rest/projectMetadata.ashx?request={Projects:%27Innlandet%20laser%202023%27,ReturnMetadata:true,ReturnGeometry:true}
#https://hoydedata.no/laserservices/rest/projectMetadata.ashx?request={Projects:%27Fauske%2010pkt%202023%27,ReturnMetadata:true,ReturnGeometry:true}

url <- 'https://hoydedata.no/laserservices/rest/projectMetadata.ashx?request={Projects:%27NDH%20Gjemnes-Molde-Rauma%20Nord%202pkt%202016%27,ReturnMetadata:true,ReturnGeometry:true}'
req <- request(url)
response <- req_perform(req)
responseParsed <- fromJSON(resp_body_string(response)[1])$ProjectMetadata

prjCrs <- as.numeric(responseParsed$properties$KOORDINATSYSTEM)

toJSON(responseParsed, auto_unbox = TRUE, digits = NA)
test <- geojson_sf(toJSON(responseParsed), input=25833, wkt=st_crs(25833)$wkt) 

test%>%
  ggplot() +
  geom_sf()
leaflet() %>%
  addTiles() %>%
  addPolygons(data = test %>% st_transform(st_crs(4326)), 
              color = "blue", 
              weight = 1, 
              opacity = 1.0,
              popup = ~paste("project:", LAS_PROJECT_NAME, "<br>"), 
              fillOpacity = 0.5)

#### Create API call runner function --------------------------------------------------

getProjectMetadata <- function(apiDomain, testNumber){
  
  # Get project names
  req <- request(paste0(apiDomain, "/rest/projectMetadata.ashx"))
  response <- req_perform(req)
  #print(response)
  #resp_header(response, "content-type")
  projectsString <- resp_body_string(response)[1]
  projects <- tibble(name = str_match_all(projectsString, "\"([^\"]+)\"")[[1]][,2]) %>%
    filter(!name %in% c('Success', 'ErrorMessage', 'ProjectList', 'ProjectMetadata'))
  
  # Loop over all projects and get metadata
  master <- st_sf(geometry = st_sfc(crs=st_crs(25833))) # empty SF object to concatenate onto
  i <- 8 # testing
  nrow(projects)
  
  # For loop
  if (!is.null(testNumber)){
    sequence <- seq(1,testNumber)
  } else {
    sequence <- seq(1,nrow(projects))
  }
  
  errors <- tibble(index = integer(), apiCall = character(), errorMessage = character())
  for (i in sequence){
    print(paste0('Fetching metadata for index: ', i, '-----------------------------------'))
    
    prjName <- projects$name[i]
    url <- paste0(apiDomain, "/rest/projectMetadata.ashx?request={Projects:%27", URLencode(prjName),"%27,ReturnMetadata:true,ReturnGeometry:true}")
    req <- request(url)
    tryCatch({
      response <- req_perform(req)
      responseParsed <- fromJSON(resp_body_string(response)[1])$ProjectMetadata
      
      # Convert to sf object
      prjShape <- geojson_sf(toJSON(responseParsed), input=25833, wkt=st_crs(25833)$wkt)
      
      #prjShape %>% ggplot() + geom_sf()
      master <- master %>% 
        bind_rows(prjShape)
      
    }, error = function(e) {
      print(e$message)
      errors <<- errors %>% bind_rows(tibble(index = i, apiCall=url, errorMessage = e$message))
      return (NULL)
    })
    
  }
  
  results_dict <- list()
  results_dict$data <- master
  results_dict$errors <- errors
  
  return (results_dict)
}


#### Lidar data API requests ---------------------------------------------------------------------

lidarMetadata <- getProjectMetadata('https://hoydedata.no/laserservices', NULL)

# Check errors
lidarMetadata$errors

# Check all attributes of data
str(lidarMetadata$data)

# Convert some attributes to numeric
lidarMetadataOut <- lidarMetadata$data %>%
  mutate(AARSTALL = as.numeric(AARSTALL),
         PUNKTTETTHET = as.numeric(PUNKTTETTHET),
         OPPLOSNING = as.numeric(OPPLOSNING)) 
hist(lidarMetadataOut$AARSTALL)
hist(lidarMetadataOut$PUNKTTETTHET)

# Write out to file
# Already pre-exported as of 10.06.2024
lidarMetadataOut %>%
  st_write('./lidar_projects_metadata.geojson')


#### Orthophoto data API requests ---------------------------------------------------------------------

orthoMetadata <- getProjectMetadata('https://tjenester.norgeibilder.no', NULL)

# Check errors
orthoMetadata$errors

# Check all attributes of data
str(orthoMetadata$data)

# Convert some attributes to numeric
orthoMetadataOut <- orthoMetadata$data %>%
  mutate(aar = as.numeric(aar),
         pixelstorrelse = as.numeric(pixelstorrelse)) 
hist(orthoMetadataOut$aar)
hist(orthoMetadataOut$pixelstorrelse)

# Filter out satellite products (type == 6)
orthoMetadataOut <- orthoMetadataOut %>%
  filter(ortofototype != "6")

# Write out to file
# Already pre-exported as of 10.06.2024
orthoMetadataOut %>%
  st_write('./orthophoto_projects_metadata.geojson')


