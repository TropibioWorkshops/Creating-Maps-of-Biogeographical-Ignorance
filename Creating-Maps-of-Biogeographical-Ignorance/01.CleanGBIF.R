# Information -------------------------------------------------------------

# Terrestrial biodiversity reported in GBIF
# Script developed by Fernanda Alves-Martins
# Last update August, 20th 2021

# Libraries -------------------------------------------------------------
libs = c("rgbif","readr","data.table","rworldmap", "ggplot2", "countrycode",
         "CoordinateCleaner", "stringr", "taxize", "XML", "foreach", 
         "doParallel", "writexl", "readxl", "beepr", "dplyr")
lapply(libs, require, character.only = TRUE)

# GBIF Data -------------------------------------------------------------

country_name = "Namibia"
country_code = "NAM"

## Working directories
input.dir = "C:/Users/ferfe/Documents/Projects/TROPIBIO/Namibia biodiversity biases/Original data/" #diretório com a pasta zip do GBIF
output.dir = "C:/Users/ferfe/Documents/Projects/TROPIBIO/Namibia biodiversity biases/Data/"
setwd(input.dir)

# GBIF Login -------------------------------------------------------------
options(gbif_user="seu_user_name")
options(gbif_pwd="sua_senha_gbif")
options(gbif_email="seu_email_vinculado_ao_gbif")

## Managing GBIF downloaded file
gbif_code = list.files(input.dir)
unzip(paste(input.dir,gbif_code,sep=""))#extrai a pasta do zip

## Check number of rows and type of variables##
n.lines = count.fields("occurrence.txt", sep = "/t") # give number of rows
length(n.lines)
guess_encoding("occurrence.txt")
df=data.table::fread("occurrence.txt",
          sep = "auto" ,
          stringsAsFactors=FALSE,
          dec = ".",
          colClasses = NA_character_,
          header="auto", strip.white = TRUE, data.table = FALSE,
          encoding = 'UTF-8',
          integer64=getOption("datatable.integer64", "integer64"),
          na.strings=c("NA","null","", " "),
          verbose = TRUE)

head(df)

## Save the original database
save(df, file= paste(country_name,"_gbif.rda",sep = ""))
rm(gbif_code,libs,n.lines)

# Coordinate cleaning -------------------------------------------------------------
  
# Load the original database
file = list.files(pattern = "rda")
load(file = file)
names(df)

# Country map
sPDF = getMap()  
country_code2 = subset(sPDF, ADM0_A3 == country_code)
rm(sPDF)


# Plot data to get an overview
X11()
ggplot()+
  coord_fixed(xlim = c(-30, 60), ylim = c(-40, 45), ratio = 1.3)+
  borders("world", colour="gray50", fill="gray50")+
  geom_point(data = df, aes(x = decimalLongitude, y = decimalLatitude), colour = "darkred", size = 0.5)+
  theme_bw()

# Exclude problematic records
## Remove records without coordinates
df1 = df%>%
  filter(!is.na(decimalLongitude))%>%
  filter(!is.na(decimalLatitude))

## Records outside country's border
### Convert occurrences to SpatialPointDataframe
# spdf = df1
# coordinates(spdf) = ~ decimalLongitude + decimalLatitude
# proj4string(spdf) = CRS(proj4string(country_code2))
# 
# ### Keep only those points that are inside the bounds of the study area
# inside_country = !is.na(over(spdf, as(country_code2, "SpatialPolygons"))) 
# inside_country = spdf[which(inside_country),]
# 
# X11()
# plot(inside_country)
# df = as.data.frame(inside_country)

## Convert country code from ISO2c to ISO3c
# df$countryCode = country_code
df1$countryCode =  countrycode(df1$countryCode, origin =  'iso2c', destination = 'iso3c')

## Flag problems
flags = clean_coordinates(x = df1, 
                          lon = "decimalLongitude", 
                          lat = "decimalLatitude",
                          countries = "countryCode",
                          species = "scientificName",
                          tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                    "zeros", "countries", "seas")) # most test are on by default

summary(flags)
X11()
plot(flags, lon = "decimalLongitude", lat = "decimalLatitude")

## Exclude problematic records
df1 = df1[flags$.summary,]

## Flagged records for checking
df_fl = df1[!flags$.summary,]

rm(country_code2,inside_country,spdf,flags, df_fl)


# Uncertain/invalid date -------------------------------------------------------------
sum(is.na(df$year))
df = df[!is.na(df$year),]


# Species names cleaning -------------------------------------------------------------

## Keep only records identified until species level
df = df[df$scientificName != "incertae sedis",]
taxon_list = as.data.frame(str_split_fixed(df$scientificName, " ", 4))
taxon_list$speciesLevel = ifelse(substr(taxon_list$V2,1,1) == toupper(substr(taxon_list$V2,1,1)),1,0)
taxon_list$gbifID = df$gbifID

df = cbind(df, taxon_list$speciesLevel)
colnames (df) [ncol(df)] = "speciesLevel"
df = df[df$speciesLevel == 0, ]

## Check for inconsistencies on Scientific Names 
taxon_list = taxon_list[taxon_list$speciesLevel == 0, ]
species = as.character(paste(taxon_list$V1,taxon_list$V2, sep = " "))
df$scientificName2 = species
species = as.data.frame(unique(species))

## Parallelization
UseCores = detectCores() -2 # Define how many cores you want to use
cl = makeCluster(UseCores) # Register CoreCluster
registerDoParallel(cl)

myfunction = function(mydata){
  output = foreach(x = iter(mydata, by = "row"), .combine = rbind,
                   .packages = c("dplyr","taxize")) %dopar% {
                     x = x %>%
                        taxize::gnr_resolve(data_source_ids = c(1,3,11,105,167,173,174,175,179),with_canonical_ranks=T)
                      return(x)
                     }
  output
}

sci_name_check = myfunction(species)
stopCluster(cl) #Stop cluster
beep(sound = 2)

sci_name_check$implement = ifelse(sci_name_check$score >= 0.98,FALSE,NA) # Inform which species name (column) is the valid one
write_xlsx(sci_name_check,paste(input.dir,"sci_name_check.xlsx",sep="")) # Save the list on a file and check species names
rm(UseCores,cl,myfunction)

## Import checked names list
sci_name_check = read_excel(paste(input.dir,"sci_name_check.xlsx",sep=""))
sci_name_check$implement = as.logical(sci_name_check$implement)
sci_name_check = sci_name_check %>% distinct(submitted_name, .keep_all= TRUE)

# spKeep = sci_name_check[sci_name_check$score>=0.95,]
# spKeep = as.character(unique(spKeep$submitted_name))
# df = filter(df, df$scientificName2 %in% spKeep)
# df$valid_name = df$scientificName2
  
df = df %>%
  left_join(sci_name_check, by=c("scientificName2" = "submitted_name"))%>%
  mutate(valid_name = ifelse(implement == TRUE, matched_name2, scientificName2))

### Check for duplicated records
table(duplicated(df))
df = distinct(df)

## Save the filtered database
save(df, file = paste(output.dir, country_name, "_gbif_filtered.rda", sep = ""))
rm(sci_name_check,species,taxon_list)
