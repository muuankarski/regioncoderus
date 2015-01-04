library(devtools)
document()

regioncoderus_data = read.csv('data/regioncoderus_data.csv', stringsAsFactors=FALSE, na.strings='')
#names(regioncoderus_data)[names(regioncoderus_data)=="region.name.english"] <- "region.name"
#countrycode_data$regex = iconv(countrycode_data$regex, to='UTF-8')

# Custom region names such as rosstat, rlms, obdx
#custom_region_schemes = read.csv('data/custom_region_schemes.csv', stringsAsFactors=FALSE, na.strings='')
#regioncoderus_data <- merge(regioncoderus_data,custom_region_schemes,by="region.name.russian")

save(regioncoderus_data, file='data/regioncoderus_data.rda')