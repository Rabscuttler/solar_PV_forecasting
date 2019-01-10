# Explore UK Power Networks Solar Data ------------------------------------
cust <- read.csv("data/PV Data/2014-11-28 Cleansed and Processed/EXPORT TenMinData/EXPORT TenMinData - Customer Endpoints.csv", 
                 stringsAsFactors = FALSE)

unique(cust$Substation)
#  "Forest Road"      "Suffolk Road"     "Bancroft Close"   "Alverston Close"  "Maple Drive East" "YMCA"  

feeders <- read.csv("data/PV Data/2014-11-28 Cleansed and Processed/EXPORT TenMinData/EXPORT TenMinData - Feeders.csv", 
                 stringsAsFactors = FALSE)
unique(feeders$Substation)
# [1] "Warninglid Lane"          "Chapel Lane"              "Southcroft"               "Bircham Newton"          
# [5] "Rampling Court"           "Forest Road"              "Alverston Close"          "Carters Mead"            
# [9] "Maple Drive East"         "Suffolk Road"             "Old Mill"                 "Bankfield Way"           
# [13] "Fairview Road"            "Elm Crescent"             "Bancroft Close"           "Rookery Farm"            
# [17] "Priesthawes"              "Upper Staplefield Common" "East Hill Costessey"      "YMCA"

# Looks like we've got Elm Crescent (92kW), Forest Road (95kW) and YMCA (25kW)
# "Forest Road",  "Elm Crescent", "YMCA"

