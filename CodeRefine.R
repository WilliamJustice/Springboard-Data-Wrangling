library(tidyr)
library(dplyr)

refine_original$company <- tolower(refine_original$company)
 
head(refine_original)
tail(refine_original)
str(refine_original)
View(refine_original, "company")


#Clean up brand names
refined <- mutate(refine_original, company = substr(refine_original$company, 1 , stop = 1))
refined$company<- gsub("^[p|f]", "philips", refined$company)
refined$company<- gsub("^[a]", "akzo", refined$company)
refined$company<- gsub("^[u]", "unilever", refined$company)
refined$company<- gsub("^[v]", "van houten", refined$company)




#Separate product code and number
refined<- refined %>% separate('Product.code...number', into = c("product_code","product_number"), sep = "-")


#Add product catagories
refined<- mutate(refined, product_category= product_code)
refined$product_category<- gsub("^[q]", "Tablet", refined$product_category) 
refined$product_category<- gsub("^[x]", "Laptop", refined$product_category)
refined$product_category<- gsub("^[v]", "TV", refined$product_category)
refined$product_category<- gsub("^[p]", "Smartphone", refined$product_category)


#Add full address for geocoding
refined<- unite(refined, full_address, address: city, country, sep = ",")

#Create dummy variables for company
refined <- refined %>% mutate(company_philips = ifelse(company =="philips", 1, 0)) %>%
  +     mutate(company_akzo = ifelse(company == "akzo", 1, 0)) %>%
  +     mutate(company_van_houten = ifelse(company == "van_houten", 1, 0)) %>%
  +     mutate(company_unilever = ifelse(company == "unilever", 1, 0))

#Create dummy variables for category
refined <- refined %>% mutate(product_smartphone = ifelse(product_category == "Smartphone", 1, 0)) %>%
  +     mutate(product_TV = ifelse(product_category == "TV", 1, 0)) %>%
  +     mutate(product_laptop = ifelse(product_category == "Laptop", 1, 0)) %>%
  +     mutate(product_tablet = ifelse(product_category == "Tablet", 1, 0))


refine_clean<- refined
write.csv(refine_clean, file = "refine_clean.csv")
