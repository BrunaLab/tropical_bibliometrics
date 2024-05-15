
# load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
# library(tidystringdist)


# TODO: Need to add

# golfo
# isla
# especies
# quinntana and roo separated", make quintanaroo
# bosque
# bosques
# sotobosque
# murcielago
# insectos



# geographic --------------------------------------------------------------


geo <- c(
"africa",
"african",
"african",
"african savanna",
"amazon",
"amazonia",
"amazonian",
"amazonian forest",
"andean",
"andes",
"angola",
"arctic",
"aruba",
"atlantic",
"atlantic forest",
"australia",
"australian wet",
"austria",
"bahamas",
"baja california",
"barro colorado",
"bci",
"belgium",
"belize",
"benin",
"bolivia",
"bolivian",
"boreal",
"boreal forest",
"bornean",
"borneo",
"brazil",
"brazilian",
"brazilian atlantic",
"british isles",
"brunei",
"burkina faso",
"burma",
"burundi",
"caatinga",
"california mexico",
"cambodia",
"cameroon",
"canada",
"canadian",
"caribbean",
"cayman islands",
"central",
"central amazonia",
"cerrado",
"chamela",
"chiapas mexico",
"colombia",
"colombian caribbean",
"comoros",
"congo",
"costa rica",
"costarica",
"cuba",
"dominica",
"dominican republic",
"dry tropical",
"east timor",
"eastern decid",
"eastern pacific",
"ecuador",
"el salvador",
"england",
"equatorial guinea",
"ethiopia",
"finland",
"france",
"french guiana",
"gabon",
"galapagos",
"germany",
"ghana",
"ghats india",
"golfo dulce",
"great basin",
"grenada",
"guadeloupe",
"guam",
"guatemala",
"guyana",
"haiti",
"hawaii",
"honduras",
"india",
"indian",
"indonesia",
"ivory coast",
"jamaica",
"kenya",
"kruger",
"la selva",
"lowland tropical",
"madagascar",
"malawi",
"malaysia",
"manu national",
"martinique",
"mauritius",
"mayotte",
"mexico",
"mozambique",
"myanmar",
"national park",
"neotropic",
"neotropical",
"neotropical forest",
"neotropical rainforest",
"netherlands",
"netherlands antilles",
"new zealand",
"nicaragua",
"nicoya costarica",
"nigeria",
"north america",
"north american",
"norway",
"nouragues",
"pacific",
"pacific coast",
"paleotropic",
"panama",
"papua new",
"paraguay",
"park costarica",
"peru",
"philippines",
"polar",
"puerto rico",
"reunion",
"rwanda",
"saint lucia",
"saint maarten",
"serengeti",
"seychelles",
"singapore",
"south",
"south eastern",
"southeastern brazil",
"southern",
"suriname",
"sweden",
"tanzania",
"temperate rain forest",
"thailand",
"togo",
"trinidad",
"tropic",
"tropical",
"tropical dry",
"tropical dryforest",
"tropical forest",
"tropical montane",
"tropical rainforest",
"tropical tree",
"tropical wet",
"tropical wet forest",
"tropics",
"tuxtlas",
"uganda",
"united kingdom",
"usa",
"venezuela",
"veracruz mexico",
"vietnam",
"virgin islands",
"western ghats",
"yasuni",
"zambia"
)



# taxonomic ---------------------------------------------------------------
taxon<-c(
"amphibian",
"ant",
"ant plant",
"anuran",
"arbuscular mycorrhizal",
"atta",
"bats",
"bird",
"bird community",
"chiroptera",
"coral reef",
"cutting ant",
"dipterocarp forest",
"dipterocarpaceae",
"drosophila",
"drosophila melanogaster",
"drosophila pseudoobscura",
"dung beetle",
"elephant",
"epiphyte",
"ficus",
"formicidae",
"heliconia",
"hummingbird",
"hymenoptera formicidae",
"leafcutter ant",
"leafcutting ant",
"lepidoptera",
"liana",
"mammal",
"mangrove",
"mangrove forest",
"primate",
"reptile",
"rodent",
"sea urchin",
"zooplankton"
)



# habitat -----------------------------------------------------------------
habitat<-c(
"cloud forest",
"deciduous forest",
"dry forest",
"evergreen forest",
"forest",
"grassland",
"island",
"lowland",
"lowland forest",
"lowland rainforest",
"montane",
"montane forest",
"montane rainforest",
"rain forest",
"rainforest",
"rainforest tree",
"salt marsh",
"savanna",
"secondary forest",
"wet forest",
"african savanna",
"amazonian forest",
"atlantic forest",
"australian wet",
"boreal forest",
"brazilian atlantic",
"cerrado",
"dry tropical",
"eastern decid",
"lowland tropical",
"neotropical forest",
"neotropical rainforest",
"pacific coast",
"polar",
"temperate rain forest",
"tropical dry",
"tropical dryforest",
"tropical forest",
"tropical montane",
"tropical rainforest",
"tropical wet",
"tropical wet forest",
"tropics",
"western ghats",
"dipterocarp forest",
"mangrove",
"mangrove forest"
)



# plant parts -------------------------------------------------------------

part<-c(
"canopy",
"leaf",
"leaves",
"litter",
"litter decomposition",
"seed",
"seedling",
"tree",
"rainforest tree"
)



# bind and process --------------------------------------------------------



system<-c(geo,
          taxon,
          habitat,
          part)

geo<-as_tibble(geo)
taxon<-as_tibble(taxon)
habitat<-as_tibble(habitat)
part<-as_tibble(part)

system<- as_tibble(system) %>% 
  distinct() %>% 
  rename(system=value) %>% 
  mutate(geo=case_when(
    system%in%geo$value ~ TRUE,
    .default = FALSE)) %>% 
  mutate(taxon=case_when(
    system%in%taxon$value ~ TRUE,
    .default = FALSE)) %>% 
  mutate(habitat=case_when(
    system%in%habitat$value ~ TRUE,
    .default = FALSE)) %>% 
  mutate(part=case_when(
    system%in%part$value ~ TRUE,
    .default = FALSE)) 



# save the file -----------------------------------------------------------

write_csv(system,"./data/data_archive/system.csv")

















































# save to the data_archive folder
write_csv(journals",here("data"","data_archive"","jrnls.csv"))

