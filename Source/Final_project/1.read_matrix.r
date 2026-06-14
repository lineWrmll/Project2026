# Load the matrix
matrix_full <- read.csv("data/matrix_full_clim_eco_elev.csv")

names(matrix_full)

# My Ecological project: 

# Ursus arctos, commonly called brown bear, is a large omnivorous mammal native to North America and Eurasia. It is found mainly 
# in forested habitats, mostly in northern temperate forests, up to 5000 m in elevation (1). 
# Ursus arctos horribilis is a sub-species of U. arctos, and is called grizzly bear. It shares the ecoogical niche of U. arctos.
# Ursus maritimus, the polar bear, is found around the North Pole. It relies on sea ice and depends on water to hunt. 
# It is therefore mainly found near coasts and doesn't venture inland (2).
# U. arctos and U.maritimus have different ecological needs and don't share the same habitat. 

# Ursus arctos and Ursus maritimus are tboth present in Canada (1, 2).

# U. arctos and U. maritimus can hybridize and produce a fertile descendance suggesting that they share a 
# relatively close common ancestor (3). 
# A bidirectional gene flow between has been reported by several studies (4, 5). 
# This introgression has resulted in a replacment of the original mtDNA of a part of polar bear populations (6). 
# However, both species are considered two distinct species (6). 
# Hybrids present a fur colour between brown and white. This colour is not adapted to neither ecological niches (7). 
# Even if natural events of hybridization have been reported in the evolutionary history of the genus Ursus, the hybridization 
# rate might increase with global change and rising temperatures. 

# Global change is particularly threatening U. maritimus. Rising temperatures reduce sea ice resulting in a diminution of 
# of U. maritimus distribution (6). On the contrary, the distibution of U. arctos might be expanded as more habitats are becoming suitable. 
# This can lead to more regions where both species coexist and breed, leading to a higher rate of hybridization. 

# It is of importance to predict the impact of global change in future years in order to implement adequate conservation measures 
# to protect bear species. 

# As global warming is causing temperature to rise and sea ice to melt, my prediction is that the distribution of
# Ursus maritimus will shrink in 2050 while the distribution of Ursus arctos will expand northward. 

# To test this hypothesis, I will use my matrix to analyze the ecological variables that are link to both 
# species distribution and how they are expected to evolve in the future. 



# References
# (1) Pasitschniak-Arts, M. (1993). Ursus arctos. Mammalian Species, (439), 1–10. https://doi.org/10.2307/3504138 
# (2) Ian Stirling (2009). Polar Bear: Ursus maritimus, (888-890). https://doi.org/10.1016/B978-0-12-373553-9.00204-2
# (3) Pongracz, J. D., Paetkau, D., Branigan, M., & Richardson, E. (2017). Recent Hybridization between a Polar Bear 
#vand Grizzly Bears in the Canadian Arctic. Arctic, 70(2), 151–160. http://www.jstor.org/stable/26379758
# (4) T. Lan, K. Leppälä, C. Tomlin, S.L. Talbot, G.K. Sage, S.D. Farley, R.T. Shideler, L. Bachmann, Ø. Wiig, V.A. Albert, 
# J. Salojärvi, T. Mailund, D.I. Drautz-Moses, S.C. Schuster, L. Herrera-Estrella, & C. Lindqvist, Insights into bear evolution 
# from a Pleistocene polar bear genome, Proc. Natl. Acad. Sci. U.S.A. 119 (24) e2200016119, https://doi.org/10.1073/pnas.2200016119 (2022).
# (5) Shafer, C.L. (2023) Contribution of hybridization between polar bears and grizzly bears to polar bear extinction. Wildlife Letters, 
# 1, 178–193. https://doi.org/10.1002/wll2.12024
# (6) Frank Hailer et al. ,Nuclear Genomic Sequences Reveal that Polar Bears Are an Old and Distinct Bear Lineage.Science336,344-347(2012).
# DOI:10.1126/science.1216424
# (7) Shafer, C. L. (2023). Contribution of hybridization between polar bears and grizzly bears to polar bear extinction. Wildlife Letters. 
# https://doi.org/10.1002/wll2.12024
