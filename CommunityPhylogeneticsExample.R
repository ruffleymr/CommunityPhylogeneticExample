#Title: Building a Community Phylogeny using a megaphylogenetic appraoch
#Author: Megan Ruffley
#Date: March 27, 2019

########################################################################
## 0.) Load packages needed
require(ape)
require(phytools)

## 1.) Load Tree
#set the working directory to where the tree file is located
setwd("/Users/Megan/Documents/")

big.tre <- read.tree(file="GBOTB.tre")

#check out features of the tree object
attributes(big.tre)

#you can access any attribute by using the '$' sign
#for example, to show all tip labels in tree (~80,000):
big.tre$tip.label

########################################################################
## 2.) Load Species names for Regional community
#Set directory for species names file, if different from above
setwd("/Users/Megan/Documents/")

#load file with species names
species <- read.csv(file="updatedspecieslist1.28.csv")

#species names
species

#To use these species names they all need to be writtin as:
# 'Genus_species' with an underscore seperating the genus and species.
#code below gets the species names in the correct format

#create empty vector to hold the correctly written species names
species.names <- c()

# for each species listed in the 'species' object
for (i in 1:nrow(species)){
	#combine the first row name and the second row name with a '_' in between
	#then add that new name to the end of species_name vector
	species.names <- c(species.names, paste(species[i,1], "_", species[i,2], sep=""))
}

#they should now all be in the right format and not appearing as two columns
species.names

#I have a tendancy to have duplicates in my data files, so this just removes any duplicate names in the vector of species_names
species.names <- unique(species.names)

########################################################################
## 3.) Make your community phylogeny
#create function 'keep.tip'
#R has a function 'drop.tip', but no keep.tip, so we have to write one
keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

#keep.tip function ditches all of the species in the tree that are not in the species_name vector
Prunned.Community.Tre <- keep.tip(big.tre, species.names)

#tree object
Prunned.Community.Tre

#plot tree
plot(Prunned.Community.Tre)

#tree object has fewer species than in the species_name vector because the big.tre did not include all of the species that are in the list
#22 species that are in our species_name vector that are not included in the tree
length(species.names)
length(Prunned.Community.Tre$tip.label)

########################################################################
## 4.) Figure out which species are not included in the community tree

#create a vector to store the list of species not in the tree
missing.sp <- c() 

#for every species in the species.names vector, which is our entire community list
for (i in 1:length(species.names)){
	#ask if the species is present in the community tree we created
	if (species.names[i] %in% Prunned.Community.Tre$tip.label){
		#if it is, do nothing
	}
	else{
		#but if it is not, add it to the list of missing species
		missing.sp <- c(missing.sp, species.names[i])
	}
}

#check the species in missing.sp and verify the number of species in the vector is equal to the number of species missing in the tree
missing.sp
length(missing.sp)
#looks like it is

#sort the vector
missing.sp <- sort(missing.sp)

########################################################################
## 5.) Identify close relative replacements for the species missing in the tree. 

#empty vector to store the names of the species we choose to be replacments
replacment.sp <- c()

#begin with the first species in the list
missing.sp[1]

#see what other species in this genus are present in the big.tre
#always check for your species and verify there are no spelling mistakes either in the big tree or in your own species list
sort(big.tre$tip.label[grepl("Achnatherum", big.tre$tip.label)])

#pull genus and make a tree to view the relationships within the genus
Achnath <- sort(big.tre$tip.label[grepl("Achnatherum", big.tre$tip.label)])
Achnath.tre <- keep.tip(big.tre, Achnath)
plot(Achnath.tre)

#Which species to pick? Not sure. There's probably not one right answer. 
#You could consult some systematics papers on Achnatherum lemonii and see if you can identify which of these species is the closest relative. 
#You could also just select one species at random, which is probably fine too

#going to pick A.calamagrostis and add that species name to our replacement vector
replacment.sp <- c(replacment.sp, "Achnatherum_calamagrostis")

#Repeat with all of the species missing from your tree
missing.sp[2]

###############################
##Once you have completed going through the 22 species not in the big.tree, and found suitable relatives (or not).

Prunned.Community.Tre <- keep.tip(big.tre, c(species.names, replacement.sp))
plot(Prunned.Community.Tre)
