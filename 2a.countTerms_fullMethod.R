# -------------------------------------------------------------------- #
# Text Analysis - Marine Protected Area Management Plan Review
#
# Script 2a: "Full method" text search
#
# __Main author:__  Katie Gale
# __Affiliation:__  Fisheries and Oceans Canada (DFO)
# __Group:__        Marine Spatial Ecology and Analysis
# __Location:__     Institute of Ocean Sciences
# __Contact:__      e-mail: katie.gale@dfo-mpo.gc.ca | tel: 250-363-6411
#
# Objective: Summarize the full text of a series of PDFs, and optionally to search for a set of terms of interest.
#
# Version date: 23 April 2020
# -------------------------------------------------------------------- #

# 1.countTerms_setup.R must have been run first.
dir.create("data-generated", showWarnings = FALSE)
outdir <- "data-generated"

# -------- Start Analysis --------- #
# FULL Method
start_time_full<-Sys.time()

rootWords<-allTerms
# make the "Corpus" - the database of documents
corpus<-Corpus(URISource(list.of.pdfs),readerControl = list(reader = readPDF))
saveRDS(corpus, file = "corpus.rds")
corpus <- readRDS("corpus.rds")

# writeCorpus(corpus) # optional - writes individual text files

#remove punctuation
corpus.1 <- tm_map(corpus, removePunctuation, ucp = TRUE)

length(list.of.pdfs) #665 in list
length(corpus.1) # 665 loaded ## compare this to the papers that didn't parse in the search version

# read PDFs
# make a "term document matrix"
# the "stemming" option does not work very well so I'm not using it
# root words can be searched at the end once everything is compiled
tdm <- TermDocumentMatrix(corpus.1, control = list(removePunctuation = TRUE,stopwords = TRUE,tolower = TRUE, removeNumbers = TRUE))
inspect(tdm) #216256 terms for 665 documents

#convert to dataframe
mat<-as.data.frame(as.matrix(tdm))
mat$term<-row.names(mat)

# check for papers that have no/few terms, which will need to be OCR'd- need to OCR these
nWordsPerPaper<-cbind.data.frame(paper=colnames(mat[!names(mat) %in% c("term", "terms")]), nWords=colSums(mat[!names(mat) %in% c("term", "terms")]))
nWordsPerPaper[nWordsPerPaper$nTerms<50,] # 0 papers need fixed

write.csv(nWordsPerPaper, paste0(outdir, "/FULL_nWordsPerPaper.csv"), row.names=F) # this is the only output that is used in script 2b.

# ---- * if not interested in full method results - could stop here.

#Look at terms and count them across all papers
head(unique(mat$term)) # there are unicode/weird characters
mat$totalCountInAllPapers<-rowSums(mat[,!names(mat) %in% "term"])
nrow(mat) #216868

#Check for terms present only once in a single paper. Generally these are the error/bad read terms.
termsOnlyInOnePaper<-mat$term[mat$totalCountInAllPapers==1]
#write.csv(termsOnlyInOnePaper, paste0(outdir, "/singleTerms.csv"), row.names=F) #optional - take a look

#Remove them
mat<-mat[mat$totalCountInAllPapers>1,]
nrow(mat) # 108478

#Remove trouble terms
mat<-mat[!c(grepl("[^[:alnum:][:blank:]?&/\\-]", mat$term)),] #remove terms with punctuation
mat<-mat[nchar(mat$term)<30,] #remove really long words which are bad reads
mat<-mat[!c(grepl("http|www", mat$term)),] #remove webites
mat<-mat[!c(grepl("^\\s",mat$term)),] #remove those with leading spaces (easier to deal with than rolling them into others)
nrow(mat) #105500

# # #Look at top terms
mat$nPapersWithTerm<-rowSums(mat[,!names(mat) %in% c("term", "totalCountInAllPapers")]!=0)
# write.csv(mat[order(-mat$totalCountInAllPapers),][,c("term", "totalCountInAllPapers")], paste0(outdir, "/totalCountInAllPapers.csv"), row.names=F)
# write.csv(mat[order(-mat$nPapersWithTerm),][,c("term", "nPapersWithTerm")],paste0(outdir, "/nPapersWithTerm.csv"), row.names=F)

# #optional word cloud (takes a long time)
# png(paste0(outdir, "/textCloud.png"), height=20, width=20, res=300, units="cm")
#   wordcloud::wordcloud(words = mat$terms, freq = mat$totalCountInAllPapers, min.freq = 100,random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))
# dev.off()

# ---- Process Output for terms of Interest ----

# grep root terms using get matching terms
# Will have to do manual selection of some of the terms
matchFreq<-data.frame(searchTerm=NA,term=NA, totalCountInAllPapers=NA)
for (i in 1:length(rootWords)){
  find<-mat[grep(rootWords[i], mat$term),]
  if(nrow(find)>0){
  findTerms<-ddply(find, c("term"), summarize,  totalCountInAllPapers=sum(totalCountInAllPapers))
  matchFreq<-rbind.data.frame(matchFreq,cbind.data.frame(searchTerm=rootWords[i],findTerms[order(-findTerms$totalCountInAllPapers),]))
  }
}

matchFreq<-unique(matchFreq[order(matchFreq$searchTerm,-matchFreq$totalCountInAllPapers),])
head(matchFreq) # total count of each term by paper

# write this, manually remove bad lines, re-query
# bad lines include mis spellings, words that don't match our context, species names, compound words with spaces removed/misread, and non-english words
write.csv(matchFreq, paste0(outdir,"/FULL_termFreq.csv"), row.names=F)
#MANUAL STEP
keepTerms<-read.csv(paste0(outdir, "/FULL_termFreq2.csv"))

# The output currently is the count of each term in each paper, including zeros
# it has papers as columns and terms by rows. Want the reverse.
# It's too big - subset the matrix by only the terms we're interested in to improve performance
dim(mat) #104080 x 705

# matrix by terms (full words, not roots)
# transpose and reformat
termMatrixInt_full<-data.frame(t(mat[mat$term %in% keepTerms$term,!names(mat)%in% c("totalCountInAllPapers", "nPapersWithTerm")]))
termMatrixInt_full$paper<-row.names(termMatrixInt_full)
row.names(termMatrixInt_full)<-NULL
termMatrixInt_full<-termMatrixInt_full[termMatrixInt_full$paper!="term",]
unique(termMatrixInt_full$paper)

for(i in (1:(ncol(termMatrixInt_full)-1))){ termMatrixInt_full[,i]<-as.numeric(as.character(gsub("\\s","",termMatrixInt_full[,i])))}

head(termMatrixInt_full) ##note the term "function" has been replaced with "function." by R
termMatrixInt_full<-merge(termMatrixInt_full, luPaper[, c("paper","Country", "PA.Name", "Grouping")], by="paper", all.x=T)
head(termMatrixInt_full) # this is the matrix with terms as columns

# convert this to combine the root words
termMatrixInt.melt<-melt(termMatrixInt_full, id.vars=c("paper", "Country", "PA.Name", "Grouping"))
names(termMatrixInt.melt)[names(termMatrixInt.melt)=="variable"]<-"term"
termMatrixInt.melt$term<-as.character(termMatrixInt.melt$term)
termMatrixInt.melt$term[termMatrixInt.melt$term=="function."]<-"function"
termMatrixInt.melt<-merge(termMatrixInt.melt, keepTerms[,c("searchTerm", "term")], by="term", all.x=T)
head(termMatrixInt.melt)

#recast matrix
termMatrixInt_root<-reshape::cast(termMatrixInt.melt, paper+Country+PA.Name+Grouping~searchTerm, fun="sum")

head(termMatrixInt_root) # this is the matrix with roots as columns

#write output
saveRDS(termMatrixInt_full, paste0(outdir, "/FULL-full_outputMatrix.rds"))
saveRDS(termMatrixInt_root, paste0(outdir, "/FULL-root_outputMatrix.rds"))

## -- end -- ##
end_time_full<-Sys.time()
cat("full method took", end_time_full-start_time_full, " minutes") #24 mins
mid_time_full-start_time_full
end_time_full-mid2_time_full
