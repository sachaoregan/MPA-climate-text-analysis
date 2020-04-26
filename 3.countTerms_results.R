# -------------------------------------------------------------------- #
# Text Analysis - Marine Protected Area Management Plan Review
#
# Script 4:  Process results
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

rm(list=setdiff(ls(), c("dir","outdir","parkPaperByCountry","parkPaperByGrouping"))) #clean up
setwd(dir)

# Which output should be used?
# one of "full" (all matching single terms), "root" (summed root/wildcards), or "search" (multi-word phrases)
use<-"search"

if (dir.exists(paste0(outdir, "/results"))){} else {
dir.create(paste0(outdir, "/results"))}

# ---- load data
termMatrixInt_full<-readRDS(paste0(outdir, "/FULL-full_outputMatrix.rds"))
termMatrixInt_root<-readRDS(paste0(outdir, "/FULL-root_outputMatrix.rds"))
termMatrixInt_search<-readRDS(paste0(outdir, "/SEARCH_outputMatrix.rds"))

#which input to use?
if (use=="full") {matrix<-termMatrixInt_full} else {
  if (use=="root") {matrix<-termMatrixInt_root} else {
    if (use=="search") {matrix<-termMatrixInt_search} else {
      }}}

#Summarize by Park
tabByPark<-melt(unique(matrix[!names(matrix) %in% c("paper","WDPAID","NAME","DESIG", "nWords")]), id.vars=c("PA.Name", "Country", "Grouping"))
names(tabByPark)[names(tabByPark)=="value"]<-"termPresent"
tabByPark<-tabByPark[tabByPark$termPresent>0 & !is.na(tabByPark$variable),]
tabByPark$termPresent[tabByPark$termPresent>0]<-1
length(unique(tabByPark$PA.Name)) #829

#bring in helper tables from initial script to summarize counts by grouping
CountByParkByGrouping<-ddply(tabByPark, c("Grouping", "variable"), summarize, nParkswTermPres=length(unique(PA.Name)))
CountByParkByGroupingWideFill<-CountByParkByGrouping %>% spread(key="Grouping", value="nParkswTermPres", fill=0) #spread to fill in zeros
CountByParkByGrouping<-melt(CountByParkByGroupingWideFill,id.vars="variable" )
names(CountByParkByGrouping)[2:3]<-c("Grouping", "nParkswTermPres")
CountByParkByGrouping<-merge(CountByParkByGrouping, parkPaperByGrouping[,c(1,2)], by="Grouping")
CountByParkByGrouping$percentParkswTermPres<-CountByParkByGrouping$nParkswTermPres/CountByParkByGrouping$nParks #44
head(CountByParkByGrouping)

#Summarize by Paper
tabByPaper<-melt(unique(matrix[!names(matrix) %in% c("PA.Name","WDPAID","NAME","DESIG","nWords")]), id.vars=c("paper", "Country", "Grouping"))
row.names(tabByPaper)<-NULL
names(tabByPaper)[names(tabByPaper)=="value"]<-"termPresent"
tabByPaper<-tabByPaper[tabByPaper$termPresent>0 & !is.na(tabByPaper$variable),]
tabByPaper$termPresent[tabByPaper$termPresent>0]<-1
length(unique(tabByPaper$paper)) #709

CountByPaperByGrouping<-ddply(tabByPaper, c("Grouping","variable"), summarize, nPaperswTermPres=length(unique(paper)))
CountByPaperByGroupingWideFill<-CountByPaperByGrouping %>% spread(key="Grouping", value="nPaperswTermPres", fill=0)
CountByPaperByGrouping<-melt(CountByPaperByGroupingWideFill,id.vars="variable" )
names(CountByPaperByGrouping)[2:3]<-c("Grouping", "nPaperswTermPres")
CountByPaperByGrouping<-merge(CountByPaperByGrouping, parkPaperByGrouping[,c(1,3)], by="Grouping")
CountByPaperByGrouping$percentPaperswTermPres<-CountByPaperByGrouping$nPaperswTermPres/CountByPaperByGrouping$nPapers
head(CountByPaperByGrouping)

# terms present by paper
write.csv(tabByPaper, paste0(outdir,"/results/",use,"_results_terms_by_Paper_",Sys.Date(), ".csv"), row.names=F)
write.csv(tabByPark, paste0(outdir,"/results/", use, "_results_terms_by_Park_",Sys.Date(), ".csv"), row.names=F)

#Overall Results
length(unique(tabByPaper$paper))/length(unique(matrix$paper))
length(unique(tabByPark$PA.Name))/length(unique(matrix$PA.Name))

CountByPark<-ddply(tabByPark, c("variable"), summarize, nParkswTermPres=length(unique(PA.Name)))
CountByPark$percentParkswTermPres<-CountByPark$nParkswTermPres/length(unique(matrix$PA.Name))
write.csv(CountByPark, paste0(outdir,"/results/",use,"_results_terms_countParks_",Sys.Date(), ".csv"), row.names=F)

CountByPaper<-ddply(tabByPaper, c("variable"), summarize, nPaperswTermPres=length(unique(paper)))
CountByPaper$percentPaperswTermPres<-CountByPaper$nPaperswTermPres/length(unique(matrix$paper))
write.csv(CountByPaper, paste0(outdir,"/results/", use, "_results_terms_countPapers_",Sys.Date(), ".csv"), row.names=F)


# Figures
# The won't work well for the full version
if(length(unique(CountByParkByGrouping$variable))<15){
  width=20} else  if(length(unique(CountByParkByGrouping$variable))>45){
    width=80} else { width=40}


png(paste0(outdir, "/results/",use,"_TermsByPark.png"), res=250, height=12, width=width, units="cm")
ggplot(CountByPark, aes(x=reorder(variable,-percentParkswTermPres), y=percentParkswTermPres))+geom_bar(stat="identity", color="black", position="dodge")+
  theme_classic()+theme(axis.text.x = element_text(angle = 20, hjust = 1,color="black"),
                        legend.title = element_text(size = 9),legend.text = element_text(size = 8),legend.key.size =  unit(0.25, "cm"))+
  scale_y_continuous(expand=c(0,0), limits = c(0,1))+xlab(label = "")+
  ylab(label="Percent of Protected Areas \nwith Documents Containing Term")+ggtitle(paste0("n = ", length(unique(matrix$PA.Name)), " Protected Areas"))
dev.off()

png(paste0(outdir, "/results/",use,"_TermsByPaper.png"), res=250, height=12, width=width, units="cm")
ggplot(CountByPaper, aes(x=reorder(variable, -percentPaperswTermPres), y=percentPaperswTermPres))+geom_bar(stat="identity", color="black", position="dodge")+
  theme_classic()+theme(axis.text.x = element_text(angle = 20, hjust = 1,color="black"),
                        legend.title = element_text(size = 9),legend.text = element_text(size = 8),legend.key.size =  unit(0.25, "cm"))+
  scale_y_continuous(expand=c(0,0), limits = c(0,1))+xlab(label = "")+
  ylab(label="Percent of Documents Containing Term")+ggtitle(paste0("n = ", length(unique(matrix$paper)), " Documents"))
dev.off()

#By Paper Grouping

forLegendPark<-unique(CountByParkByGrouping[,c("Grouping", "nParks")])
png(paste0(outdir, "/results/",use,"_TermsByPark_ByGrouping.png"), res=250, height=12, width=width, units="cm")
ggplot(CountByParkByGrouping, aes(x=variable, y=percentParkswTermPres, fill=Grouping))+geom_bar(stat="identity", color="black", position="dodge")+
  theme_classic()+theme(axis.text.x = element_text(angle = 20, hjust = 1,color="black"),
                        legend.title = element_text(size = 9),legend.text = element_text(size = 8),legend.key.size =  unit(0.25, "cm"))+
  scale_y_continuous(expand=c(0,0), limits = c(0,1))+xlab(label = "")+ scale_fill_brewer(palette="Paired",name="Country Grouping\n(n Protected Areas)", labels=paste0(forLegendPark$Grouping, " (",forLegendPark$nParks,")"))+
  ylab(label="Percent of Protected Areas \nwith Documents Containing Term")+ggtitle("By Protected Area")+
  geom_vline(xintercept = seq(1.5, length(unique(CountByPaperByGrouping$variable))+.5), col="darkgray")
dev.off()

forLegendPaper<-unique(CountByPaperByGrouping[,c("Grouping", "nPapers")])
png(paste0(outdir, "/results/",use,"_TermsByPaper_ByGrouping.png"), res=250, height=12, width=width, units="cm")
ggplot(CountByPaperByGrouping, aes(x=variable, y=percentPaperswTermPres, fill=Grouping))+geom_bar(stat="identity", color="black", position="dodge")+
  theme_classic()+theme(axis.text.x = element_text(angle = 20, hjust = 1,color="black"),
                        legend.title = element_text(size = 9),legend.text = element_text(size = 8),legend.key.size =  unit(0.25, "cm"))+
  scale_y_continuous(expand=c(0,0), limits = c(0,1))+xlab(label = "")+
  scale_fill_brewer(palette="Paired",name="Country Grouping\n(n Documents)", labels=paste0(forLegendPaper$Grouping, " (",forLegendPaper$nPapers,")"))+
  ylab(label="Percent of Documents Containing Term")+ggtitle("By Documents")+
  geom_vline(xintercept = seq(1.5, length(unique(CountByPaperByGrouping$variable))+.5), col="darkgray")
dev.off()
