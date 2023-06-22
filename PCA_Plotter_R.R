
hapmap_rooted_pca<-read.table("DPCon_1000G.evec.tab", header=FALSE, skip=1)
names(hapmap_rooted_pca)=c("FID", "IID", "PCA1", "PCA2", "PCA3", "PCA4", "PCA5","PCA6", "PCA7", "PCA8", "PCA9", "PCA10", "STAT")

hapmap_pops<-read.table("relationships_w_pop_1kg.txt", header=TRUE)

#abraom_EA<-read.table("ABraOM_ES_1_3_2023.txt", header=TRUE)

xhapmap_rooted_pca=merge(hapmap_rooted_pca, hapmap_pops, by=c("FID", "IID"), all.x = TRUE)

#xhapmap_rooted_pca=merge(pre_xhapmap_rooted_pca, abraom_EA, by=c("FID", "IID"), all.x = TRUE)

xhapmap_rooted_pca$group=7

# East Asian = 1
xhapmap_rooted_pca$group[xhapmap_rooted_pca$population=="EAS"]<-1
# African = 2
xhapmap_rooted_pca$group[xhapmap_rooted_pca$population=="AFR"]<-2

# European = 3
xhapmap_rooted_pca$group[xhapmap_rooted_pca$population=="EUR"]<-3

# American = 4 (admixed)
xhapmap_rooted_pca$group[xhapmap_rooted_pca$population=="AMR"]<-4
# South Asian = 5
xhapmap_rooted_pca$group[xhapmap_rooted_pca$population=="SAS"]<-5
# Finnish = 6
xhapmap_rooted_pca$group[xhapmap_rooted_pca$population=="FIN"]<-6

# ABraOM_EA = 7
#xhapmap_rooted_pca$group[xhapmap_rooted_pca$PCA_population=="ABraOM_EA"]<-7


write.table(xhapmap_rooted_pca, file = "DPCon_1KG_pca_ECS_2_23_2023.txt", row.names = FALSE, quote = FALSE, sep='\t')

XSTUDY="DPCon_1000G"
XDIR_PATH="D:/Workflows/PRS_with_Chan2022/PCA/PCA_Results"


XPLOTTER=function(xDATA=xstudy_pca, xVAR="s_out", xLEGEND="STUDY", xNAME="")
{
  
  xpath<-paste(XDIR_PATH, "/", XSTUDY, "_", xNAME, "_PCA_PLOTS.pdf", sep="")
  
  pdf(file =xpath)
  
  for (xx in 1:9)
  {
    for (zz in (xx+1):10)
    {
      # X/Y Axis labels
      ZZZ<-paste("PCA",xx, sep="")
      XYZ<-paste("PCA",zz, sep="")
      
      # Count of Number of Variable Levels - for legend
      NX=length(table(xDATA[xVAR]))
      
      # Generating Plot Title 
      xlabel<-paste(XSTUDY, " - ", xNAME, "  
                                PCA Plot: ", ZZZ," vs. ", XYZ, sep="")
      
      # Generating Plot Legend
      legend.txt<-xLEGEND
      
      # Sorting data for color coding
      zDATA<-xDATA[order(xDATA[,xVAR],decreasing=F),]
      # Generating Plot
      
      plot(zDATA[,XYZ]~zDATA[,ZZZ], main=xlabel, pch=10,
           col = as.numeric(zDATA[,xVAR]), xlab=ZZZ, ylab=XYZ,  cex=.75 );
      legend("bottomleft", legend.txt, col= 1:NX,  pch = 10, cex = 0.8)

    }
  }
  dev.off()
}


XPLOTTER(xDATA=xhapmap_rooted_pca, xVAR="group", xLEGEND=c("East Asian", "African", "Non-Finnish European", "Admixed American", "South Asian", "Finnish", "DPCon"), xNAME="1000kg_rooted")
















