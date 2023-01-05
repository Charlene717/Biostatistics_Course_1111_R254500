FUN_Beautify_ggplot = function( P1,
                           LegPos = c(0.8, 0.85),LegBox = "vertical",LegDir="vertical",
                           LegTitleSize=17 ,LegTextSize = 15,
                           TH= 0.05,TV= -10, TitleSize = 20,
                           XtextSize=15,  YtextSize=15,  XaThick=0.5,  YaThick=0.5, xangle = 60,
                           AxisTitleSize=1.5, AspRat=1,SubTitSize = 15,
                           OL_Thick = 2.2
                           ){

  library(ggplot2)
  library(graphics)

  P2 <-  P1  +
    theme_classic() + # https://www.statology.org/ggplot-background-color/
    # theme_bw() # http://www.sthda.com/english/wiki/ggplot2-themes-and-background-colors-the-3-elements+
    theme(axis.text.x = element_text(face="bold", colour="black",  size = XtextSize,angle = xangle, hjust = 1, vjust = 1), # Change the size along the x axis
          axis.text.y = element_text(face="bold", colour="black",size = YtextSize), # Change the size along the y axis

          axis.line = element_line(colour = "darkblue", size = 2, linetype = "solid"),
          axis.title = element_text(size = rel(AxisTitleSize),face="bold",colour = "#3f3369"),
          plot.title = element_text(color="black",
                                    size=TitleSize,
                                    face="bold.italic",
                                    hjust = TH,vjust =TV), # margin = margin(t = 0.5, b = -7),
          #     plot.background = element_rect(fill = 'chartreuse'),
          legend.title = element_text(size=LegTitleSize, color = "black", face="bold"),
          legend.text = element_text(colour="black", size= LegTextSize,face="bold"),
          legend.background = element_rect(fill = alpha("white", 0.5)),
          #      legend.position = c(0.1, 0.18),
          #     plot.text = element_text(size = 20),
          aspect.ratio=AspRat) + #square plot
    theme(axis.line.x = element_line(colour = "black", size = XaThick),
          axis.line.y = element_line(colour = "black", size = YaThick))+
    theme(legend.position = LegPos , legend.box = LegBox ,legend.direction=LegDir)+ # vertical, horizontal
    #theme(legend.position = "top" , legend.direction = LegDir) # vertical, horizontal
    theme(strip.text = element_text(size=SubTitSize))+
    # White background
    # http://www.4k8k.xyz/article/wish_to_top/114137594
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
    # Add outline thick
    # https://www.cnblogs.com/liujiaxin2018/p/14257944.html
    theme(panel.border = element_rect(fill=NA,color="black", size= OL_Thick, linetype="solid"))

  return(P2)
}
