
setwd("C:/Users/GeoffreyWilliams/OneDrive - USDA/Project_folders_Personal/Projects/Sentinels/Top10/Workshop/other_r_code/Impacts_website")
library(R3port)
library(ggplot2)
library(xtable)
library(dplyr)
library(tidyr)


load('plots.RData')
#g$`Amylostereum areolatum` + coord_fixed()

impacts <- NULL

for (f in dir("Impacts")) {
  currtab <- read.csv(paste("Impacts",f,sep="/"))
  
  l <-  length(strsplit(f, '-')[[1]])
  
  group <- strsplit(f, '-')[[1]][l-2]
  
  user <- strsplit(f, '-')[[1]][1]
  
  impacts <- rbind(impacts, cbind(user=user, group=group, currtab))
}

impacts <-
  impacts %>%
  filter(
    !(is.na(sciname) | sciname=="")
  )

impacts <-
  impacts %>% filter(
!(
  (is.na(impact_type) | impact_type=="") &
    (is.na(impact) | impact=="")
))

########

paths_all <- read.csv("Table1_Master-T2_2024.csv")

syns <- c("Litylenchus crenatae mccannii. ","Ceratocistis huliohia ","Ceratocystis huliohia ", "Phytophthora Alni subsp. uniformis", "Harringtonia lauricola")

additionals <- setdiff(unique(impacts$sciname),paths_all$Scientific_Name) %>%
  setdiff(syns)
######

levels_ordered_dimesions <- c("species","community","ecosystem","social")

impacts$dimension <- factor(impacts$dimension,
                            levels=c(levels_ordered_dimesions,
                                     setdiff(
                                       unique(impacts$dimension),
                                       levels_ordered_dimesions))
                            )

#currname <- "Phytophthora ramorum"

################
# PATH WEBSITES
################

for(currname in c(paths_all$Scientific_Name, additionals)) {
  
  if (currname == "Ceratocystis huliohia")
    searchterm <- "Ceratocystis huliohia|Ceratocystis huliohia |Ceratocistis huliohia "
  else if (currname == "Raffaelea lauricola")
    searchterm <- "Raffaelea lauricola|Harringtonia lauricola"
  else
    searchterm <- currname

ff <- filter(impacts, grepl(searchterm,sciname, ignore.case=T)) %>%
 # select(-sciname, -user, -group) %>%
  arrange(impact_time,dimension,impact_type,host) %>%
  select(impact_time,dimension,impact_type,host,impact)

#html_table(
#,
#  x='Scientific_Name',
#  y=
#  xabove=T
#)

if (currname %in% paths_all$Scientific_Name) {
  tbl1<-paths_all %>%
    filter(Scientific_Name==currname) %>%
    select(Plant_Part, Disease_Type, Dispersal, Year, MajorHost_Range_Invaded)%>%t
  xtbl_html <- print(xtable(tbl1),include.colnames=F,print.results=FALSE,type="html", html.table.attributes = "class=table")
}
else {
  xtbl_html<-NULL
}
#rownames(tbl1)<-NULL

#names(tbl1)<-


impacts_sheets <- read.delim("Impacts input (Responses) - Form Responses 1.csv",sep=",") %>%
  filter(Scientific.name.of.pathogen == currname) %>%
  dplyr::rename(impact_time=Impact.time.scale, dimension=Dimension.of.impact, impact_type=Impact.type..refer.to.table., host=Major.main.host.s..of.pathogen..can.enter..Highly.Polyphagous.., impact=Please.describe.the.impact) %>%
  arrange(impact_time,dimension,impact_type,host) %>%
  select(impact_time,dimension,impact_type,host,impact)

ff <- rbind(ff, impacts_sheets) %>%
  select(impact_time,dimension,impact_type,host,impact)

if (dim(ff)[1]>0){
  xtbl_html2 <- print(xtable(ff),print.results=FALSE,type="html", html.table.attributes = "class=table")
} else {
    xtbl_html2 <- NULL
    }

dir.create(currname)

if (currname %in% (setdiff(names(g), c("Stigmina deflectens","Taphrina alni"))))
  html_plot(g[[currname]]+coord_fixed(), out=paste(currname,"/",currname,"-plot.html",sep=""))

if (currname %in% names(g)) {
html_doc(paste0("
<h1>",currname,"</h1>
<h2>",paths_all[paths_all$Scientific_Name==currname, 'Disease_Name'],"</h2>",
                xtbl_html,
                "<h2>Impacts</h2>",
                xtbl_html2,
                "<h1>  Current Distribution </h1>
                  <img src='./figures/", currname, "-plot001.png' alt='Oops something went wrong, check your code' class='img-resp'>"), out=paste(currname,"/",currname,".html",sep=""))
} else {
  html_doc(paste0("
<h1>",currname,"</h1>
<h2>",paths_all[paths_all$Scientific_Name==currname, 'Disease_Name'],"</h2>",
                  xtbl_html,
                  "<h2>Impacts</h2>",
                  xtbl_html2),out=paste(currname,"/",currname,".html",sep=""))
  
}
}


# MASTER WEBSITE

paths_all <- read.csv("Table1_Master-T2_2024.csv")


for(currgroup in unique(paths_all$Group)) {

#currgroup <- "Dothidiomycetes"


#paths_all <-
#  paths_all %>% mutate(sciname_link = 
#                         paste("<a href='../", Scientific_Name, "/", Scientific_Name, ".html'>",Scientific_Name,"</a>", sep=""))

tbl3 <- filter(paths_all, Group==currgroup) %>%
  # select(-sciname, -user, -group) %>%
  arrange(Invasion_Status,Plant_Part,Disease_Type,Dispersal,Scientific_Name) %>%
  select(Scientific_Name,Disease_Name,Invasion_Status,Plant_Part,Disease_Type,Dispersal)

additionals <-
  filter(impacts, group==currgroup) %>%
  filter(!(sciname %in% union(
    tbl3$Scientific_Name, syns
  ))) %>% select(sciname) %>% unlist %>% unique

if (length(additionals)>0)
tbl3 <- rbind(tbl3,
              data.frame(Scientific_Name=additionals, Disease_Name="",Invasion_Status="",Plant_Part="",Disease_Type="",Dispersal=""))

xtbl_html3 <- print(xtable(tbl3) , print.results=FALSE,type="html", html.table.attributes = "class=table")

for (a in tbl3$Scientific_Name) {
  xtbl_html3 <- sub(a,paste("<a href=\'../", a, "/", a, ".html\'>",a,"</a>", sep="")  ,xtbl_html3)
}

html_doc(paste0("
                <h1>",currgroup,"</h1>
                <div>",xtbl_html3,"</div>"),out=paste("Groups/",currgroup,".html",sep=""))

}
