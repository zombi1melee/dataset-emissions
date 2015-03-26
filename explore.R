# Exploratory Analysis & Graphics

# data storage
library("data.table")
library("assertthat")
library("Hmisc")
library("ggplot2")
library("plotrix")

# set my options
op <- options()
options(digits = 5)
# reset to initial options
# options(op)     
options(prompt = "d.tbl>")
format(Sys.time(), "%a %b %d %H:%M:%S %Y")

# PM25data class definition ----------------------
get_code <- function(object) {return(object@class_code)}
get_summary <- function(object) {return(object@summary)}
get_class <- function(object) {return(sapply(object@summary, class))}

setClass(
  Class = "pm25", 
  slots = c(
    class_code = "data.table", 
    summary = "data.table"
  )
)
# Apply one generic function.
setGeneric(
  name = "emission", 
  def = function(object) {standardGeneric("emission")}
)
setMethod("emission",signature("pm25"), get_code)
setMethod("emission",signature("pm25"), get_summary)
setMethod("emission",signature("pm25"), get_class)

# helper functions -------------------------------
load_dataset <- function(file_name) {
     dataset <- readRDS(file_name)
    return(dataset)
}

create_dir <- function(dir_name, file_name) {
  # create directory to hold downloaded file
  if(!file.exists(dir_name)) {
    dir.create(dir_name)
  }
  # unzip file into data dir
  if(file.exists(file_name) & has_extension(file_name, "zip")) {
    unzip(zipfile = file_name, exdir = path.expand(dir_name))
    list.files(dir_name)
    return(TRUE)
  }
}
# read data
nei_dt <- function(summary, class_code) {
  if(missing(summary) | missing(class_code)) {
    create_dir("data", "NEI_data.zip")  
    NEI <- load_dataset("./data//summarySCC_PM25.rds")
    SCC <- load_dataset("./data//Source_Classification_Code.rds")
    new(Class = "pm25", summary = data.table(NEI), class_code = data.table(SCC)) 
  }
}

nei_barplot <- function(d.tbl) {
  # set key and order
  # first subset data
  setkey(nei, year)
  png("./figure/plot1.png", width = 480, height = 480, units = "px", type = "cairo")
d.tbl[,
    j = .(
      emsum = sum(Emissions),
      count = .N 
    ),
    by = year               
    ][,{
      oldpar  <- par(cex.axis = .75, font.axis = 3,
                     lwd = 2, cex = 1.5)
      options(scipen = 3)
      barplot(emsum/length(emsum), names.arg = year, # looks at proportions
              xlab = "Frequency", 
              ylab = "Emission Proportions", 
              col = "#FF3300", beside = TRUE,
              main = "Total US Emissions"
      )
      par(oldpar) #on.exit(par(oldpar))
    }
    ]
dev.off()
}

nei_dotchart <- function(d.tbl) {
  # set key returns selected rows 
  setkey(d.tbl, fips)
  png("./figure/plot2.png", width = 500, height = 500, units = "px", type = "cairo")
  d.tbl[                             # secondary input
  "24510",                           #fips %in% c("24510", "06037")
  .(                                 #list(c("24510", "06037"))
    emsum = sum(Emissions)),
  by = list(year, fips)
  ][,{
    oldpar <- par(cex.axis = .75, font.axis = 3, 
                  lwd = 2, cex = .9, mfrow = c(2,1))
    fips <- factor(fips)
    # Hmisc dotchart3 & minor.tick
    dotchart3(emsum, 
              labels = year, 
              pch  = 19, 
              color = rainbow(4),
              main = "Total Emission\ngrouped by fips",
              ylab = "Year",
              groups = fips,
              xlab = "Emissions", 
              gcolor = "black")
    # custom tick marks  
    minor.tick(nx = 5, ny = 1, tick.ratio = 0.20)
    # fan plot: looks at relative quantities
    # 1999 is the largest, followed by 2005, 2002, 2008
    slices <- emsum
    lab <- year
    fan.plot(slices, labels = lab, main = "Emission Fan Plot")
    
    par(oldpar) # on.exit(par(oldpar))
  }
  ]
dev.off()
}

nei_ggplot1 <- function(d.tbl) {
  setkey(d.tbl, fips)
  p3 <- ggplot(d.tbl[
  "24510", 
  list(emsum = sum(Emissions),
       emmin = min(Emissions),
       emmax = max(Emissions)),
  by = .(year, type)
  ], aes(x = year, y = emsum, colour = type)) + 
  geom_line(aes(group = type)) + 
  ylab("Emissions") + 
  geom_point(size = 3) + 
  ggtitle("Baltimore City, Maryland\nTotal PM2.5 Emissions")
# plot file
ggsave(filename = "./figure/plot3.png", p3, dp = 72, width = 7, height = 7)
}

nei_ggplot2 <- function(map) {
  #setkey(map, NULL)
  #setkey(map, Short.Name)  
  p4 <- ggplot(map[ 
  Short.Name %like% "Coal", 
  list(emsum = sum(Emissions)),
  by = .(year)], aes(x = year, y = emsum, colour = year)) +
  geom_line(size = 1.2) + 
  geom_point(colour = "black", size = 2, shape = 19, fill = "black") + 
  ggtitle("Coal Combustion Sources") + 
  ylab("Emission Totals") +
  theme(plot.title = element_text(lineheight = .9, face = "bold")) +
  theme(legend.text = element_text(colour = "blue", size = 16, face = "bold")) +
  theme(legend.title = element_text(colour = "blue", size = 16, face = "bold"))
# plot file
ggsave(filename = "./figure/plot4.png", p4, dp = 72, width = 7, height = 7)
}

nei_ggplot3 <- function(map) {
  setkey(map, type, fips)
  p5 <- ggplot(map[ 
  list(c("ON-ROAD","NON-ROAD"), c("24510")), #type %in% c("ON-ROAD", "NON-ROAD") & fips == "24510", 
  list(emsum = sum(Emissions),
       avg = mean(Emissions)),
  by = .(year, type)
  ], aes(x = year, y = emsum, colour = type)) + 
  geom_line() + 
  geom_point(size = 3) + 
  ggtitle("Motor Vehicle Sources\nBaltimore City, Maryland (24510)") + 
  ylab("Emission Totals - Nonroad and Onroad")
# plot file
ggsave(filename = "./figure/plot5.png", p5, dp = 72, width = 7, height = 7)
}

nei_ggplot4 <- function(map) {
  setkey(map, fips, type)
  p6 <- ggplot(map[ 
  list(c("24510", "06037"), c("ON-ROAD","NON-ROAD")),  #type %in% c("ON-ROAD", "NON-ROAD") & fips %in% c("24510", "06037"), 
  list(emsum = sum(Emissions)),
  by = .(year, fips, type)
  ], aes(x = year, y = emsum, colour = fips)) + 
  geom_line(aes(group = fips)) + 
  geom_point(size = 3) + 
  ggtitle("Motor Vehicle Sources\nBaltimore City (06037) vs Los Angeles County (24510)") + 
  ylab("Emission Totals - Nonroad and Onroad")
# plot file
ggsave(filename = "./figure/plot6.png", p6, dp = 72, width = 7, height = 7)
}

# pm25: create a pm25 object --------------------- 
pm25 <- nei_dt()
nei <- get_summary(pm25)
scc <- get_code(pm25)
get_class(pm25)
contents(nei)

# set key and merge data on key
setkey(nei, SCC)
setkey(scc, SCC)
# merge data.table
map <- scc[nei]

# plot data ------------------------------------- 
nei_barplot(nei)
nei_dotchart(nei)
nei_ggplot1(nei)
nei_ggplot2(map)
nei_ggplot3(map)
nei_ggplot4(map)

#save(nei, scc, map, nei_barplot, nei_dotchart, nei_ggplot1, 
#      nei_ggplot2, nei_ggplot3, nei_ggplot4, 
#      file = "pm25.rda", compress = TRUE)

