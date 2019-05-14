#----------------------------------------------------------------------------------------
# Configurations
#----------------------------------------------------------------------------------------

g.path <- paste0 (getwd(),'/')

packages <- c('data.table', 'DT','ggplot2','shiny')
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    cat ( paste (' package ', x, '\n'))
    library(x, character.only = TRUE)
  }
})


#----------------------------------------------------------------------------------------
# Functions
#----------------------------------------------------------------------------------------
mean.r <- function (x,d=3) round ( mean (x),d)


read.data <- function () {
  
  salary   <<- data.table ( read.csv (paste0(g.path,'nba-salary.csv') ) )
  draft   <<- data.table ( read.csv (paste0(g.path,'nba-draft.csv') ) )

  
  salary [ , `:=` ( Player = as.character(Player) )]
  
  # data is inconsistent, some have id's and some do not,  use a for loop instead, sigghhhhh
  # Player.Name <- unlist ( strsplit (salary$Player,"[\\\\]|[^[:print:]]",fixed=FALSE) ) [2*(1:length(salary$Player))-1]
  # Player.Id <- unlist ( strsplit (salary$Player,"[\\\\]|[^[:print:]]",fixed=FALSE) ) [2*(1:length(salary$Player)) ]
  for ( i in 1:nrow(salary)) {
    salary [ i , Player.Name := unlist ( strsplit (salary [ i, Player] ,"[\\\\]|[^[:print:]]",fixed=FALSE) ) [1] ]
    salary [ i , Player.Id := unlist ( strsplit (salary [ i, Player] ,"[\\\\]|[^[:print:]]",fixed=FALSE) ) [2] ]
  }

  salary [ , `:=` ( Tm = as.character(Tm)
                    , Salary.2018.19 = as.numeric ( gsub('\\$|\\,' , '', Salary.2018.19 ))
                    # , Player.Name = Player.Name 
                    # , Player.Id = Player.Id 
                    , n = 1)]  
  

  
  draft [ , `:=` ( Player = as.character (Player)
                   , Tm = as.character(Tm))]

  # data is inconsistent, some have id's and some do not,  use a for loop instead, sigghhhhh
  # Player.Name <- unlist ( strsplit (draft$Player,"[\\\\]|[^[:print:]]",fixed=FALSE) ) [2*(1:length(draft$Player))-1]
  # Player.Id <- unlist ( strsplit (draft$Player,"[\\\\]|[^[:print:]]",fixed=FALSE) ) [2*(1:length(draft$Player)) ]
  for ( i in 1:nrow(draft)) {
    draft [ i , Player.Name := unlist ( strsplit (draft [ i, Player] ,"[\\\\]|[^[:print:]]",fixed=FALSE) ) [1] ]
    draft [ i , Player.Id := unlist ( strsplit (draft [ i, Player] ,"[\\\\]|[^[:print:]]",fixed=FALSE) ) [2] ]
  }
  
  
  draft [ , `:=` ( Years.Since.Draft = 2019 - Year  )]
  draft [ , `:=` ( Years.Since.Draft.cut = cut ( Years.Since.Draft , breaks = c(0,4,10,20,50))  
                   , n = 1
                   ) ]
  
}

graph.draft.ws <- function(){

  n = nrow (draft  [ Rd == 1 & ! is.na (Player.Id) ] )
  
  by.vars <- c('Rd','Pk')
  g <- draft  [ Rd == 1 & ! is.na (Player.Id) , . ( WS = round (mean ( WS,na.rm=T),1) )
                , by = by.vars ]
  
  theme_set(theme_bw())
  summary (draft $ Year  )
  summary ( draft)
  
  # Plot
  title <- 'NBA   Draft Picks  and  Win Share'
  subtitle <- 'Players Drafted 1985 thru 2018'
  caption <- 'data - basketball-reference.com'
  p <- ggplot ( data = g [Rd==1]) + 
    geom_col (  aes( x = Pk, y = WS ),colour="black", fill = 'light grey'  ) + 
    geom_text ( aes ( x = Pk, y = WS + 2, label = WS), size = 2.3) + 
    geom_text ( aes ( x = 28, y = 65 ), size = 3, label = 'n = 961') + 
    labs (title=title,  subtitle=subtitle,  caption=caption, x = 'Draft Pick (First Round)', y = 'Win Share') +
    scale_x_continuous (breaks = c(1:30)) +
    theme(text = element_text(size=12) )
  print (p)

  file <- paste0 ( g.path, 'draft.pick.win.share.jpg' )
  ggsave ( file, p, device = 'jpeg', width=8, height=4 )
  
  t.test (draft [ Pk==1, WS] , draft [ Pk==3, WS]  )
  
  return (p)  
  
}

#----------------------------------------------------------------------------------------
# Analysis
#----------------------------------------------------------------------------------------

read.data()
graph.draft.ws()

