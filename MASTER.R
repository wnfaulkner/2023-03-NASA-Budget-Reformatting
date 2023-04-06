#0000000000000000000000000000000000000000000000000000#
#    	NASA BUdget Reformatting          	           #
#0000000000000000000000000000000000000000000000000000#

# 0-SETUP --------------------------------------------------------------------------------

  #INITIAL SETUP
    rm(list=ls()) #Remove lists
    options(java.parameters = "- Xmx8g") #helps r not to fail when importing large xlsx files with xlsx package
    options(encoding = "UTF-8")
    
  # SECTION & CODE CLOCKING
    sections.all.starttime <- Sys.time()
    section0.starttime <- sections.all.starttime
    
  # ESTABLISH BASE DIRECTORIES
  
    #Figure out what machine code is running on
      computer.id <- tolower(Sys.info()[which(row.names(as.data.frame(Sys.info())) == "nodename")])
    
    #Set Working Directory and R Project Directory
      
      #Thinkpad T-16
        if(computer.id == "wnf-t16"){  
          wd <- "G:\\.shortcut-targets-by-id\\0B4-G_fB0j9HyNlY1bHNVeFZfMU0\\1. FLUX PROJECTS - CURRENT\\2023-01 Existential Risk Research\\2023-03 NASA Budget Reformatting"
        }
      
      #Thinkpad T470
        if(computer.id == ""){
          wd <- ""
        }
    
    #Check Directories
      wd <- if(!dir.exists(wd)){choose.dir()}else{wd}

    #Source Tables Directory (raw data, configs, etc.)
      #source.tables.dir <- paste0(wd, "\\1.source.data")
      #if(dir.exists(source.tables.dir)){ 
      #  print("source.tables.dir exists.")
      #}else{
      #  print("source.tables.dir DOES NOT EXIST.")
      #}
      #print(source.tables.dir)
      
      
    
  # LOAD LIBRARIES/PACKAGES
    library(wnf.utils)
    LoadCommonPackages()
    #library(xlsx)
    library(chron)
    library(haven)
    library(googledrive)
    library(microbenchmark)
  
  # SECTION CLOCKING
    section0.duration <- Sys.time() - section0.starttime
    section0.duration
    
    
# 1-IMPORT CONFIGS --------------------------------------------------------------------------------
    
  #gs4_auth(email = "william@fluxrme.com")
  
  #configs.ss <-
  #  as_sheets_id("")
  
  #sheet.names.v <- sheet_names(configs.ss)
  
  #all.configs.ls <-
  #  lapply(
  #    sheet.names.v, 
  #    function(x){
  #      read_sheet(configs.ss, sheet = x)
  #    }
  #  )
  
  #names(all.configs.ls) <- sheet.names.v
  
  #Assign each table to its own tibble object
  #  ListToTibbleObjects(all.configs.ls) #Converts list elements to separate tibble objects names with
                                        #their respective sheet names with ".tb" appended
  
  #Extract global configs from tibble as their own character objects
  #  TibbleToCharObjects(
  #    tibble = config.global.tb,
  #    object.names.colname = "config.name",
  #    object.values.colname = "config.value"
  #  )

# 1-IMPORT SOURCE DATA --------------------------------------------------------------------------------
  
  #gs4_auth(email = "william@fluxrme.com")

  #dat.import.tb <-
  #    as_sheets_id("https://docs.google.com/spreadsheets/d/e/2PACX-1vTU9FhDV4U6X4suHtvoiMLYDN-y56ipoGh-N7n9fNq7BW1PiMsx5fVlj10LsgvTYVbu3CiUDO_WD0We/pubhtml")
  
  setwd(wd)
  #list.files()
  
  import.sheetnames <- 
    excel_sheets("Historical NASA Budget Data - The Planetary Society.xlsx") %>%
    .[grepl("FY", .)]
  
  dat.ls <- list()
  
  for(i in 1:length(import.sheetnames)){
    sheetname.i <- import.sheetnames[i]
    dat.ls[[i]] <- read_xlsx("Historical NASA Budget Data - The Planetary Society.xlsx", sheet = sheetname.i)

    if(ncol(dat.ls[[i]]) == 4){
      dat.ls[[i]]$Notes <- ""
    }
  }
  
  names(dat.ls) <- import.sheetnames
  
  
# 2-CLEANING 1 & COMPILATION/STACKING --------------------------------------------------------------------------------	
  
  #Add variable for fiscal year, standardize names
    #i = 4
    for(i in 1:length(dat.ls)){
      
      dat.import.i <- dat.ls[[i]] 
      fiscal.year.i <- names(dat.ls)[i] %>% gsub("FY", "", .) %>% trimws
      
      dat.i <-
        dat.import.i %>%
        mutate(
          num.nas = apply(dat.import.i %>% select(header.1, header.2, header.3, header.4), 1, function(x){x %>% is.na %>% sum})
        ) %>%
        group_by(header.1, header.2, header.3) %>%
        mutate(min.nas = min(num.nas)) %>%
        ungroup %>%
        filter(num.nas == min.nas) %>%
        #filter(header.3 == "Technology demonstration") %>%
        group_by(header.1, header.2) %>%
        mutate(min.nas = min(num.nas)) %>%
        ungroup %>%
        filter(num.nas == min.nas) %>%
        group_by(header.1) %>%
        mutate(min.nas = min(num.nas)) %>%
        ungroup %>%
        filter(num.nas == min.nas) %>%
        filter(header.1 != "Total") %>%
        select(-num.nas, -min.nas) %>%
        mutate(fiscal.year = fiscal.year.i) 
      
      if(dat.i %>% ncol %>% equals(9)){
        names(dat.i) <- 
          c("header.1","header.2","header.3","header.4","request","appropriated","actual","notes","fiscal.year")
      }
      
      if(dat.i %>% ncol %>% equals(8)){
        names(dat.i) <- 
          c("header.1","header.2","header.3","header.4","request","appropriated","actual","fiscal.year")
        
        dat.i %<>%
          mutate(notes = "") %>%
          select(header.1,header.2,header.3,header.4,request,appropriated,actual,notes,fiscal.year)
      }
      
      dat.ls[[i]] <- dat.i
      
    }
  
  #Compile/Stack
    dat.tb <- do.call(rbind, dat.ls)

  #Test Export
    setwd(wd)
    write.csv(dat.tb, "nasa compiled budget data.csv")

#CREATE HEADER CONFIS TABLE WITH IDS SO CAN MATCH HEADERS THAT CHANGE NAMES