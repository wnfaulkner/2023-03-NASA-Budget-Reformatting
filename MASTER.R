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
    i = 1
    #for(i in 1:length(dat.ls)){
      dat.import.i <- dat.ls[[i]] 
      
      #### METHOD FOR MANUALLY MOVED HEADERS (E.G. TAB "FY 2021.2") ####
      #APPEARS TO BE WORKING TO GET RID OF SUMMED CELLS
      
      x<-dat.import.i %>%
        mutate(
          num.nas = apply(dat.import.i %>% select(header.1, header.2, header.3, header.4), 1, function(x){x %>% is.na %>% sum})
          #is.na.header.3 = is.na(header.3),
          #is.na.header.4 = is.na(header.4)
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
        filter(num.nas == min.nas)
      
      
      ####  METHOD FOR HEADER.LEVEL/HEADER.PARENT (E.G. TAB "FY 2021") ####
      #NOT WORKING YET
      
      dat.import.i$header.import <- paste(dat.import.i$header.parent, dat.import.i$header, sep = "_")
      
      dat.import.i.1 <- dat.import.i %>% filter(header.level == 1) %>% select(header)
      dat.import.i.2 <- dat.import.i %>% filter(header.level == 2) %>% select(header, header.parent)
      dat.import.i.3 <- dat.import.i %>% filter(header.level == 3) %>% select(header, header.parent)
      dat.import.i.4 <- dat.import.i %>% filter(header.level == 4) %>% select(header, header.parent)
      
      headers.i <- 
        left_join(
          dat.import.i.1,
          dat.import.i.2,
          by = c("header" = "header.parent"),
          multiple = "all"
        ) %>%
        left_join(
          ., 
          dat.import.i.3,
          by = c("header.y" = "header.parent"),
          multiple = "all"
        ) %>%
        left_join(
          ., 
          dat.import.i.4,
          by = c("header.y.y" = "header.parent"),
          multiple = "all"
        ) 
      
      names(headers.i) <- c("header.1","header.2","header.3","header.4")
      
      headers.i$header.import <- 
        paste(headers.i$header.1, headers.i$header.2, headers.i$header.3, headers.i$header.4, sep = "_") %>% 
        sapply(
          ., 
          function(x){
            strsplit(x, "_")
          }
        ) %>%
        lapply(
          ., 
          function(x){
            x[x != "NA"]
          }
        ) %>%
        lapply(
          ., 
          function(x){
            ifelse(length(x) == 1, x, paste(x[length(x)-1], x[length(x)], sep = "_"))   
          }
        ) %>%
        UnlistVector()
      
      dat.i <-
        left_join(
          headers.i,
          dat.import.i,
          by = "header.import"
        )
  
      dat.i$i <- names(dat.ls)[i] %>% tolower %>% gsub("fy","",.) %>% trimws
      
      names(dat.i) <- c("header","request","appropriation","actual","notes","fiscal.year")
      
      
      dat.ls[[i]] <- dat.i
      
    }
  
  #Compile/Stack
    dat.tb <- do.call(rbind, dat.ls)
    
  
# 3-COMPILATION/STACKING --------------------------------------------------------------------------------	
  
  #Stack/join
  # have to find a way to designate larger categories and subcategories (done with formatting in original spreadsheet) - config table?
  
  #Clean Names
    dat.ls %<>%
      lapply(
        ., 
        function(x){
          names(x) <- c("header","request","appropriation","actual","notes","fiscal.year")
        }
      )
  
  
  #Stack past and current semester data into single table
  	byscore.tb <-
  	  rbind(
  	    byscore.prev.tb %>% select(-student.id), 
  	    byscore.current.tb
	    )
  	
	#Name columns
  	names(byscore.tb) <- c(
  	            "file.name",
  							"sheet.num",
  							"sheet.name",
  							"school",
  							"teaching.artist",
  							"grade",
  							"coteacher",
  							"arts.non.arts.connection",
  							"skill.indicator",
  							"date.schyr",
  							"date.calyr",
  							"date.semester",
  							"date.schyr.semester",
  							"date.calyr.semester",
  							"class.size",
  							"date.of.score",
  							"date.of.score.column.order",
  							"student.name",
  							#"student.id",
  							"score")

# 3-CLEANING --------------------------------------------------------------------------------	
	
	#Check date variable for dates that aren't either blank or in proper format (should result in tibble with zero rows if everything correct)
	 #byscore.tb %>% 
	 #   filter(byscore.tb$date.of.score %>% substr(., 1, 2) %>% is_in(c(NA, "42","43", "44")) %>% not) %>% 
	 #  select(date.schyr.semester, sheet.name,date.of.score) %>% 
	 #   unique  
	  
  #Lower-case all character vars
  	  #byscore.tb %<>%
  	    #apply(., c(1,2), function(x){str_replace_all(x, "[^[:alnum:]]", "")}) %>%
  	    #LowerCaseCharVars() %>%
  	    #as_tibble()
  	  
  #Cleaning up some character variables
  	
	  byscore.tb$skill.indicator <- 
  	    gsub("\\n"," ",byscore.tb$skill.indicator) %>% #to remove carriage returns	
  	    tolower %>%
  	    trimws
	  byscore.tb$arts.non.arts.connection <- gsub(",","",byscore.tb$arts.non.arts.connection) #to remove commas (seems to be creating problems with csv)	
	  byscore.tb$arts.non.arts.connection <- gsub("&","and",byscore.tb$arts.non.arts.connection) #to standardize ampersands
  
  #Standardizing 'grade' variable
	  byscore.tb$grade %<>% tolower
	  byscore.tb <- 
	    left_join(byscore.tb, grades.tb, by = c("grade" = "grade.raw")) %>%
	    select(-grade)
	  
  #Standardizing 'teaching.artist' variable
	  byscore.tb$teaching.artist %<>% tolower
	  byscore.tb <- 
	    left_join(byscore.tb, teaching.artists.tb, by = c("teaching.artist" = "teaching.artist.raw")) %>%
	    select(-teaching.artist)

  #Convert Score Date to Date Format
	  byscore.tb$date.of.score %<>% as.numeric %>% as.Date(., origin = "1970-01-01") 
	  
  #Correct school year, calendar year, and semester variables
	  byscore.tb$date.calyr[!is.na(byscore.tb$date.of.score)] <-  byscore.tb$date.of.score[!is.na(byscore.tb$date.of.score)] %>% year
	  byscore.tb$date.semester[!is.na(byscore.tb$date.of.score)] <-  
	    ifelse(byscore.tb$date.of.score[!is.na(byscore.tb$date.of.score)] %>% month %>% is_less_than(7), "spring", "fall")
	
  #Final row filtering and column ordering
	  
	  byscore.tb <- byscore.tb[!is.na(byscore.tb$score),] #Remove any lines with no score
	  byscore.tb <- byscore.tb[,order(names(byscore.tb))] #Sort columns alphabetically by name
	  
# 4-CREATE NEW USEFUL VARIABLES --------------------------------------------------------------------------------
	
	#Student ID
	  byscore.tb %<>%
	    mutate(
	      student.id = 
	        paste(
    	      gsub(" ", "", gsub("[[:punct:]]","",tolower(student.name))), 
    	      gsub(" ", "", gsub("[[:punct:]]","",tolower(school))),
    	      tolower(gsub(" ", "", date.schyr.semester)),
    	      sep = "."
  	      )
	    )
	  
	#Row (Score) ID
    byscore.tb <- byscore.tb[with(byscore.tb, order(date.schyr.semester,student.id,skill.indicator,date.of.score.column.order)),]	
  	byscore.tb$score.id <- 1:nrow(byscore.tb)
  
	#Classroom ID
  	byscore.tb %<>% 
  	  mutate(classroom.id = paste(date.schyr.semester, school, grade.standardized, teaching.artist.id, coteacher, sep = "."))
	
  #Score.firstlast variable
  	byscore.tb$combo <- 
  	  paste(
    	  data.table::shift(byscore.tb$date.of.score.column.order, n=1, fill=NA, type = "lag"),
    	  byscore.tb$date.of.score.column.order,
    	  data.table::shift(byscore.tb$date.of.score.column.order, n=1, fill=NA, type = "lead"),
    	  sep=""
    	) %>%
    	as.character
  	
    byscore.tb <- 
      left_join(
        byscore.tb, 
        firstlast.permutations.tb %>% select(permutation.combo, permutation.text), 
        by = c("combo" = "permutation.combo")
      ) %>%
      ReplaceNames(., "permutation.text", "score.firstlast") %>%
      mutate(score.firstlast = score.firstlast %>% tolower)
    
    byscore.tb$score.firstlast[byscore.tb$score.id == 1] <- "first"
    byscore.tb$score.firstlast[byscore.tb$score.id == length(byscore.tb$score.firstlast)] <- "last"

	#Outcome Area Variables - if in outcomes table, will match outcome area, otherwise assigns to academic (#! Check if reasonable with pivot table)
		skill.indicators.tb %<>%
		  left_join(
		    ., 
		    objectives.tb,
		    by = c("objective.id")
		  ) %>%
		  LowerCaseCharVars()
		
    byscore.tb %<>%
		  left_join(	
		    ., 
				skill.indicators.tb, 
				by = c("skill.indicator","date.schyr.semester"),
		    multiple = "all"
			) 
		byscore.tb$objective.area.num[which(is.na(byscore.tb$objective.area.num))] <- 6
		byscore.tb$objective.area.abrev[which(byscore.tb$objective.area.num == 6)] <- "ac"
		byscore.tb$objective.area.title[which(byscore.tb$objective.area.num == 6)] <- "academic"
		
	#Proficiency character & numerical dummies
		byscore.tb$proficient.text <- ifelse(byscore.tb$score >= 3, "proficient", "not proficient")
		byscore.tb$proficient.binary <- ifelse(byscore.tb$score >= 3, 1, 0)
	
	#Mastery character & numerical dummies
		byscore.tb$mastery.text <- ifelse(byscore.tb$score >= 5, "mastered", "not mastered")
		byscore.tb$mastery.binary <- ifelse(byscore.tb$score >= 5, 1, 0)
		
	#All names of byscore.tb except row.id
		longdata.names.nochange <- names(byscore.tb)[names(byscore.tb) != "row.id"]
		

# 5-CREATE BY-STUDENT-SKILL TABLE --------------------------------------------------------------------------------  
  	
  #Date-dependent Variables
  	datevars.source.tb <- 
  	  byscore.tb %>% 
  	  select(score.id, skill.indicator, date.schyr.semester, student.id, date.of.score, score, sheet.name, score.firstlast) %>%
  	  arrange(., date.schyr.semester, sheet.name, student.id, skill.indicator, date.of.score) %>%
  	  LowerCaseCharVars()
    
		datevars.firstonly.tb <-
		  datevars.source.tb %>%
		  filter(score.firstlast == "first/only") %>%
		  mutate( #new date-dependent variables
  			score.beg = score %>% as.numeric,
  			score.end = NA
  		) %>%
  		mutate(
  			date.firstlast.diff = NA,
  			growth = NA,
  			growth.pct = NA,
  			growth.twds.mastery = NA
  		) %>%
      select(
  	    c(
  	      "score.id",
  	      names(.)[!(names(.) %in% names(byscore.tb))]
  	    )
  	  )
		  
  	datevars.mult.tb <- 
  	  datevars.source.tb %>%
  	  #filter(grepl("travius", student.id)) %>%
  	  inner_join(datevars.source.tb, by=c("skill.indicator", "date.schyr.semester", "student.id", "sheet.name"), multiple = "all") %>%
  	  inner_join(datevars.source.tb, by=c("skill.indicator", "date.schyr.semester", "student.id", "sheet.name"), multiple = "all") %>%
  	  filter(score.firstlast == "first" & score.firstlast.y == "last") %>%
  	  mutate( #new date-dependent variables
  			score.beg = score %>% as.numeric,
  			score.end = score.y %>% as.numeric
  		) %>%
  		mutate(
  			date.firstlast.diff = as.numeric(date.of.score.y) - as.numeric(date.of.score),
  			growth = score.end - score.beg,
  			growth.pct = ifelse(score.beg == 0, 100,100*(score.end - score.beg)/score.beg),
  			growth.twds.mastery = 100*ifelse(score.beg == 5, 0, growth/(5-score.beg))
  		) %>%
  	  filter(!duplicated(score.id)) %>%
      select(names(.)[!grepl("\\.x|\\.y", names(.))]) %>%
  	  select(
  	    c(
  	      "score.id",
  	      names(.)[!(names(.) %in% names(byscore.tb))]
  	    )
  	  )
  	
  	datevars.tb <- 
  	  rbind(datevars.firstonly.tb, datevars.mult.tb) %>%
  	  arrange(score.id)
  	
	#Create By-Student-Skill table
  	bystudentskill.tb <- 
  	  left_join(datevars.tb, byscore.tb, by = "score.id", multiple = "all") %>%
  	  filter(!duplicated(score.id))

# 6-ADD USEFUL VARIABLES TO BY-STUDENT-SKILL TABLE --------------------------------------------------------------------------------    	
	
	#Student-Level Variables: growth in at least one skill, growth in all skills
  	bystudentskill.tb %<>%
  	  mutate(growth.binary = ifelse(growth %>% is_greater_than(0), 1, 0))  %>%
  	  group_by(across(all_of(c("date.schyr.semester", "student.id")))) %>%
  	  mutate(
  	    growth.any.skills = ifelse(growth.binary %>% sum %>% is_greater_than(0), 1, 0),
  	    growth.all.skills = ifelse(growth.binary %>% sum %>% equals(3), 1, 0)
  	  ) %>%
  	  ungroup
  	
	#Student-Level Variables: growth in at least one skill, growth in all skills
  	#bystudent.tb %<>%
  	#  mutate(growth.binary = ifelse(growth %>% is_greater_than(0), 1, 0)) %>%
  	#  group_by(across(all_of(c("date.schyr.semester", "student.id")))) %>%
  	#  mutate(
  	#    growth.any.skills = ifelse(growth.binary %>% any, 1, 0),
  	#    growth.all.skills = ifelse(growth.binary %>% all, 1, 0)
  	#  ) %>%
  	#  ungroup

# 7-CREATE BY-STUDENT TABLE --------------------------------------------------------------------------------
  
  bystudent.tb <-
	  bystudentskill.tb %>%
	  select(
	    student.id,
	    date.schyr,
	    date.semester,
	    date.schyr.semester,
	    file.name,
	    sheet.name,
	    classroom.id,
	    class.size,
	    coteacher,
	    teaching.artist.id,
	    grade.standardized,
	    school,
	    growth.any.skills,
	    growth.all.skills
	  ) %>%
	  filter(!duplicated(.))
  
# 8-ORGANIZE FINAL TABLES FOR EXPORT --------------------------------------------------------------------------------
	
	bystudentskill.tb %<>%
  	select(
  	  -c(
  	    "score.id",
  	    "score",
  	    "combo",
  	    "score.firstlast",
  	    "date.of.score",
  	    "date.of.score.column.order",
  	    "growth.all.skills",
  	    "growth.any.skills"
  	  )
  	) %>%
  	.[,order(names(.))] %>%
  	MoveColsLeft(., c("student.id","skill.indicator")) %>%
  	arrange(student.id,skill.indicator) %>%
  	as_tibble()
  	
	byscore.tb %<>% 
	  select(
	    -combo, -date.of.score.column.order, -sheet.num #Remove extraneous columns
    ) %>%
	  .[,order(names(.))] %>%
	  MoveColsLeft(., c("score.id","skill.indicator")) %>%
	  arrange(student.id, skill.indicator) %>%
	  filter(!duplicated(.)) %>%
	  as_tibble()

# 9-EXPORT --------------------------------------------------------------------------------
	
	#Output Directory
      output.dir <- 
        paste0(
          wd, 
          "\\2.outputs\\", 
           gsub(":| ",".",Sys.time())
        )
    
	#Defining Export File Names with Date-Time included
  	export.file.name.csv <- 	
			paste(
				#min(unique(bystudent.tb$date.calyr)),
				#"-",
				#max(unique(bystudent.tb$date.calyr)),
				#"_",
				Sys.time(), 
				".csv", 
				sep=""
			) %>%
  	  gsub(":","-",.)
  
  #Store Files	
    if(!dir.exists(output.dir)){dir.create(output.dir, recursive = TRUE)}
  	setwd(output.dir)
	
	#CSV
  	write.csv(	#by score data
  	  bystudent.tb,			
			file = paste("by.student_",export.file.name.csv, sep = ""),
			row.names = FALSE
		)
  	
  	write.csv(	#by student data (with growth vars)
  	  bystudentskill.tb,			
  	  file = paste("by.student.skill_",export.file.name.csv, sep = ""),
  	  row.names = FALSE
  	)
  	
  	write.csv(	#by student data (with growth vars)
  	  byscore.tb,			
			file = paste("by.score_",export.file.name.csv, sep = ""),
			row.names = FALSE
		)
  	
  #Excel
  	#export.ls <- 
  	#  list(
    #	  if(exists("byscore.tb")){byscore.tb}else{NA},
    #	  if(exists("bystudentskill.tb")){bystudentskill.tb}else{NA},
  	#    if(exists("bystudent.tb")){bystudent.tb}else{NA}
    #	)
  	
  	#for(i in 1:length(export.ls)){
  	#  append.i <- ifelse(i == 1, TRUE, FALSE)
  	#  write.xl
  	#}
  	  
  	
  	getwd()
  	list.files()
  	Sys.time()-sections.all.starttime
  	windows()
  	