# --------------------------------------------------
# importMTCdata.R
# 
# This script takes input from MTC's travel model and outputs various aggregations (vmt, mean travel costs 
# and times by trip and traveler type, etc.). Specifically, we use the scenarios developed for the Plan Bay Area 
# DEIR. There are five scenarios for 2020 and 2040 as well as one base year 2010 scenario. Here, we build results
# databases aimed at determining aggregate VMT at the household and TAZ levels, producing tables ready to be 
# linked to GIS.
#
# Big thanks to Anthony Damico for providing the inspiration to use MonetDB.
# More info available at: https://github.com/ajdamico/usgsd
# --------------------------------------------------

# Load required packages
require(MonetDB.R)

# Uncomment and set your working directory
# setwd("C:/data ... ")

# ---------------------------------------------------
# Create the database (these commands only need to be executed once)
# ---------------------------------------------------

# ONLY RUN ONCE: create a monetdb executable (.bat) file for the MTC travel data
batfile <-
	monetdb.server.setup(
		
		# Set the path to the directory where the initialization batch file and all data will be stored
		database.directory = paste0( getwd() , "/MonetDB" ),
		# must be empty or not exist
		
		# Set the main path to the monetdb installation program
		monetdb.program.path = "C:/Program Files/MonetDB/MonetDB5",
		
		# choose a database name
		dbname = "MTCDEIR",
		
		# choose a database port
		# this port should not conflict with other monetdb databases
		# on your local computer.  two databases with the same port number
		# cannot be accessed at the same time
		dbport = 55000
	)


# The variable 'batfile' now contains the path the the MonetDB startup script you just created.
# Make sure you note that location. You'll need it if you plan to run this script more than once.

# ---------------------------------------------------
# Connect to database
# ---------------------------------------------------

# Set this variable to the location of the batfile generated above and uncommen.
# batfile <- "C:/data/MTCDEIR..."
pid <- monetdb.server.start(batfile)
dbname <- "MTCDEIR" 
dbport <- 55000
drv <- dbDriver("MonetDB")
monet.url <- paste0("monetdb://localhost:", dbport, "/", dbname)
db <- dbConnect(drv, monet.url, "monetdb", "monetdb")

# ---------------------------------------------------
# Disconnect from database
# ---------------------------------------------------

# Use these commands to disconnect from the database as needed.
# dbDisconnect(db)
# monetdb.server.stop(pid)

# -------------------------------------------------------
# Variable, path, and scenario definitions
# -------------------------------------------------------

# This section defines the names and locations of all travel model outputs so that later functions
# can be automated.

# 'cpath' is a function that takes a scenario name and appends it to the base directory. 
# Within the base, there should be one directory for each scenario that you would
# like to enter into the database. Uncomment the line below and input your own base directory
# (i.e. replace "C:/data/From MTC/ ..." with the directory that holds your travel model files.)
# cpath <- function(x) { paste0("C:/data/From MTC/ ...", x) }

# MTC's travel model uses five time periods
times <- c("EA", "AM", "MD", "PM", "EV")

# MTC's travel model skims costs, distances, and travel times
types <- c("Cost", "Distance", "Time")

# Tours have an 'outbound' and 'inbound' direction
direction <- c("out", "in")

# Now we'll generate dataframes containing the full path to each travel model output of interest
# for each individual scenario. Each data frame has two columns - the first (name) lists the name
# of the scenario. The second (path) lists the full path to the file. 

# For the data anlyzed here there were five scenarios of interest: 2010, Proposed Pan Bay Area (2020, 2040),
# and the EEJ scenario (2020, 2040). 

# Edit the list of scenarios for individual travel, joint travel, skims, and synthetic populations as required

# Trips
trips <- data.frame(names = c("2010", "2020_propplan", "2020_EEJ", "2040_propplan", "2040_EEJ"),
	paths = c(
		cpath("2010/2010_03_YYY/main/indivTripData_3.csv"),
		cpath("2020/02 Proposed Plan (2020_03_102)/main/indivTripData_3.csv"),
		cpath("2020/05 EEJ (2020_03_105)/main/indivTripData_3.csv"),
		cpath("2040/02 Proposed Plan (2040_03_079)/main/indivTripData_3.csv"),
		cpath("2040/05 EEJ (2040_03_082)/main/indivTripData_3.csv")))

# Joint trips
tripsJ <- data.frame(names = c("2010", "2020_propplan", "2020_EEJ", "2040_propplan", "2040_EEJ"),
	paths = c(
		cpath("2010/2010_03_YYY/main/jointTripData_3.csv"),
		cpath("2020/02 Proposed Plan (2020_03_102)/main/jointTripData_3.csv"),
		cpath("2020/05 EEJ (2020_03_105)/main/jointTripData_3.csv"),
		cpath("2040/02 Proposed Plan (2040_03_079)/main/jointTripData_3.csv"),
		cpath("2040/05 EEJ (2040_03_082)/main/jointTripData_3.csv")))

# Tours
tours <- data.frame(names = c("2010", "2020_propplan", "2020_EEJ", "2040_propplan", "2040_EEJ"),
	paths = c(
		cpath("2010/2010_03_YYY/main/indivTourData_3.csv"),
		cpath("2020/02 Proposed Plan (2020_03_102)/main/indivTourData_3.csv"),
		cpath("2020/05 EEJ (2020_03_105)/main/indivTourData_3.csv"),
		cpath("2040/02 Proposed Plan (2040_03_079)/main/indivTourData_3.csv"),
		cpath("2040/05 EEJ (2040_03_082)/main/indivTourData_3.csv")))

# Joint tours
toursJ <- data.frame(names = c("2010", "2020_propplan", "2020_EEJ", "2040_propplan", "2040_EEJ"),
	paths = c(
		cpath("2010/2010_03_YYY/main/jointTourData_3.csv"),
		cpath("2020/02 Proposed Plan (2020_03_102)/main/jointTourData_3.csv"),
		cpath("2020/05 EEJ (2020_03_105)/main/jointTourData_3.csv"),
		cpath("2040/02 Proposed Plan (2040_03_079)/main/jointTourData_3.csv"),
		cpath("2040/05 EEJ (2040_03_082)/main/jointTourData_3.csv")))

# Skims
skims <- data.frame(names = c("skims_2010", "skims_2020_propplan", "skims_2020_eej", "skims_2040_propplan",
	"skims_2040_eej"),
	paths = c(
		cpath("2010/2010_03_YYY/database/"),
		cpath("2020/02 Proposed Plan (2020_03_102)/database/"),
		cpath("2020/05 EEJ (2020_03_105)/database/"),
		cpath("2040/02 Proposed Plan (2040_03_079)/database/"),
		cpath("2040/05 EEJ (2040_03_082)/database/")))

# TAZ properties
tazs <- data.frame(names=c("2010","2020_propplan","2020_EEJ","2040_propplan","2040_EEJ"),
	paths = c(
		cpath("2010/2010_03_YYY/landuse/tazData.csv"),
		cpath("2020/02 Proposed Plan (2020_03_102)/landuse/tazData.csv"),
		cpath("2020/05 EEJ (2020_03_105)/landuse/tazData.csv"),
		cpath("2040/02 Proposed Plan (2040_03_079)/landuse/tazData.csv"),
		cpath("2040/05 EEJ (2040_03_082)/landuse/tazData.csv")))

# Synthetic population
# Uses a 'token' in the path that will be substituted later
synpops <- data.frame(names=c("2010","2020_propplan","2020_EEJ","2040_propplan","2040_EEJ"),
	paths = c(
		cpath("2010/2010_03_YYY/INPUT/popsyn/token.p2011s3a.2010.csv"),
		cpath("2020/02 Proposed Plan (2020_03_102)/INPUT/popsyn/token.p2011s6d.2020.csv"),
		cpath("2020/05 EEJ (2020_03_105)/INPUT/popsyn/token.p2011s50b.2020.csv"),
		cpath("2040/02 Proposed Plan (2040_03_079)/INPUT/popsyn/token.p2011s6d.2040.csv"),
		cpath("2040/05 EEJ (2040_03_082)/INPUT/popsyn/token.p2011s50b.2040.csv")))

# Modeled characteristics for the synthetic population
modeledpops <- data.frame(names=c("2010","2020_propplan","2020_EEJ","2040_propplan","2040_EEJ"),
	paths = c(
		cpath("2010/2010_03_YYY/main/tokenData_3.csv"),
		cpath("2020/02 Proposed Plan (2020_03_102)/main/tokenData_3.csv"),
		cpath("2020/05 EEJ (2020_03_105)/main/tokenData_3.csv"),
		cpath("2040/02 Proposed Plan (2040_03_079)/main/tokenData_3.csv"),
		cpath("2040/05 EEJ (2040_03_082)/main/tokenData_3.csv")))

# Usual work and school location choice results
wslocations <- data.frame(names=c("2010","2020_propplan","2020_EEJ","2040_propplan","2040_EEJ"),
	paths = c(
		cpath("2010/2010_03_YYY/main/wsLocResults_3.csv"),
		cpath("2020/02 Proposed Plan (2020_03_102)/main/wsLocResults_3.csv"),
		cpath("2020/05 EEJ (2020_03_105)/main/wsLocResults_3.csv"),
		cpath("2040/02 Proposed Plan (2040_03_079)/main/wsLocResults_3.csv"),
		cpath("2040/05 EEJ (2040_03_082)/main/wsLocResults_3.csv")))

#-------------------------------------------------------
# Read individual trip lists into the database
#-------------------------------------------------------

for(i in trips$paths) {
	
	# Quickly figure out the number of lines in the data file
	chunk_size <- 1000
	testcon <- file(i ,open = "r")
	nooflines <- 0
	while((linesread <- length(readLines(testcon, chunk_size))) > 0)
		nooflines <- nooflines + linesread
	close(testcon)
		
	# Read each .csv into the database and name accordingly
	monet.read.csv(db, i, paste0("trips_", scenarios[which(scenarios$paths==i), "names"]), nooflines, locked=TRUE)
	
	}

# Add a column to all individual trip tables that contains the "number of participants" 
# This will be used later to adjust VMT.
for(i in scenarios$names) {
	
	dbSendUpdate(db, paste0("ALTER TABLE trips_", i, " ADD COLUMN num_participants float"))
	dbSendUpdate(db, paste0("UPDATE trips_", i, " SET num_participants = 
		(CASE trip_mode 
		WHEN 3 THEN 2 
		WHEN 4 THEN 2 
		when 5 THEN 3.25 
		when 6 THEN 3.25 
		ELSE 1 
		END)"))

	}

# ------------------------------------------------------
# Read joint trip lists into the database and add them to the individual trip lists
# ------------------------------------------------------

for(i in tripsJ$paths) {
	
	# quickly figure out the number of lines in the data file
	chunk_size <- 1000
	testcon <- file(i ,open = "r")
	nooflines <- 0
	while((linesread <- length(readLines(testcon , chunk_size))) > 0)
		nooflines <- nooflines + linesread
	close(testcon)
	
	# Read each .csv into the database and name accordingly
	monet.read.csv(db, i, paste0("jointTrips_", scenariosJ[which(scenariosJ$paths==i), "names"]), nooflines, 
		locked=TRUE)

}

# Insert joint records into the individual trip lists, selecting only the common columns 
# TODO: Edit to not have to manually select common columns.
for(i in 1 : length(trips$names)) {
	
	dbSendUpdate(db, paste0("INSERT INTO trips_", paste0(scenarios$names[i]), " 
		(hh_id, tour_id, stop_id, inbound, tour_purpose, orig_purpose, dest_purpose, orig_taz, orig_walk_segment, 
		dest_taz, dest_walk_segment, parking_taz, depart_hour, trip_mode, tour_mode, tour_category, num_participants) 
		SELECT hh_id, tour_id, stop_id, inbound, tour_purpose, orig_purpose, dest_purpose, orig_taz, orig_walk_segment, 
		dest_taz, dest_walk_segment, parking_taz, depart_hour, trip_mode, tour_mode, tour_category, num_participants 
		FROM jointtrips_", paste0(scenariosJ$names[i])))

}

#-------------------------------------------------------
# Add a primary key to the trip lists
# ------------------------------------------------------
for(i in trips$names) {
	
	dbSendUpdate(db,paste0("ALTER TABLE trips_", i, " ADD COLUMN idkey int auto_increment" ))
	dbSendUpdate(db,paste0("ALTER TABLE trips_", i, " ADD PRIMARY KEY (idkey)"))

}

#-------------------------------------------------------
# --- TOUR DATA ---
#-------------------------------------------------------

#-------------------------------------------------------
# Read individual TOUR lists into the database
#-------------------------------------------------------

for(i in tours$paths) {
	
		# quickly figure out the number of lines in the data file
		chunk_size <- 1000
		testcon <- file(i ,open = "r")
		nooflines <- 0
		(while((linesread <- length( readLines(testcon , chunk_size))) > 0)
			nooflines <- nooflines + linesread)
		close(testcon)
		
		monet.read.csv(db, i, paste0("tours_",tours[which(tours$paths == i),"names"]), nooflines, locked=TRUE)
}

# Add a column to all individual TOUR tables that contains the "number of participants".
# This will be used later to adjust VMT.
for(i in tours$names) {
	
	dbSendUpdate(db, paste0("ALTER TABLE tours_", i, " ADD COLUMN num_participants float"))
	dbSendUpdate(db, paste0("UPDATE tours_", i, " SET num_participants = 
		(CASE tour_mode 
		WHEN 3 THEN 2 
		WHEN 4 THEN 2 
		WHEN 5 THEN 3.25 
		WHEN 6 THEN 3.25
		ELSE 1 
		END)"))

}

# ------------------------------------------------------
# Read joint TOUR lists into the database and add them to the individual TOUR lists
# ------------------------------------------------------

for(i in toursJ$paths) {
	
	# quickly figure out the number of lines in the data file
	chunk_size <- 1000
	testcon <- file(i ,open = "r")
	nooflines <- 0
	(while((linesread <- length(readLines(testcon , chunk_size))) > 0)
		nooflines <- nooflines + linesread)
	close(testcon)
	
	monet.read.csv(db, i, paste0("jointTours_", toursJ[which(toursJ$paths == i), "names"]), nooflines, locked=TRUE)
}

# insert joint records into the individual TOUR lists, selecting only the common columns from the joint table
for(i in 1 : length(tours$names)) {
	
	dbSendUpdate(db, paste0("insert into tours_", paste0(tours$names[i]), " 
		(hh_id, tour_id, tour_category, tour_purpose, orig_taz, orig_walk_segment, dest_taz, dest_walk_segment, 
		start_hour, end_hour, tour_mode, num_ob_stops, num_ib_stops) 
		SELECT hh_id, tour_id, tour_category, tour_purpose, orig_taz, orig_walk_segment, dest_taz, 
		dest_walk_segment, start_hour, end_hour, tour_mode, num_ob_stops, num_ib_stops 
		FROM jointtours_", paste0(toursJ$names[i])))

}

#-------------------------------------------------------
# Add a primary key to the TOUR lists
# ------------------------------------------------------
for(i in tours$names) {
	
	dbSendUpdate(db,paste0("ALTER TABLE tours_", i, " ADD COLUMN idkey int auto_increment" ))
	dbSendUpdate(db,paste0("ALTER TABLE tours_", i, " ADD PRIMARY KEY (idkey)"))

}

#-------------------------------------------------------
# --- END TOUR DATA ---
#-------------------------------------------------------


# ------------------------------------------------------
# Read skims into the database
# ------------------------------------------------------

# distance = D, time = T, cost = C

# each skim file contains 1454^2 (# of TAZs squared) + 1 (for the header row) lines
nooflines <- 1454^2 + 1

for(i in skims$paths) {
	for(j in times) {
		for(k in types) {
	
		# Create the full path to the skim file, specific to time period and type
		fn <- paste0(i, k, "SkimsDatabase", j, ".csv")
			
		# Read the file into the database and name accordingly
		monet.read.csv(db, fn, paste0(k, "_", skims[which(skims$paths==i), "names"], "_", j), nooflines, locked = TRUE)
			
		}
	}
}

# ------------------------------------------------------
# End skim data
# ------------------------------------------------------


# ---------------------------------------------
# Match TRIPS to skims and update TRIP tables
# Outcome: Trip tables contain new columns containing the time, distance, and cost of each trip
# ---------------------------------------------

# Create a new table for each scenario that will contain the matching skim for each trip in the trip list.
# This table will initially have no data, and we'll fill it with an 'insert into' query
# in the next step. This makes it easier to deal with the five different time periods used by the travel model.
for(i in scenarios$names) {
	for(j in types) {
	
	ifelse(j == "Distance",
	dbSendUpdate(db, paste0(
		"CREATE TABLE matched_skims_", i, "_", j," 
		AS SELECT idkey, orig_taz, dest_taz, depart_hour, trip_mode, inbound, da, daToll, s2, s2toll, s3, s3toll, 
		walk, bike 
		FROM trips_", i, " t1 
		INNER JOIN ", j, "_skims_",i ,"_ea t2 
		ON t1.orig_taz = t2.orig AND t1.dest_taz = t2.dest WITH NO DATA")),
	
	ifelse(j == "Cost",	
	dbSendUpdate(db, paste0(
		"CREATE TABLE matched_skims_", i, "_", j, " 
		AS SELECT idkey, orig_taz, dest_taz, depart_hour, trip_mode, inbound, da, daToll, s2, s2toll, s3, s3toll, 
		wtrnw, dtrnw, wtrnd 
		FROM trips_", i, " t1 
		INNER JOIN ", j, "_skims_", i , "_ea t2 
		ON t1.orig_taz = t2.orig AND t1.dest_taz = t2.dest WITH NO DATA")),
	
	# Time
	dbSendUpdate(db, paste0(
		"CREATE TABLE matched_skims_", i, "_", j," 
		AS SELECT idkey, orig_taz, dest_taz, depart_hour, trip_mode, inbound, da, daToll, s2, s2toll, s3, s3toll, 
		walk, bike, wtrnw, dtrnw, wtrnd 
		FROM trips_", i, " t1 
		INNER join ", j, "_skims_", i, "_ea t2 
		ON t1.orig_taz = t2.orig AND t1.dest_taz = t2.dest WITH NO DATA"))			
	)) # ifelse statements
		
	}
}

# Loop through the skim tables for the five time periods and join to the appropriate trip records 
for(i in scenarios$names) {
	for(j in types) {
		for(k in times) {
	
		# define a time-of-day flag so that we join the correct skims
		depart_hours <- ifelse(k == "EA", "3,4,5",
										ifelse(k == "AM", "6,7,8,9",
										ifelse(k == "MD", "10,11,12,13,14",
										ifelse(k == "PM", "15,16,17,18","1,2,19,20,21,22,23,24"))))
		
		ifelse(j == "Distance",
		dbSendUpdate(db, paste0("INSERT INTO matched_skims_", i, "_", j, " 
			SELECT idkey, orig_taz, dest_taz, depart_hour, trip_mode, inbound, da, daToll, s2, s2toll, s3, s3toll, 
			walk, bike 
			FROM trips_", i," t1 
			INNER JOIN ",j, "_skims_", i, "_", k, " t2 
			ON t1.orig_taz = t2.orig AND t1.dest_taz = t2.dest 
			WHERE depart_hour IN (", depart_hours, ")")),
			
		ifelse(j == "Cost",
		dbSendUpdate(db, paste0("INSERT INTO matched_skims_", i, "_", j, " 
			select idkey, orig_taz, dest_taz, depart_hour, trip_mode, inbound, da, daToll, s2, s2toll, s3, s3toll, 
			wtrnw, dtrnw, wtrnd 
			FROM trips_", i," t1 
			INNER JOIN ",j, "_skims_", i,"_", k," t2 
			ON t1.orig_taz = t2.orig AND t1.dest_taz = t2.dest 
			WHERE depart_hour IN (", depart_hours, ")")),
		
		# Time
		dbSendUpdate(db, paste0("INSERT INTO matched_skims_", i, "_", j, " 
			SELECT idkey, orig_taz, dest_taz, depart_hour, trip_mode, inbound, da, daToll, s2, s2toll, s3, s3toll, 
			walk, bike, wtrnw, dtrnw, wtrnd 
			FROM trips_", i," t1 
			INNER JOIN ",j, "_skims_", i,"_", k," t2 
			ON t1.orig_taz = t2.orig AND t1.dest_taz = t2.dest 
			WHERE depart_hour IN (", depart_hours, ")"))
		)) #ifelse statements
			
		}
	}
}

# create columns in the matched skim tables and trip tables to store the skim results
for(i in scenarios$names) {
	for(j in types) {
	
	# Add columns in the matched table and trip tables to store the matched skim results
	dbSendUpdate(db, paste0("ALTER TABLE matched_skims_", i, "_", j," ADD COLUMN matched float"))
		
	# these updates are the same regardless of skim type
	dbSendUpdate(db, paste0("UPDATE matched_skims_", i, "_", j," SET matched = da WHERE trip_mode = 1"))
	dbSendUpdate(db, paste0("UPDATE matched_skims_", i, "_", j," SET matched = daToll WHERE trip_mode = 2"))
	dbSendUpdate(db, paste0("UPDATE matched_skims_", i, "_", j," SET matched = s2 WHERE trip_mode = 3"))
	dbSendUpdate(db, paste0("UPDATE matched_skims_", i, "_", j," SET matched = s2Toll WHERE trip_mode = 4"))
	dbSendUpdate(db, paste0("UPDATE matched_skims_", i, "_", j," SET matched = s3 WHERE trip_mode = 5"))
	dbSendUpdate(db, paste0("UPDATE matched_skims_", i, "_", j," SET matched = s3Toll WHERE trip_mode = 6"))
		
	ifelse(j == "Distance", {
		dbSendUpdate(db, paste0("UPDATE matched_skims_", i, "_", j," SET matched = walk WHERE trip_mode = 7"))
		dbSendUpdate(db, paste0("UPDATE matched_skims_", i, "_", j," SET matched = bike WHERE trip_mode = 8"))
		# Approximate transit distance using drive alone distance
		dbSendUpdate(db, paste0("UPDATE matched_skims_", i, "_", j," SET matched = da WHERE trip_mode > 8")) },
		
	ifelse(j == "Cost", {
		dbSendUpdate(db, paste0("UPDATE matched_skims_", i, "_", j," SET matched = 0 WHERE trip_mode = 7"))
		dbSendUpdate(db, paste0("UPDATE matched_skims_", i, "_", j," SET matched = 0 WHERE trip_mode = 8"))
		dbSendUpdate(db, paste0("UPDATE matched_skims_", i, "_", j," SET matched = wtrnw 
			WHERE trip_mode IN (9,10,11,12,13)")) 
		dbSendUpdate(db, paste0("UPDATE matched_skims_", i, "_", j," SET matched = dtrnw 
			WHERE trip_mode IN (14,15,16,17,18) AND inbound = 0"))
		dbSendUpdate(db, paste0("UPDATE matched_skims_", i, "_", j," SET matched = wtrnd 
			WHERE trip_mode IN (14,15,16,17,18) AND inbound = 1")) },
		
		{ # Otherwise it's a time skim
			dbSendUpdate(db, paste0("UPDATE matched_skims_", i, "_", j," SET matched = walk WHERE trip_mode = 7"))
			dbSendUpdate(db, paste0("UPDATE matched_skims_", i, "_", j," SET matched = bike WHERE trip_mode = 8"))
			dbSendUpdate(db, paste0("UPDATE matched_skims_", i, "_", j," SET matched = wtrnw 
				WHERE trip_mode in (9,10,11,12,13)")) 
			dbSendUpdate(db, paste0("UPDATE matched_skims_", i, "_", j," SET matched = dtrnw 
				WHERE trip_mode in (14,15,16,17,18) AND inbound = 0"))
			dbSendUpdate(db, paste0("UPDATE matched_skims_", i, "_", j," SET matched = wtrnd 
				WHERE trip_mode in (14,15,16,17,18) AND inbound = 1"))
		}
	) ) #ifelse statements
		
	}
}	
		
# Update trip tables with skims to create a complete trip table
for(i in scenarios$names) {
	for(j in types) {

	# create a new column in the original trips update the the distance skims in the full trip tables
	dbSendUpdate(db, paste0("ALTER TABLE trips_", i, " ADD COLUMN ", j, " float"))

	#	update the new column to contain the skim results, matched on the primary key
	dbSendUpdate(db, paste0("
		UPDATE trips_", i, " 
		SET ", j, " = 
		(SELECT matched_skims_", i, "_", j, ".matched 
		FROM matched_skims_", i, "_", j, " 
		WHERE trips_", i, ".idkey = matched_skims_", i, "_", j, ".idkey)"))

	}
}

# TRIP import finished

# ---------------------------------------------
# Match TOURS to skims and update TOUR tables
# Outcome: Tour tables contain a column that describes the time, distance, and cost of each trip
# ---------------------------------------------

# create a new table for each scenario with no data so we can use an insert query in the for loop below
for(i in tours$names) {
	for(j in types) {
		for(k in direction) {
	
		ifelse(j == "Distance",
		dbSendUpdate(db, paste0(
			"CREATE TABLE ", k, "_tour_skims_", i, "_", j," 
			AS SELECT idkey, orig_taz, dest_taz, start_hour, end_hour, tour_mode, da, daToll, s2, s2toll, s3, s3toll, 
			walk, bike from tours_", i, " t1 
			INNER JOIN ", j, "_skims_",i ,"_ea t2 
			ON t1.orig_taz = t2.orig AND t1.dest_taz = t2.dest WITH NO DATA")),
		
		ifelse(j == "Cost",	
		dbSendUpdate(db, paste0(
			"CREATE TABLE ", k, "_tour_skims_", i, "_", j, " as select idkey, orig_taz, dest_taz, start_hour, 
			end_hour, tour_mode, da, daToll, s2, s2toll, s3, s3toll, wtrnw, dtrnw, wtrnd 
			FROM tours_", i, " t1 
			INNER JOIN ", j, "_skims_", i , "_ea t2 
			ON t1.orig_taz = t2.orig AND t1.dest_taz = t2.dest WITH NO DATA")),
		
		# Time
		dbSendUpdate(db, paste0(
			"CREATE TABLE ", k, "_tour_skims_", i, "_", j," 
			AS SELECT idkey, orig_taz, dest_taz, start_hour, end_hour, tour_mode, da, daToll, s2, s2toll, s3, s3toll, 
			walk, bike, wtrnw, dtrnw, wtrnd 
			FROM tours_", i, " t1 
			INNER JOIN ", j, "_skims_", i, "_ea t2 
			ON t1.orig_taz = t2.orig AND t1.dest_taz = t2.dest WITH NO DATA"))			
		)) #ifelse statements
		
		}
	}
}

# loop through the skim tables for the five time periods and join to the appropriate OUTBOUND TOUR records 
for(i in tours$names) {
	for(j in types) {
		for(k in times) {
	
		# define a time-of-day flag so that we join the correct skims
		depart_hours <- ifelse(k == "EA", "3,4,5",
										ifelse(k == "AM", "6,7,8,9",
										ifelse(k == "MD", "10,11,12,13,14",
										ifelse(k == "PM", "15,16,17,18","1,2,19,20,21,22,23,24"))))
		
		ifelse(j == "Distance",
		dbSendUpdate(db, paste0(
			"INSERT INTO out_tour_skims_", i, "_", j," 
			SELECT idkey, orig_taz, dest_taz, start_hour, end_hour, tour_mode, da, daToll, s2, s2toll, s3, s3toll, 
			walk, bike 
			FROM tours_", i," t1 
			INNER JOIN ",j, "_skims_", i, "_", k, " t2 
			ON t1.orig_taz = t2.orig AND t1.dest_taz = t2.dest 
			WHERE start_hour IN (", depart_hours, ")")),
			
		ifelse(j == "Cost",
		dbSendUpdate(db, paste0(
			"INSERT INTO out_tour_skims_", i, "_", j," 
			SELECT idkey, orig_taz, dest_taz, start_hour, end_hour, tour_mode, da, daToll, s2, s2toll, s3, s3toll, 
			wtrnw, dtrnw, wtrnd 
			FROM tours_", i," t1 
			INNER JOIN ",j, "_skims_", i,"_", k," t2 
			ON t1.orig_taz = t2.orig AND t1.dest_taz = t2.dest 
			WHERE start_hour in (", depart_hours, ")")),
		
		# Time
		dbSendUpdate(db, paste0(
			"INSERT INTO out_tour_skims_", i, "_", j," 
			SELECT idkey, orig_taz, dest_taz, start_hour, end_hour, tour_mode, da, daToll, s2, s2toll, s3, s3toll, 
			walk, bike, wtrnw, dtrnw, wtrnd 
			FROM tours_", i," t1 
			INNER JOIN ",j, "_skims_", i,"_", k," t2 
			ON t1.orig_taz = t2.orig AND t1.dest_taz = t2.dest 
			WHERE start_hour IN (", depart_hours, ")"))
		)) #ifelse statements
			
		}
	}
}

# loop through the skim tables for the five time periods and join to the appropriate INBOUND TOUR records 
# for these loops, origin and destination from the skim table are switched
for(i in tours$names) {
	for(j in types) {
		for(k in times) {
	
		# define a time-of-day flag so that we join the correct skims
		depart_hours <- ifelse(k == "EA", "3,4,5",
										ifelse(k == "AM", "6,7,8,9",
										ifelse(k == "MD", "10,11,12,13,14",
										ifelse(k == "PM", "15,16,17,18","1,2,19,20,21,22,23,24"))))
		
		ifelse(j == "Distance",
		dbSendUpdate(db, paste0(
			"INSERT INTO in_tour_skims_", i, "_", j," 
			SELECT idkey, orig_taz, dest_taz, start_hour, end_hour, tour_mode, da, daToll, s2, s2toll, s3, s3toll, 
			walk, bike 
			FROM tours_", i," t1 
			INNER JOIN ",j, "_skims_", i, "_", k, " t2 
			ON t1.orig_taz = t2.dest AND t1.dest_taz = t2.orig
			WHERE end_hour IN (", depart_hours, ")")),
			
		ifelse(j == "Cost",
		dbSendUpdate(db, paste0(
			"INSERT INTO in_tour_skims_", i, "_", j," 
			SELECT idkey, orig_taz, dest_taz, start_hour, end_hour, tour_mode, da, daToll, s2, s2toll, s3, s3toll, 
			wtrnw, dtrnw, wtrnd 
			FROM tours_", i," t1 
			INNER JOIN ",j, "_skims_", i,"_", k," t2 
			ON t1.orig_taz = t2.dest AND t1.dest_taz = t2.orig 
			WHERE end_hour IN (", depart_hours, ")")),
		
		# Time
		dbSendUpdate(db, paste0(
			"INSERT INTO in_tour_skims_", i, "_", j," 
			SELECT idkey, orig_taz, dest_taz, start_hour, end_hour, tour_mode, da, daToll, s2, s2toll, s3, s3toll, 
			walk, bike, wtrnw, dtrnw, wtrnd 
			FROM tours_", i," t1 
			INNER JOIN ",j, "_skims_", i,"_", k," t2 
			ON t1.orig_taz = t2.dest AND t1.dest_taz = t2.orig 
			WHERE end_hour IN (", depart_hours, ")"))
		)) # ifelse statements
			
		}
	}
}


# Create columns in the outbound and inbound matched skim tables and TOUR tables to store the skim results
for(i in tours$names) {
	for(j in types) {
		for(k in direction) {
	
		# Add columns in the matched table and trip tables to store the matched skim results
		dbSendUpdate(db, paste0("ALTER TABLE ", k, "_tour_skims_", i, "_", j," ADD COLUMN matched float"))
			
		# These updates are the same regardless of skim type
		dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = da WHERE tour_mode = 1"))
		dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = daToll WHERE tour_mode = 2"))
		dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = s2 WHERE tour_mode = 3"))
		dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = s2Toll WHERE tour_mode = 4"))
		dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = s3 WHERE tour_mode = 5"))
		dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = s3Toll WHERE tour_mode = 6"))
			
		ifelse(j == "Distance", {
			dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = walk WHERE tour_mode = 7"))
			dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = bike WHERE tour_mode = 8"))
			
			# Approximate transit distance using drive alone distance
			dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = da WHERE tour_mode > 8")) },
			
		ifelse(j == "Cost" & k == "out", {
			dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = 0 WHERE tour_mode = 7"))
			dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = 0 WHERE tour_mode = 8"))
			dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = wtrnw 
				WHERE tour_mode in (9,10,11,12,13)")) 
			dbSendUpdate(db, paste0("update ", k, "_tour_skims_", i, "_", j," set matched = dtrnw 
				WHERE tour_mode in (14,15,16,17,18)")) },
			
		ifelse(j == "Cost" & k == "in", {
			dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = 0 WHERE tour_mode = 7"))
			dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = 0 WHERE tour_mode = 8"))
			dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = wtrnw 
				WHERE tour_mode IN (9,10,11,12,13)")) 
			dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = wtrnD WHERE tour_mode 
				IN (14,15,16,17,18)")) },
			
		ifelse(j == "Time" & k == "out", {			
				dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = walk WHERE tour_mode = 7"))
				dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = bike WHERE tour_mode = 8"))
				dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = wtrnw 
					WHERE tour_mode IN (9,10,11,12,13)")) 
				dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = dtrnw 
					WHERE tour_mode IN (14,15,16,17,18)")) },
			
			{ # otherwise it's an inbound time skim
				dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = walk WHERE tour_mode = 7"))
				dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = bike WHERE tour_mode = 8"))
				dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = wtrnw 
					WHERE tour_mode in (9,10,11,12,13)")) 
				dbSendUpdate(db, paste0("UPDATE ", k, "_tour_skims_", i, "_", j," SET matched = wtrnd 
					WHERE tour_mode IN (14,15,16,17,18)")) }
	)))) #ifelse statements
	
		}
	}
}	


# Update TOUR tables with skims to create a complete TOUR table
for(i in tours$names) {
	for(j in types) {
		for(k in direction) {

		# Create a new column in the original tour table to contain the matched skims
		dbSendUpdate(db, paste0("ALTER TABLE tours_", i, " ADD COLUMN ", j, "_", k, " float"))

		#	Update the new column to contain the skim results, matched on the primary key
		dbSendUpdate(db, paste0(
			"UPDATE tours_", i, " set ", j, "_", k, " = 
			(SELECT ", k, "_tour_skims_", i, "_", j, ".matched 
			FROM ", k, "_tour_skims_", i, "_", j, " 
			where tours_", i, ".idkey = ", k, "_tour_skims_", i, "_", j, ".idkey)"))

		}
	}
}


# Add and populate column for total roundtrip cost, average one-way time and average one-way distance
for(i in tours$names) {
	for(j in types) {

		# create a new column in the original tour table to contain the matched skims
		#dbSendUpdate(db, paste0("alter table tours_", i, " add column ", j, " float"))

		ifelse(j == "Cost", dbSendUpdate(db, paste0(
			"UPDATE tours_", i, " SET ", j, " = cost_out + cost_in")),  # round-trip cost
		ifelse(j == "Time", dbSendUpdate(db, paste0(
			"UPDATE tours_", i, " SET ", j, " = (time_out + time_in) / 2")),  # mean one-way time
		dbSendUpdate(db, paste0(
			"UPDATE tours_", i, " SET ", j, " = (distance_out + distance_in) / 2"))))  # mean one-way distance
	
	}
}

# TOUR import finished

# ---------------------------------------------
# Import land use (TAZ properties) for each scenario
# ---------------------------------------------

# Import TAZ data for 2010 only.
# Add other csv import calls for other scenarios as needed.
monet.read.csv(db, as.character(tazs$paths[1]), "taz_props_2010", 1455, locked = TRUE)

# ---------------------------------------------
# Import synthetic households and individuals for each scenario 
# ---------------------------------------------

for(i in synpops$paths) {
	for(j in c("personFile","hhFile")) {
		
		# The paths variable within synpops has a token that needs to be replaced
		path <- gsub("token", j, i)
		
		# quickly figure out the number of lines in the data file
		chunk_size <- 1000
		testcon <- file(path, open = "r")
		nooflines <- 0
		while((linesread <- length(readLines(testcon, chunk_size))) > 0)
			nooflines <- nooflines + linesread
		close(testcon)
		
		monet.read.csv(db, path, paste0(j, "_", synpops[which(synpops$paths == i), "names"]), nooflines, locked = TRUE)
	}
}

# ---------------------------------------------
# Import *modeled* households and individuals for each scenario 
# The synthetic population goes through several modeling procedures (auto ownership, parking eligibility)
# after its creation.
# ---------------------------------------------

for(i in modeledpops$paths) {
	for(j in c("person","household")) {
		
		# the paths variable within synpops has a token that needs to be replaced
		path <- gsub("token", j, i)
		
		# quickly figure out the number of lines in the data file
		chunk_size <- 1000
		testcon <- file(path, open = "r")
		nooflines <- 0
		while((linesread <- length(readLines(testcon, chunk_size))) > 0)
			nooflines <- nooflines + linesread
		close(testcon)
		
		monet.read.csv(db, path, paste0("modeled_", j, "_", modeledpops[which(modeledpops$paths==i), "names"]), 
			nooflines, locked = TRUE)
	}
}

# --------------------------------------------
# Import usual work/school location choice model results for each scenario
# --------------------------------------------

for(i in wslocations$paths) {
	# The model results contain uppercase letters, they need to be converted to lowercase
	headers <- c("hhid", "hometaz", "homesubzone", "income", "personid", "personnum", "persontype", 
		"personage", "employmentcategory", "studentcategory", "worklocation", "worksubzone", "schoollocation", 
		"schoolsubzone")

	colTypes <- c("double", "int", "int", "double", "double", "int", "text", "int", "text", "text", "int", "int", 
		"int", "int")

	# Create a character vector grouping each column name with each column type..
	colDecl <- paste(headers, colTypes)

	tablename <- paste0("wsloc_", wslocations[which(wslocations$paths == i), "names"])
	
	# ..and then construct an entire 'create table' sql command
	sql <- sprintf(paste("CREATE TABLE", tablename, "(%s)"), paste(colDecl, collapse = ","))
	
	dbSendUpdate(db, sql)
	
	chunk_size <- 1000
	testcon <- file(i, open = "r")
	nooflines <- 0
	while((linesread <- length(readLines(testcon, chunk_size))) > 0)
		nooflines <- nooflines + linesread
	close(testcon)

	# Read the ws location table into the database
	monet.read.csv(db, i, paste0("wsloc_", wslocations[which(wslocations$paths == i), "names"]), nooflines, 
		locked = TRUE)
}


# ---------------------------------------------
# Example of recoding variables
# ---------------------------------------------

# Youth

for(i in synpops$names) {
	
	dbSendUpdate(db, paste0("alter table personfile_", i, " add column youth int"))
	dbSendUpdate(db, paste0("update personfile_", i, " set youth = 1 where age < 18"))  # Age < 18 is taken from the mode choice model
	dbSendUpdate(db, paste0("update personfile_", i, " set youth = 0 where age >= 18"))

}

# Transit

for(i in trips$names) {
	
	dbSendUpdate(db, paste0("alter table trips_", i, " add column transit int"))
	dbSendUpdate(db, paste0("update trips_", i, " set transit = 1 where tour_mode > 8"))  # Age < 18 is taken from the mode choice model
	dbSendUpdate(db, paste0("update trips_", i, " set transit = 0 where tour_mode <= 8"))

}