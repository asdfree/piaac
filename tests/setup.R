# what color collar
# workforce poets, potters, or
# pythagoreans
tf <- tempfile()

this_url <- "https://webfs.oecd.org/piaac/puf-data/CSV/Prgusap1_2017.csv"

download.file( this_url , tf , mode = 'wb' )

unzipped_files <- unzip( tf , exdir = tempdir() )

piaac_csv <- grep( '\\.csv$' , unzipped_files , value = TRUE )

piaac_df <- read.csv( piaac_csv )

names( piaac_df ) <- tolower( names( piaac_df ) )

library(survey)
library(mitools)

pvals <- c( "pvlit" , "pvnum" , "pvpsl" )
pvars <- outer( pvals , 1:10 , paste0 )
non.pvals <- names(x)[ !( names(x) %in% pvars ) ]

for(k in 1:10){
	
	y <- x[ , c( non.pvals , paste0( pvals , k ) ) ]
	
	for( j in pvals ){
		
		y[ , j ] <- y[ , paste0( j , k ) ]
		
		y[ , paste0( j , k ) ] <- NULL

	}
	
	if( k == 1 ) w <- list( y ) else w <- c( w , list( y ) )
}

jk.method <- unique(x$vemethod)

stopifnot(length(jk.method) == 1)

stopifnot(jk.method %in% c("JK1", "JK2"))

if (jk.method == "JK2") jk.method <- "JKn"

piaac_design <-
	svrepdesign(
		weights = ~spfwt0 ,
		repweights = "spfwt[1-9]" ,
		rscales = rep( 1 , 80 ) ,
		scale = ifelse( jk.method == "JKn" , 1 , 79/80 ) ,
		type = jk.method ,
		data = imputationList( w ) ,
		mse = TRUE
	)
piaac_design <-
	update(
		piaac_design ,
		
		one = 1 ,
		
		sex = factor( gender_r , labels = c( "male" , "female" ) ) ,

		age_categories = 
			factor( 
				ageg10lfs , 
				levels = 1:5 , 
				labels = c( "24 or less" , "25-34" , "35-44" , "45-54" , "55 plus" ) 
			) ,
		
		working_at_paid_job_last_week = as.numeric( c_q01a == 1 )
		
	)
MIcombine( with( piaac_design , svyby( ~ one , ~ one , unwtd.count ) ) )

MIcombine( with( piaac_design , svyby( ~ one , ~ age_categories , unwtd.count ) ) )
MIcombine( with( piaac_design , svytotal( ~ one ) ) )

MIcombine( with( piaac_design ,
	svyby( ~ one , ~ age_categories , svytotal )
) )
MIcombine( with( piaac_design , svymean( ~ pvnum , na.rm = TRUE ) ) )

MIcombine( with( piaac_design ,
	svyby( ~ pvnum , ~ age_categories , svymean , na.rm = TRUE )
) )
MIcombine( with( piaac_design , svymean( ~ sex ) ) )

MIcombine( with( piaac_design ,
	svyby( ~ sex , ~ age_categories , svymean )
) )
MIcombine( with( piaac_design , svytotal( ~ pvnum , na.rm = TRUE ) ) )

MIcombine( with( piaac_design ,
	svyby( ~ pvnum , ~ age_categories , svytotal , na.rm = TRUE )
) )
MIcombine( with( piaac_design , svytotal( ~ sex ) ) )

MIcombine( with( piaac_design ,
	svyby( ~ sex , ~ age_categories , svytotal )
) )
MIcombine( with( piaac_design ,
	svyquantile(
		~ pvnum ,
		0.5 , se = TRUE , na.rm = TRUE 
) ) )

MIcombine( with( piaac_design ,
	svyby(
		~ pvnum , ~ age_categories , svyquantile ,
		0.5 , se = TRUE ,
		ci = TRUE , na.rm = TRUE
) ) )
MIcombine( with( piaac_design ,
	svyratio( numerator = ~ pvnum , denominator = ~ pvlit , na.rm = TRUE )
) )
sub_piaac_design <- subset( piaac_design , i_q08 %in% 4:5 )
MIcombine( with( sub_piaac_design , svymean( ~ pvnum , na.rm = TRUE ) ) )
this_result <-
	MIcombine( with( piaac_design ,
		svymean( ~ pvnum , na.rm = TRUE )
	) )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	MIcombine( with( piaac_design ,
		svyby( ~ pvnum , ~ age_categories , svymean , na.rm = TRUE )
	) )

coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( piaac_design$designs[[1]] )
MIcombine( with( piaac_design , svyvar( ~ pvnum , na.rm = TRUE ) ) )
# SRS without replacement
MIcombine( with( piaac_design ,
	svymean( ~ pvnum , na.rm = TRUE , deff = TRUE )
) )

# SRS with replacement
MIcombine( with( piaac_design ,
	svymean( ~ pvnum , na.rm = TRUE , deff = "replace" )
) )
MIsvyciprop( ~ working_at_paid_job_last_week , piaac_design ,
	method = "likelihood" )
MIsvyttest( pvnum ~ working_at_paid_job_last_week , piaac_design )
MIsvychisq( ~ working_at_paid_job_last_week + sex , piaac_design )
glm_result <- 
	MIcombine( with( piaac_design ,
		svyglm( pvnum ~ working_at_paid_job_last_week + sex )
	) )
	
summary( glm_result )

austria_design <-
	readRDS( file.path( path.expand( "~" ) , "PIAAC" , "prgautp1 design.rds" ) )

austria_pvlit <-
	MIcombine( with( austria_design , svymean( ~ pvlit , na.rm = TRUE ) ) )
	
austria_pvnum <-
	MIcombine( with( austria_design , svymean( ~ pvnum , na.rm = TRUE ) ) )

austria_pvpsl <-
	MIcombine( with( austria_design , svymean( ~ pvpsl , na.rm = TRUE ) ) )
	
# confirm each estimate and standard error matches the published statistics
stopifnot( round( coef( austria_pvlit ) ) == 269 )
stopifnot( round( SE( austria_pvlit ) , 1 ) == 0.7 )
stopifnot( round( coef( austria_pvnum ) ) == 275 )
stopifnot( round( SE( austria_pvnum ) , 1 ) == 0.9 )
stopifnot( round( coef( austria_pvpsl ) ) == 284 )
stopifnot( round( SE( austria_pvpsl ) , 1 ) == 0.7 )

