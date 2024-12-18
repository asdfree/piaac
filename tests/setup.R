# what color collar
# workforce poets, potters, or
# pythagoreans
library(haven)
library(httr)

tf <- tempfile()

this_url <- "https://webfs.oecd.org/piaac/cy1-puf-data/SAS/SAS7BDAT/prgusap1_2012.sas7bdat"

GET( this_url , write_disk( tf ) , progress() )

piaac_tbl <- read_sas( tf )

piaac_df <- data.frame( piaac_tbl )

names( piaac_df ) <- tolower( names( piaac_df ) )
# piaac_fn <- file.path( path.expand( "~" ) , "PIAAC" , "this_file.rds" )
# saveRDS( piaac_df , file = piaac_fn , compress = FALSE )
# piaac_df <- readRDS( piaac_fn )
library(survey)
library(mitools)

pvals <- c( "pvlit" , "pvnum" , "pvpsl" )
pvars <- outer( pvals , 1:10 , paste0 )
non.pvals <- names(piaac_df)[ !( names(piaac_df) %in% pvars ) ]

for(k in 1:10){
	
	piaac_imp <- piaac_df[ , c( non.pvals , paste0( pvals , k ) ) ]
	
	for( j in pvals ){
		
		piaac_imp[ , j ] <- piaac_imp[ , paste0( j , k ) ]
		
		piaac_imp[ , paste0( j , k ) ] <- NULL

	}
	
	if( k == 1 ){
		piaac_mi <- list( piaac_imp )
	} else {
		piaac_mi <- c( piaac_mi , list( piaac_imp ) )
	}
}

jk.method <- unique( piaac_df[ , 'vemethod' ] )

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
		data = imputationList( piaac_mi ) ,
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
# MIsvyciprop( ~ working_at_paid_job_last_week , piaac_design ,
# 	method = "likelihood" )
# MIsvyttest( pvnum ~ working_at_paid_job_last_week , piaac_design )
# MIsvychisq( ~ working_at_paid_job_last_week + sex , piaac_design )
glm_result <- 
	MIcombine( with( piaac_design ,
		svyglm( pvnum ~ working_at_paid_job_last_week + sex )
	) )
	
summary( glm_result )
usa_pvlit <-
	MIcombine( with( piaac_design , svymean( ~ pvlit , na.rm = TRUE ) ) )
	
usa_pvnum <-
	MIcombine( with( piaac_design , svymean( ~ pvnum , na.rm = TRUE ) ) )

usa_pvpsl <-
	MIcombine( with( piaac_design , svymean( ~ pvpsl , na.rm = TRUE ) ) )

stopifnot( round( coef( usa_pvlit ) ) == 270 )
stopifnot( round( SE( usa_pvlit ) , 1 ) == 1.0 )
stopifnot( round( coef( usa_pvnum ) ) == 253 )
stopifnot( round( SE( usa_pvnum ) , 1 ) == 1.2 )
stopifnot( round( coef( usa_pvpsl ) ) == 277 )
stopifnot( round( SE( usa_pvpsl ) , 1 ) == 1.1 )

