# what color collar
# workforce poets, potters, or
# pythagoreans

library(survey)
library(mitools)

piaac_design <- readRDS( file.path( path.expand( "~" ) , "PIAAC" , "prgusap1 design.rds" ) )
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

