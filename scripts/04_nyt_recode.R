##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##
##      NYT Soft Train 		 
##		Last Edited: 2.26.15  	         
##   	Gaurav Sood				
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++

# Run this script and save processed data

	nyt10 <- read.csv("nyt_cleaned.csv")


# News Desk
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# inconsistencies
	nyt10$News.Desk[nyt10$News.Desk %in% c('Metropolitan desk', 'Metropolitian desk')]  <- 'Metropolitan Desk'	
	
	##recode one factor due to change in mid 1990s
	nyt10$News.Desk[nyt10$News.Desk == 'Business/Financial Desk'] <- 'Financial Desk'

	## News Desk Based Classification
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	nyt10$categories <- NA

	# Arts
	arts 	<- c("Art and Leisure Desk", "Museums", "Arts Almanac Supplement", "Cultural Desk - SummerTimes Supplement", "Cultural",
				"Arts", "The Arts", "Arts/Cultural Desk", "Cultural Desk", "Arts and Leisure Desk", "Arts & Leisure", "Cultural Desk;", 
				"Arts & Liesure Desk", "The Art/Cultural Desk", "Cultural/Arts Desk", "The Arts/Cultural Desk", "Tha Arts/Cultural Desk",
				"<br>The Arts/Cultural Desk", "The Arts\\Cultural Desk", "Summer Arts Supplement", "Arts & Ideas/Cultural Desk",
				"Art & Ideas/Cultural Desk", "The Arts/Cultrual Desk", "Cultural desk")

	style 	<- c("Style", "Style Desk", "Stlye Desk", "Styles of the Times Desk", "Men's Fashions of The Times Magazine", 
				"Style Desk;", "Style of The Times", "Styles of The Times Desk", "Styles of The TimesStyles of The Times", "Styles of the Times",
				"Men's Fashions of The Times", "Men's Fashions of The TimesMagazine", "Men's Fashion of The Times Magazine", 
				"Men's Fashion of the Times Magazine", "T: Women's Fashion Magazine", "T: Men's Fashion Magazine", "Thursday Styles Desk",
				"Style and Entertaining Magazine","Men's Fashions of the Times Magazine", "Fashions of the Times Magazine", "Fashions of The Times Magazine",
				"Living DeskStyle Desk", "House & Home/Style Desk", "Styles of The Times", "Thursday Styles", "Style and Entertaining MagazineStyle and Enter") 

	books	<- c("Book Review Desk", "Book Review Dest", "Book Reviw Desk")

	travel  <- c("TravelDesk", "Travel Desk", "Sophisticated Traveler Magazine", "T: Travel Magazine", "Travel DeskTravel Desk", "Escapes")

	local 	<- c("Metropitan Desk", "Metroploitan Desk", "Metropoliton Desk", "Metropolitan", "Metropolitan Desk;", "Metrpolitan Desk", "Metropolian Desk", "Metropoltan Desk", 
				"Metropolitan Desak", "Metropolitan Deski", "Metroplitan Desk", "qMetropolitan Desk", "Metrolpolitian Desk", "Metropolitain Desk", "Metroploitian Desk", "Metropolitan Desk", 
				"Connecticut Weekly Desk", "New Jersey Desk", "New Jersey Weekly Desk", "Westchester Weekly Desk", "Westchester Weekly Deask", 
				"Long Island Weekly Desk", "Long Island Waekly Desk", "Long Island Weekly", "Long Island Desk", "New Jersey Weely Desk",
				"The City Weekly Desk", "The City Weekly", "The City" , "The City Weekly Section", "The City Desk", "City Weekly Desk", 
				"The City Weekly/Queens", "The City Weelky Desk", "TheCity Weekly Desk", "The City/Weekly Desk", "New Jersey/Weekly Desk",
				"The City Weeky Desk", "Connecticut Desk", "Metropoliatn Desk", "Metropoltian Desk", "Metro Desk","The City Weekly Deslk",
				"The City Weekly Desk\n The City Weekly Desk", "The City Weekl Desk", "Metropolitan DeskMetropolitan Desk", "Connecticut Weekly desk", 
				"New Jerey Weekly Desk", "Metropolitan Desk Section D", "Metropolitian Desk", "Metropolitan Dsk", "Metropolitan Deskreign Desk")

	sports 	<- c("Sport Desk", "Sports", "Sports Desk", "Sports Deks", "Adventure Sports", "Sports DEsk", "Sports DeskSports Desk", 
				"Sports Deskk")
	
	gensoft <- c("Holiday Times Supplement", "Spring Times Supplement" , "Summer Times Supplement", "Summer Times Supplementa", "Autumn Times Supplement",
				"Winter Times Supplement", "Home Entertaining Magazine",  "Television Desk", "Social Desk", "House & Home/Style", "Style and Entertaining Magaziner",
				"House & Home\\Style Desk","Wireless Living","T: Design Magazine", "T: Living Magazine", "Televison Desk", "T: Beauty",
				"THoliday","Play","Play Magazine", "Home Design Desk", "Home Design Magazine", "Entertaining Magazine", "Televison", 
				"Society Dek", "Springs Times Supplement",  "Television", "Living Desk;", "Society DeskMetropolitan Desk", "Technology", 
				"Business Travel", "The New Season Magazine", "House and Home Style", "The Marathon", "Society Desk" )

	realest <- c("Real Estate", "Real Estate Desk", "Commercial Real Estate Report", "Residential Real Estate Report", "Real Estate Desk", "Real Estate desk",
				"Real Estate desk", "Real Estate", "Commercial Real Estate Report", "Residential Real Estate Report", "Real Estate desk",
				"Commercial Real Estate Report", "Residential Real Estate Report", "Real Estate Desk")

	persfin	<- c("Careers Supplement Desk", "Careers Supplement" , "Financial Planning Guide: Personal Investing", "Financial Planning Guide: Personal", 
				"Workplace", "Retirement", "Financial Planning Guide: Your Taxes", "Job Market Desk", "Job Market", "Your Taxes Supplement", 
				"Personal Investing Supplement Desk")


	bizfin		<- c("Financial Desk", "Financial", "Business Desk", "Business/Finance Desk", "Business/Financial Desk;", "Financial/Business Desk", "Business/Foreign Desk", "Money and Business/Financial Desk", "Money & Business/Financial Desk",
					"Money & Business/Financial Desk","Monet and Business/Financial Desk", "Money and Busines/Financial Desk", "Money andBusiness/Financial Desk",
					"Money and Business/FinancialDesk", "Business/FinancialDesk", "Business/Finacial Desk", "Business/Financial Desk Section D", 
					"Money and Business/Financial", "Money & Businees Desk", "Business Day/Financial", "Money and Business/Financial DeskMoneyand Bus",
					"Business\\Financial Desk", "Business/Finanical Desk", "Financial Desk;", "Money and Business/Financial DeskMoney and Bus",
					"Business/Financial desk", "DealBook", "Moneyand Business/Financial Desk", "SundayBusines", "SundayBusinessSundayBusiness", "SundayBusiness",
					"Business/Financial", "Business/Financial DeskBusiness/Financial Desk", "Money and Business/Fiancial Desk", "Small Business", 
					"Business World Magazine", "Sunday Business", "BIZ", "The Business of Green", "The Business of Health", "Retail")

	cars 		<- c("Autmobiles", "Automobile Show Desk", "Automobies", "Automoiles", "AuTomobiles", "Automobile Desk", "Cars", 
					"Automobliles", "Automoblies", "Autombiles", "Automobile", "Automobiles Desk", "Automobles", "Automobiles")

	leisure 	<- c( "Arts CultureStyle Leisure", "Weekend Desk", "Weekend Desk", "Leisure/Weekend Desk", "Weekend DeskWeekend Desk", "Weekend Desk;", "Vacation", 
				"Arts and Leisure Desk Desk", "Movies, Performing Arts/Weekend Desk", "Movies,Performing Arts/Weekend Desk", "Summer Movies",
				"Movies, Performing Arts/Weekend DeskMovies, Pe","Business Financial Desk", "Arts and Leisure")

	health 		<- c("Good Health Magazine", "The Good Health Magazine", "Health and Fitness", "Health&Fitness", "Women's HealthWomen's Health", "Women's Health", "Health & Fitness", 
					"Health & Fitness Desk" , "Men's Health", "Men & Health", "PersonalHealth", "Health")

	fnews		<- c("Foreign desk", "Foriegn Desk", "Foreign DEsk", "Foreignl Desk", "1;             Foreign Desk", "Foreign Desk")
	
	natdesk 	<- c("National Desk", "National Edition - Final", "National Deskl", "Natioanl Desk", "National Dsek", "National News", "National", 
					"National desk", "National DeskNational Desk", "National Desk;"  )

	living 		<- c("Living Desk  Section C","Living Desk", "Living DeskLiving Desk")
	
	classifieds <- c("Classified", "Classified Desk", "Classifed", "Classifieds", "Classsified", "Classfied","classified" )

	dining		<- c("Dining In, Dining Out", "Dining in, Dining out/Style Desk" , "Dining In/Dining Out" , "Dining In/Dining Out/Living Desk", 
					"Dining In, Dining Out/Style Desk", "Dining In, Dining Out/Cultural Desk", "Dining, Dining Out/Cultural Desk", 
					"Dining In, Dining Out/Style DeskDining In, Din"  )

	misc 		<- c("Survey of Education Desk", "Summer Survey of Education", "Op-Ed at 20 Supplement", "World of New York Magazine", 
					"Education Life SupplementMetropolitan Desk" ,  "Education Life", "Education Life Supplement", "Education Life Supple", 
					"Magazine DeskMetropolitan Desk", "Citcuits", "Circuits", "Circuits Desk","CircuitsCircuits", 
					"Education Life SupplementEducation Life Supple", "E-Commerce", "Entrepreneurs", "The Millennium", "The Millenium", 
					"Generations", "Flight", "Week" , "Metro", "Working", "Giving",  "The Year in Pictures", "The Year In Pictures" , 
					"2005: The Year In Pictures", "ContinuousNews", "Voter Guide 2004" )  

	home 	<- c("Home Desk", "Home DeskHome Desk", "Home Desk;", "Home Desk;", "Home Desk", "Home DeskHome Desk")                                   
	wkinrev <- c("Week in Review desk", "Week in Review Desk", "Week In Review", "Week in Review", "Week in Review Desk" , "Week in Review desk", "Weekin Review Desk", 
				"Week In Review Desk", "Week in review desk", "Week in Review Deskn", "Week In Review DeskWeek In Review Desk")
 	edit 	<- c("Editorial desk", "Editoral Desk", "Editorial Desk", "Editorial Desk")
 	science <- c("Science Desk", "Science","Science Desk;", "The Natural World", "Science Desk")
 	obits 	<- c("Obituary",  "Obits", "Obituary")
 	mgz 	<- c("Magazine Desk", "New York, New York Magazine", "Magazine", "Magazine Desk", "New York, New York Magazine")                                                                     
                                                                

	nyt10$categories[nyt10$News.Desk %in% arts]		<- "Arts"
	nyt10$categories[nyt10$News.Desk %in% style]	<- "Style"
	nyt10$categories[nyt10$News.Desk %in% books]	<- "Books"
	nyt10$categories[nyt10$News.Desk %in% travel]	<- "Travel"
	nyt10$categories[nyt10$News.Desk %in% local]	<- "Local"
	nyt10$categories[nyt10$News.Desk %in% sports]	<- "Sports"
	nyt10$categories[nyt10$News.Desk %in% gensoft]	<- "Gen Soft"
	nyt10$categories[nyt10$News.Desk %in% realest]	<- "Real Estate"
	nyt10$categories[nyt10$News.Desk %in% persfin]	<- "Personal Finance"
	nyt10$categories[nyt10$News.Desk %in% bizfin]	<- "Business Finance"
	nyt10$categories[nyt10$News.Desk %in% cars]		<- "Cars"

	nyt10$categories[nyt10$News.Desk %in% leisure]	<- "Leisure"
	nyt10$categories[nyt10$News.Desk %in% health]	<- "Health"
	nyt10$categories[nyt10$News.Desk %in% fnews]	<- "Foreign News"
	nyt10$categories[nyt10$News.Desk %in% natdesk]	<- "National"
	nyt10$categories[nyt10$News.Desk %in% living]	<- "Living"
	nyt10$categories[nyt10$News.Desk %in% classifieds]	<- "Classifieds"
	nyt10$categories[nyt10$News.Desk %in% dining]	<- "Dining"
	nyt10$categories[nyt10$News.Desk %in% misc]		<- "Misc"
	
	nyt10$categories[nyt10$News.Desk %in% home]		<- "Home Desk"
	nyt10$categories[nyt10$News.Desk %in% wkinrev]	<- "Week in Review"
	nyt10$categories[nyt10$News.Desk %in% edit]		<- "Editorial"
	nyt10$categories[nyt10$News.Desk %in% science]	<- "Science"
	nyt10$categories[nyt10$News.Desk %in% obits]	<- "Obits"
	nyt10$categories[nyt10$News.Desk %in% mgz]		<- "Magazine"

	# Numeric categories
	nyt10$catn <- as.numeric(as.factor(nyt10$categories))

# Online Section
# ~~~~~~~~~~~~~~~~~~~~~
	nyt10$Opinion 	<- grepl("Opinion", 	as.character(nyt10$Online.Section))
	nyt10$Obituaries 	<- grepl("Obituaries", 	as.character(nyt10$Online.Section))
	nyt10$Corrections <- grepl("Corrections", as.character(nyt10$Online.Section))
	nyt10$Classifieds	<- grepl("Classified", 	as.character(nyt10$Online.Section))

# Get No News
# ~~~~~~~~~~~~~~~~~~~~~~
	# Based on Online Sections
		# On the fence: "Real Estate", "Magazine", "Technology"

		# NA to Sections with more than 1 label
			nyt10$OnlineSectionr <- ifelse(grepl(";", nyt10$Online.Section), NA, nyt10$Online.Section)

		# Match only where there is one label
			nyt10$OnlineSSoft <-  nyt10$OnlineSectionr %in% c("Automobiles", "Arts", "Theater", "Dining and Wine", "Movies", "Style","Books", "Home and Garden", "Sports", "Travel")
		
		# Match any of the labels; If total soft matches half or more	
			matches 			<- lapply(c("Automobiles", "Arts", "Theater", "Dining and Wine", "Movies", "Style","Books", "Home and Garden", "Sports", "Travel"), grepl, nyt10$Online.Section, fixed=TRUE)
			nyt10$OnlineMSoft 	<- rowSums(as.data.frame(matches))
	
	# Based on News Desk
		# No News/Soft News
			nyt10$NewsDeskSoft <-  nyt10$categories %in% c("Arts", "Books", "Cars", "Dining", "Gen Soft", "Leisure", "Living", "Sports", "Style", "Travel", "Personal Finance",  "Health", "Real Estate")

##The next lines create a time variable 
##for quarter/year and day/month/year observations. 
	nyt10$Publication.Month 	<- as.numeric(nyt10$Publication.Month)
	nyt10$quarter 				<- with(nyt10, ifelse( Publication.Month <= 3, 1,  ifelse(Publication.Month <=6, 2, ifelse(Publication.Month <= 9, 3, 4))))

	## Updated by Suriyan (2019/06/01)
	# ID
	nyt10$ID	<- 1:nrow(nyt10)

	# move ID to 1st column
	nyt <- nyt10[, c(length(colnames(nyt10)), seq(length(colnames(nyt10))-1))]

	# Write
    write.csv(nyt, "nyt_recode_clean.csv", row.names=F)
