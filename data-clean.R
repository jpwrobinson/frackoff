

dat<-read.csv('~frackoff/frack-dat-28-10-2018.csv')


dat$date<-as.character(dat$date)
dat$month<-str_split_fixed(dat$date, '/', 3)[,2]
dat$year<-str_split_fixed(dat$date, '/', 3)[,3]
dat$date.ym<-with(dat, paste('01', month, year, sep = '-'))
dat$date.ym<-as.Date(dat$date.ym, format='%d-%m-%Y')
dat$date<-as.Date(dat$date, format='%d/%m/%Y')

dat$locality<-str_replace_all(dat$locality, '\xca', '')
dat$region<-str_replace_all(dat$region, '\xca', '')

## filter to england only
unique(dat$region)

remove<-c('D & G', 'LOTHIAN', 'FIFE', 'GWYNEDD', 'STRATHCLYDE', 'HIGHLAND', 'EIRE',  
            'TAYSIDE', 'BORDERS', 'DYFED', "S'CLYDE", 'CLWYD', 'POWYS', 'MID GLAMORGAN', 'DUMFRIES', 'S GLAMORGAN', 'GWENT',
          'WESTERN ISLES', 'ANTRIM', 'GLAMORGAN', 'SOUTH WALES', 'M GLAMORGAN', "STRA'CLYDE", 'CO DURHAM', 'GYWNEDD', 'FRANCE', 'WEST GLAMORGAN', 
          'D&G', 'WALES', 'S', 'W GLAMORGAN', 'DONEGAL', 'OUTER HEBRIDES', 'SOUTH GLAMORGAN', 'M GLAM', 'IRISH SEA', 'MID GLAM', 'N IRELAND', 
          'IRELAND', 'INNER HEBRIDES', 'HIGHAND', 'MID GLAMO', 'POWY', 'GWYNED', 'HIGHL', 'M GLAMO', 'SOUTH GLAM', ' NORTH WALES', ' D & G', 'MIDLOTHIAN',
          'N WALES', 'CEREDIGION', 'BRIDGEND', 'EAST AYRSHIRE', 'FALKIRK', 'S LANARKSHIRE', 'PERTH/KINROSS', 'ARGYLL/BUTE', 'STIRLING',
          'MERTHYR TYDFIL', 'ARGYLL & BUTE', 'CARMARTHENSHIRE', 'CAERPHILLY', 'CARMARTHNSHIRE', 'WREXHAM', 'CONWY','PERTHSHIRE', 'MONMOUTHSHIRE', 
          'SHETLAND', 'BLAENAU GWENT', 'NORTH WALES', ' NW OF IRELAND',  'RENFREWSHIRE', 'NEATH PORT TALBOT', 'MOYLE', 
          'NORTH AYRSHIRE', 'TORFAEN', 'NW OF IRELAND', 'SWANSEA', 'COUNTY DOWN', 'N LANARKSHIRE', 'DENBIGHSHIRE', 'PERTH & KINROSS', 'E DUNBARTON', 
          'COLERAINE', 'MERTHYR', 'ARGYL & BUTE', 'CARDIFF','CENTRAL', 'NP TALBOT', 'HGHLAND', 'GRAMPIAN', 'AYRSHIRE', 'INVERCLYDE', 'NPT', 'MONMOUTHSIRE', 'GWYNEDDD')

dat <- dat %>% filter(!(dat$region %in% remove))

unique(dat$locality )

write.csv(dat, file = '~frackoff/frack-clean.csv')
