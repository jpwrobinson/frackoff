library(dplyr)
library(tidyr)
library(stringr)
library(here)
setwd(here('frackoff'))

dat<-read.csv('frack-dat-23-11-2018.csv')


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

remove<-c('D & G', 'LOTHIAN', 'FIFE', 'GWYNEDD', 'STRATHCLYDE', 'HIGHLAND', 'EIRE',  " NW WALES", " GWYNEDD", " GWNET", "NW WALES",
            'TAYSIDE', 'BORDERS', 'DYFED', "S'CLYDE", 'CLWYD', 'POWYS', 'MID GLAMORGAN', 'DUMFRIES', 'S GLAMORGAN', 'GWENT',
          'WESTERN ISLES', 'ANTRIM', 'GLAMORGAN', 'SOUTH WALES', 'M GLAMORGAN', "STRA'CLYDE", 'CO DURHAM', 'GYWNEDD', 'FRANCE', 'WEST GLAMORGAN', 
          'D&G', 'WALES', 'S', 'W GLAMORGAN', 'DONEGAL', 'OUTER HEBRIDES', 'SOUTH GLAMORGAN', 'M GLAM', 'IRISH SEA', 'MID GLAM', 'N IRELAND', 
          'IRELAND', 'INNER HEBRIDES', 'HIGHAND', 'MID GLAMO', 'POWY', 'GWYNED', 'HIGHL', 'M GLAMO', 'SOUTH GLAM', ' NORTH WALES', ' D & G', 'MIDLOTHIAN',
          'N WALES', 'CEREDIGION', 'BRIDGEND', 'EAST AYRSHIRE', 'FALKIRK', 'S LANARKSHIRE', 'PERTH/KINROSS', 'ARGYLL/BUTE', 'STIRLING',
          'MERTHYR TYDFIL', 'ARGYLL & BUTE', 'CARMARTHENSHIRE', 'CAERPHILLY', 'CARMARTHNSHIRE', 'WREXHAM', 'CONWY','PERTHSHIRE', 'MONMOUTHSHIRE', 
          'SHETLAND', 'BLAENAU GWENT', 'NORTH WALES', ' NW OF IRELAND',  'RENFREWSHIRE', 'NEATH PORT TALBOT', 'MOYLE', 
          "DUMF&GA", "GWYN", "LLEYN", "HANTS", "GWYND",'W.GLAMORGAN', " GWENT", "PWYS", 'SALOP', 'POWYS.','PENIN',"POW",
           'PEMBROKESHIRE','RUTLAND','FLINTSHIRE','CARMARTHS', 'CARMS','NEWPORT','BRISTOL CHANNEL','ANGLESEY',
          'NORTH AYRSHIRE', 'TORFAEN', 'NW OF IRELAND', 'SWANSEA', 'COUNTY DOWN', 'N LANARKSHIRE', 'DENBIGHSHIRE', 'PERTH & KINROSS', 'E DUNBARTON', 
          'COLERAINE', 'MERTHYR', 'ARGYL & BUTE', 'CARDIFF','CENTRAL', 'NP TALBOT', 'HGHLAND', 'GRAMPIAN', 'AYRSHIRE', 'INVERCLYDE', 'NPT', 'MONMOUTHSIRE', 'GWYNEDDD')

dat <- dat %>% filter(!(dat$region %in% remove))
dat$region<-trimws(dat$region)
unique(dat$region)
## fix county spelling errors
dat$region[dat$region %in% c('STAFFS', 'STAFF', 'STAFFODSHIRE')]<-"STAFFORDSHIRE"
dat$region[dat$region %in% c('NOTTS.', 'NOTTS')]<-"NOTTINGHAMSHIRE"
dat$region[dat$region %in% c('CORN', 'CORNWAL', 'CORNWL', 'COR', 'CORNW')]<-"CORNWALL"
dat$region[dat$region == 'LINCS']<-"LINCOLNSHIRE"
dat$region[dat$region %in% c('CUM', 'CUMB', 'CBR')]<-"CUMBRIA"
dat$region[dat$region %in% c('SOUTH YORK', 'SOUTH YORKS', 'S. YORKSHIRE', 'S YORKSHIRE', 'S YORKS', 'S.YORKSHIRE')]<-"SOUTH YORKSHIRE"
dat$region[dat$region %in% c('N.YORKS', 'N YORK', 'CLEVELAND')]<-"NORTH YORKSHIRE"
dat$region[dat$region %in% c('DUR', 'DRHM', 'DURHAM')]<-"COUNTY DURHAM"
dat$region[dat$region %in% c('MANCHESTER', 'GT MANCHESTER','G MANCHESTER','GTR MCH', 'GTR MAN', "GTR MANCHESTER",
                             'GRT MANCHESTER', 'W MANCHESTER')]<-"GREATER MANCHESTER"
dat$region[dat$region %in% c('HERFD&WOR', 'HERF&WOR','HER & WORC','HER & WORCS', 'HER & WO',
                             "H & W", 'HER&WORC','HEREFORD & WOR', 'HEREFORD&WORC', 'HER & WOR',
                             'HER&WOR', 'HEREF', 'HEREFORD')]<-"HEREFORDSHIRE & WORCESTERSHIRE"
dat$region[dat$region %in% c('N.YORKS', 'N YORKS', 'N YORKSHIRE')]<-"NORTH YORKSHIRE"
dat$region[dat$region %in% c('GLOUCS,', 'GLOUC', 'GLOUCS','GLOUCS.', "GL'SHIRE",'GLOS', "GLOUC'SHIRE")]<-"GLOUCESTERSHIRE"
dat$region[dat$region %in% c('LEICS', 'LEICESTERSHI', 'LEICESTER')]<-"LEICESTERSHIRE"
dat$region[dat$region %in% c('TYN&WR', 'TYNESIDE')]<-"TYNE & WEAR"
dat$region[dat$region %in% c('LANCS', 'LANCS.')]<-"LANCASHIRE"
dat$region[dat$region == 'SOMER']<-"SOMERSET"
dat$region[dat$region == 'BERKS.']<-"BERKSHIRE"
dat$region[dat$region %in% c('W YORK', 'W. YORKS', 'W.YORKS', 'W YORKS', 'W YORKSHIRE', 'WEST YORKS')]<-"WEST YORKSHIRE"
dat$region[dat$region == 'BUCKS']<-"BUCKINGHAMSHIRE"
dat$region[dat$region %in% c('DER', 'DERBYSHIR')]<-"DERBYSHIRE"
dat$region[dat$region %in% c('NOTTS.', 'NOTTS', "NOTTS E")]<-"NOTTINGHAMSHIRE"
dat$region[dat$region %in% c('W MIDLANDS', 'W.MIDLANDS')]<-"WEST MIDLANDS"
dat$region[dat$region == 'WORCS']<-"HEREFORDSHIRE & WORCESTERSHIRE"
dat$region[dat$region == 'W MIDLANDS']<-"WEST MIDLANDS"
dat$region[dat$region == 'CAMBS']<-"CAMBRIDGESHIRE"
dat$region[dat$region == 'SHROPS']<-"SHROPSHIRE"
dat$region[dat$region %in% c('NOTTS/DERBS', 'DERBS', 'DERBYS', 'DERBY')]<-'DERBYSHIRE'
dat$region[dat$region == 'SHROPS']<-"SHROPSHIRE"
dat$region[dat$region == 'OXON']<-'OXFORDSHIRE'
dat$region[dat$region %in% c('WAR', 'WARWICK')]<-'WARWICKSHIRE'
dat$region[dat$region %in% c("N'THUMBERLAND", "N'UMBERLND", "N'THMBLAND")]<-'NORTHUMBERLAND'
dat$region[dat$region == 'W SUSSEX']<-'WEST SUSSEX'
dat$region[dat$region == 'OXON']<-'OXFORDSHIRE'
dat$region[dat$region %in% c('SUFFOL', 'EAST ANGLIA')]<-'SUFFOLK'
dat$region[dat$region %in% c('NORTH LINCS', 'LINCOLSHIRE', 'LINCONSHIRE', 'NE LINCOLNSHIRE')]<-'LINCOLNSHIRE'
dat$region[dat$region %in% c('W MID','W MIDS')]<-'WEST MIDLANDS'
dat$region[dat$region %in% c('HEREFORDSHIRE', 'HERFORDSHIRE')]<-'HEREFORDSHIRE'
dat$region[dat$region %in% c('CHESHIRE EAST', 'CHESIRE')]<-'CHESHIRE'
dat$region[dat$region %in% c('BATH & NE SOMERSET', 'NORTH SOMERSET')]<-'SOMERSET'
dat$region[dat$region %in% c('CITY OF BRISTOL', 'AVON')]<-'BRISTOL'
dat$region[dat$region %in% c('EAST YORKS', 'E YORKSHIRE', 'HUMBERSIDE')]<-'EAST YORKSHIRE'
dat$region[dat$region %in% c('NORTHANTS')]<-'NORTHAMPTONSHIRE'


## fix localities for regions with NA 
dat$locality<-trimws(dat$locality)
unique(dat$locality[dat$region == ''] )

dat$region[dat$locality == "LONGTOWN ?"            ] <-'CUMBRIA'
dat$region[dat$locality == "LONGTOWN"              ] <-'CUMBRIA'
dat$region[dat$locality == "STOKE AREA"            ] <-'STAFFORDSHIRE'
dat$region[dat$locality == "BIDDULPH"              ] <-'STAFFORDSHIRE'
dat$region[dat$locality == "CONGLETON"             ] <-'CHESHIRE'
dat$region[dat$locality == "PACKMOOR"              ] <-'STAFFORDSHIRE'
dat$region[dat$locality == "BARNARD CSTL"          ] <-'COUNTY DURHAM'
dat$region[dat$locality == "STONE AREA"            ] <-'STAFFORDSHIRE'
dat$region[dat$locality == "KNYPERSLEY"            ] <-'STAFFORDSHIRE'           
dat$region[dat$locality == "OFF SUNDERLD"          ] <-'TYNE & WEAR'
dat$region[dat$locality == "RIBBLESDALE"           ] <-'NORTH YORKSHIRE'
dat$region[dat$locality == "FLAMBOROUGH HD"        ] <-'EAST YORKSHIRE'
dat$region[dat$locality == "MANCHESTER"            ] <-'GREATER MANCHESTER'
dat$region[dat$locality == "MANCHESTER AREA"       ] <-'GREATER MANCHESTER'
dat$region[dat$locality == "DOCKRAY.CUMBRIA"       ] <-'CUMBRIA'
dat$region[dat$locality == "STOKE CLFD"            ] <-'STAFFORDSHIRE'
dat$region[dat$locality == "PETERLEE"              ] <-'COUNTY DURHAM'
dat$region[dat$locality == "NE WHITBY"             ] <-'NORTH YORKSHIRE'
dat$region[dat$locality == "SCARBOROUGH"           ] <-'NORTH YORKSHIRE'
dat$region[dat$locality == "NEWCHAPEL"             ] <-'STAFFORDSHIRE'
dat$region[dat$locality == "CONSTANTINE"           ] <-'CORNWALL'
dat$region[dat$locality == "LUDLOW AREA"           ] <-'SHROPSHIRE'
dat$region[dat$locality == "NOTTINGHAM"            ] <-'NOTTINGHAMSHIRE'
dat$region[dat$locality == "DONCASTER"             ] <-'SOUTH YORKSHIRE'
dat$region[dat$locality == "YORK"                  ] <-'NORTH YORKSHIRE'
dat$region[dat$locality == "NE ROSEMANOWES CORNWAL"] <-'CORNWALL'
dat$region[dat$locality == "BARROW-IN-FURNESS"     ] <-'CUMBRIA'
dat$region[dat$locality == "CHAPEL-EN-LE-FRITH"    ] <-'DERBYSHIRE'
dat$region[dat$locality == "ISLE OF MAN"           ] <-'ISLE OF MAN'
dat$region[dat$locality == "PENNINES"              ] <-'NORTH YORKSHIRE'
dat$region[dat$locality == "LEOMINSTER"            ] <-'HEREFORDSHIRE'
dat$region[dat$locality == "TALKE AREA"            ] <-'STAFFORDSHIRE'
dat$region[dat$locality == "BRAMPTON"              ] <-'CUMBRIA'
dat$region[dat$locality == "BARLASTON"             ] <-'STAFFORDSHIRE'
dat$region[dat$locality == "WHITBY"                ] <-'NORTH YORKSHIRE'
dat$region[dat$locality == "KEELE AREA"            ] <-'STAFFORDSHIRE'
dat$region[dat$locality == "MANSFIELD"             ] <-'NOTTINGHAMSHIRE'
dat$region[dat$locality == "BARNARD CAS."          ] <-'COUNTY DURHAM'
dat$region[dat$locality == "WINDERMERE"            ] <-'CUMBRIA'
dat$region[dat$locality == "S HELSTON"             ] <-'CORNWALL'
dat$region[dat$locality == "RICHMOND YKS"          ] <-'NORTH YORKSHIRE'
dat$region[dat$locality == "B. AUKLAND"            ] <-'COUNTY DURHAM'
dat$region[dat$locality == "STOKE-ON-TRENT"        ] <-'STAFFORDSHIRE'
dat$region[dat$locality == "LEICESTER"             ] <-'LEICESTERSHIRE'
dat$region[dat$locality == "NE DODMAN POINT"       ] <-'CORNWALL'
dat$region[dat$locality == "NR SCUNTHORPE"         ] <-'LINCOLNSHIRE'
dat$region[dat$locality == "GREATER MANCHESTER"    ] <-'GREATER MANCHESTER'

dat<-dat[!dat$locality == "EBBW VALE",] 
dat<-dat[!dat$locality == "MERTHYR TYD.",] 
dat<-dat[!dat$locality == "HAY ON WYE",] 
dat<-dat[!dat$locality == "LLANDOVERY",] 
dat<-dat[!dat$locality == "WEM",] 
dat<-dat[!dat$locality == "KISH BASIN",] 
dat<-dat[!dat$locality == "OFF TREVOSE HD.CORNWL",] 
dat<-dat[!dat$locality == "SEVERN ESTUARY",] 
dat<-dat[!dat$locality == "S OF ISLE OF WIGHT",] 
dat<-dat[!dat$locality == "OFFSHORE RAMSGATE",] 
dat<-dat[!dat$locality == "LLEYN AFTERSHOCK",] 
dat<-dat[!dat$locality == "EAST OF BALA",] 
dat<-dat[!dat$locality == "SOUTHERN NORTH SEA",] 
dat<-dat[!dat$locality == "LLEYN GWYNEDD",] 
dat<-dat[!dat$locality == "CAERNARFON BAY",] 
dat<-dat[!dat$locality == "LLANDRINDOD WELLS",] 
dat<-dat[!dat$locality == "OFFSHORE SWANAGE",] 
dat<-dat[!dat$locality == "NEWPORT AREA",] 
dat<-dat[!dat$locality == "TREFORREST",] 
dat<-dat[!dat$locality == "BUILTH WELLS",] 
dat<-dat[!dat$locality == "SOLWAY FIRTH",] 
dat<-dat[!dat$locality == "OFF ANGLESEY",] 
dat<-dat[!dat$locality == "OFF TYNEMTH",] 
dat<-dat[!dat$locality == "OFF WHITBY",] 
dat<-dat[!dat$locality == "OFF FLAMBOURGH HEAD",] 
dat<-dat[!dat$locality == "ST GEORGES CHANNEL",] 
dat<-dat[!dat$locality == "ENGLISH CHANNEL",] 
dat<-dat[!dat$locality == "WEST OF ANGLESEY",] 
dat<-dat[!dat$locality == "BRISTOL CHANNEL",] 
dat<-dat[!dat$locality == "NEWPORT GWENT",] 
dat<-dat[!dat$locality == "WELSHPOOL POWYS",] 
dat<-dat[!dat$locality == "SOUTHERN IRISH SEA",] 
dat<-dat[!dat$locality == "SOUTH OF PLYMOUTH",] 
dat<-dat[!dat$locality == "STRAIT OF DOVER",] 
dat<-dat[!dat$locality == "OFF HARTLAND POINT",] 
dat<-dat[!dat$locality == "CWMBRAN",] 
dat<-dat[!dat$locality == "IRISH SEA",] 
dat<-dat[!dat$locality == "CAERNARVON BAY",] 
dat<-dat[!dat$locality == "MID IRISH SEA",] 
dat<-dat[!dat$locality == "LLEYN PENINSULA",] 
dat<-dat[!dat$locality == "SW OF BARMOUTH BAY",] 
dat<-dat[!dat$locality == "5 KM E OF LUNDY ISLAND",] 
dat<-dat[!dat$locality == "CARDIGAN BAY",] 
dat<-dat[!dat$locality == "SE START POINT",] 
dat<-dat[!dat$locality == "ST GEORGE'S CHANNEL",] 
dat<-dat[!dat$locality == "BLAENAU FFESTINIOG",] 
dat<-dat[!dat$locality == "NORTH CHANNEL",] 
dat<-dat[!dat$locality == "ISLE OF ANGLESEY",] 
dat<-dat[!dat$locality == "CARMARTHEN BAY",] 

dat$region[dat$locality == 'LEEDS' & dat$region == 'YORKSHIRE']<-'WEST YORKSHIRE'
dat$region[dat$locality == 'DONCASTER' & dat$region == 'YORKSHIRE']<-'SOUTH YORKSHIRE'



unique(dat$region)
dat<-dat[-which(dat$region==''),]

write.csv(dat, file = 'frack-clean.csv')