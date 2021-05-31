# Electoral Accountability and Particularistic Legislation: Evidence from an Electoral Reform in Mexico
#Author: Lucia Motolinia*/
#Replication Code for Cleaning the Text data and generating the Correlated Topic Model

###########################
#### CLEANING TEXT DATA####
###########################

rm(list = ls())
memory.limit(16000)
setwd('D:/Users/Lucia/Dropbox/NYU/Papers/Reelection and promises update/Diputados Locales')
library("koRpus")
library("quanteda")
library("devtools")
library("quanteda.corpora")
library("MASS")
library("readtext")
library("stringi")
libraries <- c("devtools","quanteda.corpora","MASS","quanteda","Matrix","readtext","lda","stm","bursts","lattice","ldatuning", "topicmodels", "ggplot2", "dplyr", "rjson", "quanteda", "lubridate", "parallel", "doParallel", "tidytext", "stringi")
lapply(libraries, require, character.only = TRUE)

set.seed(12345678)

maketok<-function(y){(tokens(char_tolower(y), remove_punct = TRUE))}
makedfm<-function(x){(dfm(x,stem=F,remove_punct = TRUE, remove = stopwords("spanish")))}
removenonchnum<-function(z){stri_replace_all_charclass(z,"[^[:alpha:]]"," ")}
removeaccents<-function(w){stri_replace_all_fixed(w, c("á","í","ú","é","ó"), c("a","i","u","e","o"), vectorize_all=FALSE)}

all<- get(load("all.RData"))
all$cleannum<-sapply(all$text, removenonchnum)
all$cleannumna<-sapply(all$cleannum, removeaccents)

#Removing names of Legislators
namesleg<- read.csv("nombres.csv")
nl<- namesleg[,2]
nl<- unlist(stringi::stri_extract_all_words(nl),"")

nlna<- namesleg[,2]
nlna<- sapply(nlna, removeaccents)
nlna<- unlist(stringi::stri_extract_all_words(nlna),"")

#Removing municipios
namesmun<- read.csv("municipios.csv")
nmun<- namesmun[,2]
nmun<- unlist(stringi::stri_extract_all_words(nmun),"")

nmunna<- namesmun[,2]
nmunna<- sapply(nmunna, removeaccents)
nmunna<- unlist(stringi::stri_extract_all_words(nmunna),"")

#Removing non words and stopwords
stopwd<- read.csv("stopwords.csv")
stwd<- stopwd[,1]
stwd<- unlist(stringi::stri_extract_all_words(stwd),"")

gent<- read.csv("gentilicios.csv")
gent<- gent[,1]
gent<- unlist(stringi::stri_extract_all_words(gent),"")

#Cleaning other names and common words in the context of legislative discussions
custom00<-c('deaquino','pelaez','ulloa','cueva','silvano','rossana','rosaura','añorve','neil','vicario','cisneros','legarreta','eloisa','yuridia','falcon','ociel','hugar','resendiz','vadillo','flavia','irving','ensaldo','antelmo','granda','crescencio','lxm','dfcl','dtl','muiküsiatura','kilslajura','muitcisiatura','cüxgrtso','conurbso','escolin','stado','medellin','ivea','fstado','fais','zapot','manterola','sheridan','kirsch','sainz','zuzuki','montijo')
custom01<-c('diputado','dip','c', 'h','diputada', 'honorable', 'congreso', 'asamblea', 'solemne', 'ordinaria', 'extraordinaria','sesion', 'diario','debates', 'estenografica', 'version', 'legislatura', 'presidente', 'articulo','anexos', 'anexo', 'acuerdo', 'fraccion', 'lectura', 'presidenta', 'p', 'n', 'i', 'pa', 'diputados', 'legislativo', 'año', 'hoy', 'gracias','dia','presidencia', 'aqui', 'pues', 'creo', 'si', 'periodo', 'josé', 'ver', 'quiero')
custom02<-c('gto', 'diputadas', 'recibo', 'escrito', 'lviii', 'mesa', 'directiva', 'secretario', 'f', 'agrego', 'cumplimiento', 'uma', 'ap', 'obl', 'lat', 'der', 'séptima', 'sexagésima', 'secretaría', 'gustavo', 'julio','guylaine','alcira','abdon', 'ustedes', 'herrera','teran','vernon','shirley','mancil','rincón','mendivil','maximiliano','sergio', 'voz', 'junio','amabel','muñiz', 'fernando', 'eriselda', 'licon', 'padilla', 'documento', 'legislador')
custom03<-c('punto', 'secretaria', 'mas', 'tambien', 'tema', 'entonces', 'bueno', 'informe', 'uso','articulos', 'correspondiente', 'va', 'm', 's', 'd', 'periodo','dijo', 'r', 'm', 'b','parlamentarios', 'discusion', 'parrafo', 'numero', 'desahogo', 'ciudadano', 'ciudadana', 'acta', 'mediante', 'oficio', 'manifestarlo', 'hacer', 'estan', 'ser', 'compañeros','compañero','rodríguez', 'ejercicio', 'sesión', 'dio', 'seguidamente', 'mayo', 'agosto','insúa')
custom04<-c('primer', 'primera', 'segunda', 'segundo', 'tercer', 'tercera', 'cuarta', 'quinta', 'quinto', 'sexta', 'septima', 'ii', 'iii', 'fecha', 'permanente', 'presente', 'tema', 'sexagesima', 'usted', 'parlamentaria', 'iniciativa', 'aprueba', 'propone', 'particular', 'lx', 'lxi', 'lix', 'parlamentario', 'conocer', 'momento', 'solicito', 'mano','muchas', 'esten', 'pro', 'cano', 'lopez', 'pdte', 'torres', 'gutierrez', 'así', 'comisión', 'solicitado')
custom05<-c('maria', 'cesar', 'felipe', 'jose', 'martinez', 'ramirez', 'rodriguez', 'asi', 'mendoza', 'rocio', 'blanca', 'gonzalez', 'saenz', 'anguiano', 'ocupa', 'señores','señor','señora','señoras','perez', 'hernandez', 'manuel', 'guadalupe', 'carlos', 'luis','siguiente', 'rivera', 'mesina', 'anterior', 'leticia','palabra', 'lxvi', 'lxv', 'lxiv', 'arroyo', 'legisladores', 'legisladoras','jesus', 'alonso','núm','tribuna', 'hernández', 'martínez')
custom06<-c('foja', 'legislativos','w', 'manera', 'dar', 'proyecto','dictaminacion', 'consideracion', 'presentada', 'aprobacion', 'sesión', 'comisión','rodríguez', 'horas','decir', 'vamos', 'adelante', 'srio', 'hacerlo', 'valdovinos', 'terminos', 'lxiii', 'rubrica', 'sello', 'demas', 'hace', 'sexagésima', 'artículo','sirvanse', 'diputacion', 'legislativa', 'asistencia', 'sesiones', 'orden','rangel', 'ortiz', 'solo', 'ahi', 'parte', 'martha')
custom07<-c('karina', 'diana', 'alberto', 'miguel', 'velazquez', 'miguel', 'pag', 'número', 'sanchez', 'pregunta', 'ma', 'asunto', 'medina', 'cuanto', 'caso', 'informo', 'lista', 'minutos','integrantes','sirvase', 'manifestar', 'levantando', 'ceballos', 'federico', 'contreras','rafael','flores', 'lxii', 'día', 'dos', 'pablo', 'dado', 'garcia', 'francisco', 'gloria', 'grisel', 'mejia', 'veces', 'borrego', 'lira', 'debera', 'juan', 'dice', 'ahorita')
custom08<-c('septiembre', 'octubre', 'aguilar', 'gonzález', 'tener', 'antonio', 'santiago', 'loya', 't', 'antonieta', 'carmen', 'tac', 'dicho', 'rosa', 'bañuelos', 'abraham', 'ana', 'octavia', 'receso', 'van', 'gente', 'ia', 'ios', 'ei', 'ruiz', 'fernández', 'javier', 'galindo', 'sosa', 'figueroa', 'lxviii', 'robles', 'rogeio', 'chavez', 'hugo', 'pr', 'v', 'puede', 'cristina', 'rúbrica', 'vega', 'pv', 'tellez', 'marin', 'tovar', 'portillo', 'vallejo')
custom09<-c('nadia','terrazas','xochitl','maribel','prieto','hever','ontiveros','frias','quezada','maynez','bencomo','amaya','siqueiros', 'crystal','valenciano','citlalic','malaxechevarria', 'xxix','zambrano','desearan','balboa','manlio','zentella','utrilla','atila','sometiera','bolon','gamas','madrazo','isset','soler','zurita','somellera','ordorica','silbestre','mollinedo','dagdug','solange','asmitia','lutzow','rebelo','berdeja','agraz','gretel')
custom10<-c('dunyaska','kuri','malpica','copete','saut','carola','griego','zenyazen','bingen','judith','zuccolotto','amado','yazmin','feito','lobato','cinthya','regina','chazaro','tanya','torales','roldan','unanue','picazo','cueto','izq','schuller','gurrion','atristain','vibsania','basaldu','matias','ifbwn','ifcisi','donovan','cerqueda','kssi','carol','leslie','nvti','vignon','fcñkí','olivera','miv','zavaleta','irineo','obvie','carreño','infanzon')
custom11<-c('geovani','ceaipes','urias','yudith','osuna','melesio','cuen','irizar','imaz','yudit','habiendose','angely','xiclali','elota','gomer','zatarain','pijijiapan','coutiño','peñate','adabache','santiváñez','isadora','santivañez','bugarín','lizalde','geovanna','oropeza','berrelleza','castorena','tizcareño','cabral','osvaldo','zamarripa','roy','isaura','nalleli','lyndiana','mavela','alcaraz', 'fs','francis','bbb','alvear','noviembre', 'enero')
custom12<-c('reveles','olguín', 'badillo','celia','dávila','iris','góngora','román','bañuelos','bugarin','enríque','benítez','jáquez','maximiliano','ati','nevárez','samaniego','gerardina','silero','huizar','adán','hjaxl','ever','nápoles','marisol','longoria','gavilán','lxvil','grecia','triana','dorador','campuzano','vitela','lxvii','durangow','salum','soría','don','escribio','osafig','op','assam','dgch','pdta','nnnnri','antero','yescas','culin','trillo')
custom13<-c('perea','murrieta','almada','manzanedo','carrazco','robinson','bours','armenta','gildardo','coboj','mireya','chang','durazo','cota','agramon','everardo','perla','navarro','nieves','dunia','rubiolo','pirin','lescas','corchado','texta','jany','vania','yannette','campa','atayde','rebeca','mancera','paulo','alavez','fisher','aleida','velarde','acevedo','nury','xxxiii','anilu','rat','raf','hh','victoriano','levet','renato','arq','muitcisiatura')
custom14<-c('gibb','corro','gudiño','arango','habeis','todavia','vaya','hagamos','veo','hablando','venga','habla','hablan','hablo','dice','diga','ayer','quiere','quiera','ojala','voy','siquiera','decirles','digo','decia','venir','van','dije','iba','dicen','nadie','parece','quieren','oliver','montivar','icaza','montaño','velez','salinas','arizmendi','castrejon','cristino','canuto','febrero', 'marzo', 'abril','zúñiga','zúñíga','alvear','xxiv','valli')
custom15<-c('pec','estásmíe','lxhi','fgislatura','suz','tph','tumoto','ixiii','estátos','liave','hr','gomsa','gb+A11A10:B1A9:B16','articuio','aab','dei','rp','ei','ia','zumaya','asambeia','xaiapa','dell','eladio','ias','k','mm','infe','rior','monsalvo','mirian','raymundo','vergara','casasola','vladimir','bolnik','sue','ellen','manzur','rellstab','carreto','smd','subtítulo','aquí','prácticamente','después','ahí', 'nevarez', 'antonieta', 'franco', 'pn')
custom16<-c('durrngo','hacía','además','cómo','dcl','fernández','cepeda','achem','nogueira','aurelio','julián','barragán','félix','rosales','raúl','méndez','cardiel','núñez','oviedo','rivero','bonilla','lujan','israel','agustín','frac','chih','andreu','deba','eunice','angli','roger','arizmendi','aburto','delfina','nicanor','heladio','acapulco','sotelo','villalva','majul','goicochea','eli','benjamin','garnelo','jalil','karen','gallegos','abelina','peralta')
custom17<-c('cjon','ca','kilslajura','dela','riult','florián','licet','larios','ladino','govea','sevilla','joel','tena','crispín','andrés','nicolás','crispin','florian','oregón','oregon','neri','rivas','sosa','graciela','zepeda','lucía','magaña','mirna','lozano','benavides','cárdenas','daesy','atali','herlinda','iñiguez','alvorada','ayde','proal','aleman','mungia','tapia','sofia','qro','hinojosa','eric','lazaro','aracely','corregidora','rubio','pliana')
custom18<-c('isaf','mod','etca','arizona','only','kx','arnaldo','loeza','avilez','baqueiro','ermila','may','adda','cachon','quijano','ocaña','ceh','olimpia','montufar','ferrer','ake','quiñones','baudelio','aurora','xool','procell','mazcorro','lazuna','yulma','galo','juventino','lango','elizondo','bocanegra','rabago','georgina','villalpando','jato','alejandrina','carlo','barreñada','videgaray','miranda','apaseo','canul','dzul','uc','teida','tun','ita')
custom19<-c('romellon','santoyo','bernes','noel','miera','vila','karmen','aída','lisette','brown','lina','cid','escobosa','lam','célida','aida','godínez','chiu','kitty','pavlovich','acuña','celida','buelna','clark','palafox','fu','palafox','mazon','mazón','homero','fermin','emeterio','papayan','ballado','doring','yuriri','gaviño','ariadna','ambriz','chairez','polimnia','magos','sesma','amoros','priscila','guaida','taboada','llerenas','ondarza','santillan')
custom20<-c('rovira','padierna','olivia','dinorah','miron','gurza','ernestina','poblete','portuguéz','abad','norberto','yurixi','elda','couttolenc','castilla','irad','zayas','annel','mazo','jaffet','millán','arzola','apolinar','elvia','marrón','portuguez','posada','ganza','higinio','yáñez','garnica','monroy','jocias','castrejón','üo','tumo','ce','ffl','tn','rn','kehila','secretar','dd','abigail','num','dada','ario', 'almodova','femat','yulenny','ignacia')
custom21<-c('cliserio','regis','haro','déras','érica','buendía','pedroza','luévano','cantú','galván','ruvalcaba','calderón','nohemí','balderas','xóchitl','márquez','issstezac','deras','luevano','zac','hurtado','figueroa','buendia','simey','viggiano','daladier','cardón','gadoth','coradalia','zurutuza','mayka','perusquía','galland','ábrego','ruperto','crisóstomo','carpio','mabel','calva','zamorano','morato','austria','hidal','zerón','matilde','dé','muñoz')
custom22<-c('ramiro','coello','soriano','ricci','diestel','valdiviezo','ancheita','lilian','kanter','legisladora','lucrecia','leído','valera','límbano','levanto','pedrero','hortencia','saín','pola','ahued','lac','salarlos','bardahuil','mariela','julen','mic','ciro','vpracru','heber','landeta','gladys','baxzi','gandara','leandro','antonino','bringas','carballo','alan','guadarrama','bahena','hoyos','zairick','garrido','morante','sedas','ed','licea')
custom23<-c('calyecac','cortero','xicohtencatl','stankiewicz','baldemar','carlota','erendira','zenteno','evangelina','axayacatzin','xochitiotzin','ortencia','maura','elsa','sinahi','orea','atonal','zainos','cuatecontzi','sesin','cuahutle','pág','nov','pamanes','plazola','morones','bañales','indepte','rivial','jun','édgar','borbolla','petersen','justin','monraz','farah','consuelo','corvera','chivas','anahi','chavira','jal','sepulveda','nadiezhda','kú')
custom24<-c('riestra','raesfeld','itottion','serteioi','jiiiaéoi','cotoñeto','tanús','natale','budib','maiella','coriche','buxadé','nácer','doger','eeroitioa','regordosa','quincuagesimo','gali','lejiilatoi','zoletto','opadr','buxade','abaid','zeferino', 'nakamura','colomos','secretar','ucea','cham','beas','machin','atengo','trewick','kumamoto','anexonúm','lea','andalon','cecilia','ornar','isaias','aubry','camarena','rn','fela','it','rocio','jo','jaliscien')
custom25<-c('giner','mayra','rosemberg','anchondo','chaparro','compean','villalobos','jauregui','adalberto','andreu','hortensia','avitia','tarin','eloy','soraya','murguia','lardizabal','tania','rogelio','elvira','lilia','rascon','gil','trevizo','iciplam','araujo','garduño','ci','zebadúa','estefan','villatoro','garcés','bielma','cal','máximo','valente','chapas','simán','rhosbita','chilon','castañon','williams','cobos','sria','donaldo','pinto','anel')
custom26<-c('penagos','bienvenido','lanuza','chico','saavedra','rendon','karla','érika','cario','zavala','arreola','í','villagran','barrenada','carmona','guillen','solano','bello','obregon','arredondo','bernardi','vasquez','eva','arellanes','atura','casanova','toribio','ciilceo','zonia','samuel','melgar','cerecedo','xciii','sylvia','uzeta','insua','urritua','lucila','urincho','dione','espino','fragoso','verduzco','castañeda','aristoteles','xalapa','er')
custom27<-c('paola','sl','gopar','nallely','virginia','ijcisi','callejas','ambell','yunes','uscanga','amaranta','barradas','corbala','racru','abascal','camerino','buganza','esquitin','chiunti','llamas','munguia','josefina','arteaga','marques','amoles','zuñiga','mauricio','zapata','cadereyta','jaqueline','macías','silerio','rnr','gina','soria','marina','elia','delgado','alma','oliva','ávalos','in','rocha','rodolfo','quiñónez','rigoberto','moya')
custom28<-c('fidel','quiñonez','rr','silvia','isela','ojeda','jiménez','blum','calakmul','lavalle','carrizales','sansores','janini','calkini','valladares','hopelchen','champoton','pinzon','nordhausen','negrin','bazua','salcido','payan','payán','linares','valdes','valdés','fermín','rochin','etv','cazares','brenda','epifanio','teresa','prieta','peñasco','fernanda','moisés','daniela','pacheco','marlen','águeda','corral','caro','jaquez','bladimir','bautista')
custom29<-c('rodrigo','elias','varela','paura','tels','monarrez','g','diez', 'iv','morales','eruviel','lerdo','bastida','corroviñas','tassio','anuar','bernardino', 'garza','quiroga','cheja','agundis','areli','irazema','baz','gusmán','yomali','guéz','ind','godoy','granados','moguel', 'arevalo','casar','ballesteros','barcena','esthela','antuna','candia','mancill','michel','angelina','uriegas','art','ferat','zarrabal','ponton','dp',	'roclo','zarur')
custom30<-c('racruz','sg','do','ingrid','cedaw','guadiana','caldera','arco','dicie','cantu','pleg', 'rea','rfcnt','aragon','alfaro','liliana','isela','irene','loera','sáenz','rene','boton','macias','pe','amelia','ruben','exdec','villareal','rfley','monica','imelda','anavel','carrete','rosauro','kampfner','gurrola','saracco','meraz','matuk','saucedo','sifuentes','cuitláhuac','valenzuela','cstrdo','pizarro','eusebio','durñngo','avalos','bustos','eréndira')
custom31<-c('meza','iván','alicia','soto','silva','gamboa','encarnación','luján','jacquez','césar','beltrán','adame','esquivel','susana','mota','soledad','lizardo','cuauhtémoc','gilberto','eugenia','yassmín','bibiana','oretza','anaya','elisa','irene','cricp','nohemi','ipadr','cupertino','lauro','geraldine','germán','lauro','aréchiga','camelia','dirtctian','salomón','elías','lixlegislatura','alejo','eleofermes','novena','dgo','albarran','florentino')
custom32<-c('gallan','dalgo','óe','zeron','idalgo','qu','becerril','lá','lizbeth','pelayo','digelag','grullo','cesjal','vaca',	'mayela',	'mónica',	'cortés',	'serna',	'cristerna',	'escobedo',	'lorena',	'salaverna',	'olguin',	'julia',	'lupita',	'arcelia',	'barragan',	'lesli',	'domínguez',	'yahaira','cardona',	'sandoval',	'joan',	'valparaiso',	'cardon',	'charrez','cipriano','olvera','marcelino','burgos','icedo','elorduy','castelán','dora','miguelito')
custom33<-c('carrillo',	'froylan','rabelo',	'filigrana',	'herrada',	'corrales',	'lazo',	'lanz',	'dajtaip',	'tcya',	'zoila',	'rueda',	'rosendo',	'charles',	'calderon',	'caballero',	'irvin',	'mandujano',	'andrade','marroquín',	'fayad',	'cerón',	'efrén',	'vieyra',	'villeda',	'saab',	'alamilla',	'baca',	'eguiluz',	'canek',	'perusquia','estefany','morón','narciso','ancira','tito','ivonne','barrón','gisela','amador','lastiri','maritza','samano')
custom34<-c('ip',	'abundar',	'hidalg',	'ubre',	'aracelivelázquez',	'hll',	'marcela',	'vázquez','éctor',	'dennise',	'paniagua',	'bazaldua',	'barba',	'ledo',	'libia',	'villagomez',	'manrique',	'landeros',	'brunel',	'fonseca',	'aseg',	'isidoro',	'grimaldo',	'sagrario',	'constantino',	'villafaña',	'cerrillo',	'yuriria','origel',	'breton',	'ángelo',	'serafin',	'ascencion','mastranzo','luque','vela','cordova','córdova','luvianos','abel','serrato')
custom35<-c('valdivia',	'zarate',	'cova',	'floria',	'mendieta','briones','rogers','mileidy',	'casilda',	'neyda',	'alipio',	'silvan',	'mirella',	'ivette',	'taracena',	'bosch',	'alaniz',	'michaus',	'guel',	'eudave',	'anayeli',	'patricio',	'saldivar',	'torruco',	'ags',	'sabino',	'uriel',	'madrigal','anexonum',	'anda',	'letipichia',	'od',	'tfl',	'cr',	'palomino','ct','cn','lizbeth',	'jalisca','lev','berumen','konstanza','x','diciembre','villa')
custom36<-c('vallaria',	'casian',	'jahaziel',	'zabdiel','tintos','lino',	'sindicalizada',	'quincuagesima','orozco','villalvazo','arias',	'nicolas',	'col',	'gabriela','presidenteesta',	'presidenteen',	'pucheta',	'atondo',	'carrizoza',	'roque',	'robespierre',	'chaidez',	'osbaldo',	'nubia',	'navolato',	'gastelum',	'garzon','renata',	'leobardo',	'manjarrez','myriam','simental','emery','cordourier','tocitica','flavino','jara','escalante')
custom37<-c('rosalío',	'hl',	'cornejo',	'celestino',	'pedraza',	'ruíz',	'durán',	'víctor',	'abrego',	'barraza',	'indalecio',	'xantolo',	'rosalio',	'cuéllar',	'rocío',	'luciano','astudillo','cantoran',	'peñaloza',	'evencio',	'alarcon',	'carabias',	'florencio',	'dimayuga',	'salgado',	'liborio',	'gatica',	'catalan',	'edilicias','artee','agramón',	'terán','valdéz','baltazar','berenice','colin','diego','narváez','lucia','benavidez','nabor')
custom38<-c('adela',	'castell',	'mendívil','valeria',	'miriam',	'borja',	'amorós',	'irais',	'salido',	'orlando',	'cipactli','huanosta',	'pinal',	'abadia',	'ludlow',	'espina',	'vite',	'mateos',	'delgadillo',	'rubalcava',	'wendy',	'betanzos',	'jimeno',	'peredo',	'citlalli',	'menez',	'roxana','janet',	'pardo',	'deloya',	'penelope',	'aguilera',	'nora',	'cravioto','dario','cházaro','vasconcelos','gregorio','hidau','pooer','odeh','nodele','deh')
custom39<-c('choperena','dél','armendariz','mastretta','carolina','trejo','santana','tello','yesenia','ku','arana','co','suárez','ivone','müller','paot','alvaro','eliseo','león','po','guachochi','licón','villareal','bocoyna','camargo','moel','moriano','nn','sare','quila','angelica','code','and','hiram','rovelo','erubiel',	'caceres',	'jovita',	'dasp','rf','du','loris','ü','siop','blp','nodelestadodeh','ern','narcía','hamr','tet','tlax','delfino','barranco')
custom40<-c('cu','barca','co','cuquio','oct','zula','oa','itei','minor',	'sampedro',	'piñon',	'aitzury',	'zárate',	'bretón',	'montiel',	'ordoñez',	'corona',	'popocatl',	'nahum',	'tecuapacho',	'mena',	'villard',	'fabiola',	'sabines',	'anzueto',	'sasil',	'villaflores','waring','marlene',	'uuh',	'edda',	'silverio','quevedo',	'briceño',	'asuncion','facundo','cauich','seme','veracru','silvio','landa','daesyalvorada','toño','mauro','rosillo','aide')	
custom41<-c('tenabo	','rosado',	'carmelitas',	'sabancuy',	'escarcega',	'aguada',	'dzitbalche',	'montalvo',	'barona',	'haydar','nnnrin',	'lllllililil',	'garfias','montañez',	'cedillo',	'najera',	'esparza',	'monteros',	'tejada','alejandre','leos',	'varona',	'violeta',	'davila',	'vf',	'melendez',	'baez',	'talamantes',	'morquecho',	'amezquita','duron','olayo','tony','néstor','opoisq','amfnto','jáuregui','demetrio','tromucgacion','mdprsa','wasi')
custom42<-c('angélica',	'uribe',	'aguirre','servando',	'gilda',	'tello',	'verónica',	'omar',	'ruiseco',	'elena',	'teul',	'cañitas',	'secretariadiputada',	'amalia','renteria',	'damian',	'pt',	'bertha',	'victorhugo',	'efrain',	'genaro',	'pizano',	'vidal',	'richard',	'roehrich',	'covarrubias',	'pan',	'blancas',	'agustin','von',	'zenorina',	'denisse',	'petriz',	'mejía','banck','mdppsosa','nmejiah','amento','ño','pari','uo','erto','cynthia','xm')
custom43<-c('arechiga',	'manzanilla',	'garcilazo',	'escorza',	'lucio',	'saturnino',	'filiberto',	'zaldívar',	'manzo',	'lorenzini',	'santamaría',	'blas',	'conuremj',	'zaldivar',	'legist',	'blancarte',	'piña',	'venancio',	'codina',	'izea','ordinariotercer',	'teul',	'ordinarioprimer',	'hilda',	'rafa',	'ivan',	'trancoso',	'vetagrande',	'morismas','adrián',	'juana',	'mugica',	'ayala','nt','nmej','oe','camberos','villarino','balleza','laura','máynez')
custom44<-c('durangolllr','gavilan','aispuro','maciel','arandas','eos','aguayo','on','mezcala','trelles','barbosa','gobernadora','bazúa','cázares',	'rochín',	'guillén',	'reyna','olivares','mercedes',	'godinez',	'imuris',	'sara','peregrina',	'céspedes',	'gaudencio',	'dgajepl',	'cirilo',	'mier',	'giorgana',	'rendón',	'eukid',	'alvizar',	'piña',	'quincuagésimo',	'irma','mociños',	'nelyda','baruch','estela','candelaria','ih','fortoul','osornio','dolores')
custom45<-c('ildefonso',	'juvenal',	'roa',	'tinoco',	'astorga',	'carbajal',	'ariel',	'presidentadip',	'prosecretario',	'huitrón',	'entzana',	'cortazar',	'bernal','teta',	'dic',	'ene',	'vll',	'feb',	'lepétim',	'vlll',	'aerbitiosi',	'vl',	'üirtctian',	'be',	'diodoro',	'beauregard',	'll',	'lv',	'osorio',	'gérman',	'tanus',	'atrbitios',	'chedraui',	'feral','zarzosa','azuz','chacon','villegas','izontli','mondragón','topete','huízar','ss')
custom46<-c('leeder','xliii',	'rojas',	'priego',	'yolanda',	'lizarraga','lfcisi','eufrosina',	'jefte',	'calvo',	'nvm',	'remedios',	'herminio',	'neli',	'mercedes',	'ericel',	'rosalia','henestrosa',	'aquino',	'iwtl',	'mivn',	'vnu',	'altamirano','encarnacion',	'díaz',	'solis',	'amaro',	'solís','enríquez','beatriz',	'emilio',	'bernardo','jesús','montebello',	'tina',	'henestroza','cstfldo','durangolsr','jh','ochoa','fp','adan','maxilimiano','camed')
custom47<-c('chiva',	'ebates',	'rulfo',	'rebaño','huehue',	'luvina',	'aranzazu',	'everaert',	'luminica',	'barron',	'lizarraga',	'alcantara',	'rojo',	'uriaspresidencia',	'eldorado',	'monterogüido',	'lucas',	'lucero',	'montoya',	'elina','vallines',	'rementeria',	'tronco',	'ledezma',	'garduza',	'minerva',	'belen',	'mazariegos',	'rementería',	'vt',	'veracr',	'uz',	'salcedo','mata',	'estád',	'gonzalo','rutiaga','elizabeth','viii','xv','vi','limbano')
custom48<-c('garay','ahsamw',	'tarj','heriberto',	'noe',	'valencia',	'dgg',	'oscar',	'santos','meneses',	'licda','noé',	'leal','insuvi',	'barajas','janeth',	'olmos',	'teresita','ángela',	'mariana',	'salmeron',	'murillo',	'dulce',	'cuevas',	'mancilla',	'deschamps',	'arenas',	'barros',	'hipolito',	'basilio','camacho','om','julieta','azueta','parra','erika','magdalena','olea','evodio','jonathan','cabrera','chong','iliana','mijito','bc','campillo')
custom49<-c('néti','menes','cob','alrango','zenori','miercoles','olegario','amet',	'rovirosa','yumka',	'abner',	'troconis','velasquez',	'amando',	'bohorquez','simon','anselmo','murat','horacio','gabino','aparicio','saldaña','matus','itaisa','guzman','lety','veronica','galvan','acu','luna','eleuterio','castro','ase','gaxiola','crecenciano','montoyapresidencia','gusman','fontaine','portu','conzuelo','yañez','nancy','echegoyen','marlon','juárez','rincon')
custom50<-c('burgueño','carrancio','jucopo','fredy','japac','ruelas','gaspar','edilicia','valentin','smutny','efren','german','patron','olaguer','mondragon','rafaela','serrano','galindez','farias','jsrc','castañón','esponda','valanci','tzimol','rodulfo','chi','fatima','ileana','morfin','tenabo','christian','gp','tonatiuh','escamilla','angeles','santillán','romana',	'curul','erik','jeronimo','octavio','enriquez','mura','arzate','evelyn','ahorne','meray')
custom51<-c('difem','piñón','pichardo','marron','jocías','harinas','clamont','ix','xiv','xi','xii','rgi','qui','lti','ig','cuellar','aci','rú','avilés','dfl','pi','ue','an','qe','rfi','castelan',	'llaguno','néti','jalis','mebs','jpma','ddgg','attvos','rrs','nuiviero','cienses','rehila','modiano','lmvo','numercl','gb','ingram','nr','so','ferroso','segundoreceso','úrsulo','emilia','tivt','congresochihuahua','adriana','nahomi','pedro','sffiüfjzft','cienfuegos')
custom52<-c('ovalle','adolfo','lara','ortega','lyndi','auza','paula','villarreal','ibarra','araceli','beltran','landin','sebastian','melchor','blanco','salomon','moises','galeana','pilar','flor','guevara','quirasco','sonoyta','horcasitas','huachinera','guizar','natalia','lej','servin','monreal','osiris','casso','pineda','yanira','alpizar','colindres','galvez','hill','carbo','anilú','lib','menica','anahí','cantero','gálvez','lazcano','fac','qna','tir','all')
custom53<-c('acosta','chávez','cos','velázquez','pepe','ordinariosegundo','ismael','panfilo','cervantes','empresazac','uaz','natera','brindis','enrique','jp','cep','arellano','barrientos','espinosa','edgardo','carreón','gerardo','inés','jueves','octava','carranco','alcántara','evelia','lizeth','cespedes','julian','solana','leyva','serra','retaro','carmelita','otero','ezequiel','oswaldo','alba','carreon','ulises','paloma','nidia','zaachila','nayeli','nucamendi')
custom54<-c('ildelfonso','zebadua','culebro','adacelia','dimas','aldf','gro','http','zmg','cj','ocampo','esperanza','lisbeth','estrada','ávila','chema','gongora','ichitaip','noriega','suarez','case','carrasco','nestor','bastidas','zazueta','inzunza','lopezvocal','presidentaen','presidenteno','sinuhe','castropresidencia','loaiza','esther','enna','echavarria','bolonchen','atasta','obrador','prego','jorge','gj','www','pdf','ai','guadalupano','º','ieez','lós')
custom55<-c('nunkini','crisanty','loaeza','becal','osorno','pomuch','tancoco','ixhuatlan','chocaman','naolinco','misantla','huatusco','perote','oluta','altotonga','huayacocotla','tantima','nava','margarita','héctor','meltis','momax','salas','tepetongo','bracho','yassmin','obierno','én','estaoo','lqs','edith','gabriel','victor','estrada','massieu','bravo','dpl','iza','int','comp','rot','usq','vilma','esteban','romero','ricardez','leonardo','cue','san','podep')
custom56<-c('arsenio','monteagudo','servien','jalpan','villar','catalán','almanza','lechuga','vilchis','bedolla','garita','colorada','ramón','plutarco','arizpe','kino','narconon','pesqueira','tarimoro','isseg','sppjp','beltrones','munro','sotomayor','neacion','rommel','franz','fone','ojinaga','okay','anua','ehuan','jannette','go','sámano','eleazar','delia','xavier','ovando','ebrard','amieva','rev','mts','albarrán','huerta','cote','jaimes','cil','saul','itzel')
custom57<-c('monge','cea','cecop','lugo','ojuelos','gordiano','arceo','brizuela','mivtl','vri','mew','ctapb','rfdec','dds','obdulia','burguete','santini','castaños','ímaz','pas','barroso','carr','oquitoa','bacerac','suaqui','isssteson','bavispe','opodepe','maría','hlb','cerna','ángeles','espinoza','peñamiller','romos','sada','melo','árcega','narvaez','aquiles','benjamín','inocencio','gasca','colín','mendiola','alisco','cardonal','ooeh','trinidad','huepa')
custom58<-c('almazan','uset','acoltzi','sanctorum','xicohténcatl','gracia','amparada','mcj','lxt','parrara','ialisco','ctj','cedhj','folej','neftali','santiz','viridiana','marquelia','heliodoro','saúl','varez','ál','marlón','mazutti','carla','mismos','hacen','buen','lunes','martes','miercoles','jueves','viernes','sabado','domingo','tengase','remita','remitida','remitido','recinto','indicada','elabore','siguientes','conforme','llevar','mes','cinco','veinte')
custom59<-c('favo', 'deseasen','señalando', 'favor','hora','haberse','pasa','becerra','ff','siguientes','tomar','añoscomo','auince','spto','antici','capítulovi','dipuatdos','lugar','pasa','continua','despues','misma','dentro','pido','pase','nuevamente','obstante','elabore','vii','dispuesto','objeto','deja','continue','solicita','treinta','cabo','acto','forma','lugar','nacho','ochavo','ils','ponerlo','indicando','choix','quieran','deseen','anotarse','separando')
custom60<-c('jaime','tomo','formulan','anexa','continúa','incluye','procedemos','turnados','signado','asignado','asiente','continúa','ábrase','pasan','comunica','indiquenlo','part','saitama','apertura','clausura','sirvan','dias','tendra','pasar','buenos','vez','toda','inicio','hagan','apertura','mes','celebrada','encuentra','tendra','castillejos','baldomero','pide','mande','catorce','veintitres','dirige','personals','orrecitia','ncial','maldanado','irvng')
custom61<-c('jacobo','fraestmc','ajpao','egitimo','enpteo','exsactora','fraestmc','tel','ext','cp','clausuras','ing','tomen','inicia','poniendose','loza','jul','abr','dirija','índice','anunciada','recibidas','recibida','recibidos','recibido','oo','oníjklsí','ago','ene','feb','may','jun','sep','oct','nov','dic','sanz','soberon','quieran','iniciando','inscritos','inscribirse','reservar','pasamos','siguiendo','dispensan','dispensando','pasa','suficientemente')
custom62<-c('sertitioi','dara','desea','pido','discutido','establecido','declara','diera','declarado','hubiere','seguida','sometido','disponiendo','acusar','procedio','iniciaron','mencionan','determinandose','determinaron','obteniendo','observados','observado','observadas','observada','anuncie','anote','comenzando','pasaremos','quedan','procedan','procederemos','indicado','hecelchakan','renedo','reitera','reiterar','reitero','vigesima','previsto','prosiguio','leguas')
custom63<-c('jf','prosiga','chal','integrada','asuntos','doce','coordinadores','coordinador','coordinan','diputación','conducente','deliberante','provea','publiquese','sirvan','admite','enterada','exposición','período','rogando','protestáis','hiciéreis','leido','vigésima','trigésima','poner','cuadragésima','extraordinarioprimer','velardeña','mw','turna','nota','anota','desee','sha','acaba','actual','síntesis','sintesis','banny','sesai','nybia','deja','hallaron')
custom64<-c('barrientes','reynalda','lome','insertare','llanes','deje','dejes','unises','obed','notar','escucharemos','someteremos','haremos','daremos','petit','pidio','lincoln','bocardo','phillips','noemi','forman','slativo','íñiguez','ongreso','ierno', 'dk','cda','ampl','secc','prol','ecol','inst','viguri','opcg','procedo','sp,eta','ugalde','muchisimas','encontrandonos','invocado','traera','pla','determinando','someta','procedo','agradecer','ing','mucha','darle')
custom65<-c('emiliano','campeche','chiapas','chihuahua','colima','distrito','federal','ciudad', 'mexico','guanajuato','durango','guerrero','puebla','tlaxcala','estado', 'mexico','tabasco','veracruz','sinaloa','aguascalientes','hidalgo','jalisco','queretaro','sonora','zacatecas','oaxaca', 'querétaro')
custom66<-c('espero','priv','diese','cc','levantan','levanta','envía','expresarlo','comunicaron','seguida','diese','sale','dirigidos','consigno','lie','ux','sgg','efectuado','pééiii','lirettion','leglmoi','declaro','diera','habia','señalo','anotaran','concedio','anotandose','seguido','legiéiím','ux','destacando','motiven','formulado','motive','señala','algun','alguno','realice','emitir','rutilio','escandón','escandon','toco','expreso','tratarán','dieron','insertada')
custom67<-c('jdc','sup','lvii','pantoja','jrc','sr','as','pinedo','castaño','obviamente','merlo','cavazos','agradecemos','gustaria','tato','bob','lizet','strc','trigesimo','cuadragesimo','cuadragésimo','trigésimo','estara','sdes','vigesimo','decirle')
last<-c('cio',"io",'ion','aun','aún','sino','ca','ses')

alldfm<-dfm(all[,6], tolower = TRUE, stem=FALSE,remove_punct = TRUE, remove= stopwords("spanish"))

alldfm<-dfm(alldfm, remove=as.vector(nl))
alldfm<-dfm(alldfm, remove=as.vector(nlna))
alldfm<-dfm(alldfm, remove=as.vector(nmun))
alldfm<-dfm(alldfm, remove=as.vector(nmunna))
alldfm<-dfm(alldfm, remove=as.vector(stwd))
alldfm<-dfm(alldfm, remove=as.vector(gent))
alldfm<-dfm(alldfm, remove=as.vector(custom00))
alldfm<-dfm(alldfm, remove=as.vector(custom01))
alldfm<-dfm(alldfm, remove=as.vector(custom02))
alldfm<-dfm(alldfm, remove=as.vector(custom03))
alldfm<-dfm(alldfm, remove=as.vector(custom04))
alldfm<-dfm(alldfm, remove=as.vector(custom05))
alldfm<-dfm(alldfm, remove=as.vector(custom06))
alldfm<-dfm(alldfm, remove=as.vector(custom07))
alldfm<-dfm(alldfm, remove=as.vector(custom08))
alldfm<-dfm(alldfm, remove=as.vector(custom09))
alldfm<-dfm(alldfm, remove=as.vector(custom10))
alldfm<-dfm(alldfm, remove=as.vector(custom11))
alldfm<-dfm(alldfm, remove=as.vector(custom12))
alldfm<-dfm(alldfm, remove=as.vector(custom13))
alldfm<-dfm(alldfm, remove=as.vector(custom14))
alldfm<-dfm(alldfm, remove=as.vector(custom15))
alldfm<-dfm(alldfm, remove=as.vector(custom16))
alldfm<-dfm(alldfm, remove=as.vector(custom17))
alldfm<-dfm(alldfm, remove=as.vector(custom18))
alldfm<-dfm(alldfm, remove=as.vector(custom19))
alldfm<-dfm(alldfm, remove=as.vector(custom20))
alldfm<-dfm(alldfm, remove=as.vector(custom21))
alldfm<-dfm(alldfm, remove=as.vector(custom22))
alldfm<-dfm(alldfm, remove=as.vector(custom23))
alldfm<-dfm(alldfm, remove=as.vector(custom24))
alldfm<-dfm(alldfm, remove=as.vector(custom25))
alldfm<-dfm(alldfm, remove=as.vector(custom26))
alldfm<-dfm(alldfm, remove=as.vector(custom27))
alldfm<-dfm(alldfm, remove=as.vector(custom28))
alldfm<-dfm(alldfm, remove=as.vector(custom29))
alldfm<-dfm(alldfm, remove=as.vector(custom30))
alldfm<-dfm(alldfm, remove=as.vector(custom31))
alldfm<-dfm(alldfm, remove=as.vector(custom32))
alldfm<-dfm(alldfm, remove=as.vector(custom33))
alldfm<-dfm(alldfm, remove=as.vector(custom34))
alldfm<-dfm(alldfm, remove=as.vector(custom35))
alldfm<-dfm(alldfm, remove=as.vector(custom36))
alldfm<-dfm(alldfm, remove=as.vector(custom37))
alldfm<-dfm(alldfm, remove=as.vector(custom38))
alldfm<-dfm(alldfm, remove=as.vector(custom39))
alldfm<-dfm(alldfm, remove=as.vector(custom40))
alldfm<-dfm(alldfm, remove=as.vector(custom41))
alldfm<-dfm(alldfm, remove=as.vector(custom42))
alldfm<-dfm(alldfm, remove=as.vector(custom43))
alldfm<-dfm(alldfm, remove=as.vector(custom44))
alldfm<-dfm(alldfm, remove=as.vector(custom45))
alldfm<-dfm(alldfm, remove=as.vector(custom46))
alldfm<-dfm(alldfm, remove=as.vector(custom47))
alldfm<-dfm(alldfm, remove=as.vector(custom48))
alldfm<-dfm(alldfm, remove=as.vector(custom49))
alldfm<-dfm(alldfm, remove=as.vector(custom50))
alldfm<-dfm(alldfm, remove=as.vector(custom51))
alldfm<-dfm(alldfm, remove=as.vector(custom52))
alldfm<-dfm(alldfm, remove=as.vector(custom53))
alldfm<-dfm(alldfm, remove=as.vector(custom54))
alldfm<-dfm(alldfm, remove=as.vector(custom55))
alldfm<-dfm(alldfm, remove=as.vector(custom56))
alldfm<-dfm(alldfm, remove=as.vector(custom57))
alldfm<-dfm(alldfm, remove=as.vector(custom58))
alldfm<-dfm(alldfm, remove=as.vector(custom59))
alldfm<-dfm(alldfm, remove=as.vector(custom60))
alldfm<-dfm(alldfm, remove=as.vector(custom61))
alldfm<-dfm(alldfm, remove=as.vector(custom62))
alldfm<-dfm(alldfm, remove=as.vector(custom63))
alldfm<-dfm(alldfm, remove=as.vector(custom64))
alldfm<-dfm(alldfm, remove=as.vector(custom65))
alldfm<-dfm(alldfm, remove=as.vector(custom66))
alldfm<-dfm(alldfm, remove=as.vector(custom67))
alldfm<-dfm(alldfm, remove=as.vector(last))

alldfm<-dfm_trim(alldfm, min_docfreq = 73)#removes less common words -1%

#Correcting for accents
spells<- as.data.frame(read.csv("spells.csv",encoding = "UTF-8", header = T))
spells[,2] <- as.character(spells[,2])
spells[,1] <- as.character(spells[,1])
missp<-spells[,1]
correc<-spells[,2]
alldfm<-dfm_replace(alldfm, missp,correc)

#to lemmatize
diction_es<- as.data.frame(read.csv("lemmatization-es.csv"))
diction_es <- diction_es[match(unique(diction_es$tokens), diction_es$tokens),]
diction_es[,2] <- as.character(diction_es[,2])
diction_es[,1] <- as.character(diction_es[,1])
words<-diction_es[,1]
lemmas<-diction_es[,2]
alldfm<-dfm_replace(alldfm, words,lemmas)
alldfm<-dfm_trim(alldfm, max_docfreq = 6544) #removes most common words (90%)

save(x=alldfm, file="alldfmnomun.RData")


###############################
#### CORRELATED TOPIC MODEL####
###############################

rm(list = ls())
memory.limit(16000)
library("quanteda")
library("stm")

setwd('/scratch/lmc671/reeldiploc/')
set.seed(12345678)

alldfm<- get(load("alldfmnomun.RData"))
all_ctm <- stm(alldfm, K=450, seed=12345678)

top<-sageLabels(all_ctm,n=15)
topwords<-t(top$marginal$frex)
temp<-seq(1,ncol(topwords))
colnames(topwords)<-c(temp)
Topicstetha<-all_ctm$theta
save(x=all_ctm, file="all_ctm450.RData")
save(x=topwords, file="topwords450.RData")
save(x=Topicstetha, file="theta450.RData")


####################################
#### PREPARING FILE FOR ANALYSIS####
####################################

rm(list = ls())
setwd('D:/Users/Lucia/Dropbox/NYU/Papers/Reelection and promises update/Diputados Locales/00Results/_Replication_')

library("quanteda")
library("devtools")
library("quanteda.corpora")
library("MASS")
library("readtext")
library("stringi")
#install.packages("stringi")
libraries <- c("devtools","quanteda.corpora","MASS","quanteda","Matrix","readtext","lda","stm","bursts","lattice","ldatuning", "topicmodels", "ggplot2", "dplyr", "rjson", "quanteda", "lubridate", "parallel", "doParallel", "tidytext", "stringi")
lapply(libraries, require, character.only = TRUE)

set.seed(12345678)

topwords <- get(load("topwords450.RData"))
Topicstetha <- get(load("theta450.RData"))
all <- get(load("all.RData"))
alldfm<- get(load("alldfmnomun.RData"))

docnames(alldfm)[which(rowSums(alldfm)==0)] 
all<-all[which(rowSums(alldfm)>0),]
Topicstetha<-as.data.frame(Topicstetha)

all$mes<-stringr::str_extract(all$text,"enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|octubre|noviembre|diciembre|ENERO|FEBRERO|MARZO|ABRIL|MAYO|JUNIO|JULIO|AGOSTO|SEPTIEMBRE|OCTUBRE|NOVIEMBRE|DICIEMBRE")
all$mes<-tolower(all$mes)
allres<-cbind(all[,2:5], Topicstetha) 
allres<- allres[allres$state!='Sonora',]

allres$treat4<- as.numeric(allres$state=='Campeche'|allres$state=='Chiapas'|allres$state=='Durango'|allres$state=='Guanajuato'|allres$state=='Guerrero'|allres$state=='Jalisco'|allres$state=='Mexico'|allres$state=='Queretaro'|allres$state=='Sinaloa'|allres$state=='Tabasco'|allres$state=='Tlaxcala'|allres$state=='Veracruz')
allres$treat4[which(allres$state=='Aguascalientes'|allres$state=='Chihuahua'|allres$state=='Colima'|allres$state=='Hidalgo'|allres$state=='Oaxaca'| allres$state=='Zacatecas')]<-NA

allres$treat2<- as.numeric(allres$state=='Aguascalientes'|allres$state=='Chihuahua'|allres$state=='Colima'|allres$state=='Hidalgo'|allres$state=='Oaxaca'| allres$state=='Zacatecas')
allres$treat2[which(allres$state=='Campeche'|allres$state=='Chiapas'|allres$state=='Durango'|allres$state=='Guanajuato'|allres$state=='Guerrero'|allres$state=='Jalisco'|allres$state=='Mexico'|allres$state=='Queretaro'|allres$state=='Sinaloa'|allres$state=='Tabasco'|allres$state=='Tlaxcala'|allres$state=='Veracruz')] <-NA

allres$year<-as.numeric(paste0(allres$year))
allres$yeart<-round(allres$year)

allres$reform[allres$state=='Puebla' & allres$year>=2014]<- 1
allres$reform[allres$state=='Puebla' & allres$year<2014]<- 0
allres$reform[allres$year>=2015.2 & (allres$state=='Campeche'|allres$state=='CdMex'|allres$state=='Chiapas'|allres$state=='Colima'|allres$state=='Guanajuato'|allres$state=='Guerrero'|allres$state=='Mexico'|allres$state=='Tabasco'|allres$state=='Jalisco'|allres$state=='Queretaro')]<- 1
allres$reform[allres$year<2015.2 & (allres$state=='Campeche'|allres$state=='CdMex'|allres$state=='Chiapas'|allres$state=='Colima'|allres$state=='Guanajuato'|allres$state=='Guerrero'|allres$state=='Mexico'|allres$state=='Tabasco'|allres$state=='Jalisco'|allres$state=='Queretaro')]<- 0
allres$reform[allres$year>=2016.2 & (allres$state=='Chihuahua'|allres$state=='Durango'|allres$state=='Tlaxcala'|allres$state=='Veracruz'|allres$state=='Sinaloa'|allres$state=='Aguascalientes'|allres$state=='Oaxaca'|allres$state=='Hidalgo'|allres$state=='Zacatecas')]<- 1
allres$reform[allres$year<2016.2 & (allres$state=='Chihuahua'|allres$state=='Durango'|allres$state=='Tlaxcala'|allres$state=='Veracruz'|allres$state=='Sinaloa'|allres$state=='Aguascalientes'|allres$state=='Oaxaca'|allres$state=='Hidalgo'|allres$state=='Zacatecas')]<- 0

allres$month[allres$mes=='enero']<-'January'
allres$month[allres$mes=='febrero']<-'February'
allres$month[allres$mes=='marzo']<-'March'
allres$month[allres$mes=='abril']<-'April'
allres$month[allres$mes=='mayo']<-'May'
allres$month[allres$mes=='junio']<-'June'
allres$month[allres$mes=='julio']<-'July'
allres$month[allres$mes=='agosto']<-'August'
allres$month[allres$mes=='septiembre']<-'September'
allres$month[allres$mes=='octubre']<-'October'
allres$month[allres$mes=='noviembre']<-'November'
allres$month[allres$mes=='diciembre']<-'December'


library(plyr)
partic<- read.csv("particularistic.csv")
partic<- partic$part
partic<- c(paste0("V",partic))
allres$particularistic<-rowSums(allres[,c(partic)])

#particularistic types
unions<- read.csv("unions.csv")
unions<- unions[,]
unions<- c(paste0("V",unions))
allres$part_unions<-rowSums(allres[,c(unions)])

transfers<- read.csv("transfers.csv")
transfers<- transfers[,]
transfers<- c(paste0("V",transfers))
allres$part_transfers<-rowSums(allres[,c(transfers)])

pg<- read.csv("pg.csv")
pg<- pg[,]
pg<- c(paste0("V",pg))
allres$part_pg<-rowSums(allres[,c(pg)])

awards<- read.csv("awards.csv")
awards<- awards[,]
awards<- c(paste0("V",awards))
allres$part_awards<-rowSums(allres[,c(awards)])

infra<- read.csv("infra.csv")
infra<- infra[,]
infra<- c(paste0("V",infra))
allres$part_infra<-rowSums(allres[,c(infra)])

#subjects to map to spending
sal<- read.csv("salud.csv")
salud<- sal[,1]
salud<- c(paste0("V",salud))
allres$salud<-rowSums(allres[,c(salud)])

edu<- read.csv("edu.csv")
edu<- edu[,1]
edu<- c(paste0("V",edu))
allres$education<-rowSums(allres[,c(edu)])

farming<- read.csv("farming.csv")
farming<- farming[,1]
farming<- c(paste0("V",farming))
allres$farming<-rowSums(allres[,c(farming)])

publ<- read.csv("public.csv")
pub<- publ$pub
pub<- c(paste0("V",pub))
allres$public<-rowSums(allres[,c(pub)])

proced<- read.csv("procedural.csv")
pro<- proced$pro
pro<- c(paste0("V",pro))
allres$procedural<-rowSums(allres[,c(pro)])


#write.csv(allres,"allres.csv")
#NOTE: the csv printed includes a first column withouth a name with an index of the documents. 
#When imported in stata, Stata will confuse the nameless column with variable V1 (Thetas for Topic 1)
#Manually change the name of the first colum index before importing in stata


#FIGURE 2
allres$year[allres$year<2012]<-NA
Feat <- c(colnames(allres[,5:454]))
allres$ttype[allres$treat4==0]<-"Term-Limited"
allres$ttype[allres$treat2==1]<-"Short-Term Reelection Incentives"
allres$ttype[allres$treat4==1]<-"Long-Term Reelection Incentives"

resultdf <- unique(select(allres,year,ttype))
df_sum <- allres %>% 
  group_by(year,ttype) %>%
  summarise_(.dots = setNames("mean(V418)","V418"))
resultdf <- merge(resultdf, df_sum, by = c("year","ttype"))
plots<-ggplot(resultdf, aes(x=year, y=V418))+ geom_smooth(aes(linetype=ttype, color=ttype), se=FALSE)+
  ylab("Proportion of Discussion allocated to Topic 418") + ggtitle("Topic 418") + xlab("Years") + theme_light()+theme(legend.title=element_blank(),legend.position=c(0.8,0.8))
print(plots)
#ggsave("418.pdf")


resultdf <- unique(select(allres,year,ttype))
df_sum <- allres %>% 
  group_by(year,ttype) %>%
  summarise_(.dots = setNames("mean(V198)","V198"))
resultdf <- merge(resultdf, df_sum, by = c("year","ttype"))
plots<-ggplot(resultdf, aes(x=year, y=V198))+ stat_smooth(aes(linetype=ttype, color=ttype),se=FALSE)+
  ylab("Proportion of Discussion allocated to Topic 198") + ggtitle("Topic 198") + xlab("Years") + theme_light()+theme(legend.title=element_blank(),legend.position=c(0.8,0.8))
print(plots)
#ggsave("198.pdf")
