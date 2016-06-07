Datu korekcijas

amo2016.csv
========================
Kanonizēti skolu nosaukumi. 
df1 <- df[order(df$Skola),]
unique(df1$Skola)
(1) Aiz vidusskolas numura un punkta liekam vienu tukšumu 
    ("Daugavpils 3.vidusskola" ir tas pats, kas "Daugavpils 3. vidusskola")
(2) Domuzīmes (n-dash) vietā izmantojam defisi jeb mīnusu; abās pusēs tai atstājam tukšumus.
    "Daugavpils krievu licejs-vidusskola" utml. pārveidojas par 
    kanonisku nosaukumu: "Daugavpils Krievu vidusskola - licejs". 
	Tāpat - dekoratīvu pēdiņu (smart-quotes) vietā izmantojam parastas pēdiņas (double-quote - ")
(3) Sugasvārdus "vidusskola", "pamatskola", "internātvidusskola" u.c. rakstām ar mazo burtu. 
    "Iļģuciema Vidusskola" -> "Iļģuciema vidusskola"; 
	"Mārupes Vidusskola" -> "Mārupes vidusskola"
(4) Novēršam nelielas drukas kļūdas, atjaunojam atdalošos tukšumus: 
    ISMA vidusskola"Premjers" -> ISMA vidusskola "Premjers" 
(5) "Valsts ģimnāzija" rakstām ar lielo burtu, bet "valsts poļu ģimnāzija" ar mazo. 
(6) Skolas Kultūras ministrijas pārvaldībā: "Jelgavas Mūzikas vidusskola". 
(7) Īpašos nosaukumus liekam pēdiņās: 
    Privātā sākumskola Vinnijs -> Privātā sākumskola "Vinnijs"
(8) Īpašvārdisko daļu no skolas nosaukuma rakstām lielajiem burtiem: 
    Rīgas centra humanitārā vidusskola -> Rīgas Centra humanitārā vidusskola
(9) Skolas nosaukums sākas ar lielo burtu: 
    sākumskola "Taurenītis" -> Sākumskola "Taurenītis"
	
Skolu pārsaukšana
"Dakšāru pamatskola" -> "Dekšāru pamatskola"
"J.G.Herdera Rīgas Grīziņklana vidusskola" -> "J.G.Herdera Rīgas Grīziņkalna vidusskola"
"Jūrmalas Pumpuru vidusskola" -> "Pumpuru vidusskola"
"Liepājas pilsētas 8. vidusskola" -> "Liepājas 8. vidusskola"
"O.Kalpaka Rīgas Tautas daiļamata pamatskola" -> "O.Kalpaka Rīgas Tautas daiļamatu pamatskola"