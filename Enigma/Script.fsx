#load "Enigma.fs"
open Enigma


//To use the Enigma machine you have to know the base settings which changed from day to day.
//If you want an easy encryption you can use the following function representing (Enigma.String.de_encrypt ('A','A','A') Rotor.UKW_B Rotor.I Rotor.II Rotor.III [||] (1,1,1) "TEST").

Enigma.String.de_encrypt_default "TEST"


//To modify the settings you have to set several parameters:
//Description                                                                                                               Type                Example
//1. base positions of the rotors (Characters you see on the rotors before typing the first letter)                         `char*char*char`    `('K','Q','C')`
//2. Reflector rotor by which the signal is returned to pass the three main rotors a second time                            `Rotor.UKW`         `Rotor.UKW_B`
//3-5. Rotor positions: A set of three main rotors. Note: Every rotor was present only once back in WWII (order sensitive)  `Rotor.Rotor`       3x `Rotor.IV`
//6. Plugboard settings which exchange characters. Note: there were only 10 cables avaliable                                `string []`         `[|"AB";"CZ";"DK"|]`
//7. By changing the outer ring of the rotors you can change the ring settings.                                             `int*int*int`       `(2,3,4)` 
//   Note: Ring settings were represented as integers to not be mixed up with base positions!
//8. Text you want to en- or decrypt.                                                                                       `string`            "Test"
//   Note: All characters are changed to its upper case version. Nothing but the alphabet characters are allowed.

Enigma.String.de_encrypt ('K','Q','C') Rotor.UKW_B Rotor.IV Rotor.II Rotor.V [|"AB";"CZ";"DK"|] (2,3,4) "Test"
 
(*
TAGESSCHLÜSSEL
Every day the 'Tagesschlüssel' changed. It specified the rotor positions, the ring settings and the plugboard settings.
Example: B425 agm DM EP FL HI JR KY NQ OU SW TZ

    B425: Rotor settings                        UKWB IV II V
    agm: Ring settings a->1 g->7 m->13          1,7,13
    DM EP FL HI JR KY NQ OU SW TZ: Plugboard    [|"DM";"EP";"FL";"HI";"JR";"KY";"NQ";"OU";"SW";"TZ"|]

KOPF
Every message started with a 'Kopf' which specified message named changes of the daily basic settings
Example: kr-2300-182-ZZXpwq
    kr: war relevant (due to unique sound in morse code -.-.-.)
    2300: time of encryption -> 11pm
    182: Number of characters in message
    ZZXpwq: To get the right message specified base positions of the rotors you have to set the rotors to ZZX and type pwq. 
The resulting characters are the new rotor base positions called 'Spruchschlüssel'.

NOTES
    Back then an 'X' was often used as word separating signal.                      "TEST TEXT" -> "TESTXTEXT"
    "ch" was often replaced by 'q'                                                  "WICHTIG" -> "WIQTIG"
    Characters were replaced by the corresponding german spelling alphabet name     "KOMPANIE H" -> "KOMPANIEXHEINRIQ"
*)



//// Message No. 233 of 19 July 1941 
//TAGESSCHLUESSEL:  B425 agm DM EP FL HI JR KY NQ OU SW TZ 
//KOPF:             kr-2300-182-ZZXpwq
Enigma.String.de_encrypt ('Z','Z','X') Rotor.UKW_B Rotor.IV Rotor.II Rotor.V [|"DM";"EP";"FL";"HI";"JR";"KY";"NQ";"OU";"SW";"TZ"|] (1,7,13) "PWQ"
    //SPRUCHSCHLUESSEL: QAY
Enigma.String.de_encrypt ('Q','A','Y') Rotor.UKW_B Rotor.IV Rotor.II Rotor.V [|"DM";"EP";"FL";"HI";"JR";"KY";"NQ";"OU";"SW";"TZ"|] (1,7,13) 
    "QKXETVPZQOHSXMBIZPHTCTRMAUZYSTJIMDUYOZBFRTZOUHBGOROUVRQEJRDRJHZPZIBQQHKMMJZCIIRCUOLXLCIOgKHRLIGGFJFTLLGDRARDZQUQKLTKXXXYKRUVFULBQLAYRZVJFULCGQJXFJURMURSELYFVFOKUHYUHSYLOMEFYAIIP"
//Übersetzung: "WIEVIELKLEINSIEGFRIEDGWOSMFRIEDRICHXHEINWIGHMUNXINSQRZAQTEINSSIEBENSTRIQAINSAQHXSIEBENWINXPLESNAUXABGEHOLTFRAGEXWERPZKMEFYHLKOMMADOHZATZUXOLENFRAGOVUNKANTWORTXDERQNAATYEKVEVSTER"
//             "WIE VIEL sFH (schwere Feldhaubitze) MUN. INSGESAMT 17.-18.07. WIRD PLESNAU ABGEHOLT FRAGE WER P---ZK MEFYHL KOMMA DOHZ ATZUXOLEN FRAGO VUNKANTWORT DER QNAATYEKVEVSTER"
   
    
//// Message No. 25 of 13 July 1941 
//TAGESSCHLUESSEL:  B423 gto AD EH GY IM KN LR OZ QV TX UW 
//Kopf:             xxxx-0830-219-HLCzmz
Enigma.String.de_encrypt ('H','L','C') Rotor.UKW_B Rotor.IV Rotor.II Rotor.III [|"AD";"EH";"GY";"IM";"KN";"LR";"OZ";"QV";"TX";"UW"|] (7,20,15) "zmz"
    //SPRUCHSCHLUESSEL: SDV
Enigma.String.de_encrypt ('S','D','V') Rotor.UKW_B Rotor.IV Rotor.II Rotor.III [|"AD";"EH";"GY";"IM";"KN";"LR";"OZ";"QV";"TX";"UW"|] (7,20,15) 
    "FDZCJJDKVWPYFDWPOQZGTJQYYXAFRHSQESERKGJBWBYPEOOKFMMPOMKQDDOLCPKHYPGUZYXBZYANYSAXIPXVQCPJBFFFDRDXFIJJPPPEYALCYKVLKXQHWIRZANGWUJBWVJYCKESMJQRYKQHCQOKMMYWMCKVLZJDVZXRUMRMNWFDZBQGXJQAPFFFZTAHJQZPWQWNIVZWUIJTHOYXGDCOJUW"
//Übersetzung: "ANXPANZXGRUPPEXVIERXSIEGFRIEDSIEGFRIEDTONIXDIVXSTEHTSEITXEINSZWOXSIEBENXEINSEINSNULLNULLXUHRMITANFAENGENAMUNTERKUNFTSRAUMXKANNNIQTEINFLIESZENXDAXDRITTEXINFXDIVXUNDXAQTEXPANZXDIVXBLOQIERENUNDRANMBELEGTHALTEXDIVXKDRX"
//             "AN PANZ GRUPPE 4 SIEGFRIED SIEGFRIED TONI DIV STEHT SEIT 12.07. 11:00 UHR MIT ANFAENGEN AM UNTERKUNFTSRAUM. KANN NIQT EINFLIESZEN DA 3. INF DIV UND 8. PANZ DIV BLOQIEREN UND RANM BELEGT HALTE DIV KDR"


//// Message No. 30 of 22 August 1941 
//TAGESSCHLUESSEL:  B341 wgr AC BE HW IP JZ KY LU OS QR VX 
//Kopf:             (1930)-51-gfraha
Enigma.String.de_encrypt ('G','F','R') Rotor.UKW_B Rotor.III Rotor.IV Rotor.I [|"AC";"BE";"HW";"IP";"JZ";"KY";"LU";"OS";"QR";"VX"|] (23,7,18) "aha"
    //SPRUCHSCHLUESSEL: TOR
Enigma.String.de_encrypt ('T','O','R') Rotor.UKW_B Rotor.III Rotor.IV Rotor.I [|"AC";"BE";"HW";"IP";"JZ";"KY";"LU";"OS";"QR";"VX"|] (23,7,18) 
    "CFVUAHZHPIWNUCXTMJGXPMVWKFVHZJTJGXMSSDJYESRCNX"
//Übersetzung: "WOGEFEQTSSTANDQUARTIERMEISTERABTXROEMEINSBERTA" --> "WO GEFEQTSSTAND QUARTIERMEISTER ABT X ROEM EINS BERTA"


//// Message No. 94 of 24 September 1941 
//TAGESSCHLUESSEL:  B231 szi AQ BO CM DP EW FT HS JZ KX LU 
//Kopf:             (1200)-81-ncwttg
Enigma.String.de_encrypt ('N','C','W') Rotor.UKW_B Rotor.II Rotor.III Rotor.I [|"AQ";"BO";"CM";"DP";"EW";"FT";"HS";"JZ";"KX";"LU"|] (19,26,9) "TTG"
    //SPRUCHSCHLUESSEL: DRI
Enigma.String.de_encrypt ('D','R','I') Rotor.UKW_B Rotor.II Rotor.III Rotor.I [|"AQ";"BO";"CM";"DP";"EW";"FT";"HS";"JZ";"KX";"LU"|] (19,26,9) "PWCQFEZLPXGENCLBOXJFVWWPXOOGLRIPJKOUIOTCTNSLZDKYYJQNTVCTMPLUOAUNESZVKXRCTMHM"
//Übersetzung:  "UMZUG GEPAEQ TROSS UND REST KOMMANDO STOPFEN X SQRIFTLIQER BEFEHL UNTERWEGS X SCHNEIDDR X"
//              "UMZUG GEPAECK TROSS UND REST KOMMANDO STOPPEN X SCHRIFTLICHER BEFEHL UNTERWEGS. SCHNEIDER"


(*
module Text
if you want to en or decrypt a text containing [' ';'.';',';'?';'-';'!' and numbers] you can use the 'Text module' which replaces every of these characters with a serial of unsusal character triplets. 
This process is performed before encryption and reversed after decryption. Because of that there are different functions for de- or encryption respectively.
*)

let encryptedText = Enigma.Text.encrypt_default "Testtext! 01234567890,-?"

let decryptedText = Enigma.Text.decrypt_default encryptedText
