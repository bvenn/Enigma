namespace Enigma

module Converter =
    
    ///standard alphabet as string
    let alphabet =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    
    ///converts a character (upper or lower case) to its corresponding index in the standard alphabet (1-based)
    let charToIntStandard (char:char)=
        let charUp =  System.Char.ToUpper char
        alphabet.IndexOf charUp + 1

    ///the alphabet which was altered by changing the plugs on the plugboard
    let mutable enigmaAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    ///by changing the plugboard you can alter the order of the Enigma alphabet
    let changeEnigmaAlphabet (strArr:string [])=
        ///resets the alphabet to standard 
        enigmaAlphabet <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        
        ///searches indices of the character pair which has to be changed and changes them.
        let newABC (char1:char) (char2:char)=         
            let index = (char1,"ABCDEFGHIJKLMNOPQRSTUVWXYZ".IndexOf(char1)),(char2,"ABCDEFGHIJKLMNOPQRSTUVWXYZ".IndexOf(char2))
            enigmaAlphabet.Remove(snd (fst index),1)
            |> fun x -> x.Insert(snd (fst index), fst (snd index) |> string)
            |> fun x -> x.Remove(snd (snd index),1)
            |> fun x -> x.Insert(snd (snd index), fst (fst index) |> string)
            |> fun x -> enigmaAlphabet <- x 
        
        ///every character pair have to be changed
        for i in strArr do newABC i.[0] i.[1]   

    ///converts a character to its corresponding index in the Enigma alphabet (1-based)
    let charToIntEnigma c  =
        let cUpper = System.Char.ToUpper c
        enigmaAlphabet.IndexOf cUpper + 1

    ///converts an int to its corresponding character in the Enigma alphabet (1-based)
    let intToCharENIGMA i = 
        match i with
        | 0 -> enigmaAlphabet.[25]
        | _ -> enigmaAlphabet.[i-1]

    ///converts a char [] into a string
    let charArrToString (arr:char[]) =
        System.String.Concat(arr)

    ///covers characters: [' ';'.';',';'?';'-';'!' and numbers]
    let coverSpacesAndPeriods (str:string) =
        let cover = 
            str.Replace(". ","ZQZXQX")
            |> fun x -> x.Replace(" ","XQX")
            |> fun x -> x.Replace(",","YQY")
            |> fun x -> x.Replace(".","ZQZ")
            |> fun x -> x.Replace("?","QQQ")
            |> fun x -> x.Replace("-","MQM")
            |> fun x -> x.Replace("!","XVY")
            |> fun x -> x.Replace("1","ZQV")
            |> fun x -> x.Replace("2","QVQ")
            |> fun x -> x.Replace("3","MQV")
            |> fun x -> x.Replace("4","VQY")
            |> fun x -> x.Replace("5","ZVZ")
            |> fun x -> x.Replace("6","QQV")
            |> fun x -> x.Replace("7","VZM")
            |> fun x -> x.Replace("8","ZQY")
            |> fun x -> x.Replace("9","VQZ")
            |> fun x -> x.Replace("0","VQQ")
        cover


    ///decovers characters: [' ';'.';',';'?';'-';'!' and numbers]
    let decoverSpacesAndPeriods (str:string) =
       let decover = 
           str.Replace("ZQZXQX",". ")
           |> fun x -> x.Replace("XQX"," ")
           |> fun x -> x.Replace("YQY",",")
           |> fun x -> x.Replace("ZQZ",".")
           |> fun x -> x.Replace("QQQ","?")
           |> fun x -> x.Replace("MQM","-")
           |> fun x -> x.Replace("XVY","!")
           |> fun x -> x.Replace("ZQV","1")
           |> fun x -> x.Replace("QVQ","2")
           |> fun x -> x.Replace("MQV","3")
           |> fun x -> x.Replace("VQY","4")
           |> fun x -> x.Replace("ZVZ","5")
           |> fun x -> x.Replace("QQV","6")
           |> fun x -> x.Replace("VZM","7")
           |> fun x -> x.Replace("ZQY","8")
           |> fun x -> x.Replace("VQZ","9")
           |> fun x -> x.Replace("VQQ","0")
       decover

///module Rotor contains all possible rotors (Walzen) of the enigma machine
module Rotor =

    ///rotors to use to de_encrypt your text
    type Rotor =
        {
         ///Name of the Rotor
         Number : int
         ///determines in which character the input character gets converted. If ("ABC" -> "CAB") then [|3;1;2|] + last int as first int ---> [|2;3;1;2|]
         Forward: int []
         Reverse: int []
         Kerbe  : int list
         ///determines at which position the rotor shifts rotor left to it one step further
         Ring   : int list}


    let createRotor number forward kerbe ring= 
        ///creates the reverse order of the rotor by taking the forward order
        let createReverseOrder (arr :int[])= 
            let mutable newArray = Array.zeroCreate 27
            let mode = 
                arr.[1..26] 
                |> Array.mapi (fun i x -> newArray.[x] <- i + 1) |> ignore
                newArray.[0] <- newArray.[26]
            newArray
        {Number = number; Forward= forward;Reverse= createReverseOrder forward; Kerbe= kerbe |> List.map Converter.charToIntStandard; Ring= ring |> List.map Converter.charToIntStandard}
    
    //the last int has to be the first as well because char #26 is equal to char #0 if x%26        
    let I    = createRotor 1 [|10;5;11;13;6;12;7;4;17;22;26;14;20;15;23;25;8;24;21;19;16;1;9;2;18;3;10|] ['Y'] ['Q']
    let II   = createRotor 2 [|5;1;10;4;11;19;9;18;21;24;2;12;8;23;20;13;3;17;7;26;14;16;25;6;22;15;5 |] ['M'] ['E']
    let III  = createRotor 3 [|15;2;4;6;8;10;12;3;16;18;20;24;22;26;14;25;5;9;23;7;1;11;13;21;19;17;15|] ['D'] ['V']
    let IV   = createRotor 4 [|2;5;19;15;22;16;26;10;1;25;17;21;9;18;8;24;12;14;6;20;7;11;4;3;13;23;2 |] ['R'] ['J']
    let V    = createRotor 5 [|11;22;26;2;18;7;9;20;25;21;16;19;4;14;8;12;24;1;23;13;10;17;15;6;5;3;11|] ['H'] ['Z']
    let VI   = createRotor 6 [|23;10;16;7;22;15;21;13;6;25;17;2;5;14;8;26;18;4;11;1;19;24;12;9;3;20;23|] ['H';'U'] ['Z';'M']
    let VII  = createRotor 7 [|20;14;26;10;8;7;18;3;24;13;25;19;23;2;15;21;6;1;9;22;12;16;5;11;17;4;20|] ['H';'U'] ['Z';'M']
    let VIII = createRotor 8 [|22;6;11;17;8;20;12;24;15;3;2;10;19;16;4;26;18;1;13;5;23;14;9;21;25;7;22|] ['H';'U'] ['Z';'M']
    
    //Umkehrwalzen(UKW) (reflector): after the first path through the three rotors, the signal is reversed by this rotor type
    type UKW =
        {Name    : char
         Forward : int []}

    let createUKW name forward=
        {Name = name; Forward = forward}

    let UKW_A = createUKW 'A' [|4;5;10;13;26;1;12;25;24;22;2;23;6;3;18;17;21;15;14;20;19;16;9;11;8;7;4 |]
    let UKW_B = createUKW 'B' [|20;25;18;21;8;17;19;12;4;16;24;14;7;15;11;13;9;5;2;6;26;3;23;22;10;1;20|]
    let UKW_C = createUKW 'C' [|12;6;22;16;10;9;1;15;25;5;4;18;26;24;23;7;3;20;11;21;17;19;2;14;13;8;12|]



module Enigma =

    let private fst3 (a,b,c) = a
    let private snd3 (a,b,c) = b
    let private trd3 (a,b,c) = c

    ///because the rotor range is 1 to 26 all other numbers have to be converted |  0->26  1->1  2->2 ... 26->0->26  27->1  28->2
    let private norm i = 
        let newI = i % 26
        if newI = 0 then 26 else newI

    //represents the rotor positions
    let mutable private counter = (1,1,1)

    ///in every step the counter is changed by this function.                                    
    let private countCounter counter' sndRotorRing trdRotorRing =         
        match trd3 counter' with                                                                                                               
        | a when trdRotorRing |> List.exists (fun x -> x = a)  ->                                                                              
            match snd3 counter' with                                                                                                           
            | aa when sndRotorRing |> List.exists (fun x -> x = aa)  -> counter <- norm (fst3 counter' + 1), norm (aa + 1), norm (a + 1)       
            | _  -> counter <- fst3 counter', norm (snd3 counter + 1), norm(trd3 counter' + 1)                                                 
        | b when  trdRotorRing |> List.exists (fun x -> x = norm(b - 1))  ->                                                                   
            match snd3 counter' with                                                                                                           
            | ba when sndRotorRing |> List.exists (fun x -> x = ba)                                                                            
                -> counter <- norm (fst3 counter' + 1), norm (ba + 1), norm (b + 1)                                                            
            | _ -> counter <-fst3 counter', snd3 counter, trd3 counter' + 1                                                                    
        | _  -> counter <- fst3 counter', snd3 counter', norm (trd3 counter' + 1) 
        
    ///resets the counter to the desired position
    let resetCounter (a,b,c)= 
        let triple = 
            a |> Converter.charToIntStandard, b |> Converter.charToIntStandard, c |> Converter.charToIntStandard
        //printfn "Counter resetted to (%c,%c,%c)" a b c
        counter <- triple    
    
    ///converts a character into another by performing a rotor step
    let private convertCharInRotor (rotorOrder:int []) fu buchstabenNr = 
        let newInt = (fu counter + (buchstabenNr - 1)) % 26
        let innerDecrypt i = (i - (fu counter - 1) + 26) % 26
        rotorOrder.[newInt] |> innerDecrypt

    ///for decrypting single characters
    module Char =
        ///Does reset counter each time!
        let de_encryptSingle (rotorPositions: char*char*char) (ukw:Rotor.UKW) (fstRotor:Rotor.Rotor) (sndRotor:Rotor.Rotor) (trdRotor:Rotor.Rotor)  (stecker:string []) char=
            resetCounter rotorPositions
            Converter.changeEnigmaAlphabet stecker
            countCounter counter sndRotor.Ring trdRotor.Ring
            char
            |> Converter.charToIntEnigma 
            |> convertCharInRotor trdRotor.Forward trd3
            |> convertCharInRotor sndRotor.Forward snd3
            |> convertCharInRotor fstRotor.Forward fst3
            |> fun x -> ukw.Forward.[x] 
            |> convertCharInRotor fstRotor.Reverse fst3
            |> convertCharInRotor sndRotor.Reverse snd3
            |> convertCharInRotor trdRotor.Reverse trd3
            |> Converter.intToCharENIGMA

        ///Does NOT reset counter each time!
        let de_encryptMulti (ukw:Rotor.UKW) (fstRotor:Rotor.Rotor) (sndRotor:Rotor.Rotor) (trdRotor:Rotor.Rotor)  (ringSt:int*int*int) char=
            
            countCounter counter sndRotor.Ring trdRotor.Ring
            char
            |> Converter.charToIntEnigma                                //Ring settings changes input here  //and output here 
            |> fun x -> norm ((convertCharInRotor trdRotor.Forward trd3 (norm (x - trd3 ringSt + 1 + 26))) + trd3 ringSt - 1 + 26)              
            |> fun x -> norm ((convertCharInRotor sndRotor.Forward snd3 (norm (x - snd3 ringSt + 1 + 26))) + snd3 ringSt - 1 + 26)              
            |> fun x -> norm ((convertCharInRotor fstRotor.Forward fst3 (norm (x - fst3 ringSt + 1 + 26))) + fst3 ringSt - 1 + 26)              
            |> fun x -> ukw.Forward.[x]                                                                                          
            |> fun x -> norm ((convertCharInRotor fstRotor.Reverse fst3 (norm (x - fst3 ringSt + 1 + 26))) + fst3 ringSt - 1 + 26)
            |> fun x -> norm ((convertCharInRotor sndRotor.Reverse snd3 (norm (x - snd3 ringSt + 1 + 26))) + snd3 ringSt - 1 + 26)
            |> fun x -> norm ((convertCharInRotor trdRotor.Reverse trd3 (norm (x - trd3 ringSt + 1 + 26))) + trd3 ringSt - 1 + 26)
            |> Converter.intToCharENIGMA

        //De_encrypts a single character with resetting the rotor positions before de_encrypting. Use de_encryptMulti if you do not want the rotors to be resetted.
        let de_encryptSingle_Default char = 
            de_encryptSingle ('A','A','A') Rotor.UKW_B Rotor.I Rotor.II Rotor.III [||] char

    ///use this module if you want to de_encrypt a text as string like the original Enigma was used.
    module String =
        let de_encrypt (rotorPositions: char*char*char) (ukw:Rotor.UKW) (fstRotor:Rotor.Rotor) (sndRotor:Rotor.Rotor) (trdRotor:Rotor.Rotor)  (stecker:string []) (ringStellung:int*int*int) string =
            resetCounter rotorPositions
            Converter.changeEnigmaAlphabet stecker
            [|for i in string do yield Char.de_encryptMulti ukw fstRotor sndRotor trdRotor  ringStellung i|]
            |> Converter.charArrToString

        ///De_encrypts a string with default parameters ('A','A','A') Rotor.I Rotor.II Rotor.III Rotor.UKW_B [||] (1,1,1)
        let de_encrypt_default string =
            de_encrypt ('A','A','A') Rotor.UKW_B Rotor.I Rotor.II Rotor.III [||] (1,1,1) string

    ///use this module if you want to de_encrypt a text as string with spaces, commas and other special characters so that the text is easily understandable.
    module Text =
        ///Encrypts a string with [' ';'.';',';'?';'-';'!' and numbers]
        let encrypt rotorPositions ukw fstRotor sndRotor trdRotor stecker ringStellung string =
            let coveredSpaces = Converter.coverSpacesAndPeriods string
            String.de_encrypt rotorPositions ukw fstRotor sndRotor trdRotor stecker ringStellung coveredSpaces
        
        ///Encrypts a string with [' ';'.';',';'?';'-';'!' and numbers] with default parameters ('A','A','A') Rotor.I Rotor.II Rotor.III Rotor.UKW_B [||] (1,1,1)
        let encrypt_default text = 
            encrypt ('A','A','A') Rotor.UKW_B Rotor.I Rotor.II Rotor.III [||] (1,1,1) text
        
        ///Decrypts a string recovering [' ';'.';',';'?';'-';'!' and numbers].
        let decrypt rotorPositions ukw fstRotor sndRotor trdRotor  stecker ringSt string =
            String.de_encrypt rotorPositions ukw fstRotor sndRotor trdRotor  stecker ringSt string
            |> Converter.decoverSpacesAndPeriods
        
        ///Decrypts a string recovering [' ';'.';',';'?';'-';'!' and numbers] with default parameters ('A','A','A') Rotor.I Rotor.II Rotor.III Rotor.UKW_B [||] (1,1,1)
        let decrypt_default str = 
            decrypt ('A','A','A') Rotor.UKW_B Rotor.I Rotor.II Rotor.III [||] (1,1,1) str