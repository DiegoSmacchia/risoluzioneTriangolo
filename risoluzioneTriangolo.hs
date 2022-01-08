{- Programma Haskell per la risoluzione di un triangolo nel caso in cui una di queste ipotesi sia vera:
    - conosciamo due angoli e il lato compreso tra loro;
    - conosciamo due angoli e un lato non compreso tra loro;
    - conosciamo due lati e l'angolo compreso tra loro;
    - conosciamo due lati e un angolo non compreso tra loro;
    - conosciamo i tre lati. -}

import Text.Read --Necessaro per utilizzare la funzione readMaybe
import Control.Monad --Necessario per usare when nel main

main :: IO ()
valore x = x
main = do
    scelta <- sceltaCaso
    eseguiScelta scelta
    when (scelta /= 0) $ main

{- La funzione sceltaCaso stampa un semplice menu per la scelta del caso che si desidera trattare,
    una volta che l'utente ha inserito il valore corrispondente al caso voluto lo valida,
    ripetendo la richiesta in caso di input non valido, cioè qualunque valore diverso da un intero
    compreso tra 0 e 5. -}
sceltaCaso :: IO Int
sceltaCaso = do
    putStrLn "-----------------------------------------------------------------"
    putStrLn "Scegliere uno dei seguenti casi:"
    putStrLn "1) Sono noti due angoli ed il lato tra essi compreso."
    putStrLn "2) Sono noti due angoli ed un lato NON compreso tra loro."
    putStrLn "3) Sono noti due lati e l'angolo tra essi compreso."
    putStrLn "4) Sono noti due lati ed un angolo NON compreso tra loro."
    putStrLn "5) Sono noti i tre lati."
    putStrLn "0) Uscita."
    putStrLn "-----------------------------------------------------------------"

    sceltaStringa <- getLine
    let n = verificaSceltaCaso sceltaStringa
    if n /= -1
        then return n
        else do
            putStrLn "\nValore inserito non valido. Riprovare.\n"
            sceltaCaso


{- La funzione verificaSceltaCaso prende in ingresso il valore inserito dall'utente quando seleziona uno dei casi possibili,
    e si assicura che il valore inserito sia un intero compreso nell'intervallo aperto (-1, 6) tramite la funzione verificaValoreCompreso.
    La funzione restituisce il valore -1 in caso di errore, o l'intero inserito in caso di scelta corretta. -}
verificaSceltaCaso :: String -> Int
verificaSceltaCaso casoScelto
    | (readMaybe casoScelto :: Maybe Int) == Nothing = -1
    | verificaValoreCompreso (read casoScelto :: Float) (-1) 6 == -1 = -1
    | otherwise = (read casoScelto :: Int)

{- La funzione verificaNumeroCompreso controlla che un valore sia un numero decimale e che sia compreso nell'intervallo aperto (minimo, massimo),
    per farlo richiama le funzioni verificaNumero e verificaValoreCompreso.
    La funzione prende in ingresso tre argomenti:
    - il primo è il valore da verificare;
    - il secondo è il valore minimo dell'intervallo aperto;
    - il terzo è il valore massimo dell'intervallo aperto.
    La funzione restituisce il valore inserito se la verifica va a buon fine, altrimenti -1. -}
verificaNumeroCompreso :: String -> Float -> Float -> Float
verificaNumeroCompreso valoreDaVerificare minimo massimo
    | verificaNumero valoreDaVerificare /= -1 = verificaValoreCompreso (read valoreDaVerificare :: Float) minimo massimo
    | otherwise = -1

{- La funzione verificaNumero prende in ingresso un valore e verifica che tale valore sia un numero decimale,
    in quel caso lo restituisce e in caso contrario restituisce -1. -}
verificaNumero :: String -> Float
verificaNumero valoreDaVerificare
    | (readMaybe valoreDaVerificare :: Maybe Float) == Nothing = -1
    | otherwise = read valoreDaVerificare :: Float

{- La funzione verificaValoreCompreso controlla se un dato valore appartiene o meno ad un 
    determinato intervallo aperto, prende in ingresso tre argomenti:
    - il primo è il valore da verificare;
    - il secondo è il valore minimo dell'intervallo aperto;
    - il terzo è il valore massimo dell'intervallo aperto.
    La funzione restituisce il valore inserito in caso di appartenenza, altrimenti -1. -}
verificaValoreCompreso :: Float -> Float -> Float -> Float
verificaValoreCompreso valoreDaVerificare minimo massimo
    | valoreDaVerificare <= minimo = -1
    | valoreDaVerificare >= massimo = -1
    | otherwise = valoreDaVerificare

{- La funzione eseguiScelta prende in ingresso un numero intero, e richiama la funzione 
    scelta dall'utente per risolvere il triangolo.
    l'ultimo caso, che stampa un messaggio di errore, non sarebbe necessario dal momento che
    nella funzione sceltaCaso ci siamo già occupati della validazione della scelta, ma è stato
    inserito comunque per definire la funzione per ogni numero intero, quindi per non avere una funzione
    parziale. -}
eseguiScelta :: Int -> IO ()
eseguiScelta 0 = putStrLn "Uscita."
eseguiScelta 1 = risolviDueAngoliLatoCompreso
eseguiScelta 2 = risolviDueAngoliLatoNonCompreso
eseguiScelta 3 = risolviDueLatiAngoloCompreso
eseguiScelta 4 = risolviDueLatiAngoloNonCompreso
eseguiScelta 5 = risolviTreLati
eseguiScelta n = putStrLn "Scelta non valida."

{- La funzione risolviDueAngoliLatoCompreso richiede all'utente l'inserimento del valore (in gradi) dell'ampiezza di due angoli,
    alpha e beta, e della lunghezza del lato compreso tra loro, AB, tramite la funzione richiestaDatoTriangolo.
    Dopo averli ottenuti calcola per differenza l'angolo gamma, poi tramite il teorema dei seni
    calcola i due lati mancanti. 
    Una volta ottenuti tutti i dati richiama la funzione stampaTriangolo.
    NOTA: l'angolo alpha può assumere tutti i valori compresi nell'intervallo aperto (0, 180), mentre
        beta dovrà appartenere all'intervallo aperto (0, (180 - alpha)) poiché la somma degli angoli interni di un triangolo è sempre uguale a 180. -}
risolviDueAngoliLatoCompreso :: IO ()
risolviDueAngoliLatoCompreso = do
    alpha <- richiestaDatoTriangolo "angolo alpha" 0 180
    beta <- richiestaDatoTriangolo "angolo beta" 0 (180 - alpha)
    latoAB <- richiestaDatoTriangolo "lato AB" 0 (fromIntegral (maxBound::Int)/3)

    let gamma = 180 - (alpha + beta)
    let latoBC = ottieniLatoTeoremaSeni latoAB gamma alpha 
    let latoAC = ottieniLatoTeoremaSeni latoAB gamma beta

    stampaTriangolo latoAB latoBC latoAC alpha beta gamma

{- La funzione risolviDueAngoliLatoNonCompreso richiede all'utente l'inserimento del valore (in gradi) dell'ampiezza di due angoli,
    alpha e beta, e della lunghezza di un lato NON compreso tra loro, che chiamiamo BC, tramite 
    la funzione richiestaDatoTriangolo.
    Dopo averli ottenuti calcola per differenza l'angolo gamma, poi tramite il teorema dei seni
    calcola i due lati mancanti. 
    Una volta ottenuti tutti i dati richiama la funzione stampaTriangolo.
    NOTA: l'angolo alpha può assumere tutti i valori compresi nell'intervallo aperto (0, 180), mentre
        beta dovrà appartenere all'intervallo aperto (0, (180 - alpha)) poiché la somma degli angoli interni di un triangolo è sempre uguale a 180. -}
risolviDueAngoliLatoNonCompreso :: IO()
risolviDueAngoliLatoNonCompreso = do
    alpha <- richiestaDatoTriangolo "angolo alpha" 0 180
    beta <- richiestaDatoTriangolo "angolo beta" 0 (180 - alpha)
    latoBC <- richiestaDatoTriangolo "lato BC" 0 (fromIntegral (maxBound::Int))

    let gamma = 180 - (alpha + beta)
    let latoAB = ottieniLatoTeoremaSeni latoBC alpha gamma
    let latoAC = ottieniLatoTeoremaSeni latoAB gamma beta

    stampaTriangolo latoAB latoBC latoAC alpha beta gamma

{- La funzione risolviDueLatiAngoloCompreso richiede all'utente l'inserimento della lunghezza di due lati, AB e BC, e del valore 
    (in gradi) dell'ampiezza dell'angolo compreso tra loro, beta, tramite la funzione richiestaDatoTriangolo.
    Dopo averli ottenuti calcola la dimensione del lato mancante tramite il teorema di Carnot,
    l'angolo gamma tramite il teorema dei seni e alpha per differenza.
    Una volta ottenuti tutti i dati richiama la funzione stampaTriangolo. -}
risolviDueLatiAngoloCompreso :: IO ()
risolviDueLatiAngoloCompreso = do
    latoAB <- richiestaDatoTriangolo "lato AB" 0 (fromIntegral (maxBound::Int))
    latoBC <- richiestaDatoTriangolo "lato BC" 0 (fromIntegral (maxBound::Int))
    beta <- richiestaDatoTriangolo "angolo beta" 0 180

    let latoAC = ottieniLatoTeoremaCarnot latoAB latoBC beta
    let gamma = ottieniAngoloTeoremaSeni latoAC latoAB beta
    let alpha = 180 - (gamma + beta)

    stampaTriangolo latoAB latoBC latoAC alpha beta gamma
    
{- La funzione risolviDueLatiAngoloNonCompreso richiede all'utente l'inserimento della lunghezza di due lati, AB e BC, e del valore 
    (in gradi) dell'ampiezza di un angolo NON compreso tra loro, che chiamiamo alpha, tramite la funzione richiestaDatoTriangolo.
    Dopo averli ottenuti calcola il valore dell'ampiezza dell'angolo gamma tramite il teorema dei seni, beta per differenza
    e la dimensione del lato mancante tramite il teorema di Carnot.
    Una volta ottenuti tutti i dati richiama la funzione stampaTriangolo. -}
risolviDueLatiAngoloNonCompreso :: IO ()
risolviDueLatiAngoloNonCompreso = do
    latoAB <- richiestaDatoTriangolo "lato AB" 0 (fromIntegral (maxBound::Int))
    latoBC <- richiestaDatoTriangolo "lato BC" 0 (fromIntegral (maxBound::Int))
    alpha <- richiestaDatoTriangolo "angolo alpha" 0 180

    let gamma = ottieniAngoloTeoremaSeni latoBC latoAB alpha
    let beta = 180 - (gamma + alpha)
    let latoAC = ottieniLatoTeoremaCarnot latoAB latoBC beta

    stampaTriangolo latoAB latoBC latoAC alpha beta gamma

{- La funzione risolviTreLati richiede all'utente l'inserimento della lunghezza dei tre lati, AB, BC e AC,
    tramite la funzione richiestaDatoTriangolo.
    Dopo averli ottenuti calcola alpha tramite l'inverso del teorema di Carnot, beta tramite il teorema dei seni
    e gamma per differenza.
    Una volta ottenuti tutti i dati richiama la funzione stampaTriangolo.
    NOTA: i lati AB e BC possono assumere tutti i valori compresi nell'intervallo aperto (0, (fromIntegral (maxBound::Int)) ), mentre
        AC dovrà appartenere all'intervallo aperto  ( |latoAB - latoAC| ) , (latoAB + latoBC) ) 
        poiché ogni lato deve essere di lunghezza inferiore alla somma degli altri due. -}
risolviTreLati :: IO ()
risolviTreLati = do
    latoAB <- richiestaDatoTriangolo "lato AB" 0 (fromIntegral (maxBound::Int))
    latoBC <- richiestaDatoTriangolo "lato BC" 0 (fromIntegral (maxBound::Int))
    latoAC <- richiestaDatoTriangolo "lato AC" (abs (latoAB - latoBC)) (latoAB + latoBC)

    let alpha = ottieniAngoloTeoremaCarnot latoAC latoAB latoBC
    let beta =  ottieniAngoloTeoremaSeni latoBC latoAC alpha
    let gamma = 180 - (alpha + beta)

    stampaTriangolo latoAB latoBC latoAC alpha beta gamma


{- La funzione richiestaDatoTriangolo richiede all'utente il valore di un dato del triangolo e ne verifica l'appartenenza ad un intervallo aperto
    (minimo, massimo), prendendo in ingresso tre argomenti:
    - il primo è il nome del dato che vogliamo ottenere;
    - il secondo indica il valore minimo dell'intervallo aperto;
    - il terzo indica il valore massimo dell'intervallo aperto.
    Quando l'utente inserisce il valore richiesto si verifica che sia un numero decimale e che sia compreso nell'intervallo aperto (minimo, massimo)
    tramite la funzioneverificaNumeroCompreso, in quel caso il valore viene restituito dalla funzione, altrimenti viene richiesto nuovamente. -}
richiestaDatoTriangolo :: String -> Float -> Float -> IO Float
richiestaDatoTriangolo datoRichiesto minimo massimo = do
    let richiestaDaFare = "Inserire il valore di '" ++ datoRichiesto ++ "' (maggiore di " ++ show minimo ++ " e minore di " ++ show massimo ++ ") : "
    putStrLn richiestaDaFare

    valoreDato <- getLine
    let n = verificaNumeroCompreso valoreDato minimo massimo
    if n /= (-1)
        then return n
        else do
            putStrLn "\nValore inserito non valido. Riprovare.\n"
            richiestaDatoTriangolo datoRichiesto minimo massimo


{- La funzione ottieniLatoTeoremaSeni calcola la lunghezza di un lato del triangolo prendendo in ingresso tre argomenti:
    - il primo è la lunghezza di un lato conosciuto;
    - il secondo è la dimensione in gradi dell'angolo opposto al lato conosciuto; 
    - il terzo è la dimensione in gradi di un altro angolo (l'angolo opposto al lato di cui vogliamo calcolare la lunghezza).
    NOTA: la funzione sin prende in ingresso la dimensione degli angoli in radianti, quindi è necessario effettuare una conversione
        tramite la funzione convertiGradiInRadianti. -}
ottieniLatoTeoremaSeni :: Float -> Float -> Float -> Float
ottieniLatoTeoremaSeni latoConosciuto angoloOpposto angoloNonOpposto = latoConosciuto / sin (convertiGradiInRadianti angoloOpposto) * sin (convertiGradiInRadianti angoloNonOpposto)

{- La funzione ottieniAngoloTeoremaSeni calcola la dimensione in gradi di un angolo del triangolo prendendo in ingresso tre argomenti:
    - il primo è la lunghezza di uno dei lati conosciuti;
    - il secondo è la lunghezza del lato opposto all'angolo da calcolare;
    - il terzo è la dimensione in gradi di un angolo (l'angolo opposto al lato passato come primo argomento).
    NOTE: 
    - la funzione sin prende in ingresso la dimensione degli angoli in radianti, quindi è necessario effettuare una conversione
        tramite la funzione convertiGradiInRadianti;
    - la funzione asin calcola la dimensione dell'angolo in radianti, quindi è necessario effettuare una conversione
        tramite la funzione convertiRadiantiInGradi. -}
ottieniAngoloTeoremaSeni :: Float -> Float -> Float -> Float
ottieniAngoloTeoremaSeni primoLato secondoLato angoloCompreso = convertiRadiantiInGradi( asin( ( sin (convertiGradiInRadianti angoloCompreso) / primoLato * secondoLato)) )

{- La funzione ottieniLatoTeoremaCarnot calcola la lunghezza di un lato prendendo in ingresso tre argomenti:
    - il primo e il secondo sono la lunghezza degli altri due lati;
    - il terzo è la dimensione in gradi dell'angolo tra loro compreso.
    NOTA: La funzione cos prende in ingresso la dimensione degli angoli in radianti, quindi è necessario effettuare una conversione
        tramite la funzione convertiGradiInRadianti. -}
ottieniLatoTeoremaCarnot :: Float -> Float -> Float -> Float
ottieniLatoTeoremaCarnot primoLato secondoLato angoloCompreso = (primoLato^2 + secondoLato^2 - 2*primoLato*secondoLato * cos (convertiGradiInRadianti angoloCompreso))**0.5

{- La funzione ottieniAngoloTeoremaCarnot calcola la dimensione di un angolo prendendo in ingresso tre argomenti:
    - il primo e il secondo sono la lunghezza dei due lati NON opposti all'angolo da calcolare;
    - il terzo è la lunghezza del lato opposto all'angolo da calcolare.
    NOTE:
    - la funzione cos prende in ingresso la dimensione degli angoli in radianti, quindi è necessario effettuare una conversione
        tramite la funzione convertiGradiInRadianti.
    - la funzione acos calcola la dimensione dell'angolo in radianti, quindi è necessario effettuare una conversione
        tramite la funzione convertiRadiantiInGradi. -}
ottieniAngoloTeoremaCarnot :: Float -> Float -> Float -> Float
ottieniAngoloTeoremaCarnot primoLatoNonOpposto secondoLatoNonOpposto latoOpposto = convertiRadiantiInGradi( acos ((primoLatoNonOpposto^2 + secondoLatoNonOpposto^2 - latoOpposto^2)/(2 * primoLatoNonOpposto * secondoLatoNonOpposto)))

{- La funzione stampaTriangolo effettua la stampa del valore dei lati, degli angoli, del perimetro e dell'area del triangolo
    prendendo in ingresso sei argomenti:
    - i primi tre sono rispettivamente i lati AB, BC e AC;
    - gli ultimi tre sono rispettivamente gli angoli alpha, beta e gamma. -}
stampaTriangolo :: Float -> Float -> Float -> Float -> Float -> Float -> IO ()
stampaTriangolo latoAB latoBC latoAC alpha beta gamma = do
    let perimetro = latoAB + latoAC + latoBC
    let area = (latoAB * latoAC * sin(convertiGradiInRadianti alpha))/2
    putStrLn "-----------------------------------------------------------------"
    putStrLn "I dati del triangolo risolto sono i seguenti:"
    putStr "Lato AB =       "
    print latoAB
    putStr "Lato BC =       "
    print latoBC
    putStr "Lato AC =       "
    print latoAC
    putStr "Angolo alpha =  "
    print alpha
    putStr "Angolo beta =   "
    print beta
    putStr "Angolo gamma =  "
    print gamma
    putStr "Perimetro =     "
    print perimetro
    putStr "Area =          "
    print area
    putStrLn "-----------------------------------------------------------------"

{- La funzione convertiGradiInRadianti prende in ingresso il valore dell'ampiezza di un angolo in gradi e restituisce il
    suo valore in radianti. -}
convertiGradiInRadianti :: Float -> Float
convertiGradiInRadianti angoloGradi = angoloGradi * pi / 180

{- La funzione convertiRadiantiInGradi prende in ingresso il valore dell'ampiezza di un angolo in radianti e restituisce il
    suo valore in gradi. -}
convertiRadiantiInGradi :: Float -> Float
convertiRadiantiInGradi angoloRadianti = angoloRadianti * 180 / pi

