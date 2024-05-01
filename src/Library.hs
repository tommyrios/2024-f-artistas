module Library where
import PdePreludat

type Cancion = String

data Artista = UnArtista {
    nombre :: String,
    canciones :: [Cancion]
} deriving Show

fitito :: Artista
fitito = UnArtista "Fitito Paez" ["11 y 6", "El amor despues del amor", "Mariposa Tecknicolor"]

calamardo :: Artista
calamardo = UnArtista "Andres Calamardo" ["Flaca", "Sin Documentos", "Tuyo siempre"]

paty :: Artista
paty = UnArtista "Taylor Paty" ["Shake It Off", "Lover"]

--punto 1

calificacion :: Cancion -> Number
calificacion = (+ 10) . length  . filter (`elem` ['a'..'z'])

--punto 2

cancionBuena :: Cancion -> Bool
cancionBuena = (>20) . calificacion

cancionesBuenas :: Artista -> [Cancion]
cancionesBuenas artista = filter cancionBuena (canciones artista)

esExitoso :: Artista -> Bool
esExitoso artista = (> 50) . sum . map calificacion $ cancionesBuenas artista

--punto 3

grupo :: [Artista]
grupo = [fitito, calamardo, paty]

artistasExitosos :: [Artista] -> [Artista]
artistasExitosos = filter esExitoso

--punto 4

todoJunto :: [Artista] -> [Artista]
todoJunto artistas = filter (\artista -> sum (map (\cancion -> (>20) ((length . filter (`elem` ['a'..'z'])) cancion + 10)) (canciones artista)) > 50) artistas



--