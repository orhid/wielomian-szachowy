import System.Environment
import System.IO

-- oznaczenia dla czytelności

type Wiersz = Int
type Kolumna = Int
type Krawedz = (Wiersz, Kolumna)
type Graf = [Krawedz]


-- obliczenie wielomianu szachowego

--- sprawdza czy dane pole jest atakowane przez drugie dane pole
parujPola :: Krawedz -> Krawedz -> Bool
parujPola x y = fst x == fst y || snd x == snd y

--- usuwa z szachownicy wszystkie pola atakowane przez wieżę na wskazanym polu
czyscGraf :: Krawedz -> Graf -> Graf
czyscGraf = filter . (not .) . parujPola

--- znajduje wszystkie możliwe ustawienia k wież na szachownicy
wiezeSzukajKRazy :: Graf -> Int -> [Graf]
wiezeSzukajKRazy = (!!) . iterate ((map =<< flip czyscGraf) =<<) . return

{- bardziej czytelna wersja powyższej funkcji

--- z danej szachownicy tworzy wszystkie szachownice powstałe przez
--- wyrzucenie jednego pola (tudzież wszystkich atakowanych z niego pól) z danej szachownicy
wiezeRozbij :: Graf -> [Graf]
wiezeRozbij = map =<< flip czyscGraf

--- tworzy nową listę, kdzie każda szachownica z danej listy została rozbita na podszachownice poprzednią funkcją
wiezeSzukaj :: [Graf] -> [Graf]
wiezeSzukaj grafy = concat (map wiezeRozbij grafy) -- wersja jeszcze bardziej czytelna
wiezeSzukaj = (wiezeRozbij =<<) -- wersja zwarta

--- znajduje wszystkie możliwe ustawienia k wież na szachownicy
wiezeSzukajKRazy :: Graf -> Int -> [Graf]
wiezeSzukajKRazy = (!!) . iterate wiezeSzukaj . return
-}

--- mnoży liczbę otrzymanych szachownic przez możliwe permutacje ustawionych na nich wież
wiezePolicz :: [Graf] -> Int -> Int
wiezePolicz grafy k = quot (length grafy) (product [1..k])

--- dla danej szachownicy oblicza współczynnik o danym numerze
wieze :: Graf -> Int -> Int
wieze g k = wiezePolicz (wiezeSzukajKRazy g k) k

--- oblicza nieskończenie wiele współczynników wielomianu szachowego
szeregSzachowy :: Graf -> [Int]
szeregSzachowy = flip map [0 ..] . wieze

--- oblicza wielomian szachowy do podanego miejsca
--- domyślnie ma to być ostatnie definitywnie niezerowe miejsce
wielomianSzachowy :: Int -> Graf -> [Int]
wielomianSzachowy = flip (.) szeregSzachowy . take


-- rozkodowanie szachownicy z pliku tekstowego

--- funkcja pomocnicza do następnej
--- podmienia pierwszą wartość w danej parze na podany indeks
oznakuj :: Wiersz -> (Char, Kolumna) -> Krawedz
oznakuj metka para = (metka, snd para)

--- zamiania indeks wiersza, wiersz oraz indeksy kolumn w listę krawędzi danego wiersza
--- odrzuca te pola, które zostały oznaczone jako zablokowane
czytajWiersz :: Wiersz -> String -> [Kolumna] -> [Krawedz]
czytajWiersz metkaWiersza wiersz metki = map (oznakuj metkaWiersza) (filter (('o'==) . fst) (zip wiersz metki))

--- funkcja pomocnicza dla następnej
--- zamienia wiersz oraz jego indeks w listę krawędzi przy tym wierszu
_czytajWiersze :: String -> Wiersz -> [Krawedz]
_czytajWiersze wiersz metka = czytajWiersz metka wiersz [0 .. length wiersz]

--- zaminia listę wierszy oraz ich indeksów na graf
czytajWiersze :: [String] -> [Wiersz] -> Graf
czytajWiersze wiersze metki = concat (map (uncurry _czytajWiersze) (zip wiersze metki))

--- zamienia szachownicę wczytaną z pliku w graf dwudzielny indeksowany numerami wierszy i kolumn
--- w grafie istnieje krawędź pomiędzy wierszem w oraz kolumną k, jeśli pole (w,k) jest dozwolone
czytajPola :: [String] -> Graf
czytajPola wiersze = czytajWiersze wiersze [0 .. length wiersze]

--- określa krótszy bok szachownicy w celu ograniczenia szeregu szachowego
--- wszystkie wyrazy powyżej (krótszy bok + 1) muszą być zerowe
czytajRozmiar :: [String] -> Int
czytajRozmiar = (1+) . minimum . sequence [length, length . head]


-- połączenie rozkodowania i obliczenia

--- zamienia szachownicę wczytaną z pliku w jej wielomian szachowy
wielomianSzachownicy :: String -> [Int]
wielomianSzachownicy szachownica = wielomianSzachowy (czytajRozmiar wiersze) (czytajPola wiersze) where wiersze = lines szachownica


-- międzymordzie programu

main = do
  args <- getArgs
  case args of
    [] -> error "nie podano ścieżki"
    [arg] -> do
      szachownica <- readFile arg
      print $ wielomianSzachownicy szachownica
    _ -> error "podano niespodziewaną liczbę argumentów"
