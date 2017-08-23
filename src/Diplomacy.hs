module Diplomacy where

import Data.Maybe

data Territory
  = Swi | Adr | Aeg | Alb | Ank | Apu | Arm | Bal | Bar | Bel | Ber | Bla | Boh | Bre | Bud | Bul | Bur | Cly
  | Con | Den | Eas | Edi | Eng | Fin | Gal | Gas | Gre | Lyo | Bot | Hel | Hol | Ion | Iri | Kie | Lvp | Lvn
  | Lon | Mar | Mao | Mos | Mun | Nap | Nao | Naf | Nth | Nwg | Nwy | Par | Pic | Pie | Por | Pru | Rom | Ruh
  | Rum | Ser | Sev | Sil | Ska | Smy | Spa | Stp | Swe | Syr | Tri | Tun | Tus | Tyr | Tys | Ukr | Ven | Vie
  | Wal | Wes | Yor deriving (Show, Eq)

data Move
  | Support Position Territory Territory
  | Move Position Territory
  | Hold Territory
  | Convoy Position
  deriving (Show, Eq)

data UnitType
  | Fleet
  | Army
  deriving (Show, Eq)


borders t1 t2 = maybeToBool $ f x y <|> f y x
  where
  f x y= lookup x adjacencyList >>= (y `elem`)

isLand :: Territory -> Bool
isLand = not . isSea

isSea :: Territory -> Bool
isSea Adr = True
isSea Aeg = True
isSea Bal = True
isSea Bar = True
isSea Bla = True
isSea Eas = True
isSea Eng = True
isSea Lyo = True
isSea Bot = True
isSea Hel = True
isSea Ion = True
isSea Iri = True
isSea Mao = True
isSea Nao = True
isSea Nth = True
isSea Nwg = True
isSea Ska = True
isSea Tys = True
isSea Wes = True
isSea _ = False

adjacencyList :: [(Territory, [Territory])]
adjacencyList =
  [ (Adr, [Alb, Api, Ion, Tri, Ven)
  , (Aeg, [])
  , (Alb, [Adr, Gre, Ser, Tri, Ion])
  , (Ank, [Bla, Con, Arm, Smy])
  , (Apu, [Adr, Ion, Nap, Rom, Ven])
  , (Arm, [Ank, Sev, Syr, Smy, Bla])
  , (Bal, [Pru, Lvn, Swe, Den, Hel, Ber, Kie, Bot])
  , (Bar, [Stp, Nwy])
  , (Bel, [Pic, Hol, Rub, Bur, Nth])
  , (Ber, [Kie, Bal, Pru, Sil])
  , (Bla, [Sev, Arm, Ank, Con, Bul, Aeg, Rum])
  , (Boh, [Sil, Mun, Vie, Gal, Tur])
  , (Bre, [Par, Pic, Gas, Eng, Mao])
  , (Bud, [Vie, Gal, Rum, Ser, Tri])
  , (Bul, [Ser, Rum, Bla, Aeg, Gre, Con])
  , (Bur, [Rub, Bel, Pic, Par, Mar, Gas])
  , (Cly, [Nao, Nwg, Edi, Lvp])
  , (Con, [Bul, Bla, Aeg, Smy, Ank])
  , (Den, [Ska, Bla, Hel, Nth, Kie]) -- Swe?
  , (Eas, [Smy, Syr, Aeg, Ion])
  , (Edi, [Cly, Lvp, Yor, Nth, Nwg])
  , (Eng, [Brew, Pic, Bel, Lon, Wal, Iri])
  , (Fin, [Bot, Swe, Stp, Nwy])
  , (Gal, [Sil, War, Ukr, Rum, Bud, Boh])
  , (Gas, [Mar, Bur, Par, Bre, Mao, Spa])
  , (Gre, [Aeg, Ion, Alb, Ser, Bul])
  , (Lyo, [Mar, Pie, Tus, Wes, Tys, Spa])
  , (Bot, [Fin, Swe, Lvn, Stp, Bal])
  , (Hel, [Kie, Hol, Den, Nth])
  , (Hol, [Kie, Hel, Nth, Bel, Rub])
  , (Ion, [Nap, Gre, Tun, Tys, Adr, Apu, Aeg, Eas])
  , (Iri, [Wal, Lvp, Mao, Eng, Nao])
  , (Kie, [Hel, Hol, Mun, Ber, Rub])
  , (Lvp, [Wal, Yor, Cly, Edi, Iri, Nao])
  , (Lvn, [Mos, Stp, Bot, Bal, Pru, War])
  , (Lon, [Yor, Nth, Eng, Wal])
  , (Mar, [Pie, Gas, Bur, Spa, Lyo])
  , (Mao, [Spa, Gas, Bre, Por, Iri, Eng, Nao])
  , (Mos, [Stp, Lvn, Sev, Ukr, War])
  , (Mun, [Boh, Tyr, Bur, Rub, Kie, Ber, Sil])
  , (Nap, [Tys, Ion, Api, Rom])
  , (Nao, [Nwg, Iri, Mao, Cli, Lvp])
  , (Naf, [Wes, Mao, Tun])
  , (Nth, [Edi, Lon, Hel, Ska, Nwy, Den, Yor, Hol, Bel])
  , (Nwy, [Swe, Nwg, Nth, Ska, Stp, Bar])
  , (Nwg, [Nwy, Bar, Nao, Nth, Edi, Cly])
  , (Par, [Pic, Bur, Bre, Gas])
  , (Pic, [Bel, Bur, Par, Bre, Eng])
  , (Pie, [Mar, Ven, Tus, Lyo, Tyr])
  , (Por, [Spa, Mao])
  , (Pru, [War, Lvn, Bal, Ber, Sil])
  , (Rom, [Nap, Apu, Ven, Tus, Tys])
  , (Ruh, [Kie, Mun, Bur, Bel, Hol])
  , (Rum, [Bla, Bul, Sev, Ukr, Gal, Bud, Ser])
  , (Ser, [Bud, Bul, Alb, Gre, Tri, Rum])
  , (Sev, [Bla, Mos, Ukr, Rum, Arm])
  , (Sil, [War, Pru, Ber, Mun, Boh, Gal])
  , (Ska, [Nwy, Swe, Den, Nth])
  , (Smy, [Eas, Aeg, Con, Ank, Arm, Syr])
  , (Spa, [Wes, Lyo, Mao, Mar, Gas, Por])
  , (Stp, [Mos, Lvn, Fin, Bar, Nwy])
  , (Swe, [Fin, Nwy, Ska, Bal, Bot])
  , (Syr, [Eas, Arm, Smy])
  , (Tri, [Vie, Tyr, Ven, Ser, Alb, Bud, Adr])
  , (Tun, [Naf, Tys, Ion, Wes])
  , (Tus, [Rom, Pie, Lyo, Ven, Tys])
  , (Tyr, [Mun, Boh, Vie, Tri, Ven, Pie])
  , (Tys, [Rom, Map, Tus, Ion, Wes, Lyo, Tun])
  , (Ukr, [Sev, Mos, Rum, Gal, War])
  , (Ven, [Tus, Pie, Tur, Tri, Adr])
  , (Vie, [Bud, Boh, Gal, Tri, Tyr])
  , (Wal, [Lon, Eng, Iri, Lvp, Yor])
  , (Wes, [Naf, Spa, Lyo, Tys, Tun])
  , (Yor, [Nth, Lon, Wal, Lvp, Edi])
  ]



resolve :: [PlayerMoves] -> 

validMove :: Move -> Bool
validMove (Hold _) = True
validMove (Move (source, _) dest) = source `borders` dest
validMove (Support (supTer, _) source dest) =  supporting `borders` dest && (source `borders` dest || source == dest)
validMove (Convoy (territory, _)) = isSea territory

type Position = (Territory, UnitType)

data Player = Player
  { name :: String
  , victoryPoints :: [Territory]
  , units :: [Position]
  } deriving (Show, Eq)

data Season = Spring | Fall | Winter deriving (Show, Eq)
data Turn = Turn { year :: Int, season :: Season } deriving (Show, Eq)

data Game
  { players :: [Player]
  , turn :: Turn
  } deriving (Show, Eq)

