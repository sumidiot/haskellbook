module S11_12_Programmers where

data OperatingSystem =
  GnuPlusLinux | OpenBSDPlusNevermindJustBSDStill | Mac | Windows
  deriving (Eq, Show)

data ProgLang =
  Haskell | Agda | Idris | Purescript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem, lang :: ProgLang }
  deriving (Eq, Show)


-- exercise
allProgrammers :: [Programmer]
allProgrammers = [ Programmer os lang | os <- oss, lang <- langs ]
  where
    oss = [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]
    langs = [Haskell, Agda, Idris, Purescript]

