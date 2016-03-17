module AST where

import Database.HDBC.Sqlite3 (Connection)
import Graphics.PDF (PDFFont, FontName)
import Data.Bool

import Control.Monad.State --

type Header = String
type Where = String
type FooterBool = Bool
type Distinct = Bool
type Columns = [(String,String)] --estrella (!)

{- Fuentes
FontName = Helvetica 
              | Helvetica_Bold
              | Helvetica_Oblique
              | Helvetica_BoldOblique
              | Times_Roman 
              | Times_Bold
              | Times_Italic
              | Times_BoldItalic
              | Courier
              | Courier_Bold
              | Courier_Oblique
              | Courier_BoldOblique
              | Symbol
              | ZapfDingbats   
    
data Columns = Columns [(String, String)] --Column, table
             | Star String                --all from tables
-}           
data PageSize = A4
              | Legal
              | Other Int Int --width, height
    deriving Show

data Margins = Margins Int Int Int Int --Top, Right, Botom, Left
             | Default --ver
   deriving Show
   
data Position = Center
              | Left
              | Right
              | Justified
	deriving Show
    
data Conditions = Conditions Distinct Where OrderBy
    deriving Show
    
data OrderBy = OrderBy String Dir
             | None
    deriving Show

data Dir = Asc
         | Desc
    deriving Show
    
data TStyle = TStyle PDFFont Position --PDFFont = PDFFont FontName Size
	deriving Show --hacer pretty printer

data BStyle = BStyle PageSize Margins PDFFont Position Header FooterBool
	deriving Show

data Title = Title String TStyle

data Repo = R Title Columns Conditions BStyle Connection --faltan condiciones
