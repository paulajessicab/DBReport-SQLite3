module AST where

import Database.HDBC.Sqlite3 (Connection)
import Graphics.PDF (PDFFont, FontName)
import Data.Bool

type Columns = [(String, String)] --Column, Table
type Header = String
type FooterBool = Bool

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
    
data Conditions = None  --Acomodar(!)
                | Cond1
                | Cond2
    deriving Show
           
data TStyle = TStyle PDFFont Position --PDFFont = PDFFont FontName Size
	deriving Show --hacer pretty printer

data BStyle = BStyle PageSize Margins PDFFont Position Header FooterBool
	deriving Show

data Title = Title String TStyle

data Repo = R Title Columns Conditions BStyle Connection --faltan condiciones
