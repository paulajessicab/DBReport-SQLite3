{-# LANGUAGE OverloadedStrings #-}

module DBReport where

import AST
import System.Environment
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad
import Data.List
import System.IO
--PDF
import Graphics.PDF
import Graphics.PDF.Typesetting
--


--ver http://book.realworldhaskell.org/read/using-databases.html
--con https://dev.mysql.com/doc/connector-odbc/en/connector-odbc-installation-binary-unix-tarball.html
--interprete tp lambda calculo

{-////////////////| Inicialización |\\\\\\\\\\\\\\\\-}
defaultFont :: PDFFont
defaultFont = PDFFont Times_Roman 12

initTStyle :: TStyle
initTStyle = TStyle defaultFont Center

initTitle :: Title
initTitle = Title "Nuevo Reporte" initTStyle

initBStyle :: BStyle
initBStyle = BStyle A4 Default defaultFont Center [] True

initCond :: Conditions
initCond = Conditions False [] None

initRepo :: Connection -> Repo
initRepo conn = R initTitle [("ID","Clientes"),("NAME","Clientes")] initCond initBStyle conn

{-\\\\\\\\\\\\\\\\\\\\\\\\\/////////////////////////-}

{-////////////////| Modificación |\\\\\\\\\\\\\\\\-}

{----------------Título----------------}
--Cambia el contenido del título
title :: String -> Repo -> Repo
title newttl repo@(R ttl col cond bstl conn) = R (Title newttl stl') col cond bstl conn
                                                   where stl' = get_title_stl repo 

--Cambia la fuente del título													      
title_font :: FontName -> Repo -> Repo
title_font font (R (Title ttl stl) col cond bstl conn) = R (Title ttl stl') col cond bstl conn
													          where stl' = change_tfont font stl
                                                                    
--Cambia el tamaño del título
title_size :: Int -> Repo -> Repo
title_size size (R (Title ttl stl) col cond bstl conn) = R (Title ttl stl') col cond bstl conn
													         where stl' = change_tsize size stl

--Cambia la posición del título
title_pos :: Position -> Repo -> Repo
title_pos pos (R (Title ttl stl) col cond bstl conn) = R (Title ttl stl') col cond bstl conn
													       where stl' = change_tpos pos stl

--Cambia todo el estilo del título
title_stl :: FontName -> Int -> Position -> Repo -> Repo
title_stl font size pos repo =  title_pos pos $ title_font font $ title_size size repo
{--------------------------------------}


{----------------Cuerpo----------------}

--BStyle PageSize Margins PDFFont Position Header FooterBool

--Cambia el tamaño de página
page_size :: PageSize -> Repo -> Repo
page_size psize (R ttl col cond bstl conn) = R ttl col cond bstl' conn
                                                 where bstl' = change_bpagesize psize bstl

margins :: Margins -> Repo -> Repo
margins marg (R ttl col cond bstl conn) = R ttl col cond bstl' conn
                                                 where bstl' = change_bmargins marg bstl

--Cambia las columnas (En particular, también puede eliminar todas)
columns :: Columns -> Repo -> Repo
columns col' (R ttl col cond bstl conn) = R ttl col' cond bstl conn

--Inserta una columna al principio
prepend_column :: String -> String -> Repo -> Repo
prepend_column column table (R ttl col cond bstl conn) = R ttl ((column, table):col) cond bstl conn

--Inserta una columna al final
append_column :: String -> String -> Repo -> Repo
append_column column table (R ttl col cond bstl conn) = R ttl (col++[(column,table)]) cond bstl conn

--Inserta una columna en la n-ésima posición
insert_column :: Int -> String -> String -> Repo -> Repo
insert_column n column table repo@(R ttl col cond bstl conn) = if n < length(col) && n >= 0
                                                               then R ttl col' cond bstl conn
                                                               else repo
                                                                   where (xs,ys) = splitAt n col
                                                                         col' = xs++[(column,table)]++ys

--Elimina una columna dado un nombre
erase_column :: String -> Repo -> Repo
erase_column name (R ttl col cond bstl conn) = R ttl col' cond bstl conn
                                                   where col' = filter (\(x,y) -> x /= name) col

--Elimina todas las columnas de una tabla (y condiciones)
erase_all_columns :: Repo -> Repo
erase_all_columns (R ttl col cond bstl conn) = R ttl [] initCond bstl conn
					      
--Cambia la fuente del cuerpo													      
body_font :: FontName -> Repo -> Repo
body_font font (R ttl col cond bstl conn) = R ttl col cond bstl' conn
											where bstl' = change_bfont font bstl

--Cambia el tamaño del cuerpo
body_size :: Int -> Repo -> Repo
body_size size (R ttl col cond bstl conn) = R ttl col cond bstl' conn
											where bstl' = change_bsize size bstl

--Cambia la posición del cuerpo
body_pos :: Position -> Repo -> Repo
body_pos pos (R ttl col cond bstl conn) = R ttl col cond bstl' conn
											where bstl' = change_bpos pos bstl

--Cambia todo el estilo del cuerpo
body_stl :: FontName -> Int -> Position -> Repo -> Repo
body_stl font size pos repo = body_pos pos $ body_font font $ body_size size repo

--Cambia el encabezado
header :: Header -> Repo -> Repo
header text (R ttl col cond bstl conn) = R ttl col cond bstl' conn
											where bstl' = change_header text bstl

--Inserta/Elimina número de página al pie
footer :: FooterBool -> Repo -> Repo
footer bool (R ttl col cond bstl conn) = R ttl col cond bstl' conn
                                         where bstl' = change_footer bool bstl
                                         
--Habilita/Deshabilita registros únicos
distinct :: Distinct -> Repo -> Repo
distinct bool (R ttl col cond bstl conn) = R ttl col cond' bstl conn
                                                      where cond' = change_distinct bool cond

--Inserta/Elimina condiciones where
cond_where :: String -> Repo -> Repo
cond_where clause (R ttl col cond bstl conn) = R ttl col cond' bstl conn
                                                   where cond' = change_where clause cond
                                                  
--Cambia o desactiva ordenamiento
order_by :: OrderBy -> Repo -> Repo
order_by order (R ttl col cond bstl conn) = R ttl col cond' bstl conn
                                                where cond' = change_order order cond       


{--------------------------------------}												

{-\\\\\\\\\\\\\\\\\\\\\\\\////////////////////////-}


{-/////////////////| Ejecución |\\\\\\\\\\\\\\\\\-}
generate :: Repo -> IO ()
generate repo = do xs <- quickQuery' conn ("SELECT "++cols++" from "++tables) [] --Cuidado cuando no hay nada! 
                   let stringRows = map convRow xs --stringRows :: [String]

                   let pdfFileName = (get_title repo)
                   let documentInfo = standardDocInfo 
                   let pageSize = toPageSize $ get_psize repo
                   --let defaultPageSize = PDFRect 0 0 200 300
  
                   let pdfFont = PDFFont Times_Roman 12
                   let titleFont = get_title_font repo  
                                       
                   runPdf pdfFileName documentInfo pageSize $ do
                       page <- addPage Nothing
                        
                       drawWithPage page $ do
                           --drawText $ text titleFont 10.0 10.0 $ toPDFString $ get_title repo
                           drawText $ text pdfFont 10.0 10.0 $ toPDFString (head stringRows) --(!)
                   
                   where conn = get_connection repo
                         cols_tables = unzip $ get_columns repo
                         cols = unwords' $ fst cols_tables --unwords' agrega comas
                         tables = unwords' $ nub $ snd cols_tables --nub saca repeticiones

convRow :: [SqlValue] -> String --arreglar formato
convRow [] = []
convRow [x] = case fromSql x of
                    Just y -> y
                    Nothing -> "NULL"
convRow (x:xs) = case fromSql x of
                    Just y -> y ++ " " ++ (convRow xs)
                    Nothing -> "NULL " ++ (convRow xs)

{-\\\\\\\\\\\\\\\\\\\\\\\///////////////////////-}

get_title :: Repo -> String
get_title (R (Title ttl _) _ _ _ _) = ttl

get_title_stl :: Repo -> TStyle
get_title_stl (R (Title _ stl) _ _ _ _) = stl

get_title_font :: Repo -> PDFFont
get_title_font repo = let (TStyle font pos) = get_title_stl repo
                      in font

get_psize :: Repo -> PageSize
get_psize (R _ _ _ bstl _) = get_psize' bstl
                                 where get_psize' (BStyle size _ _ _ _ _) = size

get_body_stl :: Repo -> BStyle
get_body_stl (R _ _ _ bstl _) = bstl

get_columns :: Repo -> [(String, String)]
get_columns (R _ col _ _ _) = col

get_connection :: Repo -> Connection
get_connection (R _ _ _ _ conn) = conn

-- get_data :: Repo -> [String] o algo así.. columnas c/restr
-- get_conditions

-- Funciones Auxiliares
change_tfont :: FontName -> TStyle -> TStyle
change_tfont font' (TStyle (PDFFont font size) pos) = TStyle (PDFFont font' size) pos

change_tsize :: Int -> TStyle -> TStyle
change_tsize size' (TStyle (PDFFont font size) pos) = TStyle (PDFFont font size') pos

change_tpos :: Position -> TStyle -> TStyle
change_tpos pos' (TStyle font pos) = TStyle font pos'

change_bpagesize :: PageSize -> BStyle -> BStyle
change_bpagesize ps' (BStyle ps m f p h f') = BStyle ps' m f p h f'

change_bmargins :: Margins -> BStyle -> BStyle
change_bmargins marg' (BStyle ps marg f p h f') = BStyle ps marg' f p h f'

change_bfont :: FontName -> BStyle -> BStyle
change_bfont font' (BStyle ps m (PDFFont font size) p h f) = BStyle ps m (PDFFont font' size) p h f

change_bsize :: Int -> BStyle -> BStyle
change_bsize size' (BStyle ps m (PDFFont font size) p h f) = BStyle ps m (PDFFont font size') p h f

change_bpos :: Position -> BStyle -> BStyle
change_bpos pos' (BStyle ps m f pos h f') = BStyle ps m f pos' h f'

change_header :: Header -> BStyle -> BStyle
change_header header' (BStyle ps m f p header f') = BStyle ps m f p header' f'

change_footer :: Bool -> BStyle -> BStyle
change_footer bool (BStyle ps m f p h f') = BStyle ps m f p h bool

change_distinct :: Bool -> Conditions -> Conditions
change_distinct bool (Conditions dist wh order) = Conditions bool wh order

change_where :: String -> Conditions -> Conditions
change_where clause (Conditions dist wh order) = Conditions dist clause order

change_order :: OrderBy -> Conditions -> Conditions
change_order order' (Conditions dist wh order) = Conditions dist wh order'

--BStyle PageSize Margins PDFFont Position Header FooterBool
--
toPageSize :: PageSize -> PDFRect
toPageSize A4    = PDFRect 0 0 210 297
toPageSize Legal = PDFRect 0 0 216 356
toPageSize (Other x y) = PDFRect 0 0 x y


--VER unfold
unwords' []              = ""
unwords' [w]             = w
unwords' (w:ws)          = w ++ ", " ++ unwords' ws
  where
    go [w]     = w
    go (v:vs) = ',' : (v ++ go vs)
