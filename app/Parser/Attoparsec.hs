{-# LANGUAGE OverloadedStrings #-}
module Parser.Attoparsec where

import Data.Word
import Data.Attoparsec.Internal.Types (Parser)
import qualified Data.Attoparsec.ByteString as PB
import Data.Attoparsec.ByteString.Char8 (decimal, signed, scientific, digit, rational, char, char8, endOfLine, endOfInput, isDigit, isDigit_w8, isEndOfLine, isHorizontalSpace)

import qualified Data.ByteString as B

import Control.Applicative ((<|>))

import Data.Time

-- data Customer0 =
--   Customer0 { fecha_dato :: !String,
--               ncodpers :: !Int,
--               ind_empleado :: !Char,
--               pais_residencia :: !String,
--               sexo :: !Char,
--               age :: !Int,
--               fecha_alta :: !String,       --date
--               ind_nuevo :: !Int,
--               antiguedad :: !Int,
--               indrel :: !Int,
--               ult_fec_cli_1t :: !String,
--               indrel_1mes :: !Double,
--               tiprel_1mes :: !Char,
--               indresi :: !Char,
--               indext :: !Char,
--               conyuemp :: !Char,
--               canal_entrada :: !String,
--               indfall :: !Char,
--               tipodom :: !Int,
--               cod_prov :: !Int,
--               nomprov :: !String,
--               ind_actividad_cliente :: !Int,
--               renta :: !Double,
--               segmento :: !String,
--               ind_ahor_fin_ult1, ind_aval_fin_ult1, ind_cco_fin_ult1, ind_cder_fin_ult1, ind_cno_fin_ult1, ind_ctju_fin_ult1, ind_ctma_fin_ult1, ind_ctop_fin_ult1, ind_ctpp_fin_ult1, ind_deco_fin_ult1, ind_deme_fin_ult1, ind_dela_fin_ult1, ind_ecue_fin_ult1, ind_fond_fin_ult1, ind_hip_fin_ult1, ind_plan_fin_ult1, ind_pres_fin_ult1, ind_reca_fin_ult1, ind_tjcr_fin_ult1, ind_valo_fin_ult1, ind_viv_fin_ult1, ind_nomina_ult1, ind_nom_pens_ult1, ind_recibo_ult1 :: !Int} deriving (Eq, Show)

-- 2016-06-28,15889,F,ES,V,56,1995-01-16,0,256,1,,1,A,S,N,N,KAT,N,1,28,"MADRID",1,326124.9,01 - TOP
-- 2016-06-28,1170544,N,ES,H,36,2013-08-28,0,34,1,,1,I,S,N,,KAT,N,1,3,"ALICANTE",0,,02 - PARTICULARES
-- 2016-06-28,1170545,N,ES,V,22,2013-08-28,0,34,1,,1,A,S,N,,KHE,N,1,15,"CORUÃ‘A, A",1,,03 - UNIVERSITARIO

data CustomerData =
  CustomerData {fecha_dato :: Day,
                ncodpers :: Int,
                ind_empleado :: EmployeeStatus,
                pais :: Country,
                sexo :: Gender,
                age :: Int,
                fecha_alta :: Day,
                ind_nuevo :: Bool
                }

parseCustomerData = do
  fecha_dato <- parseDate
  comma
  ncodpers <- decimal
  comma
  ind_empleado <- parseEmployeeStatus
  comma
  pais <- parseCountry
  comma
  sexo <- parseGender
  comma
  age <- decimal
  comma
  fecha_alta <- parseDate
  comma
  ind_nuevo <- parseIndNuevo
  comma
  antiguedad <- decimal
  comma
  indrel <- parseIndRel
  comma
  ult_fec_cli_1t <- decimal
  comma
  indrel_1mes <- parseIndRel1Mes
  comma
  tiprel_1mes <- parseTipRel1Mes
  comma
  indresi <- parseBooleanES
  comma
  indext <- parseBooleanES
  comma
  conyuemp <- parseConyuEmp
  comma
  canalentrada <- parseCanalEntrada
  comma
  deceased <- parseBooleanES
  comma
  tipodom <- char '1' >> return True
  comma
  codprov <- decimal
  comma
  nomProv <- parseProvince
  comma
  ind_actividad <- char '1' >> return True
  comma
  renta <- scientific
  comma
  segment <- parseSegment
  comma
  return $ CustomerData fecha_dato ncodpers ind_empleado pais sexo age fecha_alta ind_nuevo

-- responses
-- ind_ahor_fin_ult1, ind_aval_fin_ult1, ind_cco_fin_ult1, ind_cder_fin_ult1, ind_cno_fin_ult1, ind_ctju_fin_ult1, ind_ctma_fin_ult1, ind_ctop_fin_ult1, ind_ctpp_fin_ult1, ind_deco_fin_ult1, ind_deme_fin_ult1, ind_dela_fin_ult1, ind_ecue_fin_ult1, ind_fond_fin_ult1, ind_hip_fin_ult1, ind_plan_fin_ult1, ind_pres_fin_ult1, ind_reca_fin_ult1, ind_tjcr_fin_ult1, ind_valo_fin_ult1, ind_viv_fin_ult1, ind_nomina_ult1, ind_nom_pens_ult1, ind_recibo_ult1 

comma :: PB.Parser Word8
comma = char8 ','

parseDate :: Parser B.ByteString Day
parseDate = do
  y <- decimal
  char '-'
  m <- decimal
  char '-'
  d <- decimal
  return $ fromGregorian y m d

-- ind_empleado	Employee index: A active, B ex employed, F filial, N not employee, P pasive
data EmployeeStatus = Active | ExEmployee | Filial | NotEmployee | Passive deriving (Eq, Show)
parseEmployeeStatus = (char8 'A' >> return Active) <|>
                      (char8 'B' >> return ExEmployee) <|>
                      (char8 'F' >> return Filial) <|>
                      (char8 'N' >> return NotEmployee) <|>
                      (char8 'P' >> return Passive)

data Gender = M | F deriving (Eq, Show)
parseGender = (char8 'H' >> return M) <|> (char8 'V' >> return F)

parseIndNuevo = char8 '1' >> return True


-- indrel	1 (First/Primary), 99 (Primary customer during the month but not at the end of the month)
parseIndRel = (PB.string "1" >> return True) <|>
              (PB.string "99" >> return False)

-- indrel_1mes	Customer type at the beginning of the month ,1 (First/Primary customer), 2 (co-owner ),P (Potential),3 (former primary), 4(former co-owner)
data IndRel1Mes = Primary | CoOwner | Potential | FormerPrimary | FormerCoOwner deriving (Eq, Show)
parseIndRel1Mes = (char8 '1' >> return Primary) <|>
                 (char8 '2' >> return CoOwner) <|>
                 (char8 'P' >> return Potential) <|>
                 (char8 '3' >> return FormerPrimary) <|>
                 (char8 '4' >> return FormerCoOwner)

-- tiprel_1mes	Customer relation type at the beginning of the month, A (active), I (inactive), P (former customer),R (Potential)
data TipRel1Mes = TRActive | TRInactive | TRFormerCustomer | TRPotential deriving (Eq, Show)
parseTipRel1Mes = (char8 'A' >> return TRActive) <|>
                 (char8 'I' >> return TRInactive) <|>
                 (char8 'P' >> return TRFormerCustomer) <|>
                 (char8 'R' >> return TRPotential) 

-- indresi	Residence index (S (Yes) or N (No) if the residence country is the same than the bank country)
-- indext	Foreigner index (S (Yes) or N (No) if the customer's birth country is different than the bank country)
-- conyuemp	Spouse index. 1 if the customer is spouse of an employee
parseBooleanES = (char8 'S' >> return True) <|>
                 (char8 'N' >> return False)

parseConyuEmp = char8 '1' >> return True                 

data CanalEntrada = KHE | KAT deriving (Eq, Show)
parseCanalEntrada = (PB.string "KHE" >> return KHE) <|>
                    (PB.string "KAT" >> return KAT)
  
data Province = Barcelona | Madrid | Coruna | Alicante | Albacete | Valladolid
              | Cantabria | Cordoba | Zamora | Pontevedra | Girona | Caceres
              | Lerida | Jaen | Burgos | Malaga | Sevilla | CiudadReal | Cuenca
              | IllesBalears | Zaragoza | Castellon | Valencia | Salamanca | Huesca
              | Badajoz | Navarra | Leon | Palencia | Ourense | Rioja deriving (Eq, Show)
parseProvince = (PB.string "BARCELONA" >> return Barcelona) <|>
            (PB.string "MADRID" >> return Madrid) <|>
            (PB.string "CORUÃ‘A, A" >> return Coruna) <|>
            (PB.string "ALICANTE" >> return Alicante) <|>
            (PB.string "ALBACETE" >> return Albacete) <|>
            (PB.string "VALLADOLID" >> return Valladolid) <|>
            (PB.string "CANTABRIA" >> return Cantabria) <|>
            (PB.string "CORDOBA" >> return Cordoba) <|>
            (PB.string "ZAMORA" >> return Zamora) <|>
            (PB.string "PONTEVEDRA" >> return Pontevedra) <|>
            (PB.string "GIRONA" >> return Girona) <|>
            (PB.string "CACERES" >> return Caceres) <|>
            (PB.string "LERIDA" >> return Lerida) <|>
            (PB.string "JAEN" >> return Jaen) <|>
            (PB.string "BURGOS" >> return Burgos) <|>
            (PB.string "MALAGA" >> return Malaga) <|>
            (PB.string "SEVILLA" >> return Sevilla) <|>
            (PB.string "CIUDAD REAL" >> return CiudadReal) <|>
            (PB.string "CUENCA" >> return Cuenca) <|>
            (PB.string "BALEARS, ILLES" >> return IllesBalears) <|>
            (PB.string "ZARAGOZA" >> return Zaragoza) <|>
            (PB.string "CASTELLON" >> return Castellon) <|>
            (PB.string "VALENCIA" >> return Valencia) <|>
            (PB.string "SALAMANCA" >> return Salamanca) <|>
            (PB.string "HUESCA" >> return Huesca) <|>
            (PB.string "BADAJOZ" >> return Badajoz) <|>
            (PB.string "NAVARRA" >> return Navarra) <|>
            (PB.string "LEON" >> return Leon) <|>            
            (PB.string "PALENCIA" >> return Palencia) <|>
            (PB.string "OURENSE" >> return Ourense) <|>
            (PB.string "RIOJA" >> return Rioja)


data Country = ES deriving (Eq, Show)
parseCountry = PB.string "ES" >> return ES

data Segment = Universitario | Particulares | Top deriving (Eq, Show)

parseSegment = (PB.string "02 - PARTICULARES" >> return Particulares) <|>
            (PB.string "03 - UNIVERSITARIO" >> return Universitario) <|>
            (PB.string "01 - TOP" >> return Top)

-- parseNameProv = do
--   char '\"'
--   p <- string
--   char '\"'
--   return $ NameProv p