{-# LANGUAGE OverloadedStrings #-}
module Parser.Attoparsec where

import Data.Word
import Data.Attoparsec.Internal.Types (Parser)
import qualified Data.Attoparsec.ByteString as PB
import Data.Attoparsec.ByteString.Char8 (decimal, signed, double, digit, rational, char, char8, space, endOfLine, endOfInput, isDigit, isDigit_w8, isEndOfLine, isHorizontalSpace, space)

import qualified Data.ByteString as B
import qualified Data.Vector as V

import Control.Applicative ((<|>), Alternative)

import Data.Time


parseData :: Parser B.ByteString (V.Vector Customer)
parseData = do
  dd <- PB.many1 parseCustomer <* endOfInput
  return $ V.fromList dd

parseCustomer :: Parser B.ByteString Customer
parseCustomer = do
  cd <- parseCustomerData
  cr <- parseResponses <* endOfLine
  return $ Customer cd cr

data Customer = Customer CustomerData Responses deriving (Eq, Show)


-- 2016-06-28,15889,F,ES,V,56,1995-01-16,0,256,1, ,1,A,S,N,N,KAT,N,1,28,"MADRID",1,326124.9,01 - TOP
-- 2016-06-28,1170544,N,ES,H,36,2013-08-28,0,34,1, ,1,I,S,N, ,KAT,N,1,3,"ALICANTE",0,,02 - PARTICULARES
-- 2016-06-28,1170545,N,ES,V,22,2013-08-28,0,34,1,,1,A,S,N,,KHE,N,1,15,"CORUÃ‘A, A",1,,03 - UNIVERSITARIO

data CustomerData =
  CustomerData {fecha_dato :: Day,
                ncodpers :: Int,
                ind_empleado :: EmployeeStatus,
                pais :: Country,
                sexo :: Gender,
                age :: Int,
                fecha_alta :: Day,         -- date
                ind_nuevo :: Bool,
                antiguedad :: Int,   
                indrel :: Bool,
                ult_fec_cli_1t :: Maybe Char,     -- can be empty
                indrel_1mes :: IndRel1Mes,
                tiprel_1mes :: TipRel1Mes,
                indresi :: Bool,
                indext :: Bool,
                conyuemp :: Maybe Char,        -- can be empty
                canalentrada :: Maybe CanalEntrada,
                deceased :: Maybe Bool,
                tipodom :: Maybe Bool,
                codprov :: Maybe Int,
                nomprov :: Maybe Province,
                ind_actividad :: Maybe Bool,
                renta :: Maybe Double,
                segment :: Maybe Segment
                } deriving (Eq, Show)

-- | useful device
optional :: Alternative f => f a -> f (Maybe a)
optional p = PB.option Nothing (Just <$> p)


parseCustomerData :: Parser B.ByteString CustomerData
parseCustomerData = do
  fd <- parseDate <* comma
  ncod <- decimal <* comma
  ind_empl <- parseEmployeeStatus <* comma
  country <- parseCountry <* comma
  gender <- parseGender <* comma
  a <- decimal <* comma
  fa <- parseDate <* comma
  newcustomer <- parseBit <* comma
  acct_age <- decimal <* comma
  ir <- parseIndRel <* comma
  ufc1t <- optional space <* comma -- optional (parseDate <* comma) <* comma
  ir_1m <- parseIndRel1Mes <* comma     -- 1
  tr_1m <- parseTipRel1Mes <* comma     -- I 
  ind_resi <- parseBooleanES <* comma   -- S
  ind_ext <- parseBooleanES <* comma    -- N
  conyu_emp <- optional space <* comma -- optional $ parseBit <* comma
  canale <- optional $ parseCanalEntrada <* comma
  dead <- optional $ parseBooleanES <* comma
  tipo_dom <- optional $ parseBit <* comma
  cod_prov <- optional $ decimal <* comma
  nom_prov <- optional $ parseProvince <* comma
  ind_activ <- optional $ parseBit <* comma
  salary <- optional $ double <* comma
  segm <- optional $ parseSegment <* comma
  return $ CustomerData fd ncod ind_empl country gender a fa newcustomer acct_age ir ufc1t ir_1m tr_1m ind_resi ind_ext conyu_emp canale dead tipo_dom cod_prov nom_prov ind_activ salary segm

-- parseUfc1t = parseDate <|> space



data Responses =
  Responses {ind_ahor_fin_ult1, ind_aval_fin_ult1, ind_cco_fin_ult1,
             ind_cder_fin_ult1, ind_cno_fin_ult1, ind_ctju_fin_ult1,
             ind_ctma_fin_ult1, ind_ctop_fin_ult1, ind_ctpp_fin_ult1,
             ind_deco_fin_ult1, ind_deme_fin_ult1, ind_dela_fin_ult1,
             ind_ecue_fin_ult1, ind_fond_fin_ult1, ind_hip_fin_ult1,
             ind_plan_fin_ult1, ind_pres_fin_ult1, ind_reca_fin_ult1,
             ind_tjcr_fin_ult1, ind_valo_fin_ult1, ind_viv_fin_ult1,
             ind_nomina_ult1, ind_nom_pens_ult1, ind_recibo_ult1  :: Bool}
  deriving (Eq, Show)

parseResponses :: Parser B.ByteString Responses
parseResponses = Responses <$> parseBEntry <*> parseBEntry <*> parseBEntry <*> parseBEntry <*> parseBEntry <*> parseBEntry <*> parseBEntry <*> parseBEntry <*> parseBEntry <*> parseBEntry <*> parseBEntry <*> parseBEntry <*> parseBEntry <*> parseBEntry <*> parseBEntry <*> parseBEntry <*> parseBEntry <*> parseBEntry <*> parseBEntry <*> parseBEntry <*> parseBEntry <*> parseBEntry <*> parseBEntry <*> parseBit where
  parseBEntry :: Parser B.ByteString Bool            
  parseBEntry = parseBit <* comma




parseBit :: Parser B.ByteString Bool
parseBit = (char8 '1' >> return True) <|> (char8 '0' >> return False)

comma :: PB.Parser Word8
comma = char8 ','

-- | parse a date in Y-M-D format
parseDate :: Parser B.ByteString Day
parseDate = do
  y <- decimal
  char '-'
  m <- decimal
  char '-'
  d <- decimal
  return $ fromGregorian y m d

data EmployeeStatus = Active | ExEmployee | Filial | NotEmployee | Passive | EmployeeStatus_NA deriving (Eq, Show)
parseEmployeeStatus :: Parser B.ByteString EmployeeStatus
parseEmployeeStatus = (char8 'A' >> return Active) <|>
                      (char8 'B' >> return ExEmployee) <|>
                      (char8 'F' >> return Filial) <|>
                      (char8 'N' >> return NotEmployee) <|>
                      (char8 'P' >> return Passive) <|>
                      (PB.many' space >> return EmployeeStatus_NA)

data Gender = Male | Female deriving (Eq, Show)
parseGender :: Parser B.ByteString Gender
parseGender = (char8 'H' >> return Male) <|> (char8 'V' >> return Female)




-- indrel	1 (First/Primary), 99 (Primary customer during the month but not at the end of the month)
parseIndRel :: Parser B.ByteString Bool
parseIndRel = (PB.string "1" >> return True) <|>
              (PB.string "99" >> return False)

-- indrel_1mes	Customer type at the beginning of the month ,1 (First/Primary customer), 2 (co-owner ),P (Potential),3 (former primary), 4(former co-owner)
data IndRel1Mes = Primary | CoOwner | Potential | FormerPrimary | FormerCoOwner | IndRel1Mes_NA deriving (Eq, Show)
parseIndRel1Mes :: Parser B.ByteString IndRel1Mes
parseIndRel1Mes = (char8 '1' >> return Primary) <|>
                  (char8 '2' >> return CoOwner) <|>
                  (char8 'P' >> return Potential) <|>
                  (char8 '3' >> return FormerPrimary) <|>
                  (char8 '4' >> return FormerCoOwner) <|>
                  (PB.many' space >> return IndRel1Mes_NA)

-- tiprel_1mes	Customer relation type at the beginning of the month, A (active), I (inactive), P (former customer),R (Potential)
data TipRel1Mes = TRActive | TRInactive | TRFormerCustomer | TRPotential | TR_NA deriving (Eq, Show)
parseTipRel1Mes = (char8 'A' >> return TRActive) <|>
                  (char8 'I' >> return TRInactive) <|>
                  (char8 'P' >> return TRFormerCustomer) <|>
                  (char8 'R' >> return TRPotential) <|>
                  (PB.many' space >> return TR_NA)

-- indresi	Residence index (S (Yes) or N (No) if the residence country is the same than the bank country)
-- indext	Foreigner index (S (Yes) or N (No) if the customer's birth country is different than the bank country)
parseBooleanES :: Parser B.ByteString Bool
parseBooleanES = (char8 'S' >> return True) <|>
                 (char8 'N' >> return False)


data CanalEntrada = KHE | KHD | KAT deriving (Eq, Show)
parseCanalEntrada :: Parser B.ByteString CanalEntrada
parseCanalEntrada = (PB.string "KHE" >> return KHE) <|>
                    (PB.string "KHD" >> return KHD) <|>
                    (PB.string "KAT" >> return KAT)
  
data Province = Barcelona | Madrid | Coruna | Alicante | Albacete | Valladolid
              | Cantabria | Cordoba | Zamora | Pontevedra | Girona | Caceres
              | Lerida | Jaen | Burgos | Malaga | Sevilla | CiudadReal | Cuenca
              | IllesBalears | Zaragoza | Castellon | Valencia | Salamanca | Huesca
              | Badajoz | Navarra | Leon | Palencia | Ourense | Rioja deriving (Eq, Show)
parseProvince :: Parser B.ByteString Province                
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
parseCountry :: Parser B.ByteString Country
parseCountry = PB.string "ES" >> return ES

data Segment = Universitario | Particulares | Top deriving (Eq, Show)

parseSegment :: Parser B.ByteString Segment
parseSegment =
  --   (PB.string "02" >> return Particulares) <|>
  -- (PB.string "03" >> return Universitario) <|>
  -- (PB.string "01" >> return Top)
  (PB.string "02 - PARTICULARES" >> return Particulares) <|>
  (PB.string "03 - UNIVERSITARIO" >> return Universitario) <|>
  (PB.string "01 - TOP" >> return Top)

-- parseNameProv = do
--   char '\"'
--   p <- string
--   char '\"'
--   return $ NameProv p
