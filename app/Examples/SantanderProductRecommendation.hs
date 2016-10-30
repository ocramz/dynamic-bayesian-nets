{-# LANGUAGE OverloadedStrings #-}
module Examples.SantanderProductRecommendation where

import Lib
import Parser.Attoparsec

import Data.Maybe

import Data.Attoparsec.ByteString.Char8  hiding (ByteString, take)
import qualified Data.Attoparsec.ByteString as PB hiding (skip,skipWhile,take)

-- import qualified Data.List as List
-- import qualified Data.ByteString.Lazy as B hiding (split, pack)
import qualified Data.ByteString.Lazy.Char8 as BL8 -- hiding (readFile)-- (split, pack)
import qualified Data.ByteString as B hiding (take, drop, putStrLn)
import qualified Data.Vector as V

import Control.Applicative


datasetPath = "test-data/SantanderProductRecommendation/train-part-noheader.csv"


main = do
  csv <- B.readFile datasetPath
  case PB.parseOnly parseData csv of
    Left e -> putStrLn e
    Right v -> print (V.length v)-- V.forM_ v $ \customer -> 
      -- putStrLn $ show $ fprocess customer


-- try out a parser and print the result to stdout
test :: B.ByteString -> IO ()
test = parseTest parseData

-- filters a vector using a projecting function 
-- filterJust :: (a -> Maybe a1) -> V.Vector a -> V.Vector a
filterJust f = V.filter (isJust . f)

data LoadOption = LLocal | LCsv deriving (Eq, Show)

-- printData :: Show a => B.ByteString -> IO ()
-- printData :: Show a => (Customer -> Maybe a) -> B.ByteString -> IO ()
printData t f rd = do
  -- let res = PB.parseOnly parseData rd
  xr <- case t of LLocal -> return rd
                  LCsv -> B.readFile datasetPath
  let res = PB.parseOnly parseData xr
  case res of
    Left e -> putStrLn e
    Right v -> do
      let vf = filterJust f v
      V.mapM_ (print . fromJust . f) vf


-- -- split a data row in inputs and responses (NB: takes a lazy bytestring and returns two strict bytestrings)
-- preprocessRow :: BL8.ByteString -> (B.ByteString, B.ByteString)
-- preprocessRow r = (x_, y_)
--   where
--     n = 24
--     rc = BL8.split ',' r
--     x_ = intercf $ take n rc
--     y_ = intercf $ drop n rc
--     intercf = BL8.toStrict . BL8.intercalate (BL8.pack ",")

      

-- | Data strings

data0 = BL8.toStrict "2015-01-28,1050612,N,ES,V,23,2012-08-10,0,35,1, ,1,I,S,N, ,KHE,N,1,13,CIUDAD REAL,0,122179.11,03 - UNIVERSITARIO,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0\n2015-01-28,1050613,N,ES,H,22,2012-08-10,0,35,1, ,1,I,S,N, ,KHD,N,1,50,ZARAGOZA,0,119775.54,03 - UNIVERSITARIO,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0\n2015-01-28,1050615,N,ES,H,23,2012-08-10,0,35,1, ,1,I,S,N, ,KHE,N,1,45,TOLEDO,0,22220.04,03 - UNIVERSITARIO,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0\n2015-01-28,1050624,N,ES,H,65,2012-08-10,0,35,1, ,1,A,S,N, ,KHE,N,1,50,ZARAGOZA,1,61605.09,02 - PARTICULARES,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0\n2015-01-28,1050610,N,ES,V,24,2012-08-10,0,35,1, ,1,I,S,N, ,KHE,N,1,37,SALAMANCA,1,68318.46,03 - UNIVERSITARIO,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0\n2015-01-28,1050627,N,ES,H,23,2012-08-10,0,35,1, ,1,I,S,N, ,KHE,N,1,13,CIUDAD REAL,0,65608.35,03 - UNIVERSITARIO,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0\n2015-01-28,1050609,N,ES,H,22,2012-08-10,0,35,1, ,1,I,S,N, ,KFA,N,1,13,CIUDAD REAL,1,73432.47,03 - UNIVERSITARIO,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0\n2015-01-28,1050582,N,ES,V,28,2012-08-10,0,35,1, ,1,I,S,N, ,KHE,N,1,13,CIUDAD REAL,1,64620.57,03 - UNIVERSITARIO,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0\n2015-01-28,1050586,N,ES,V,23,2012-08-10,0,35,1, ,1,A,S,N, ,KHE,N,1,13,CIUDAD REAL,1,64194.99,03 - UNIVERSITARIO,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1\n2015-01-28,1050589,N,ES,V,23,2012-08-10,0,35,1, ,1,I,S,N, ,KHE,N,1,22,HUESCA,0,119173.89,03 - UNIVERSITARIO,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0\n2015-01-28,1050591,N,ES,H,23,2012-08-10,0,35,1, ,1,A,S,N, ,KHE,N,1,13,CIUDAD REAL,1,58728.39,03 - UNIVERSITARIO,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0\n2015-01-28,1050595,N,ES,V,25,2012-08-10,0,35,1, ,1,A,S,N, ,KHE,N,1,5,AVILA,1,86863.38,03 - UNIVERSITARIO,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0\n"

data1 = BL8.toStrict "2015-01-28,1050553,N,ES,H,24,2012-08-10,0,35,1, ,1,A,S,N, ,KHE,N,1,15,CORUÑA, A,1, ,03 - UNIVERSITARIO,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1\n2015-01-28,1050529,N,ES,H,24,2012-08-10,0,35,1, ,1,I,S,N, ,KHE,N,1,13,CIUDAD REAL,0,36372,03 - UNIVERSITARIO,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0\n"


s0 = BL8.toStrict "2015-01-28,1050613,N,ES,H,22,2012-08-10,0,35,1, ,1,I,S,N, ,KHD,N,1,50,ZARAGOZA,0,119775.54,03 - UNIVERSITARIO,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0\n"
s1 = BL8.toStrict "2015-01-28,1050613,N,ES,H,22,2012-08-10,0,35,1, ,1,I,S,N, ,KHD,N,1,50,ZARAGOZA,0,119775.54,03 - UNIVERSITARIO,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0\n"

s3 = BL8.toStrict "2015-01-28,1030918,N,ES,V,22,2012-07-25,0,36,1, ,1,I,S,N, ,KHE,N,1,15,CORUÑA, A,1, ,03 - UNIVERSITARIO,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0\n"

-- this is correctly parsed
s4 = BL8.toStrict "2015-01-28,1031069,N,ES,V,59,2012-07-25,0,36,1, ,1.0,A,S,N, ,KFA,N,1,28,MADRID,1,84887.88,01 - TOP,0,0,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,1,0,0,0,0,0\n"

s5 = BL8.toStrict "2015-01-28,1030835, , , , , , , , , , , , , , , , , , , , , , ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, , ,0\n"

s6 = BL8.toStrict "2015-01-28,1031396,N,ES,V,32,2012-07-25,0,36,1, ,1.0,I,S,S, ,KFC,N,1,28,MADRID,0,97886.4,02 - PARTICULARES,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0\n"


-- | Specs

{- | COVARIATES
fecha_dato	The table is partitioned for this column
ncodpers	Customer code
ind_empleado	Employee index: A active, B ex employed, F filial, N not employee, P pasive
pais_residencia	Customer's Country residence
sexo	Customer's sex
age	Age
fecha_alta	The date in which the customer became as the first holder of a contract in the bank
ind_nuevo	New customer Index. 1 if the customer registered in the last 6 months.
antiguedad	Customer seniority (in months)
indrel	1 (First/Primary), 99 (Primary customer during the month but not at the end of the month)
ult_fec_cli_1t	Last date as primary customer (if he isn't at the end of the month)
indrel_1mes	Customer type at the beginning of the month ,1 (First/Primary customer), 2 (co-owner ),P (Potential),3 (former primary), 4(former co-owner)
tiprel_1mes	Customer relation type at the beginning of the month, A (active), I (inactive), P (former customer),R (Potential)
indresi	Residence index (S (Yes) or N (No) if the residence country is the same than the bank country)
indext	Foreigner index (S (Yes) or N (No) if the customer's birth country is different than the bank country)
conyuemp	Spouse index. 1 if the customer is spouse of an employee
canal_entrada	channel used by the customer to join
indfall	Deceased index. N/S
tipodom	Addres type. 1, primary address
cod_prov	Province code (customer's address)
nomprov	Province name
ind_actividad_cliente	Activity index (1, active customer; 0, inactive customer)
renta	Gross income of the household
segmento	segmentation: 01 - VIP, 02 - Individuals 03 - college graduated
-}

{- | RESPONSES
ind_ahor_fin_ult1	Saving Account
ind_aval_fin_ult1	Guarantees
ind_cco_fin_ult1	Current Accounts
ind_cder_fin_ult1	Derivada Account
ind_cno_fin_ult1	Payroll Account
ind_ctju_fin_ult1	Junior Account
ind_ctma_fin_ult1	Más particular Account
ind_ctop_fin_ult1	particular Account
ind_ctpp_fin_ult1	particular Plus Account
ind_deco_fin_ult1	Short-term deposits
ind_deme_fin_ult1	Medium-term deposits
ind_dela_fin_ult1	Long-term deposits
ind_ecue_fin_ult1	e-account
ind_fond_fin_ult1	Funds
ind_hip_fin_ult1	Mortgage
ind_plan_fin_ult1	Pensions
ind_pres_fin_ult1	Loans
ind_reca_fin_ult1	Taxes
ind_tjcr_fin_ult1	Credit Card
ind_valo_fin_ult1	Securities
ind_viv_fin_ult1	Home Account
ind_nomina_ult1	        Payroll
ind_nom_pens_ult1	Pensions
ind_recibo_ult1	        Direct Debit
-}

{-
2015-01-28,
1050611,
N,
ES,
V,
23,
2012-08-10,
0,
35,
1,  -- indrel
 ,  -- ult
1,  -- indrel1mes
I,  -- tiprel1mes
S,  -- indresi
S,  -- indext
 ,  -- conyuemp
KHE,
N,
1,
13,
CIUDAD REAL,
0,
35548.74,
03 - UNIVERSITARIO,
0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
-}
