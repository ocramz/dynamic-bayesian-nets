{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib 

-- import Text.Printf (printf)

import Parser.Attoparsec


import Data.Attoparsec.ByteString.Char8  hiding (ByteString, take)
import qualified Data.Attoparsec.ByteString as PB hiding (skip,skipWhile,take)

-- import qualified Data.List as List
-- import qualified Data.ByteString.Lazy as B hiding (split, pack)
import qualified Data.ByteString.Lazy.Char8 as BL8 -- hiding (ByteString)-- (split, pack)
import Data.ByteString hiding (take, drop, putStrLn)
import qualified Data.Vector as V

import Control.Applicative


-- main = putStrLn "hi"

-- main = do
--   csv <- B.readFile "test-data/train-part-noheader.csv"
--   parseLogic csv

main = do
  let (feats, resps) = preprocessRow (BL8.fromStrict testStr) -- (B.pack testStr)
      f = PB.parseOnly parseCustomerData feats
      r = PB.parseOnly parseResponses resps
  case f of
    Left e -> putStrLn e
    -- Right v -> V.forM_ v $ \ c -> print c
    Right t -> print t

testStr = BL8.toStrict "2015-01-28,1050613,N,ES,H,22,2012-08-10,0,35,1,x,1,I,S,N,0,KHD,N,1,50,ZARAGOZA,0,119775.54,03 - UNIVERSITARIO,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0"
testStr1 = BL8.toStrict "2015-01-28,1050613,N,ES,H,22,2012-08-10,0,35,1, ,1,I,S,N, ,KHD,N,1,50,ZARAGOZA,0,119775.54,03 - UNIVERSITARIO,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0"
testStr2 = BL8.toStrict "2015-01-28,1050613,N,ES,H,22,2012-08-10,0,35,1,0,1,I,S,N,0,KHD,N,1,50,ZARAGOZA,0,119775.54,03 - UNIVERSITARIO,"

-- try out a parser and print the result to stdout
test :: ByteString -> IO ()
test = parseTest parseCustomerData

-- split a data row in inputs and responses (NB: takes a lazy bytestring and returns two strict bytestrings)
preprocessRow :: BL8.ByteString -> (ByteString, ByteString)
preprocessRow r = (x_, y_)
  where
    n = 24
    rc = BL8.split ',' r
    x_ = intercf $ take n rc
    y_ = intercf $ drop n rc
    intercf = BL8.toStrict . BL8.intercalate (BL8.pack ",")




    
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




-- main :: IO ()
-- main = do
--     csvData <- BL.readFile "test-data/testcsv.csv"
--     case decodeByName csvData of
--         Left err -> putStrLn err
--         Right (_, v) -> V.forM_ v $ \c ->
--             putStrLn $ unwords [show (prova0 c), show (prova1 c)]

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

