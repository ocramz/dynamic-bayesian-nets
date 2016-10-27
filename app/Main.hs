{-# language OverloadedStrings, DeriveGeneric #-}
module Main where

import Lib 

import Text.Printf (printf)


import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

import GHC.Generics




-- 2016-06-28,15889,F,ES,V,56,1995-01-16,0,256,1,,1,A,S,N,N,KAT,N,1,28,"MADRID",1,326124.9,01 - TOP
-- 2016-06-28,1170544,N,ES,H,36,2013-08-28,0,34,1,,1,I,S,N,,KAT,N,1,3,"ALICANTE",0,,02 - PARTICULARES
-- 2016-06-28,1170545,N,ES,V,22,2013-08-28,0,34,1,,1,A,S,N,,KHE,N,1,15,"CORUÃ‘A, A",1,,03 - UNIVERSITARIO


data Customer0 =
  Customer0 { fecha_dato :: !String,
              ncodpers :: !Int,
              ind_empleado :: !Char,
              pais_residencia :: !String,
              sexo :: !Char,
              age :: !Int,
              fecha_alta :: !String,  --date
              ind_nuevo :: !Int,
              antiguedad :: !Int,
              indrel :: !Int,
              ult_fec_cli_1t :: !String,
              indrel_1mes :: !Int,
              tiprel_1mes :: !Char,
              indresi :: !Char,
              indext :: !Char,
              conyeump :: !Int,
              canal_entrada :: !String,
              indfall :: !Char,
              tipodom :: !Int,
              cod_prov :: !Int,
              nomprov :: !String,
              ind_actividad_cliente :: !Int,
              renta :: !Double,
              segmento :: !String } deriving (Eq, Show, Generic)



instance FromNamedRecord Customer0 where
  parseNamedRecord r = Customer0 <$> r .: "fecha_dato"
                                 <*> r .: "ncodpers"
                                 <*> r .: "ind_empleado" 
                                 <*> r .: "pais_residencia"
                                 <*> r .: "sexo"
                                 <*> r .: "age"
                                 <*> r .: "fecha_alta"
                                 <*> r .: "ind_nuevo"
                                 <*> r .: "antiguedad"
                                 <*> r .: "indrel"
                                 <*> r .: "ult_fec_cli_1t"
                                 <*> r .: "indrel_1mes"
                                 <*> r .: "tiprel_1mes"
                                 <*> r .: "indresi"
                                 <*> r .: "indext"
                                 <*> r .: "conyeump"
                                 <*> r .: "canal_entrada"
                                 <*> r .: "indfall"
                                 <*> r .: "tipodom"
                                 <*> r .: "cod_prov"
                                 <*> r .: "nomprov"
                                 <*> r .: "ind_actividad_cliente"
                                 <*> r .: "renta"
                                 <*> r .: "segmento"

main :: IO ()
main = do
    csvData <- BL.readFile "test-data/testdata500.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \c ->
            putStrLn $ show (fecha_dato c)


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
ind_nomina_ult1	Payroll
ind_nom_pens_ult1	Pensions
ind_recibo_ult1	Direct Debit
-}

