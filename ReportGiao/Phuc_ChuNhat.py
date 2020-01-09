name_bill = "hoa_don_cod_20200102153939_5e0dac4b-4610-4e2a-897a-cc5b0a0a020b.xlsx"






#python -m pip install --upgrade pip
#python -m pip install openpyxl==2.6.2 xlrd pandas datetime
#python Phuc_BaoCaoTuan.py

import pandas as pd
import os
import openpyxl
from datetime import datetime


# Import Dataset

DataCod= pd.read_excel("CodQ1.xlsx")
#DataCod= pd.read_excel("CodTB.xlsx")
DataBill = pd.read_excel(name_bill)
Source_Q1 = DataCod.merge(DataBill, on=['Cod'], how='left')

Source_Q1['DateStamp'] = pd.to_datetime(Source_Q1['TG tạo'], errors ='coerce').dt.strftime('%m/%d/%Y').dropna()
Data_Cod = Source_Q1.groupby('Cod')[['SL ĐH',  'SL ĐH đã đối soát', 'SL ĐH đã đối soát' , 'SL ĐH delay','SL ĐH đã trả']].sum().reset_index()
Data_Dately = Source_Q1.groupby('DateStamp')[['SL ĐH',  'SL ĐH đã đối soát', 'SL ĐH đã đối soát' , 'SL ĐH delay','SL ĐH đã trả']].sum().reset_index()
Data_Cart_Dately = Source_Q1.groupby(['Cart','DateStamp'])[['SL ĐH',  'SL ĐH đã đối soát', 'SL ĐH đã đối soát' , 'SL ĐH delay','SL ĐH đã trả']].sum().reset_index()
Data_Cart = Source_Q1.groupby('Cart')[['SL ĐH',  'SL ĐH đã đối soát', 'SL ĐH đã đối soát' , 'SL ĐH delay','SL ĐH đã trả']].sum().reset_index()

with pd.ExcelWriter('Report_Quan3.xlsx') as writer: 
    Data_Cod.to_excel(writer, sheet_name='Cod')
    Data_Dately.to_excel(writer, sheet_name='Dately')
    Data_Cart.to_excel(writer,sheet_name='Cart')
    Data_Cart_Dately.to_excel(writer,sheet_name='Cart_Dately')
