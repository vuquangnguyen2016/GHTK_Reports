# pip install xlrd pandas datetime os jupyter notebook  openpyxl 

#python -m pip install --upgrade pip
python -m pip install openpyxl==2.6.2 xlrd pandas datetime os jupyter notebook  


import pandas as pd
import os
import openpyxl
from datetime import datetime 


name_bill = "hoa_don_cod_20200102183113_5e0dd481-29b4-4391-93e5-49d00a0a3428.xlsx"

os.chdir('C:\Users\BV\Downloads')


DataCod= pd.read_excel("CodTB.xlsx")
DataBill = pd.read_excel(name_bill)
Source_Q1 = DataCod.merge(DataBill, on=['Cod'], how='left')

Source_Q1['DateStamp'] = pd.to_datetime(Source_Q1['TG tạo'], errors ='coerce').dt.strftime('%m/%d/%Y').dropna()
Data_Cod = Source_Q1.groupby('Cod')[['SL ĐH',  'SL ĐH đã đối soát', 'SL ĐH đã đối soát' , 'SL ĐH delay']].sum().reset_index()
Data_Dately = Source_Q1.groupby('DateStamp')[['SL ĐH',  'SL ĐH đã đối soát', 'SL ĐH đã đối soát' , 'SL ĐH delay']].sum().reset_index()
Data_Cart_Dately = Source_Q1.groupby(['Cart','DateStamp'])[['SL ĐH',  'SL ĐH đã đối soát', 'SL ĐH đã đối soát' , 'SL ĐH delay']].sum().reset_index()
Data_Cart = Source_Q1.groupby('Cart')[['SL ĐH',  'SL ĐH đã đối soát', 'SL ĐH đã đối soát' , 'SL ĐH delay']].sum().reset_index()

with pd.ExcelWriter('Report_TB.xlsx') as writer: 
    Data_Cod.to_excel(writer, sheet_name='Cod')
    Data_Dately.to_excel(writer, sheet_name='Dately')
    Data_Cart.to_excel(writer,sheet_name='Cart')
    Data_Cart_Dately.to_excel(writer,sheet_name='Cart_Dately')
