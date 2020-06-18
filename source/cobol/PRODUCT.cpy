03 QUERY-PRODUCT-ID            PIC X(06).
         03 QFINANCE-CUST-NAME          PIC X(06).
         03 QFINANCE-CODE               PIC X(02).
         03 MSG-FINANCE                 PIC X(80).
         03 FFINANCE-RECORD.
           05 RECORD-COUNT              PIC 9(02).
           05 RECORD-DETAIL OCCURS 0 TO 99 TIMES
                     DEPENDING ON RECORD-COUNT.
             07 PRODUCT-ID              PIC X(06).
             07 PRODUCT-NAME            PIC X(20).
             07 EARNING-RATE            PIC X(06).
             07 PURCHASE-ENTRY          PIC X(18).
             07 HOLD-TIME               PIC X(04).
             07 RISK-TYPE               PIC X(10).