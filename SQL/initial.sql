SELECT t.permno -- unique id number
      ,t.date -- date (!)
      ,t.bidlo -- lowest price / bid
      ,t.askhi -- highest price / ask
      ,t.prc -- closing price
      ,t.vol -- share volume
      ,t.ret -- holding period return
      ,t.bid -- closing bid
      ,t.ask -- closing ask
      ,t.openprc -- open price
      ,t.numtrd -- number of trades
      ,t.cfacpr -- factor to adjust prices after events : only relevant when not 
FROM crsp_a_stock.dsf t
WHERE 1=1
      AND t.date > '1996-01-03' -- The first date of observations on option metrics
      AND t.date BETWEEN @from AND @to