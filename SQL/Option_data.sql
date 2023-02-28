SELECT c.*, 
                           p.put_vol, 
                           p.put_open_interest, 
                           p.put_impl_vol, 
                           call_impl_vol - put_impl_vol AS impl_spread
                    FROM 
                        (SELECT secid, 
                                date, 
                                exdate, 
                                strike_price, 
                                volume AS call_vol, 
                                open_interest AS call_open_interest, 
                                impl_volatility AS call_impl_vol
                         FROM optionm.opprcd1996
                         WHERE 1=1
                           AND cp_flag = 'C' 
                           AND contract_size = 100
                        ) c
                   INNER JOIN
                        (SELECT secid, 
                                date, 
                                exdate, 
                                strike_price, 
                                volume AS put_vol, 
                                open_interest AS put_open_interest, 
                                impl_volatility AS put_impl_vol
                         FROM optionm.opprcd1996
                         WHERE 1=1
                           AND cp_flag = 'P' 
                           AND contract_size = 100
                        ) p
                    ON    c.date         = p.date 
                      AND c.secid        = p.secid 
                      AND c.exdate       = p.exdate 
                      AND c.strike_price = p.strike_price