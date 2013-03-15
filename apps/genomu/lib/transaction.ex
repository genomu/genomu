defrecord Genomu.Transaction, n: 3, r: 2, vnodes: :any,
                              clock: nil, log: [], timeout: 5000 do
  @type entry :: {Genomu.key, ITC.t}
  
  record_type n: pos_integer, r: pos_integer, vnodes: :any | :primary,
              log: [entry], clock: ITC.t, timeout: non_neg_integer
end