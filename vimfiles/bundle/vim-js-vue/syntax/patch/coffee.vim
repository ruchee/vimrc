silent! syntax clear coffeeConstant
syn match coffeeConstant '\v<\u\C[A-Z0-9_]+>' display 
      \ containedin=@coffeeIdentifier
