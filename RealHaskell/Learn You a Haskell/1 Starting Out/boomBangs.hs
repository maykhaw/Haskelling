--a comprehension that replaces each odd number greater than 10 with "BANG!" and each odd number less than 10 with "BOOM!" 

boomBangs xs = [if x < 10 then "BOOM!" else "BANG" | x <- xs, odd x] 
