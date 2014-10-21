foldr transform nullcase (x:xs) = if (x:xs)=[] 
      then nullcase 
      else transform x: foldr transform nullcase xs 