--findAllPaths f x (Branch left right) list 
-- | flatten2 ((findAllPaths f x left ([L]:list))) == [] = [flatten2 ((findAllPaths f x right ([R]:list)))]
-- | flatten2 ((findAllPaths f x right ([R]:list))) == [] = [flatten2 ((findAllPaths f x left ([L]:list)))]
-- | otherwise = [flatten2 ((findAllPaths f x right ([R]:list)))] ++ [flatten2 ((findAllPaths f x left ([L]:list)))]



-- = [flatten2 ((findAllPaths f x right ([R]:list)))] ++ [flatten2 ((findAllPaths f x left ([L]:list)))]




--findAllPaths f x ND list = []
--findAllPaths f x (Data d) list = if (f d) == x 
--                                 then list
--                                 else []
--findAllPaths f x (Branch left right) list = [flatten2 ((findAllPaths f x right ([R]:list)))] ++ [flatten2 ((findAllPaths f x left ([L]:list)))]

--findAllPaths f x (Branch left right) list =  (findAllPaths f x right (findAllPaths f x (Branch right))) ++ (findAllPaths f x left (findAllPaths f x (Branch left))) 




--findpath f x (Branch left right) = nodepath (findpath f x left) (findpath f x right)