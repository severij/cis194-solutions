check :: [String] -> Maybe String
check (x:y:z:xs) = case x of
    "E" -> case 100 > read y of
        True -> Just "Jee"
        _    -> Nothing
    _   -> Nothing
