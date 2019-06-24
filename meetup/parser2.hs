type Parser a = [String] -> (a, [String])

preNum :: Parser Int
preNum (n:rest) = 
    if n `elem` ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
        then return (read n, rest)
        else []

preOp :: Parser O