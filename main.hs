import Uwecode.Parser

main = do
    putStrLn "enter a thing:"
    expr <- getLine
    putStrLn "translated to AST:"
    print (equalsDeclaration `parse` expr)
    main
