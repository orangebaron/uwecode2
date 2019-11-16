import Uwecode.Parser.Parser
import Uwecode.Parser.DeclarationParsers

main = do
    putStrLn "enter a thing:"
    expr <- getLine
    putStrLn "translated to AST:"
    print (equalsDeclaration `parse` expr)
    main
