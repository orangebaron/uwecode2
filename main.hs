import Uwecode.Parser.Parser
import Uwecode.Parser.CodeReader

main = do
    putStrLn "enter a thing:"
    str <- getLine
    putStrLn "translated to AST:"
    print $ readUweString str
    main
