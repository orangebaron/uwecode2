import qualified Data.Set as Set
import Numeric.Natural

type UweVar = Natural

class (Show a, Eq a) => UweObj a where
	simplify :: (UweObj b) => (Maybe Natural) -> a -> b
	call :: (UweObj b, UweObj c) => a -> b -> c
	replace :: (UweObj b, UweObj c) => UweVar -> b -> a -> c
	allVars :: a -> Set.Set UweVar
	unboundVars :: Set.Set UweVar -> a -> Set.Set UweVar
	replaceBindings :: (UweObj b) => Set.Set UweVar -> a -> b

decrementInMaybe :: Maybe Natural -> Maybe Natural
decrementInMaybe = (>>= (return . (subtract 1)))

data ReturnVal = ReturnVal UweVar deriving (Show, Eq)
instance UweObj ReturnVal where
	simplify = const id
	call = Called
	replace m obj2 obj@(ReturnVal n)
		| n == m = obj2
		| otherwise = obj
	allVars (ReturnVal n) = Set.singleton n
	unboundVars set (ReturnVal n)
		| member n set = Set.empty
		| otherwise = Set.singleton n
	replaceBindings = const id

data Function a = Function UweVar a deriving (Show, Eq)
instance (UweObj a) => UweObj (Function a) where
	simplify (Just 0) obj = obj
	simplify depth (Function n x) = Function n $ simplify (decrementInMaybe depth) x
	call obj@(Function n x) obj2
		| n `member` vs = call (replaceBindings vs obj) obj2
		| otherwise = replace n obj2 $ replaceBindings vs x
		where vs = unboundVars Set.empty obj2
	replace m obj2 obj@(Function n x)
		| m == n = obj
		| otherwise = Function n $ replace m obj2 x
	allVars (Function n x) = Set.singleton n `union` allVars x
	unboundVars vs (Function n x) = unboundVars (vs `union` Set.singleton n) x
	replaceBindings vs (Function n x)
		| n `member` vs = Function newN $ replaceBindings vs newX
		| otherwise = Function n $ replaceBindings x
		where
			smallestValueNotInHelper :: UweVar -> Set.Set UweVar -> UweVar
			smallestValueNotInHelper num set
				| num `member` set = smallestValueNotInHelper (num+1) set
				| otherwise = num
			smallestValueNotIn :: Set.Set UweVar -> UweVar
			smallestValueNotIn = smallestValueNotInHelper 0
			newN = smallestValueNotIn $ allVars obj `union` vs
			newX = replace newN (ReturnVal newN) x

data Called a b = Called a b deriving (Show, Eq)
instance (UweObj a, UweObj b) => UweObj (Called a b) where
	simplify (Just 0) obj = obj
	simplify depth (Called a b)
		| val1 == obj && val2 == obj = obj
		| val1 == obj = simplify (decrementInMaybe depth) val2
		| otherwise = simplify (decrementInMaybe depth) val1
		where
			val1 = call obj1 obj2
			val2 = call (simplify (decrementInMaybe depth) obj1) obj2
	call = Called
	replace m obj2 (Called a b) = Called (replace m obj2 a) (replace m obj2 b)
	allVars (Called a b) = allVars a `Set.union` allVars b
	unboundVars vs (Called a b) = unboundVars vs a `Set.union` unboundVars vs b
	replaceBindings vs (Called a b) = Called (replaceBindings vs a) (replaceBindings vs b)

data ChurchNum = ChurchNum Natural deriving (Show, Eq)
instance UweObj ChurchNum where
	simplify = const id
	call (ChurchNum n) = CalledChurchNum n
	replace = const $ const id
	allVars = const Set.empty
	unboundVars = const $ const Set.empty
	replaceBindings = const $ const id

data CalledChurchNum a = CalledChurchNum Natural a deriving (Show, Eq)
instance (UweObj a) => UweObj (CalledChurchNum a) where
	simplify = const id
	call (CalledChurchNum 0 _) obj2 = obj2
	call (CalledChurchNum n x) obj2 = x $ Called (CalledChurchNum (n-1) x) obj2
	replace m obj2 (CalledChurchNum n x) = CalledChurchNum n $ replace m obj2 x
	allVars (CalledChurchNum n x) = allVars x
	unboundVars vs (CalledChurchNum n x) = unboundVars vs x
	replaceBindings vs (CalledChurchNum n x) = CalledChurchNum n $ replaceBindings vs x
