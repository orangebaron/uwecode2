# uwecode2
haskell compiler for uwecode
# TODO:
- make some opts
- make the IO system better
  - make a file in the .uwe folder that has the ios listed
  - make ios be of type `{startObj :: IO (UweObj -> IO UweObj), otp :: Reference UweObj -> UweObj -> StateT IO ThreadState, ThreadClosed :: Reference UweObj -> StateT IO ()}`
- make the proj command rewrite previous progress
- make it so you can do `f x -> y` and have it be translated into `f (x -> y)`
