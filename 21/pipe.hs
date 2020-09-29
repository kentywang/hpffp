data Query     = Query
data SomeObj   = SomeObj
data IoOnlyObj = IoOnlyObj
data Err       = Err

-- There's a decoder function that makes
-- some object from String
decodeFn :: String -> Either Err SomeObj 
decodeFn = undefined

-- There's a query, that runs against the
-- DB and returns array of strings
fetchFn :: Query -> IO [String]
fetchFn = undefined

-- an additional "context initializer",
-- that also has IO
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
-- pipelineFn query = do
--   a <- fetchFn query
--   traverse makeIoOnlyObj (traverse decodeFn a)
pipelineFn = 
  (traverse makeIoOnlyObj . traverse decodeFn =<<) . fetchFn

{- 
traverse :: (a -> f b) -> t a -> f (t b)


First traverse
-------------------------------------------------------------------------------

decodeFn                                     :: String -> Either Err SomeObj
                                                ^^^^^^    ^^^^^^^^^^ ^^^^^^^
                                               (  a    ->      f        b   )

a                                            :: [] String
                                                ^^ ^^^^^^
                                                t    a

traverse decodeFn a                          :: Either Err ([] SomeObj)
                                                ^^^^^^^^^^  ^^ ^^^^^^^
                                                     f     (t     b   )


Second traverse
-------------------------------------------------------------------------------

makeIoOnlyObj                                :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
                                                ^^^^^^^^^    ^^ ^^^^^^^^^^^^^^^^^^^^^^
                                               (    a     ->  f           b           )

traverse decodeFn a                          :: Either Err [SomeObj]
                                                ^^^^^^^^^^ ^^^^^^^^^
                                                     t         a

traverse makeIoOnlyObj (traverse decodeFn a) :: IO (Either Err [(SomeObj, IoOnlyObj)])
                                                ^^  ^^^^^^^^^^ ^^^^^^^^^^^^^^^^^^^^^^
                                                 f (     t               b           )
-}   
