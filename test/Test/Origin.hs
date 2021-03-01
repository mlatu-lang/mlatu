module Test.Origin
  ( spec,
  )
where

import Mlatu.Located qualified as Located
import Mlatu.Monad (runMlatuExceptT)
import Mlatu.Origin (Origin (Origin))
import Mlatu.Origin qualified as Origin
import Mlatu.Tokenize (tokenize)
import Relude
import Test.Hspec (Expectation, Spec, it, shouldBe)
import Text.Parsec.Pos (Column, Line)

spec :: Spec
spec = do
  it "literals" $ do
    testOrigin
      [ "0",
        "^"
      ]
    testOrigin
      [ "0xFF",
        "<-->"
      ]
    testOrigin
      [ "0o777",
        "<--->"
      ]
    testOrigin
      [ "0b1010",
        "<---->"
      ]
    testOrigin
      [ "\"\"",
        ".<.>"
      ]
    testOrigin
      [ "\"\\    \"",
        ".<.-----.>"
      ]
  it "hello world" $ do
    testOrigin
      [ "\"meow\" say",
        ".<----.> <->"
      ]
    testOrigin
      [ "\"meow\"",
        ".<----.>",
        "say",
        "<->"
      ]
    testOrigin
      [ "\"meow\"say",
        ".<----.><->"
      ]
    testOrigin
      [ "define greet (-> +io) { \"meow\" say }",
        "<----> <---> ^<> ^<>^ ^ .<----.> <-> ^"
      ]
    testOrigin
      [ "define greet (-> +io) {",
        "<----> <---> ^<> ^<>^ ^",
        "    \"meow\" say",
        "    .<----.> <->",
        "}",
        "^"
      ]
    testOrigin
      [ "define greet<+E> (-> +io +E) {",
        "<----> <--->^^^^ ^<> ^<> ^^^ ^",
        "    \"meow\" say",
        "    .<----.> <->",
        "}",
        "^"
      ]
    testOrigin
      [ "define greet<R..., +E> (R... -> R... +io +E) {",
        "<----> <--->^^<->^ ^^^ ^^<-> <> ^<-> ^<> ^^^ ^",
        "    \"meow\" say",
        "    .<----.> <->",
        "}",
        "^"
      ]
    testOrigin
      [ "define greet<R...,+E>(R...->R...+io+E){\"meow\"say}",
        "<----> <--->^^<->^^^^^^<-><>^<->^<>^^^^.<----.><->^"
      ]

testOrigin :: [Text] -> Expectation
testOrigin test =
  let (input, origins) = deinterleave test
   in fmap
        (map Located.origin)
        ( runIdentity $
            runMlatuExceptT $
              tokenize 1 "test" $
                unlines input
        )
        `shouldBe` Right (parseOrigins origins)

deinterleave :: [a] -> ([a], [a])
deinterleave = go ([], [])
  where
    go (as, bs) (a : b : xs) = go (a : as, b : bs) xs
    go _ [_] = error "deinterleave: uneven input"
    go (as, bs) [] = (reverse as, reverse bs)

data Span = Span !Column !Column

data Env = Env
  { envPoint :: !Column,
    envSpans :: [Span]
  }

parseOrigins :: [Text] -> [Origin]
parseOrigins = concatMap (uncurry goLine) . zip [1 ..]
  where
    goLine :: Line -> Text -> [Origin]
    goLine line text =
      map
        (toOrigin line)
        ( reverse
            ( envSpans
                ( foldl'
                    go
                    Env
                      { envPoint = 1,
                        envSpans = []
                      }
                    (toString text)
                )
            )
        )

    toOrigin :: Line -> Span -> Origin
    toOrigin line (Span begin end) =
      Origin
        { Origin.name = "test",
          Origin.beginLine = line,
          Origin.beginColumn = begin,
          Origin.endLine = line,
          Origin.endColumn = end
        }

    go :: Env -> Char -> Env
    go env@Env {envPoint = point, envSpans = spans} char = case char of
      '^' ->
        Env
          { envPoint = point + 1,
            envSpans = Span point (point + 1) : spans
          }
      '<' ->
        Env
          { envPoint = point + 1,
            envSpans = Span point point : spans
          }
      '-' -> case spans of
        Span begin end : spans' ->
          Env
            { envPoint = point + 1,
              envSpans = Span begin (end + 1) : spans'
            }
        [] -> malformed
      '>' -> case spans of
        Span begin end : spans' ->
          Env
            { envPoint = point + 1,
              envSpans = Span begin (end + 2) : spans'
            }
        [] -> malformed
      '|' -> case spans of
        Span begin end : spans' ->
          Env
            { envPoint = point + 1,
              envSpans = Span point point : Span begin (end + 1) : spans'
            }
        [] -> malformed
      ' ' -> env {envPoint = point + 1}
      '.' -> env
      _ -> malformed
      where
        malformed =
          error $
            toText $
              concat
                [ "malformed origin string at (",
                  show point,
                  "): '",
                  [char],
                  "'"
                ]

-- "." -> [(1, 1)-(1, 1)]
-- ".-" -> [(1, 1)-(1, 2)]
-- ".." -> [(1, 1)-(1, 1), (1, 2)-(1, 2)]
-- ".-." -> [(1, 1)-(1, 2), (1, 3)-(1, 3)]
-- ".:" -> [(1, 1)-(1, 2), (1, 2)-(1, 2)]
-- ".:-" -> [(1, 1)-(1, 2), (1, 2)-(1, 3)]
-- ".\n." -> [(1, 1)-(1, 1), (2, 1)-(2, 1)]
