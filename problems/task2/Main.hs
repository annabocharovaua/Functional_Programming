-- Import necessary modules
import Numeric
import Numeric.LinearAlgebra (reshape, randn, size)
import Numeric.LinearAlgebra.Data 
import System.Random

forward :: MLP -> Vector Double -> Vector Double
forward mlp input = sigmoid (weights2 mlp #> (sigmoid (weights1 mlp #> input + biases1 mlp)) + biases2 mlp)

data MLP = MLP
  { weights1 :: Matrix Double
  , biases1 :: Vector Double
  , weights2 :: Matrix Double
  , biases2 :: Vector Double
  }

mseLoss :: Vector Double -> Vector Double -> Double
mseLoss output target = sum ((output - target) ^ 2) / (2 * fromIntegral (size output))


sigmoid :: Vector Double -> Vector Double
sigmoid x = 1 / (1 + exp (-x))

main :: IO ()
main = do
  let input = vector [0.1, 0.2, 0.3]
      target = vector [0.4]

  weights1Init <- randn 5
  biases1Init <- randn 5
  weights2Init <- randn 1
  biases2Init <- randn 1

  let mlp = MLP (reshape 5 3 weights1Init) biases1Init (reshape 1 5 weights2Init) biases2Init

  putStrLn "Initial Output:"
  print $ forward mlp input

  putStrLn "Initial Loss:"
  print $ mseLoss (forward mlp input) target