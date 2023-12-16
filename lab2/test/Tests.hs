import Test.QuickCheck
import GaussAlgo

gaussTest :: Bool
gaussTest = gaussAlgo mat1 vec1 True == Exists [1, 2]
    where 
        mat1 = [[1, 0], [0, 1]] :: Matrix
        vec1 = [1, 2] :: Vector

generateMatrixTest :: IO Bool
generateMatrixTest = do 
    let size = 100
    new_matrix <- generateMatrix size size
    return $ length new_matrix == size && length (new_matrix !! 0) == size
        
checkRowResultTest :: Bool
checkRowResultTest = checkRowResult row1 == 0 && checkRowResult row2 == -1 && checkRowResult row3 == 1
    where 
        row1 = [0, 0, 0, 1] :: Row
        row2 = [0, 0, 0, 0] :: Row
        row3 = [0, 0, 1, 1] :: Row

forceTest :: Bool
forceTest = force [1, 2, 3, 4] == ()

maxIndexTest :: Bool 
maxIndexTest = maxIndex mat usedRows infinity 0 (-1, -1) == (-1, -1)
    where 
        mat = [[1, 2], [0, 1]] :: Matrix
        usedRows = [0, 1]


main :: IO ()
main = do
    putStrLn "Gauss Test"
    quickCheck gaussTest

    putStrLn "\nGenerate Matrix"
    result <- generateMatrixTest
    quickCheck result
    
    putStrLn "\nCheck Row"
    quickCheck checkRowResultTest

    putStrLn "\nForce Test"
    quickCheck forceTest

    putStrLn "\nMax Index Test"
    quickCheck maxIndexTest
