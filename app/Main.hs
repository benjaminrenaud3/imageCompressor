module Main where
import System.Environment
import System.Exit
import Data.List
import Data.Char
import Control.Monad
import Lib
import Data.List (elemIndex)

main = do
    args <- getArgs
    case args of
        [a, z, e] -> make_algo (read a) (read z :: Double) e
        otherwise -> (exitWith (ExitFailure 84))




readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile
makePixels :: [String] -> [Pixel]
makePixels = map read

make_index :: Int -> Int -> Int -> [Int] -> [Int]
make_index increm index nb_color tab
    | nb_color == 0 = tab
    | otherwise = make_index increm (index+increm) (nb_color-1) (tab ++ [index])


fill_dist :: [Cluster] -> Pixel -> [Double] -> Int -> [Double]
fill_dist big_tab unpixel tab_dist incrementor
    | incrementor == length big_tab = tab_dist
    | otherwise = fill_dist big_tab unpixel (tab_dist ++ [distPixel unpixel (getColorCluster(big_tab!!incrementor))]) (incrementor+1)

minim :: [Double] -> Double
minim []       = 0
minim [x]      = x
minim (x:xs)   = mini x (minim xs)

mini :: Double -> Double -> Double
mini a b
    | a > b  = b
    | a < b  = a
    | a == b = a

convert_maybe_int_to_int :: Maybe Int -> Int
convert_maybe_int_to_int Nothing = 0
convert_maybe_int_to_int (Just x) = x

create_clust_with_pixel:: Color -> Pixel -> Cluster
create_clust_with_pixel c p = setPixelCluster (setColorCluster createCluster c) [p]

create_clust_without_pixel:: Color -> Cluster
create_clust_without_pixel c = setPixelCluster (setColorCluster createCluster c) []









make_big_tab :: Int -> Int -> [Pixel] -> [Int] -> [Cluster] -> [Cluster]
make_big_tab size increm tabPixel tab_index big_tab
    | size == 0 = big_tab
    | otherwise = make_big_tab (size - 1) (increm + 1) tabPixel tab_index (big_tab ++ [ create_clust_with_pixel (getColorPixel(tabPixel!!(tab_index!!increm))) (tabPixel!!(tab_index!!increm)) ])


make_tab_cluster :: Int -> Pixel -> [Cluster] -> [Cluster]
make_tab_cluster index pix tab_cluster  = take index tab_cluster ++ [(addPixelCluster (tab_cluster!!index) (pix) )] ++ drop (index+1) tab_cluster


search_elem :: [Double] -> Int
search_elem tab_dist = convert_maybe_int_to_int(elemIndex (minim(tab_dist)) tab_dist)


sort_tabPixel :: [Cluster] -> [Pixel] -> [Int] -> Int -> Int -> [Cluster]
sort_tabPixel tabClust [] _ _ _ = tabClust
sort_tabPixel tabClust (x:xs) ref nbref increm
    | (ref!!nbref) == increm = sort_tabPixel tabClust xs ref (nbref+1) (increm+1)
    | otherwise = sort_tabPixel  (make_tab_cluster  (search_elem (fill_dist tabClust x [] 0))  x tabClust) xs ref nbref (increm+1)

make_pixel_avg :: [Pixel] -> Color -> Int -> Color
make_pixel_avg [] color div = (divColor color div)
make_pixel_avg (x:xs) color div = make_pixel_avg xs (addColor color (getColorPixel x)) div

get_color_pos :: [Int] -> [Pixel] -> [Color] -> [Color]
get_color_pos [] tab_pixel tab_color = tab_color
get_color_pos (x:xs) tab_pixel tab_color = get_color_pos xs tab_pixel (tab_color++[(getColorPixel(tab_pixel!!x))])


make_color_avg :: [Cluster] -> [Color] -> Int -> [Color]
make_color_avg tab_clust color_tab increm
    | increm < 0 = color_tab
    | otherwise = make_color_avg tab_clust (color_tab++[(make_pixel_avg (getPixelCluster (tab_clust!!increm)) (getColorCluster (tab_clust!!increm)) (length (getPixelCluster (tab_clust!!increm))))]) (increm-1)



tri_pixel :: [Cluster] -> [Pixel] -> [Cluster]
tri_pixel tab_cluster [] = tab_cluster
tri_pixel tab_cluster (x:xs) = tri_pixel  (make_tab_cluster  (search_elem (fill_dist tab_cluster x [] 0))  x  tab_cluster)  xs

create_tab_cluster :: [Cluster] -> [Color] -> Int -> Int -> [Cluster]
create_tab_cluster tab_cluster tab_color increm size
    | increm >= size = tab_cluster
    | otherwise = create_tab_cluster (tab_cluster++[(create_clust_without_pixel (tab_color!!increm))]) tab_color (increm+1) size


verif_all_color_limit :: [Color] -> [Color] -> Double -> Int
verif_all_color_limit [] _ _ = 0
verif_all_color_limit _ [] _ = 0
verif_all_color_limit (x:xs) (j:js) limit
    | (distColor x j) <= limit = 1
    | otherwise = verif_all_color_limit xs js limit

loop :: Double -> [Cluster] -> [Color] -> [Pixel] -> [Cluster]
loop limit tab_cluster tab_old_color tab_pixel = make_final_cluster limit tab_cluster    tab_old_color    (make_color_avg tab_cluster [] ((length tab_old_color)-1))   tab_pixel


make_final_cluster :: Double -> [Cluster] -> [Color] -> [Color] -> [Pixel] -> [Cluster]
make_final_cluster limit tab_cluster tab_old_color tab_new_color tab_pixel
    | (verif_all_color_limit tab_old_color tab_new_color limit) == 0 = tab_cluster
    | otherwise = loop limit    (tri_pixel (create_tab_cluster [] tab_new_color 0 ((length tab_new_color)-1)) tab_pixel)    tab_new_color   tab_pixel


print_all :: [Cluster] -> IO()
print_all [] = return ()
print_all (x:xs) = do
                    print x
                    print_all xs


make_algo :: Int -> Double -> String -> IO()
make_algo nb_colors limit path = do
    contents <- readLines path
    let tabPixel = makePixels $ filter (not . null) contents

    let tab_ref_pos = make_index ((length tabPixel `div` nb_colors)) ((length tabPixel `div` nb_colors)-1) (nb_colors) []
    let initial_color = get_color_pos tab_ref_pos tabPixel []
    let big_tab = make_big_tab nb_colors 0 tabPixel tab_ref_pos []
    let sort_tab = sort_tabPixel big_tab tabPixel tab_ref_pos 0 0
    let color_tab = make_color_avg sort_tab [] ((length sort_tab)-1)

    let final_clust = make_final_cluster limit sort_tab initial_color color_tab tabPixel
   --print sort_tab
    print_all final_clust











data Position = Position Int Int
instance Show Position where
    show (Position x y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance Eq Position where
    (Position x1 y1) == (Position x2 y2) = x1 == x2 && y1 == y2

instance Read Position where
    readsPrec _ input = [(Position (read (takeX input)) (read (takeY input)), (takeRest input))]
        where
            takeX :: String -> String
            takeX = (takeWhile (/= ',') . tail . dropWhile (/='('))
            takeY :: String -> String
            takeY = (takeWhile (/= ')') . tail . dropWhile (/=','))
            takeRest :: String -> String
            takeRest = (tail . dropWhile (/= ')'))

data Color = Color Int Int Int
instance Show Color where
    show (Color x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

data Cluster = Cluster {
    color :: Color,
    tab_pix :: [Pixel]
}



printElements :: [String] -> String -> String
printElements [] s = s
printElements (x:xs) s = printElements xs (s ++ "\n" ++ x)

instance Show Cluster where
    show (Cluster color tab_pix) = "--\n" ++ show color ++ "\n-" ++  (printElements (map show tab_pix) "")

instance Eq Color where
    (Color x1 y1 z1) == (Color x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2

instance Read Color where
    readsPrec _ input = [(Color (read (takeR input)) (read (takeG input)) (read (takeB input)), (takeRest input))]
        where
            takeR :: String -> String
            takeR = takeWhile (/= ',') . tail . dropWhile (/='(')
            takeG :: String -> String
            takeG = takeWhile (/= ',') . tail . dropWhile (/=',')
            takeB :: String -> String
            takeB = takeWhile (/= ')') . tail . dropWhile (/= ',') . tail . dropWhile (/=',')
            takeRest :: String -> String
            takeRest =  tail . dropWhile (/=')')

data Pixel = Pixel Position Color
instance Show Pixel where
    show (Pixel pos color) = show pos ++ " " ++ show color







getColorCluster :: Cluster -> Color
getColorCluster (Cluster c _) = c

getPixelCluster :: Cluster -> [Pixel]
getPixelCluster (Cluster _ c) = c

setColorCluster :: Cluster -> Color -> Cluster
setColorCluster cluster c = cluster { color = c }

setPixelCluster :: Cluster -> [Pixel] -> Cluster
setPixelCluster cluster c = cluster { tab_pix = c }

addPixelCluster :: Cluster -> Pixel -> Cluster
addPixelCluster cluster c = cluster { tab_pix = ((getPixelCluster cluster) ++ [c]) }

getColorPixel :: Pixel -> Color
getColorPixel (Pixel _ c) = c

instance Read Pixel where
    readsPrec _ input = [(Pixel pos color, rest2)]
        where
            [(pos, rest1)] = readsPrec 0 input :: [(Position, String)]
            [(color, rest2)] = readsPrec 0 rest1 :: [(Color, String)]

createPx :: Pixel
createPx = Pixel(Position 0 0) (Color 1 2 3)


createCluster :: Cluster
createCluster = (Cluster (Color 1 2 3) [])

divColor :: Color -> Int -> Color
divColor (Color r g b) diviseur = (Color (r `div` diviseur) (g `div` diviseur) (b `div` diviseur) )

addColor :: Color -> Color -> Color
addColor (Color r g b) (Color r1 g1 b1)  = (Color (r + r1) (g + g1) (b + b1) )

distColor :: Color -> Color -> Double
distColor (Color r1 g1 b1) (Color r2 g2 b2) = sqrt (fromIntegral ((r1 - r2)^2) + fromIntegral((g1 - g2)^2) +  fromIntegral((b1 - b2)^2))

distPixel :: Pixel -> Color -> Double
distPixel (Pixel _ c1) c2 = distColor c1 c2
