module Task2 where
import Data.Word

data Rgb = Rgb { red   :: Word8
               , green :: Word8
               , blue  :: Word8 } deriving (Show,Read)

getRed (Rgb a b c) =  a
getGreen (Rgb a b c) = b 
getBlue (Rgb a b c) = c

data Image = Image { width   :: Int
                   , height  :: Int
                   , content :: [[Rgb]] } deriving (Show,Read)

getWidth (Image w h c) = w
getHeight (Image w h c) = h
getContent (Image w h c) = c

--------------------------------grayscale
makePixelGray pixel = (Rgb (round (0.3*(fromIntegral (getRed pixel)) + 0.59*(fromIntegral (getGreen pixel)) + 0.11*(fromIntegral (getBlue pixel))))
                           (round (0.3*(fromIntegral (getRed pixel)) + 0.59*(fromIntegral (getGreen pixel)) + 0.11*(fromIntegral (getBlue pixel))))
                           (round (0.3*(fromIntegral (getRed pixel)) + 0.59*(fromIntegral (getGreen pixel)) + 0.11*(fromIntegral (getBlue pixel)))))
                           
makeGrayHelper rgbArray
    |null rgbArray = []
    |otherwise     = (map makePixelGray rgbArray)

makeGray arrayOfRgbArrays
    |null arrayOfRgbArrays = [] 
    |otherwise             =  (map makeGrayHelper arrayOfRgbArrays)

grayscale :: Image -> Image
grayscale img = (Image (getWidth img) (getHeight img) (makeGray (getContent img) ) )

---------------------------------------------


makingNumberFrom0to255 number
  |number > 255 = 255
  |number < 0 = 0
  |otherwise = number

--------------------------------------edgeDetect
multiplyMatrix [[a,b,c],[d,e,f],[g,h,i]] [[j,k,l],[m,n,o],[p,q,r]] = i*j + h*k + g*l + f*m + e*n + d*o + c*p + b*q + a*r

sobel matrix = truncate (sqrt (fromIntegral (((multiplyMatrix matrix gX) ^ 2) + ((multiplyMatrix matrix gY) ^ 2))))

submatrix arrayOfRgbArray x y = [ [arrayOfRgbArray !! (y-1) !! (x-1)] ++  [arrayOfRgbArray !! (y-1) !! (x)] ++  [arrayOfRgbArray !! (y-1) !! (x+1)] ] ++ 
                                [ [arrayOfRgbArray !! (y) !! (x-1)] ++  [arrayOfRgbArray !! (y) !! (x)] ++  [arrayOfRgbArray !! (y) !! (x+1)] ] ++ 
                                [ [arrayOfRgbArray !! (y+1) !! (x-1)] ++  [arrayOfRgbArray !! (y+1) !! (x)] ++  [arrayOfRgbArray !! (y+1) !! (x+1)] ]

getAllCordinates img= [(x,y) | x<-[1..((getWidth img)-2)] , y<-[1..((getHeight img)-2)]]


rgbToIntMatrix matrix = (map (\a -> rgbToIntVector a) matrix)


rgbToIntVector rgbVector = (map (\ a -> (fromIntegral (getRed a))) rgbVector)


gX = [[1,0,-1],[2,0,-2],[1,0,-1]]

gY = [[1,2,1],[0,0,0],[-1,-2,-1]]


sobelFunction (x,y) matrix = sobel (rgbToIntMatrix (submatrix matrix x y))


imageToIntMatrix img = map (makingNumberFrom0to255) (map (\ a -> sobelFunction a (getContent img)) (getAllCordinates img))

intToRgbVector intVector = map (\a->(Rgb a a a)) (map (\ a -> (fromIntegral a)) intVector)

imgToEdgeMatrix img = intToRgbVector (imageToIntMatrix img)

fromVectorToMatrix vector number 
  |null vector = []
  |otherwise   = [take number vector] ++ (fromVectorToMatrix (drop number vector) number)

edgeDetectHelp img = fromVectorToMatrix (imgToEdgeMatrix img) ((getWidth img)-2)

edgeDetect img = (Image ((getWidth img)-2) ((getHeight img)-2) (edgeDetectHelp img))
