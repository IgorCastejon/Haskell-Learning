{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}


import Data.Map qualified as Map
{-


The QOI format (https://qoiformat.org/). It was introduced (
https://phoboslab.org/log/2021/11/qoi-fast-lossless-image-compression) in 2021 as an alternative image format for lossless image compression like PNG. Particularly, having faster encoding and decoding, and similar compression rates to PNG, while being simple enough that its specification fits a single page (https://qoiformat.org/qoi-specification.pdf).



Inspired by the video "How PNG Works: Compromising Speed for Quality"
https://www.youtube.com/watch?v=EFUYNoFRHQI


DOing both TDD flavors - Type-driven Development and Test-Driven Development
-}

{-
Let's do this by following the specification    
The first phra
"A QOI file consists of a 14-byte header, followed by any number of
data “chunks” and an 8-byte end marker.""

This can be represented by the following type:
data QoiFile = QoiHeader [Chunk] EndMarker
-}

{-
qoi_header {
 char magic[4]; // magic bytes "qoif"
 uint32_t width; // image width in pixels (BE)
 uint32_t height; // image height in pixels (BE)
 uint8_t channels; // 3 = RGB, 4 = RGBA
 uint8_t colorspace; // 0 = sRGB with linear alpha
 // 1 = all channels linear
};
We can map magic as (TODO checar string estática), width as Word32 (unsigned int of size 32), height as Word32, channels
Instead, we can create types that better encapsulate possible values.
-}


import Data.Binary qualified as Binary
import Data.Int

newtype MagicQoi = MagicQoi String
type ImageWidth = Binary.Word32
type ImageHeight = Binary.Word32

data SupportedImageChannels
    = RGB
    | RGBA
    deriving (Show, Eq) -- So we can print and compare, if necessary

data SupportedColorspaces
    = SRGBWithLinearAlpha
    | AllChannelsLinear
    deriving (Show, Eq)

{-
Having "Magic" in the type or not is something to be discussed.

-}

data MagicHeader
    = MagicHeader
    deriving (Eq)

instance Show MagicHeader where
    show :: MagicHeader -> String
    show _ = "qoif"

data QoiHeader = QoiHeader
    { magic :: MagicHeader
    , width :: ImageWidth
    , height :: ImageHeight
    , channels :: SupportedImageChannels
    , colorspace :: SupportedColorspaces
    }
    deriving (Show, Eq)

{-

We want to extract the QOI header from the image to be encoded, so we can include it in the encoded file. Basically, we want to implement the following function:
extractQoiHeaderFromAnImage :: Image -> QoiHeader



TODO vector force all same size.


"QOI encodes and decodes images in a single pass. It touches every pixel just once." - linear array?

We can define an image as having a width, height, a colorspace and a list of pixels of size width * height, such that each pixel has the contents of each channel. 

We have to support two types of pixels: 

1. RGB pixels;

-}

data RGBPixel
    = RGBPixel
    { red :: Binary.Word8
    , green :: Binary.Word8
    , blue :: Binary.Word8
    }
    deriving (Show, Eq)

{-
2. RGBA pixels.


-}

data RGBAPixel
    = RGBAPixel
    { red :: Binary.Word8
    , green :: Binary.Word8
    , blue :: Binary.Word8
    , alpha :: Binary.Word8
    }
    deriving (Show, Eq)



{-

Depending on the type of pixel, we have two types of images, RGB and RGBA images. Note that, for simplicity sake's, this implementation doesn't enforce that the size of the list of pixels is equal to ImageWidth * ImageHeight, but that's a job of a future part. 

We can't allow an image with a mix of RGB pixels and RGBA pixels.
-}


data Image
    = RGBImage ImageWidth ImageHeight SupportedColorspaces [RGBPixel]
    | RGBAImage ImageWidth ImageHeight SupportedColorspaces [RGBAPixel]
    deriving (Show, Eq)


-- Example of a valid RGB image:
rgbImg :: Image
rgbImg = RGBImage 1 1 AllChannelsLinear [RGBPixel {red = 10, blue = 100, green = 244}]

-- Example of a valid RGBA image:
rgbaImg :: Image
rgbaImg = RGBAImage 3 1 AllChannelsLinear
    [ RGBAPixel {red = 10, blue = 100, green = 244, alpha = 255}
    , RGBAPixel {red = 23, blue = 144, green = 223, alpha = 13}
    , RGBAPixel {red = 55, blue = 35, green = 212, alpha = 156}
    ]

{-


Before we implement our function, we can write tests our QOI header from the given example images:
-}

-- >>> extractQoiHeaderFromAnImage rgbImg == QoiHeader { magic = MagicHeader, width = 1, height = 1, colorspace = AllChannelsLinear, channels = RGB }
-- True

-- >>> extractQoiHeaderFromAnImage rgbaImg == QoiHeader { magic = MagicHeader, width = 3, height = 1, colorspace = AllChannelsLinear, channels = RGBA }
-- True

extractQoiHeaderFromAnImage :: Image -> QoiHeader

-- To implement our extraction function, we can use pattern matching to return different headers for each image type.
extractQoiHeaderFromAnImage (RGBImage width height colorspace _)
    = QoiHeader { magic = MagicHeader, width = width, height = height, colorspace = colorspace, channels = RGB }

extractQoiHeaderFromAnImage (RGBAImage width height colorspace _)
    = QoiHeader { magic = MagicHeader, width = width, height = height, colorspace = colorspace, channels = RGBA }



{-
In this part, we implemented types, a function and some simple tests to handle images, pixels and QOI headers. In the next part, we start implementing QOI encoding and decoding. 
    
-}


{-

"Images are encoded row by row, left to right, top to bottom. The
decoder and encoder start with {r: 0, g: 0, b: 0, a: 255} as the
previous pixel value. An image is complete when all pixels specified by width * height have been covered."

We want to implement the following functions:
encodeQOI :: Image -> QoiFile
decodeQOI :: QoiFile -> Image

One important property that we can check with property-based testing is:


img == decodeQOI $ encodeQOI img





In this part, we will start implement our first encoding function. QOI works by encoding pixels into chunks. 
As in the specification, "each chunk starts with a 2- or 8-bit tag, followed by a number of data bits". There are 4 different chunk types:
1. A run of the previous pixel;
2. An index into a previously seen pixel;
3. The difference to the previous pixel;
4. Full rgb/rgba values.


We will implement the first one:
1. A run of the previous pixel

"If the current pixel is exactly the same as the previous pixel, the run length is increased by 1. When a pixel is encountered that is different from the previous one, this run length is saved to the encoded data and the current pixel is packed by one of the other 3 methods."


┌─ QOI_OP_RUN ────────────┐
│         Byte[0]         │
│  7  6  5  4  3  2  1  0 │
│───────┼─────────────────│
│  1  1 │       run       │
└───────┴─────────────────┘
2-bit tag b11
6-bit run-length repeating the previous pixel: 1..62

"The run-length is stored with a bias of -1. Note that the runlengths 63 and 64 (b111110 and b111111) are illegal as they are
occupied by the QOI_OP_RGB and QOI_OP_RGBA tags.". So if we have a length of 1, we store 0. If we have a length of 62, we store 61.

We can represent this chunk in the following manner:

-}
{-
data TwoBitTag
    = TwoBitTag
    deriving (Show, Eq)
--data EightBitTag = EightBitTag

data Chunk
    = QOI_OP_RUN
    { tag :: TwoBitTag
    , rle :: Binary.Word8
    }
    deriving (Show, Eq)
-}
{-
We basically want to implement these functions:

encodeRgbQoi :: RGBPixel -> RGBPixel -> Maybe Chunk -> Chunk -- previous RGB pixel -> current RGB pixel -> previous chunk -> return chunk
encodeRgbaQoi :: RGBAPixel -> RGBAPixel -> Maybe Chunk -> Chunk -- previous RGBA pixel -> current RGBA pixel -> previous chunk -> return chunk


decodeRgbQoi :: RGBPixel -> Chunk -> [RGBPixel] -- previous RGB pixel -> the chunk to be decoded -> all instances
decodeRgbaQoi :: RGBAPixel -> Chunk -> [RGBAPixel] -- previous RGBA pixel -> the chunk to be decoded -> all instances
 

We use Maybe to treat the case where there are no previous chunks, which happens when we are handling the first pixel. Note that these are just tentative signatures for now, and we will probably need to evolve them when we implement the other chunks. As for the previous value in relation to the first pixel, the specification says: "The
decoder and encoder start with {r: 0, g: 0, b: 0, a: 255} as the
previous pixel value.". This can be represented as:

-}

firstPreviousRGBValue :: RGBPixel
firstPreviousRGBValue = RGBPixel { red = 0, green = 0, blue = 0}

firstPreviousRGBAValue :: RGBAPixel
firstPreviousRGBAValue = RGBAPixel { red = 0, green = 0, blue = 0, alpha = 255}


{-
We can test through property-based testing, but not now.
-}

{-
We will do this first test example before implementing the function:


-}

examplePixelRGB :: RGBPixel
examplePixelRGB = RGBPixel { red = 100, green = 100, blue = 100}

{-


-}


-- Given equal RGB pixels and no previous chunk, when encoding, then it should encode a RLE of 1
-- >>> encodeRgbQoi examplePixelRGB examplePixelRGB Nothing == QOI_OP_RUN { tag = TwoBitTag, rle = 1 }
-- True
{-
encodeRgbQoi :: RGBPixel -> RGBPixel -> Maybe Chunk -> Chunk
encodeRgbQoi p1 p2 _
    | p1 == p2 = QOI_OP_RUN { tag = TwoBitTag, rle = 1}
    | otherwise = undefined
-}

-- PEndente: Given different RGB pixels and no previous chunk, when encoding, then it should encode nothing
-- >>> encodeRgbQoi firstPreviousRGBValue examplePixelRGB Nothing == Nothing
-- True

{-
encodeRgbQoi :: RGBPixel -> RGBPixel -> Maybe Chunk -> Chunk
encodeRgbQoi p1 p2 _
    | p1 == p2 = QOI_OP_RUN { tag = TwoBitTag, rle = 1}
    | otherwise = Nothing
-}
-- Given different RGB pixels and a previous chunk, when encoding, then it should encode nothing
-- >>> encodeRgbQoi firstPreviousRGBValue examplePixelRGB (Just (QOI_OP_RUN { tag = TwoBitTag, rle = 1 }) ) == Nothing
-- True
-----------------

-- Given equal RGB pixels and a previous chunk with rle less than 63, when encoding, then it should encode RLE with value + 1.
-- >>> encodeRgbQoi examplePixelRGB examplePixelRGB (Just (QOI_OP_RUN { tag = TwoBitTag, rle = 1 }) ) == (QOI_OP_RUN { tag = TwoBitTag, rle = 1 })
-- True

{-encodeRgbQoi :: RGBPixel -> RGBPixel -> Maybe Chunk -> Chunk
encodeRleRgbQoi p1 p2 Nothing
    | p1 == p2 = Just $ QOI_OP_RUN { tag = TwoBitTag, rle = 1}
    | otherwise = undefined
encodeRgbQoi p1 p2 (Just (QOI_OP_RUN {rle}))
    | p1 == p2 = QOI_OP_RUN { tag = TwoBitTag, rle = 1 + rle}
    | otherwise = undefined
-}





-- We also need to respect that 63 onwards is an invalid value. 
-- Given equal RGB pixels and a previous chunk with rle equal to 62, when encoding, then it should encode RLE with rle 1.

-- >>> encodeRgbQoi examplePixelRGB examplePixelRGB (Just (QOI_OP_RUN { tag = TwoBitTag, rle = 62 }) ) == QOI_OP_RUN { tag = TwoBitTag, rle = 1 }
-- True
{-
encodeRgbQoi :: RGBPixel -> RGBPixel -> Maybe Chunk -> Chunk
encodeRgbQoi p1 p2 Nothing
    | p1 == p2 = QOI_OP_RUN { tag = TwoBitTag, rle = 1}
    | otherwise = undefined
encodeRgbQoi p1 p2 (Just (QOI_OP_RUN {rle}))
    | p1 == p2 && rle run < 61 = QOI_OP_RUN { tag = TwoBitTag, rle = 1 + rle}
    | p1 == p2 && rle run >= 61 = QOI_OP_RUN { tag = TwoBitTag, rle = 1}
    | otherwise = undefined
-}
-- We can do a bit of a refactoring:

firstRgbEncodeQoi :: RGBPixel -> Chunk
firstRgbEncodeQoi p
    | p == firstPreviousRGBValue = QOI_OP_RUN { tag = TwoBitTag, rle = 1}
    | otherwise = undefined

encodeRgbQoi :: RGBPixel -> RGBPixel -> Maybe Chunk -> Chunk
encodeRgbQoi p1 p2 Nothing
    | p1 == p2 = QOI_OP_RUN { tag = TwoBitTag, rle = 1}
    | otherwise = undefined
encodeRgbQoi p1 p2 (Just run)
    | p1 == p2 = QOI_OP_RUN { tag = TwoBitTag, rle = rleValue $ rle run}
    | otherwise = undefined
    where
        rleValue :: Binary.Word8 -> Binary.Word8
        rleValue val
            | val < 62 = val + 1
            | otherwise = 1


{-

We could now do the same thing to implement encoding for RGBA pixels, but the implementation would basically the same. To avoid this duplication, we can create a type class that constrains possible pixel types, like the following:

-}
{-}
class (Eq a) => IsPixel a where
    getRed :: a -> Binary.Word8
    getGreen :: a -> Binary.Word8
    getBlue :: a -> Binary.Word8

instance IsPixel RGBPixel where
    getRed (RGBPixel {red}) = red
    getGreen (RGBPixel {green}) = green
    getBlue (RGBPixel {blue}) = blue

instance IsPixel RGBAPixel where
    getRed (RGBAPixel {red}) = red
    getGreen (RGBAPixel {green}) = green
    getBlue (RGBAPixel {blue}) = blue

-}
{-
    Using this type class, we can now substitute encodeRgbQoi and encodeRgbaQoi with the following function:
-}
{-
encodePixelQoi :: (IsPixel pixel) => pixel -> pixel -> Maybe Chunk -> Chunk
encodePixelQoi p1 p2 Nothing
    | p1 == p2 = QOI_OP_RUN { tag = TwoBitTag, rle = 1}
    | otherwise = undefined
encodePixelQoi p1 p2 (Just (QOI_OP_RUN {rle}))
    | p1 == p2 = QOI_OP_RUN { tag = TwoBitTag, rle = rleValue rle}
    | otherwise = undefined
    where
        rleValue :: Binary.Word8 -> Binary.Word8
        rleValue val
            | val < 62 = val + 1
            | otherwise = 1
encodePixelQoi p1 p2 (Just _) = undefined
-}

-- We can test RGBA pixels now:

-- Given equal RGBA pixels and a previous chunk with rle equal to 62, when encoding, then it should encode RLE with rle 1.

-- >>> encodePixelQoi firstPreviousRGBAValue firstPreviousRGBAValue (Just (QOI_OP_RUN { tag = TwoBitTag, rle = 62 }) ) == QOI_OP_RUN { tag = TwoBitTag, rle = 1 }
-- True

{-
    Finished our encoding, we now need to specify how this chunk is decoded.
    With this type class, we can also substitute "decodeRgbRleQoi" and "decodeRgbaRleQoi" with "decodePixelRleQoi". Let's define our first test:
-}

{-
decodePixelRleQoi :: (IsPixel pixel) => pixel -> Chunk -> [pixel]
decodePixelRleQoi _ _ = undefined
-}

-- Given a RGB pixel with RLE of 1, when decoding, then it should return a list with the same pixel of size 1
-- >>> decodePixelRleQoi examplePixelRGB (QOI_OP_RUN { rle = 1, tag = TwoBitTag }) == [ examplePixelRGB ]
-- True

-- The most straight forward implementation to make our test pass:
{-
decodePixelRleQoi :: (IsPixel pixel) => pixel -> QOI_OP_RUN -> [pixel]
decodePixelRleQoi pixel rle = [pixel]
-}

examplePixelRGBA :: RGBAPixel
examplePixelRGBA = RGBAPixel { red = 100, green = 100, blue = 100, alpha = 200}

-- Given a RGBA pixel with RLE of 2, when decoding, then it should return a list with the same pixel of size 2
-- >>> decodePixelQoi examplePixelRGBA (QOI_OP_RUN { rle = 2, tag = TwoBitTag }) == [ examplePixelRGBA, examplePixelRGBA ]
-- True

{-
decodePixelQoi :: (IsPixel pixel) => pixel -> Chunk -> [pixel]
decodePixelQoi pixel (QOI_OP_RUN {rle}) = replicate (fromIntegral rle) pixel
decodePixelQoi _ _ = undefined
-}

-- We could implement a property-based test to assert that rle == length decodePixelQoi, but we won't.


-- We can now implement our first versions of encodeQOI and then decodeQOI: 

data EndMarker
    = EndMarker
    deriving (Show, Eq)

data QoiFile
    = QoiFile QoiHeader [Chunk] EndMarker
    deriving (Show, Eq)

encodeQOI:: Image -> QoiFile
--encodeQOI = undefined

--decodeQOI :: QoiFile -> Image
--decodeQOI = undefined

-- Given a RGB image with no pixels, when encoding, then it should encode with no chunks.
-- >>> encodeQOI (RGBImage 0 0 AllChannelsLinear []) == QoiFile (QoiHeader {magic = MagicHeader, width = 0, height = 0, channels = RGB, colorspace = AllChannelsLinear}) [] EndMarker
-- True

--encodeQOI img@(RGBImage w h _ pixels) = QoiFile (extractQoiHeaderFromAnImage img) [] EndMarker
--encodeQOI _ = undefined

-- Given a RGB image with a single pixel like the first previous pixel, when encoding, then it should encode with only QOI_OP_RUN chunks.

-- >>> encodeQOI (RGBImage 1 1 AllChannelsLinear [firstPreviousRGBValue]) == QoiFile (QoiHeader {magic = MagicHeader, width = 1, height = 1, channels = RGB, colorspace = AllChannelsLinear}) [QOI_OP_RUN {tag = TwoBitTag, rle = 1}] EndMarker
-- True

{-encodeQOI img@(RGBImage w h _ pixels) 
    | w * h == 0 = QoiFile (extractQoiHeaderFromAnImage img) [] EndMarker 
    | otherwise = QoiFile (extractQoiHeaderFromAnImage img) [encodePixelQoi firstPreviousRGBValue (head pixels) Nothing] EndMarker 

encodeQOI _ = undefined

-}

-- With our test passing, let's refactor a bit:

{-
encodeQOI img@(RGBImage w h _ pixels) 
    = QoiFile header (chunks w h pixels) EndMarker 
    where
        header :: QoiHeader
        header = extractQoiHeaderFromAnImage img

        chunks :: ImageWidth -> ImageHeight -> [RGBPixel] -> [Chunk]
        chunks w h pixels
            |  w * h == 0 = []
            | otherwise = [encodePixelQoi firstPreviousRGBValue (head pixels) Nothing]
encodeQOI _ = undefined

-}



-- Given a RGB image with two pixels like the first previous pixel, when encoding, then it should encode with only QOI_OP_RUN chunks.

-- >>> encodeQOI (RGBImage 1 1 AllChannelsLinear [firstPreviousRGBValue, firstPreviousRGBValue]) == QoiFile (QoiHeader {magic = MagicHeader, width = 1, height = 1, channels = RGB, colorspace = AllChannelsLinear}) [QOI_OP_RUN {tag = TwoBitTag, rle = 2}] EndMarker
-- True
{-
encodeQOI img@(RGBImage w h _ pixels)
    = QoiFile header (chunks w h pixels) EndMarker
    where
        header :: QoiHeader
        header = extractQoiHeaderFromAnImage img

        chunks :: ImageWidth -> ImageHeight -> [RGBPixel] -> [Chunk]
        chunks w h [] = []
        chunks 0 _ _ = []
        chunks _ 0 _ = []
        chunks w h [x] =
            [firstEncoding]
            where
                firstEncoding = encodePixelQoi firstPreviousRGBValue (head pixels) Nothing
        chunks w h pixels =
            [secondEncoding]
            where
                firstEncoding = encodePixelQoi firstPreviousRGBValue (head pixels) Nothing
                secondEncoding = encodePixelQoi (head pixels) (pixels !! 1) (Just firstEncoding)
encodeQOI _ = undefined
-}
-- We can see a recursive pattern emerging. With our test passing, let's refactor so we extract this recursive function:

{-
encodeQOI img
    = QoiFile header (chunks img) EndMarker
    where
        header :: QoiHeader
        header = extractQoiHeaderFromAnImage img

        chunks :: Image -> [Chunk]
        chunks (RGBImage _ 0 _ _) = []
        chunks (RGBImage 0 _ _ _) = []
        chunks (RGBAImage 0 _ _ _) = []
        chunks (RGBAImage _ 0 _ _) = []
        chunks (RGBAImage _ _ _ pixels) =
            encodePixelsToChunks pixels firstPreviousRGBAValue Nothing
        chunks (RGBImage _ _ _ pixels) =
            encodePixelsToChunks pixels firstPreviousRGBValue Nothing

encodePixelsToChunks :: (IsPixel pixel) => [pixel] -> pixel -> Maybe Chunk -> [Chunk]
encodePixelsToChunks [] _ Nothing = []
encodePixelsToChunks [x] first Nothing = [encodePixelQoi first x Nothing]
encodePixelsToChunks (x:xs) first Nothing =
    let
        firstChunk :: Chunk
        firstChunk = encodePixelQoi first x Nothing
    in
        encodePixelsToChunks xs x (Just firstChunk)
encodePixelsToChunks (x:xs) previous prev@(Just previousChunk) =
    let
        nextChunk = encodePixelQoi previous x prev
        toBeAdded = case previousChunk of
            (QOI_OP_RUN {rle}) -> ([previousChunk | rle == 62])
            _ -> []
    in toBeAdded ++ encodePixelsToChunks xs x (Just nextChunk)

encodePixelsToChunks [] previous prev@(Just previousChunk) =
    [previousChunk]
-}
q = "a"

-- Given a RGB image with 63 pixels like the first previous pixel, when encoding, then it should encode with only QOI_OP_RUN chunks.

-- >>> encodeQOI (RGBImage 1 1 AllChannelsLinear (replicate 63 firstPreviousRGBValue)) == QoiFile (QoiHeader {magic = MagicHeader, width = 1, height = 1, channels = RGB, colorspace = AllChannelsLinear}) [QOI_OP_RUN {tag = TwoBitTag, rle = 62}, QOI_OP_RUN {tag = TwoBitTag, rle = 1}] EndMarker
-- True

-- >>> encodePixelsToChunks [] firstPreviousRGBValue Nothing
-- []

-- We can now write our first version of the decoding function:

--decodeQoi :: QoiFile -> Image
--decodeQoi qoi = undefined


-- Given a QOI file for RGB image with 0 chunks, when decoding, then it should decode an empty RGB image.

-- >>> decodeQoi (QoiFile (QoiHeader {magic = MagicHeader, width = 0, height = 0, channels = RGB, colorspace = AllChannelsLinear}) [] EndMarker) == RGBImage 0 0 AllChannelsLinear []
-- True

--decodeQoi :: QoiFile -> Image
--decodeQoi qoi = RGBImage 0 0 AllChannelsLinear []


{-
decodeQoi :: QoiFile -> Image
decodeQoi (QoiFile header chunks _) = RGBImage (width header) (height header) (colorspace header) []

-}

-- Given a QOI file for RGB image with 0 chunks but srgb colorspace, when decoding, then it should decode a RGB image with 0 pixels but srgb colorspace.
-- >>> decodeQoi (QoiFile (QoiHeader {magic = MagicHeader, width = 0, height = 0, channels = RGB, colorspace = SRGBWithLinearAlpha}) [] EndMarker) == RGBImage 0 0 SRGBWithLinearAlpha []
-- True

-- Given a QOI file for RGBA image with 0 chunks but srgb colorspace, when decoding, then it should decode a RGBA image with 0 pixels but srgb colorspace.
-- >>> decodeQoi (QoiFile (QoiHeader {magic = MagicHeader, width = 0, height = 0, channels = RGBA, colorspace = SRGBWithLinearAlpha}) [] EndMarker) == RGBAImage 0 0 SRGBWithLinearAlpha []
-- True

{-
decodeQoi :: QoiFile -> Image
decodeQoi (QoiFile (QoiHeader _ width height RGB colorspace) chunks _) = RGBImage width height colorspace []
decodeQoi (QoiFile (QoiHeader _ width height RGBA colorspace) chunks _) = RGBAImage width height colorspace []

s = ""
--}
-- Given a QOI file for RGB image with 1 QOI_OP_RUN chunk, when decoding, then it should decode a RGB image with 1 pixel.

-- >>> decodeQoi (QoiFile (QoiHeader {magic = MagicHeader, width = 1, height = 1, channels = RGB, colorspace = AllChannelsLinear}) [QOI_OP_RUN {tag = TwoBitTag, rle = 1}] EndMarker) == RGBImage 1 1 AllChannelsLinear [RGBPixel {red = 0, green = 0, blue = 0}]
-- True
{-
decodeQoi :: QoiFile -> Image
decodeQoi (QoiFile (QoiHeader _ width height RGB colorspace) chunks _) = RGBImage width height colorspace (decodeRGBChunks chunks)
decodeQoi (QoiFile (QoiHeader _ width height RGBA colorspace) chunks _) = RGBAImage width height colorspace (decodeRGBAChunks chunks)

decodeRGBChunks :: [Chunk] -> [RGBPixel]
decodeRGBChunks _ = [RGBPixel {red = 0, green = 0, blue = 0}]

decodeRGBAChunks :: [Chunk] -> [RGBAPixel]
decodeRGBAChunks _ = [RGBAPixel {red = 0, green = 0, blue = 0, alpha = 255}]
-}

-- Given a QOI file for RGBA image with QOI_OP_RUN chunk with rle 2, when decoding, then it should decode a RGB image with 2 pixels.

-- >>> decodeQoi (QoiFile (QoiHeader {magic = MagicHeader, width = 2, height = 1, channels = RGBA, colorspace = AllChannelsLinear}) [QOI_OP_RUN {tag = TwoBitTag, rle = 2}] EndMarker) == RGBAImage 2 1 AllChannelsLinear (replicate 2 RGBAPixel {red = 0, green = 0, blue = 0, alpha = 255})
-- True
{-
decodeQoi :: QoiFile -> Image
decodeQoi (QoiFile (QoiHeader _ width height RGB colorspace) chunks _) = RGBImage width height colorspace (decodeRGBChunks chunks)
decodeQoi (QoiFile (QoiHeader _ width height RGBA colorspace) chunks _) = RGBAImage width height colorspace (decodeRGBAChunks chunks)

decodeRGBChunks :: [Chunk] -> [RGBPixel]
decodeRGBChunks [] = []
decodeRGBChunks (chunk:xs) = decodePixelQoi firstPreviousRGBValue chunk

decodeRGBAChunks :: [Chunk] -> [RGBAPixel]
decodeRGBAChunks [] = []
decodeRGBAChunks (chunk:xs) = decodePixelQoi firstPreviousRGBAValue chunk
-}

-- Given a QOI file for RGBA image with QOI_OP_RUN chunk with rle 63, when decoding, then it should decode a RGB image with 63 pixels.

-- >>> decodeQoi (QoiFile (QoiHeader {magic = MagicHeader, width = 63, height = 1, channels = RGBA, colorspace = AllChannelsLinear}) [QOI_OP_RUN {tag = TwoBitTag, rle = 62}, QOI_OP_RUN {tag = TwoBitTag, rle = 1}] EndMarker) == RGBAImage 63 1 AllChannelsLinear (replicate 63 RGBAPixel {red = 0, green = 0, blue = 0, alpha = 255})
-- True

{-
decodeQoi :: QoiFile -> Image
decodeQoi (QoiFile (QoiHeader _ width height RGB colorspace) chunks _) = RGBImage width height colorspace (decodeChunks firstPreviousRGBValue chunks)
decodeQoi (QoiFile (QoiHeader _ width height RGBA colorspace) chunks _) = RGBAImage width height colorspace (decodeChunks firstPreviousRGBAValue chunks)

decodeChunks :: (IsPixel pixel) => pixel -> [Chunk] -> [pixel]
decodeChunks _ [] = []
decodeChunks previousPixel (chunk:xs) = 
    let 
        decodedChunk = decodePixelQoi previousPixel chunk
        nextPreviousPixel = last decodedChunk
    in decodedChunk ++ decodeChunks nextPreviousPixel xs
-}

-- With this, in this part we finished our encoding and decoding of our first chunk type. In the next part we will implement our second chunk type.

{-
    In the previous part, we implemented encoding and decoding for the first chunk defined by QOI, which is basically a RLE. In this part, we will define our second chunk. THe specification defines the second chunk as QOI_OP_INDEX. However, it's not the best to be implemented now, as we can't write tests for it. We will implement the third, pixel differences, instead.


3. The difference to the previous pixel
When the current pixel color is not too far from the previous one, the difference to the previous pixel is saved to the stream.

There are two types of pixel differences, when the difference is small and when the difference is large. As noted in the specification, pixel differences "focuses on the RGB value; alpha changes are more costly".

When difference is small:
┌─ QOI_OP_DIFF ───────────┐
│         Byte[0]         │
│  7  6  5  4  3  2  1  0 │
│───────┼─────┼─────┼─────│
│  0  1 │  dr │  dg │  db │
└───────┴─────┴─────┴─────┘
2-bit tag b01
2-bit   red channel difference from the previous pixel -2..1
2-bit green channel difference from the previous pixel -2..1
2-bit  blue channel difference from the previous pixel -2..1


"The difference to the current channel values are using a wraparound
operation, so 1 - 2 will result in 255, while 255 + 1 will result
in 0.
Values are stored as unsigned integers with a bias of 2. E.g. -2
is stored as 0 (b00). 1 is stored as 3 (b11).
The alpha value remains unchanged from the previous pixel."

When the difference is large:

┌─ QOI_OP_LUMA ───────────┬─────────────────────────┐
│         Byte[0]         │         Byte[1]         │
│  7  6  5  4  3  2  1  0 │  7  6  5  4  3  2  1  0 │
│───────┼─────────────────┼─────────────┼───────────│
│  1  0 │   diff green    │   dr - dg   │  db - dg  │
└───────┴─────────────────┴─────────────┴───────────┘

2-bit tag b10
6-bit green channel difference from the previous pixel -32..31
4-bit   red channel difference minus green channel difference -8..7
4-bit  blue channel difference minus green channel difference -8..7

"The green channel is used to indicate the general direction of
change and is encoded in 6 bits. The red and blue channels (dr
and db) base their diffs off of the green channel difference. I.e.:
 dr_dg = (cur_px.r - prev_px.r) - (cur_px.g - prev_px.g)
 db_dg = (cur_px.b - prev_px.b) - (cur_px.g - prev_px.g)
The difference to the current channel values are using a wraparound
operation, so 10 - 13 will result in 253, while 250 + 7 will result
in 1.
Values are stored as unsigned integers with a bias of 32 for the
green channel and a bias of 8 for the red and blue channel.
The alpha value remains unchanged from the previous pixel."

-}

-- Let's first implement QOI_OP_DIFF. First, we update our chunks:
{-
data TwoBitTag
    = TwoBitTag
    deriving (Show, Eq)

data Chunk
    = QOI_OP_RUN
    { tag :: TwoBitTag
    , rle :: Binary.Word8
    }
    | QOI_OP_DIFF
    { tag :: TwoBitTag
    , dr :: Int8
    , dg :: Int8
    , db :: Int8
    }
    deriving (Show, Eq)
-}
-- Let's write our first test for encodePixelQoi.

-- Given a RGB image with 1 pixel that is different from the previous by a small red difference, when encoding, then it should encode a QOI_OP_DIFF chunk

rgbPixelSmallRDiff :: RGBPixel
rgbPixelSmallRDiff = RGBPixel {red = 1, green = 0, blue = 0}

-- >>> encodePixelQoi firstPreviousRGBValue rgbPixelSmallRDiff Nothing == QOI_OP_DIFF {tag = TwoBitTag, dr = 1, dg = 0, db = 0}
-- True
{-

encodePixelQoi :: (IsPixel pixel) => pixel -> pixel -> Maybe Chunk -> Chunk
encodePixelQoi previous current Nothing
    | previous == current = QOI_OP_RUN { tag = TwoBitTag, rle = 1}
    | (getRed current - getRed previous) == 1 = QOI_OP_DIFF {tag = TwoBitTag, dr = 1, dg = 0, db = 0}
encodePixelQoi previous current (Just (QOI_OP_RUN {rle}))
    | previous == current = QOI_OP_RUN { tag = TwoBitTag, rle = rleValue rle}
    | otherwise = undefined
    where
        rleValue :: Binary.Word8 -> Binary.Word8
        rleValue val
            | val < 62 = val + 1
            | otherwise = 1
encodePixelQoi p1 p2 (Just _) = undefined
-}

-- Given a RGB image with 1 pixel that is different from the previous by a small GREEN difference, when encoding, then it should encode a QOI_OP_DIFF chunk
-- >>> encodePixelQoi firstPreviousRGBValue rgbPixelSmallGDiff Nothing == QOI_OP_DIFF {tag = TwoBitTag, dr = 0, dg = 1, db = 0}
-- True

{-

class (Eq a) => IsPixel a where
    getRed :: a -> Binary.Word8
    getGreen :: a -> Binary.Word8
    getBlue :: a -> Binary.Word8
-}

rgbPixelSmallGDiff :: RGBPixel
rgbPixelSmallGDiff = RGBPixel {red = 0, green = 1, blue = 0}

{-}
encodePixelQoi :: (IsPixel pixel) => pixel -> pixel -> Maybe Chunk -> Chunk
encodePixelQoi previous current Nothing
    | previous == current = QOI_OP_RUN { tag = TwoBitTag, rle = 1}
    | (getRed current - getRed previous) == 1 = QOI_OP_DIFF {tag = TwoBitTag, dr = 1, dg = 0, db = 0}
    | (getGreen current - getGreen previous) == 1 = QOI_OP_DIFF {tag = TwoBitTag, dr = 0, dg = 1, db = 0}
    | otherwise = undefined
encodePixelQoi previous current (Just (QOI_OP_RUN {rle}))
    | previous == current = QOI_OP_RUN { tag = TwoBitTag, rle = rleValue rle}
    | otherwise = undefined
    where
        rleValue :: Binary.Word8 -> Binary.Word8
        rleValue val
            | val < 62 = val + 1
            | otherwise = 1
encodePixelQoi p1 p2 (Just _) = undefined

-}

-- Given a RGB image with 1 pixel that is different from the previous by a small blue difference, when encoding, then it should encode a QOI_OP_DIFF chunk
-- >>> encodePixelQoi firstPreviousRGBValue rgbPixelSmallBDiff Nothing == QOI_OP_DIFF {tag = TwoBitTag, dr = 0, dg = 0, db = 1}
-- True

rgbPixelSmallBDiff :: RGBPixel
rgbPixelSmallBDiff = RGBPixel {red = 0, green = 0, blue = 1}
{-
encodePixelQoi :: (IsPixel pixel) => pixel -> pixel -> Maybe Chunk -> Chunk
encodePixelQoi previous current Nothing
    | previous == current = QOI_OP_RUN { tag = TwoBitTag, rle = 1}
    | (getRed current - getRed previous) == 1 = QOI_OP_DIFF {tag = TwoBitTag, dr = 1, dg = 0, db = 0}
    | (getGreen current - getGreen previous) == 1 = QOI_OP_DIFF {tag = TwoBitTag, dr = 0, dg = 1, db = 0}
    | (getBlue current - getBlue previous) == 1 = QOI_OP_DIFF {tag = TwoBitTag, dr = 0, dg = 0, db = 1}
    | otherwise = undefined
encodePixelQoi previous current (Just (QOI_OP_RUN {rle}))
    | previous == current = QOI_OP_RUN { tag = TwoBitTag, rle = rleValue rle}
    | otherwise = undefined
    where
        rleValue :: Binary.Word8 -> Binary.Word8
        rleValue val
            | val < 62 = val + 1
            | otherwise = 1
encodePixelQoi p1 p2 (Just _) = undefined
-}

-- Before we refactor, let's do this one more test:

-- Given a RGB image with 1 pixel that is different from the previous by a small blue and green difference, when encoding, then it should encode a QOI_OP_DIFF chunk
-- >>> encodePixelQoi firstPreviousRGBValue rgbPixelSmallGBDiff Nothing == QOI_OP_DIFF {tag = TwoBitTag, dr = 0, dg = 1, db = 1}
-- True

rgbPixelSmallGBDiff :: RGBPixel
rgbPixelSmallGBDiff = RGBPixel {red = 0, green = 1, blue = 1}
{-
encodePixelQoi :: (IsPixel pixel) => pixel -> pixel -> Maybe Chunk -> Chunk
encodePixelQoi previous current Nothing
    | previous == current = QOI_OP_RUN { tag = TwoBitTag, rle = 1}
    | (getRed current - getRed previous) == 1 = QOI_OP_DIFF {tag = TwoBitTag, dr = 1, dg = 0, db = 0}
    | (getGreen current - getGreen previous) == 1 && ((getBlue current - getBlue previous) /= 1) = QOI_OP_DIFF {tag = TwoBitTag, dr = 0, dg = 1, db = 0}
    | (getGreen current - getGreen previous) == 1 && ((getBlue current - getBlue previous) == 1) = QOI_OP_DIFF {tag = TwoBitTag, dr = 0, dg = 1, db = 1}
    | (getBlue current - getBlue previous) == 1 = QOI_OP_DIFF {tag = TwoBitTag, dr = 0, dg = 0, db = 1}
    | otherwise = undefined
encodePixelQoi previous current (Just (QOI_OP_RUN {rle}))
    | previous == current = QOI_OP_RUN { tag = TwoBitTag, rle = rleValue rle}
    | otherwise = undefined
    where
        rleValue :: Binary.Word8 -> Binary.Word8
        rleValue val
            | val < 62 = val + 1
            | otherwise = 1
encodePixelQoi p1 p2 (Just _) = undefined
-}

-- Let's refactor:
{-
encodePixelQoi :: (IsPixel pixel) => pixel -> pixel -> Maybe Chunk -> Chunk
encodePixelQoi previous current Nothing
    | previous == current = QOI_OP_RUN { tag = TwoBitTag, rle = 1}
    | isSmallDifference = QOI_OP_DIFF {tag = TwoBitTag, dr = rDiff, dg = gDiff, db = bDiff}
    | otherwise = undefined
    where
        rDiff = getRed current - getRed previous
        gDiff = getGreen current - getGreen previous
        bDiff = getBlue current - getBlue previous
        isSmallDifference = rDiff <= 1 && gDiff <= 1 && bDiff <= 1 
encodePixelQoi previous current (Just (QOI_OP_RUN {rle}))
    | previous == current = QOI_OP_RUN { tag = TwoBitTag, rle = rleValue rle}
    | otherwise = undefined
    where
        rleValue :: Binary.Word8 -> Binary.Word8
        rleValue val
            | val < 62 = val + 1
            | otherwise = 1
encodePixelQoi p1 p2 (Just _) = undefined
-}
-- Let's continue:

rgbPixelRPlus1GMinus2BMinus1Diff :: RGBPixel
rgbPixelRPlus1GMinus2BMinus1Diff = RGBPixel {red = 1, green = 254, blue = 255}

-- Given a RGB image with 1 pixel that is different from the previous by a small red (+1), small blue (-2) and green difference (-1), when encoding, then it should encode a QOI_OP_DIFF chunk
-- >>> encodePixelQoi firstPreviousRGBValue rgbPixelRPlus1GMinus2BMinus1Diff Nothing == QOI_OP_DIFF {tag = TwoBitTag, dr = 1, dg = -2, db = -1}
-- True

--import Data.Int
{-
encodePixelQoi :: (IsPixel pixel) => pixel -> pixel -> Maybe Chunk -> Chunk

encodePixelQoi previous current previousChunk
    | previous == current = getRle previousChunk
    | isSmallPixelDifference = QOI_OP_DIFF {tag = TwoBitTag, dr = rDiff, dg = gDiff, db = bDiff}
    | otherwise = undefined
    where
        rleValueConsideringPreviousRle :: Binary.Word8 -> Binary.Word8
        rleValueConsideringPreviousRle previousRle
            | previousRle < 62 = previousRle + 1
            | otherwise = 1

        getRle :: Maybe Chunk -> Chunk
        getRle (Just (QOI_OP_RUN {rle})) = QOI_OP_RUN { tag = TwoBitTag, rle = rleValueConsideringPreviousRle rle}
        getRle _ = QOI_OP_RUN { tag = TwoBitTag, rle = 1}

        rDiff :: Int8
        rDiff = fromIntegral (getRed current - getRed previous) :: Int8

        gDiff :: Int8
        gDiff = fromIntegral (getGreen current - getGreen previous) :: Int8

        bDiff :: Int8
        bDiff = fromIntegral (getBlue current - getBlue previous) :: Int8

        isSmallPixelDifference :: Bool
        isSmallPixelDifference = isSmallDiff rDiff && isSmallDiff gDiff && isSmallDiff bDiff

        isSmallDiff :: Int8 -> Bool
        isSmallDiff x = x >= -2 && x <= 1

-}

-- Now let's implement decode considering QOI_OP_DIFF.

-- Our first test:

-- Given a RGB pixel and QOI_OP_DIFF of dr = 1, dg = -2, db = -1, when decoding, then it should decode the next pixel considering the differences
-- >>> decodePixelQoi firstPreviousRGBValue (QOI_OP_DIFF {tag = TwoBitTag, dr = 1, dg = -2, db = -1}) == [rgbPixelRPlus1GMinus2BMinus1Diff]
-- True

-- Because of our constraint (IsPixel) in decodePixelQoi, we need to create a new pixel that respects the difference, without directly using the RGBPixel and RGBAPixel constructors. Therefore, we need to add a new function to our IsPixel typeclass:
{-
class (Eq a) => IsPixel a where
    getRed :: a -> Binary.Word8
    getGreen :: a -> Binary.Word8
    getBlue :: a -> Binary.Word8
    addDiff :: a -> Int8 -> Int8 -> Int8 -> a


instance IsPixel RGBPixel where
    getRed :: RGBPixel -> Binary.Word8
    getRed (RGBPixel {red}) = red
    getGreen :: RGBPixel -> Binary.Word8
    getGreen (RGBPixel {green}) = green
    getBlue :: RGBPixel -> Binary.Word8
    getBlue (RGBPixel {blue}) = blue
    addDiff :: RGBPixel -> Int8 -> Int8 -> Int8 -> RGBPixel
    addDiff (RGBPixel {red, green, blue}) dr dg db = RGBPixel (red + convertInt8ToWord8 dr) (green + convertInt8ToWord8 dg) (blue + convertInt8ToWord8 db)


convertInt8ToWord8 :: Int8 -> Binary.Word8
convertInt8ToWord8 x = fromIntegral x :: Binary.Word8

instance IsPixel RGBAPixel where
    getRed :: RGBAPixel -> Binary.Word8
    getRed (RGBAPixel r _ _ _) = r
    getGreen :: RGBAPixel -> Binary.Word8
    getGreen (RGBAPixel _ g _ _) = g
    getBlue :: RGBAPixel -> Binary.Word8
    getBlue (RGBAPixel _ _ b _) = b
    addDiff :: RGBAPixel -> Int8 -> Int8 -> Int8 -> RGBAPixel
    addDiff (RGBAPixel r g b a) dr dg db = RGBAPixel (r + convertInt8ToWord8 dr) (g + convertInt8ToWord8 dg) (b + convertInt8ToWord8 db) a
-}
-- newtype Red, Blue... newtype SmallDiffRed, newtype SmallDiffBlue, newtype LargeDiffRed ...

-- We can then implement the decoding case for QOI_OP_DIFF.
{-
decodePixelQoi :: (IsPixel pixel) => pixel -> Chunk -> [pixel]
decodePixelQoi previousPixel (QOI_OP_RUN {rle}) = replicate (fromIntegral rle) previousPixel
decodePixelQoi previousPixel (QOI_OP_DIFF {dr, dg, db}) = [addDiff previousPixel dr dg db]
decodePixelQoi _ _ = undefined
-}
z = ""
-- Now, let's implement encodeQoi and decodeQoi.

-- First, a test:

-- Given a RGB image with a pixel with small difference from the first previous pixel, when encoding, then it should encode with only QOI_OP_DIFF chunks.

-- >>> encodeQOI (RGBImage 1 1 AllChannelsLinear [rgbPixelRPlus1GMinus2BMinus1Diff]) == QoiFile (QoiHeader {magic = MagicHeader, width = 1, height = 1, channels = RGB, colorspace = AllChannelsLinear}) [QOI_OP_DIFF {tag = TwoBitTag, dr = 1, dg = -2, db = -1}] EndMarker
-- True

-- This test passes without us needing to do anything! What about this next one:

-- Given a RGB image with a pixel with small difference from the first previous pixel repeated twice, when encoding, then it should encode with a first QOI_OP_DIFF and then QOI_OP_RUN chunks.

-- >>> encodeQOI (RGBImage 62 1 AllChannelsLinear (replicate 2 rgbPixelRPlus1GMinus2BMinus1Diff)) == QoiFile (QoiHeader {magic = MagicHeader, width = 62, height = 1, channels = RGB, colorspace = AllChannelsLinear}) [QOI_OP_DIFF {tag = TwoBitTag, dr = 1, dg = -2, db = -1}, QOI_OP_RUN {tag = TwoBitTag, rle = 1}] EndMarker
-- True

-- We need to change encodePixelsToChunks, so that we don't discard previous chunks that aren't QOI_OP_RUN. 
{-
encodeQOI img
    = QoiFile header (chunks img) EndMarker
    where
        header :: QoiHeader
        header = extractQoiHeaderFromAnImage img

        chunks :: Image -> [Chunk]
        chunks (RGBImage _ 0 _ _) = []
        chunks (RGBImage 0 _ _ _) = []
        chunks (RGBAImage 0 _ _ _) = []
        chunks (RGBAImage _ 0 _ _) = []
        chunks (RGBAImage _ _ _ pixels) =
            encodePixelsToChunks pixels firstPreviousRGBAValue Nothing
        chunks (RGBImage _ _ _ pixels) =
            encodePixelsToChunks pixels firstPreviousRGBValue Nothing

encodePixelsToChunks :: (IsPixel pixel) => [pixel] -> pixel -> Maybe Chunk -> [Chunk]
encodePixelsToChunks [] _ Nothing = []
encodePixelsToChunks [x] first Nothing = [encodePixelQoi first x Nothing]
encodePixelsToChunks (x:xs) first Nothing =
    let
        firstChunk :: Chunk
        firstChunk = encodePixelQoi first x Nothing
    in
        encodePixelsToChunks xs x (Just firstChunk)
encodePixelsToChunks (x:xs) previous prev@(Just previousChunk) =
    let
        nextChunk = encodePixelQoi previous x prev
        toBeAdded = case (previousChunk, nextChunk) of
            (QOI_OP_RUN {rle}, QOI_OP_RUN {}) -> ([previousChunk | rle == 62])
            _ -> [previousChunk]
    in toBeAdded ++ encodePixelsToChunks xs x (Just nextChunk)

encodePixelsToChunks [] previous prev@(Just previousChunk) =
    [previousChunk]
-}

-- One more:

-- Given a RGB image with a pixel with a small difference from the first previous pixel repeated twice, followed by one with small difference, when encoding, then it should encode with a first QOI_OP_DIFF, QOI_OP_RUN chunk and then QOI_OP_DIFF.


rgbPixelRPlus2GMinus2BMinus1Diff :: RGBPixel
rgbPixelRPlus2GMinus2BMinus1Diff = RGBPixel {red = 2, green = 254, blue = 255}
-- >>> encodeQOI (RGBImage 62 1 AllChannelsLinear ((replicate 2 rgbPixelRPlus1GMinus2BMinus1Diff) ++ [rgbPixelRPlus2GMinus2BMinus1Diff])) ==  QoiFile (QoiHeader {magic = MagicHeader, width = 62, height = 1, channels = RGB, colorspace = AllChannelsLinear}) [QOI_OP_DIFF {tag = TwoBitTag, dr = 1, dg = -2, db = -1},QOI_OP_RUN {tag = TwoBitTag, rle = 1},QOI_OP_DIFF {tag = TwoBitTag, dr = 1, dg = 0, db = 0}] EndMarker
-- True

-- QoiFile (QoiHeader {magic = qoif, width = 62, height = 1, channels = RGB, colorspace = AllChannelsLinear}) [QOI_OP_DIFF {tag = TwoBitTag, dr = 1, dg = -2, db = -1},QOI_OP_RUN {tag = TwoBitTag, rle = 1},QOI_OP_DIFF {tag = TwoBitTag, dr = 1, dg = 0, db = 0}] EndMarker

-- We need to change encodePixelsToChunks so that we return a previous QOI_OP_RUN chunks when the current chunk is not QOI_OP_RUN.
{-
encodeQOI img
    = QoiFile header (getChunksFromImage img) EndMarker
    where
        header :: QoiHeader
        header = extractQoiHeaderFromAnImage img

        getChunksFromImage :: Image -> [Chunk]
        getChunksFromImage (RGBImage _ 0 _ _) = []
        getChunksFromImage (RGBImage 0 _ _ _) = []
        getChunksFromImage (RGBImage _ _ _ pixels) =
            encodePixelsToChunks pixels firstPreviousRGBValue Nothing

        getChunksFromImage (RGBAImage 0 _ _ _) = []
        getChunksFromImage (RGBAImage _ 0 _ _) = []
        getChunksFromImage (RGBAImage _ _ _ pixels) =
            encodePixelsToChunks pixels firstPreviousRGBAValue Nothing

encodePixelsToChunks :: (IsPixel pixel) => [pixel] -> pixel -> Maybe Chunk -> [Chunk]
encodePixelsToChunks [] _ Nothing = []
encodePixelsToChunks [x] first Nothing = [encodePixelQoi first x Nothing]
encodePixelsToChunks (x:xs) firstPreviousPixel Nothing =
    let
        firstChunk :: Chunk
        firstChunk = encodePixelQoi firstPreviousPixel x Nothing
    in
        encodePixelsToChunks xs x (Just firstChunk)
encodePixelsToChunks (x:xs) previousPixel prev@(Just previousChunk) =
    let
        nextChunk = encodePixelQoi previousPixel x prev
        toBeAdded = case (previousChunk, nextChunk) of
            (QOI_OP_RUN {rle}, QOI_OP_RUN {}) -> ([previousChunk | rle == 62])
            _ -> [previousChunk]
    in toBeAdded ++ encodePixelsToChunks xs x (Just nextChunk)

encodePixelsToChunks [] previous prev@(Just previousChunk) =
    [previousChunk]
-}
-- Lets write a test for decodeQoi:

p = ""

testImg = RGBImage 62 1 AllChannelsLinear (replicate 2 rgbPixelRPlus1GMinus2BMinus1Diff ++ [rgbPixelRPlus2GMinus2BMinus1Diff])
-- >>> (decodeQoi . encodeQOI) testImg == testImg
-- True

decodeQoi :: QoiFile -> Image
decodeQoi (QoiFile (QoiHeader _ width height RGB colorspace) chunks _) = RGBImage width height colorspace (decodeChunks firstPreviousRGBValue chunks)
decodeQoi (QoiFile (QoiHeader _ width height RGBA colorspace) chunks _) = RGBAImage width height colorspace (decodeChunks firstPreviousRGBAValue chunks)

decodeChunks :: (IsPixel pixel) => pixel -> [Chunk] -> [pixel]
decodeChunks _ [] = []
decodeChunks previousPixel (chunk:restOfChunks) =
    let
        decodedChunk = decodePixelQoi previousPixel chunk
        nextPreviousPixel = last decodedChunk
    in decodedChunk ++ decodeChunks nextPreviousPixel restOfChunks


{-
Now we must implement the chunk for when the difference is large:

┌─ QOI_OP_LUMA ───────────┬─────────────────────────┐
│         Byte[0]         │         Byte[1]         │
│  7  6  5  4  3  2  1  0 │  7  6  5  4  3  2  1  0 │
│───────┼─────────────────┼─────────────┼───────────│
│  1  0 │   diff green    │   dr - dg   │  db - dg  │
└───────┴─────────────────┴─────────────┴───────────┘

2-bit tag b10
6-bit green channel difference from the previous pixel -32..31
4-bit   red channel difference minus green channel difference -8..7
4-bit  blue channel difference minus green channel difference -8..7

"The green channel is used to indicate the general direction of
change and is encoded in 6 bits. The red and blue channels (dr
and db) base their diffs off of the green channel difference. I.e.:
 dr_dg = (cur_px.r - prev_px.r) - (cur_px.g - prev_px.g)
 db_dg = (cur_px.b - prev_px.b) - (cur_px.g - prev_px.g)
The difference to the current channel values are using a wraparound
operation, so 10 - 13 will result in 253, while 250 + 7 will result
in 1.
Values are stored as unsigned integers with a bias of 32 for the
green channel and a bias of 8 for the red and blue channel.
The alpha value remains unchanged from the previous pixel."

-}

-- Let's increment our chunks:
{-
data TwoBitTag
    = TwoBitTag
    deriving (Show, Eq)

data Chunk
    = QOI_OP_RUN
    { tag :: TwoBitTag
    , rle :: Binary.Word8
    }
    | QOI_OP_DIFF
    { tag :: TwoBitTag
    , dr :: Int8
    , dg :: Int8
    , db :: Int8
    }
    | QOI_OP_LUMA 
    { tag :: TwoBitTag
    , dg :: Int8
    , dr_dg :: Int8
    , db_dg :: Int8
    }
    deriving (Show, Eq)
-}

-- Red, Green, Blue, Alpha as types
-- DiffRed, DiffGreen etc, as types
-- property nao ter QOI_INDEX um do lado do outro pro mesmo indice
-- -> [pixel] -> pixel -> Maybe Chunk -> ([pixel], [Chunk])

-- Let's write our first test:

-- Given a RGBA pixel that has a large difference from the first previous RGBA pixel, when encoding, then it should return a single QOI_OP_LUMA chunk 
-- >>> encodePixelQoi firstPreviousRGBAValue RGBAPixel {red = 5, green = 5, blue = 5} Nothing == QOI_OP_LUMA {tag = TwoBitTag, dg = 5, dr_dg = 0, db_dg = 0}
-- True

{-
encodePixelQoi :: (IsPixel pixel) => pixel -> pixel -> Maybe Chunk -> Chunk

encodePixelQoi previous current previousChunk
    | previous == current = getRle previousChunk
    | isSmallPixelDifference = QOI_OP_DIFF {tag = TwoBitTag, dr = rDiff, dg = gDiff, db = bDiff}
    | isBigPixelDifference = QOI_OP_LUMA {tag=TwoBitTag, dg = 5, dr_dg = 0, db_dg = 0}
    | otherwise = undefined
    where
        rleValueConsideringPreviousRle :: Binary.Word8 -> Binary.Word8
        rleValueConsideringPreviousRle previousRle
            | previousRle < 62 = previousRle + 1
            | otherwise = 1

        getRle :: Maybe Chunk -> Chunk
        getRle (Just (QOI_OP_RUN {rle})) = QOI_OP_RUN { tag = TwoBitTag, rle = rleValueConsideringPreviousRle rle}
        getRle _ = QOI_OP_RUN { tag = TwoBitTag, rle = 1}

        rDiff :: Int8
        rDiff = fromIntegral (getRed current - getRed previous) :: Int8

        gDiff :: Int8
        gDiff = fromIntegral (getGreen current - getGreen previous) :: Int8

        bDiff :: Int8
        bDiff = fromIntegral (getBlue current - getBlue previous) :: Int8

        isSmallPixelDifference :: Bool
        isSmallPixelDifference = isSmallDiff rDiff && isSmallDiff gDiff && isSmallDiff bDiff

        isSmallDiff :: Int8 -> Bool
        isSmallDiff x = x >= -2 && x <= 1

        isBigPixelDifference :: Bool
        isBigPixelDifference = True 
-}

-- Given a RGBA pixel that has a large difference from the first previous RGBA pixel, but for only green, when encoding, then it should return a single QOI_OP_LUMA chunk 
-- >>> encodePixelQoi firstPreviousRGBAValue RGBAPixel {red = 1, green = 5, blue = 1} Nothing == QOI_OP_LUMA {tag = TwoBitTag, dg = 5, dr_dg = -4, db_dg = -4}
-- True

{-
encodePixelQoi :: (IsPixel pixel) => pixel -> pixel -> Maybe Chunk -> Chunk
encodePixelQoi previous current previousChunk
    | previous == current = getRle previousChunk
    | isSmallPixelDifference = QOI_OP_DIFF {tag = TwoBitTag, dr = rDiff, dg = gDiff, db = bDiff}
    | isBigPixelDifference = QOI_OP_LUMA {tag=TwoBitTag, dg = gDiff, dr_dg = rBigDiff, db_dg = bBigDiff}
    | otherwise = undefined
    where
        rleValueConsideringPreviousRle :: Binary.Word8 -> Binary.Word8
        rleValueConsideringPreviousRle previousRle
            | previousRle < 62 = previousRle + 1
            | otherwise = 1

        getRle :: Maybe Chunk -> Chunk
        getRle (Just (QOI_OP_RUN {rle})) = QOI_OP_RUN { tag = TwoBitTag, rle = rleValueConsideringPreviousRle rle}
        getRle _ = QOI_OP_RUN { tag = TwoBitTag, rle = 1}

        rDiff :: Int8
        rDiff = fromIntegral (getRed current - getRed previous) :: Int8

        gDiff :: Int8
        gDiff = fromIntegral (getGreen current - getGreen previous) :: Int8

        bDiff :: Int8
        bDiff = fromIntegral (getBlue current - getBlue previous) :: Int8

        isSmallPixelDifference :: Bool
        isSmallPixelDifference = isSmallDiff rDiff && isSmallDiff gDiff && isSmallDiff bDiff

        isSmallDiff :: Int8 -> Bool
        isSmallDiff x = x >= -2 && x <= 1

        isBigPixelDifference :: Bool
        isBigPixelDifference = True

        rBigDiff :: Int8
        rBigDiff = rDiff - gDiff

        bBigDiff :: Int8
        bBigDiff = bDiff - gDiff

-}
{- FORGET FORGET
-- By how we implemented isBigPixelDifference, our pattern matching will never reach the otherwise branch. Let's fill this hole:

-- Given a RGBA pixel that has a large difference from the first previous RGBA pixel, but for only blue and exceeding QOI_OP_LUMA capacities, when encoding, then it should not return a QOI_OP_LUMA chunk 
-- >>> (encodePixelQoi firstPreviousRGBAValue RGBAPixel {red = 0, green = 0, blue = 8} Nothing) /= QOI_OP_LUMA {tag = TwoBitTag, dg = 0, dr_dg = 0, db_dg = 8}
-- Prelude.undefined

encodePixelQoi :: (IsPixel pixel) => pixel -> pixel -> Maybe Chunk -> Chunk
encodePixelQoi previous current previousChunk
    | previous == current = getRle previousChunk
    | isSmallPixelDifference = QOI_OP_DIFF {tag = TwoBitTag, dr = rDiff, dg = gDiff, db = bDiff}
    | isBigPixelDifference = QOI_OP_LUMA {tag=TwoBitTag, dg = gDiff, dr_dg = rBigDiff, db_dg = bBigDiff}
    | otherwise = undefined
    where
        rleValueConsideringPreviousRle :: Binary.Word8 -> Binary.Word8
        rleValueConsideringPreviousRle previousRle
            | previousRle < 62 = previousRle + 1
            | otherwise = 1

        getRle :: Maybe Chunk -> Chunk
        getRle (Just (QOI_OP_RUN {rle})) = QOI_OP_RUN { tag = TwoBitTag, rle = rleValueConsideringPreviousRle rle}
        getRle _ = QOI_OP_RUN { tag = TwoBitTag, rle = 1}

        rDiff :: Int8
        rDiff = fromIntegral (getRed current - getRed previous) :: Int8

        gDiff :: Int8
        gDiff = fromIntegral (getGreen current - getGreen previous) :: Int8

        bDiff :: Int8
        bDiff = fromIntegral (getBlue current - getBlue previous) :: Int8

        isSmallPixelDifference :: Bool
        isSmallPixelDifference = isSmallDiff rDiff && isSmallDiff gDiff && isSmallDiff bDiff

        isSmallDiff :: Int8 -> Bool
        isSmallDiff x = x >= -2 && x <= 1

        isBigPixelDifference :: Bool
        isBigPixelDifference = isLargeRedDiff rBigDiff && isLargeBlueDiff bBigDiff && isLargeGreenDiff gDiff

        isLargeRedDiff :: Int8 -> Bool
        isLargeRedDiff dr_dg = dr_dg >= -8 && dr_dg <= 7

        isLargeBlueDiff :: Int8 -> Bool
        isLargeBlueDiff = isLargeRedDiff

        isLargeGreenDiff :: Int8 -> Bool
        isLargeGreenDiff dg = dg >= -32 && dg <= 31

        rBigDiff :: Int8
        rBigDiff = rDiff - gDiff

        bBigDiff :: Int8
        bBigDiff = bDiff - gDiff

-- We could have implemented more tests, each for a edge case, but isBigPixelDifference is simple enough for us to implement everything in a single pass.
-}
{-
encodePixelQoi :: (IsPixel pixel) => pixel -> pixel -> Maybe Chunk -> Chunk
encodePixelQoi previous current previousChunk
    | previous == current = getRle previousChunk
    | isSmallPixelDifference = QOI_OP_DIFF {tag = TwoBitTag, dr = rDiff, dg = gDiff, db = bDiff}
    | isBigPixelDifference = QOI_OP_LUMA {tag=TwoBitTag, dg = gDiff, dr_dg = rBigDiff, db_dg = bBigDiff}
    | otherwise = undefined
    where
        rleValueConsideringPreviousRle :: Binary.Word8 -> Binary.Word8
        rleValueConsideringPreviousRle previousRle
            | previousRle < 62 = previousRle + 1
            | otherwise = 1

        getRle :: Maybe Chunk -> Chunk
        getRle (Just (QOI_OP_RUN {rle})) = QOI_OP_RUN { tag = TwoBitTag, rle = rleValueConsideringPreviousRle rle}
        getRle _ = QOI_OP_RUN { tag = TwoBitTag, rle = 1}

        rDiff :: Int8
        rDiff = fromIntegral (getRed current - getRed previous) :: Int8

        gDiff :: Int8
        gDiff = fromIntegral (getGreen current - getGreen previous) :: Int8

        bDiff :: Int8
        bDiff = fromIntegral (getBlue current - getBlue previous) :: Int8

        isSmallPixelDifference :: Bool
        isSmallPixelDifference = isSmallDiff rDiff && isSmallDiff gDiff && isSmallDiff bDiff

        isSmallDiff :: Int8 -> Bool
        isSmallDiff x = x >= -2 && x <= 1

        isBigPixelDifference :: Bool
        isBigPixelDifference = True

        rBigDiff :: Int8
        rBigDiff = rDiff - gDiff

        bBigDiff :: Int8
        bBigDiff = bDiff - gDiff
-}
-- Let's write tests for the decoding:




-- Lets write a test for decodePixelQoi:


rgbPixelRPlus2GMinus10BMinus1Diff :: RGBPixel
rgbPixelRPlus2GMinus10BMinus1Diff = RGBPixel {red = 2, green = 246, blue = 255}

-- Given a RGB pixel and QOI_OP_LUMA with dg = -10, dr_dg = 12, dr_db = -9, when decoding, then it should decode the next pixel considering the differences
-- >>> decodePixelQoi firstPreviousRGBValue (QOI_OP_LUMA {tag = TwoBitTag, dg = -10, dr_dg = 12, db_dg = 9}) == [rgbPixelRPlus2GMinus10BMinus1Diff] 
-- True


decodePixelQoi :: (IsPixel pixel) => pixel -> Chunk -> [pixel]
decodePixelQoi pixel (QOI_OP_RUN {rle}) = replicate (fromIntegral rle) pixel
decodePixelQoi pixel (QOI_OP_DIFF {dr, dg, db}) = [addDiff pixel dr dg db]
decodePixelQoi pixel (QOI_OP_LUMA {dg, dr_dg, db_dg}) = [addDiff pixel (dg + dr_dg) dg (dg + db_dg)]
--decodePixelQoi _ _ = undefined

-- Let's write a test for encodeQOI and decodeQoi:

testImg2 = RGBImage 62 1 AllChannelsLinear (replicate 2 rgbPixelRPlus1GMinus2BMinus1Diff ++ [rgbPixelRPlus2GMinus10BMinus1Diff])
-- >>> (decodeQoi . encodeQOI) testImg2 == testImg2
-- True


{-
With this, we conclude our implementation of the QOI_OP_DIFF and QOI_OP_LUMA chunks. In the next part, we will implement the QOI_OP_INDEX chunk type.



-}

{-
    In the previous part, we implemented encoding and decoding for the QOI_OP_DIFF and QOI_OP_LUMA chunks defined by QOI. In this part, we will define the second chunk, QOI_OP_INDEX.

"The encoder keeps a running array of the 64 pixels it previously encountered. When the encoder finds the current pixel still present in this array, the index into this array is saved to the stream.

To keep things O(n) when encoding, there's only one lookup into this array. The lookup position is determined by a “hash” of the rgba value (really just (r * 3 + g * 5 + b * 7 + a * 11). A linear search or some more complex bookkeeping would result in a marginally better compression ratio, but would also slow things down a bit.

┌─ QOI_OP_INDEX ──────────┐
│         Byte[0]         │
│  7  6  5  4  3  2  1  0 │
│───────┼─────────────────│
│  0  0 │     index       │
└───────┴─────────────────┘
2-bit tag b00
6-bit index into the color index array: 0..63

A running array[64] (zero-initialized) of previously seen pixel
values is maintained by the encoder and decoder. Each pixel that is
seen by the encoder and decoder is put into this array at the
position formed by a hash function of the color value. In the
encoder, if the pixel value at the index matches the current pixel,
this index position is written to the stream as QOI_OP_INDEX. The
hash function for the index is:
 index_position = (r * 3 + g * 5 + b * 7 + a * 11) % 64

-}

-- Let's update our chunks:

data TwoBitTag
    = TwoBitTag
    deriving (Show, Eq)

data Chunk
    = QOI_OP_RUN
    { tag :: TwoBitTag
    , rle :: Binary.Word8
    }
    | QOI_OP_DIFF
    { tag :: TwoBitTag
    , dr :: Int8
    , dg :: Int8
    , db :: Int8
    }
    | QOI_OP_LUMA
    { tag :: TwoBitTag
    , dg :: Int8
    , dr_dg :: Int8
    , db_dg :: Int8
    }
    | QOI_OP_INDEX
    { tag :: TwoBitTag
    , index :: Binary.Word8
    }
    deriving (Show, Eq)

-- Now, let's implement our hash function. We want to add the following function to our type class IsPixel:  
-- calculateIndexPosition :: a -> Binary.Word8

class (Eq a) => IsPixel a where
    getRed :: a -> Binary.Word8
    getGreen :: a -> Binary.Word8
    getBlue :: a -> Binary.Word8
    addDiff :: a -> Int8 -> Int8 -> Int8 -> a
    calculateIndexPosition :: a -> Binary.Word8

{-
instance IsPixel RGBPixel where
    getRed :: RGBPixel -> Binary.Word8
    getRed (RGBPixel {red}) = red
    getGreen :: RGBPixel -> Binary.Word8
    getGreen (RGBPixel {green}) = green
    getBlue :: RGBPixel -> Binary.Word8
    getBlue (RGBPixel {blue}) = blue
    addDiff :: RGBPixel -> Int8 -> Int8 -> Int8 -> RGBPixel
    addDiff (RGBPixel {red, green, blue}) dr dg db = RGBPixel (red + convertInt8ToWord8 dr) (green + convertInt8ToWord8 dg) (blue + convertInt8ToWord8 db)
    calculateIndexPosition :: RGBPixel -> Binary.Word8
    calculateIndexPosition = undefined
-}
convertInt8ToWord8 :: Int8 -> Binary.Word8
convertInt8ToWord8 x = fromIntegral x :: Binary.Word8


-- Our first test:

-- Given a RGB pixel with r = 10, g = 10, b = 0, when hashing, then it should return 5

-- >>> calculateIndexPosition RGBPixel {red = 10, green = 10, blue = 0} == 5
-- True

instance IsPixel RGBPixel where
    getRed :: RGBPixel -> Binary.Word8
    getRed (RGBPixel {red}) = red
    getGreen :: RGBPixel -> Binary.Word8
    getGreen (RGBPixel {green}) = green
    getBlue :: RGBPixel -> Binary.Word8
    getBlue (RGBPixel {blue}) = blue
    addDiff :: RGBPixel -> Int8 -> Int8 -> Int8 -> RGBPixel
    addDiff (RGBPixel {red, green, blue}) dr dg db = RGBPixel (red + convertInt8ToWord8 dr) (green + convertInt8ToWord8 dg) (blue + convertInt8ToWord8 db)
    calculateIndexPosition :: RGBPixel -> Binary.Word8
    calculateIndexPosition (RGBPixel {red, green, blue}) = (red * 3 + green * 5 + blue * 7 + 255 * 11) `mod` 64



-- Given a RGBA pixel with r = 10, g = 10, b = 0, alpha = 10, when hashing, then it should return 62

-- >>> calculateIndexPosition RGBAPixel {red = 10, green = 10, blue = 0, alpha = 10} == 62
-- True


instance IsPixel RGBAPixel where
    getRed :: RGBAPixel -> Binary.Word8
    getRed (RGBAPixel {red}) = red
    getGreen :: RGBAPixel -> Binary.Word8
    getGreen (RGBAPixel {green}) = green
    getBlue :: RGBAPixel -> Binary.Word8
    getBlue (RGBAPixel {blue}) = blue
    addDiff :: RGBAPixel -> Int8 -> Int8 -> Int8 -> RGBAPixel
    addDiff (RGBAPixel {red, green, blue, alpha}) dr dg db = RGBAPixel (red + convertInt8ToWord8 dr) (green + convertInt8ToWord8 dg) (blue + convertInt8ToWord8 db) alpha
    calculateIndexPosition :: RGBAPixel -> Binary.Word8
    calculateIndexPosition (RGBAPixel {red, green, blue, alpha}) = (red * 3 + green * 5 + blue * 7 + alpha * 11) `mod` 64

-- With our hash functions, we should now adapt our encodePixelQoi and decodePixelQoi. Let's start with a test for encodePixelQoi. For this test, let's first import Data.map :

--

-- A map is not the ideal data structure, but it will work for now. In part ...

rgbPixelR10G0B0 :: RGBPixel
rgbPixelR10G0B0 = RGBPixel {red = 10, green = 0, blue = 0}

-- For now, the following test won't compile:

-- Given a RGB pixel that is not equal to the previous pixel and was seen previously, when encoding, then should be encoded as a QOI_OP_INDEX chunk.
-- >>>  encodePixelQoi firstPreviousRGBValue rgbPixelR10G0B0 Nothing (Map.singleton 19 rgbPixelR10G0B0) == QOI_OP_INDEX {tag = TwoBitTag, index = 19}
-- True

-- Let's make it compile:

{-
encodePixelQoi :: (IsPixel pixel) => pixel -> pixel -> Maybe Chunk -> Map.Map Binary.Word8 Binary.Word8 -> Chunk
encodePixelQoi previous current previousChunk previouslySeenPixels
    | previous == current = getRle previousChunk
    | isSmallPixelDifference = QOI_OP_DIFF {tag = TwoBitTag, dr = rDiff, dg = gDiff, db = bDiff}
    | isBigPixelDifference = QOI_OP_LUMA {tag=TwoBitTag, dg = gDiff, dr_dg = rBigDiff, db_dg = bBigDiff}
    | otherwise = undefined
    where
        rleValueConsideringPreviousRle :: Binary.Word8 -> Binary.Word8
        rleValueConsideringPreviousRle previousRle
            | previousRle < 62 = previousRle + 1
            | otherwise = 1

        getRle :: Maybe Chunk -> Chunk
        getRle (Just (QOI_OP_RUN {rle})) = QOI_OP_RUN { tag = TwoBitTag, rle = rleValueConsideringPreviousRle rle}
        getRle _ = QOI_OP_RUN { tag = TwoBitTag, rle = 1}

        rDiff :: Int8
        rDiff = fromIntegral (getRed current - getRed previous) :: Int8

        gDiff :: Int8
        gDiff = fromIntegral (getGreen current - getGreen previous) :: Int8

        bDiff :: Int8
        bDiff = fromIntegral (getBlue current - getBlue previous) :: Int8

        isSmallPixelDifference :: Bool
        isSmallPixelDifference = isSmallDiff rDiff && isSmallDiff gDiff && isSmallDiff bDiff

        isSmallDiff :: Int8 -> Bool
        isSmallDiff x = x >= -2 && x <= 1

        isBigPixelDifference :: Bool
        isBigPixelDifference = True

        rBigDiff :: Int8
        rBigDiff = rDiff - gDiff

        bBigDiff :: Int8
        bBigDiff = bDiff - gDiff
-}

encodeQOI img
    = QoiFile header (getChunksFromImage img) EndMarker
    where
        header :: QoiHeader
        header = extractQoiHeaderFromAnImage img

        getChunksFromImage :: Image -> [Chunk]
        getChunksFromImage (RGBImage _ 0 _ _) = []
        getChunksFromImage (RGBImage 0 _ _ _) = []
        getChunksFromImage (RGBImage _ _ _ pixels) =
            encodePixelsToChunks pixels firstPreviousRGBValue Nothing

        getChunksFromImage (RGBAImage 0 _ _ _) = []
        getChunksFromImage (RGBAImage _ 0 _ _) = []
        getChunksFromImage (RGBAImage _ _ _ pixels) =
            encodePixelsToChunks pixels firstPreviousRGBAValue Nothing

encodePixelsToChunks :: (IsPixel pixel) => [pixel] -> pixel -> Maybe Chunk -> [Chunk]
encodePixelsToChunks [] _ Nothing = []
encodePixelsToChunks [x] first Nothing = [encodePixelQoi first x Nothing Map.empty]
encodePixelsToChunks (x:xs) firstPreviousPixel Nothing =
    let
        firstChunk :: Chunk
        firstChunk = encodePixelQoi firstPreviousPixel x Nothing Map.empty
    in
        encodePixelsToChunks xs x (Just firstChunk)
encodePixelsToChunks (x:xs) previousPixel prev@(Just previousChunk) =
    let
        nextChunk = encodePixelQoi previousPixel x prev Map.empty
        toBeAdded = case (previousChunk, nextChunk) of
            (QOI_OP_RUN {rle}, QOI_OP_RUN {}) -> ([previousChunk | rle == 62])
            _ -> [previousChunk]
    in toBeAdded ++ encodePixelsToChunks xs x (Just nextChunk)

encodePixelsToChunks [] previous prev@(Just previousChunk) =
    [previousChunk]

-- Let's make it pass

encodePixelQoi :: (IsPixel pixel) => pixel -> pixel -> Maybe Chunk -> Map.Map Binary.Word8 pixel -> Chunk
encodePixelQoi previous current previousChunk previouslySeenPixels
    | previous == current = getRle previousChunk
    | Just pixelInIdxPosition <- Map.lookup calculatedHash previouslySeenPixels, pixelInIdxPosition == current = QOI_OP_INDEX {tag = TwoBitTag, index = calculatedHash}
    | isSmallPixelDifference = QOI_OP_DIFF {tag = TwoBitTag, dr = rDiff, dg = gDiff, db = bDiff}
    | isBigPixelDifference = QOI_OP_LUMA {tag=TwoBitTag, dg = gDiff, dr_dg = rBigDiff, db_dg = bBigDiff}
    | otherwise = undefined
    where

        rleValueConsideringPreviousRle :: Binary.Word8 -> Binary.Word8
        rleValueConsideringPreviousRle previousRle
            | previousRle < 62 = previousRle + 1
            | otherwise = 1

        getRle :: Maybe Chunk -> Chunk
        getRle (Just (QOI_OP_RUN {rle})) = QOI_OP_RUN { tag = TwoBitTag, rle = rleValueConsideringPreviousRle rle}
        getRle _ = QOI_OP_RUN { tag = TwoBitTag, rle = 1}

        calculatedHash :: Binary.Word8
        calculatedHash = calculateIndexPosition current

        rDiff :: Int8
        rDiff = fromIntegral (getRed current - getRed previous) :: Int8

        gDiff :: Int8
        gDiff = fromIntegral (getGreen current - getGreen previous) :: Int8

        bDiff :: Int8
        bDiff = fromIntegral (getBlue current - getBlue previous) :: Int8

        isSmallPixelDifference :: Bool
        isSmallPixelDifference = isSmallDiff rDiff && isSmallDiff gDiff && isSmallDiff bDiff

        isSmallDiff :: Int8 -> Bool
        isSmallDiff x = x >= -2 && x <= 1

        isBigPixelDifference :: Bool
        isBigPixelDifference = True

        rBigDiff :: Int8
        rBigDiff = rDiff - gDiff

        bBigDiff :: Int8
        bBigDiff = bDiff - gDiff


{-
-- A very important property that is in the specification is that: 

A valid encoder must not issue 2 or more consecutive QOI_OP_INDEX
chunks to the same index. QOI_OP_RUN should be used instead. 
-} 

-- Given a RGB pixel that is equal to the previous pixel and was seen previously, when encoding, then should be encoded as a QOI_OP_RUN chunk.
-- >>>  encodePixelQoi rgbPixelR10G0B0 rgbPixelR10G0B0 Nothing (Map.singleton 19 rgbPixelR10G0B0) == QOI_OP_RUN {tag = TwoBitTag, rle = 1}
-- True


rgbPixelR0G6B0 :: RGBPixel
rgbPixelR0G6B0 = RGBPixel {red = 0, green = 6, blue = 0}

-- Given a RGB pixel that is not equal to the previous pixel, has the same index as a pixel that was seen previously, but is not equal to that pixel, when encoding, then should not be encoded as a QOI_OP_INDEX chunk.
-- >>>  encodePixelQoi firstPreviousRGBValue rgbPixelR10G0B0 Nothing (Map.singleton 19 rgbPixelR0G6B0) /= QOI_OP_INDEX {tag = TwoBitTag, index = 19}
-- True

-- Because of how we implemented our encoding function, both tests pass without changes.


-- Let's see if we need to change encodeQOI:

-- A test:

-- Given a RGBA image with 3 pixels, such that: the first is equal to the first previous pixel, the second has a small diference of red of 1, and the third is the same as the first pixel,
-- When encoding,
-- Then it should encode with a QOI_OP_RUN chunk, a QOI_OP_DIFF cnunk and a QOI_OP_INDEX chunk

testImg3 :: Image
testImg3 = RGBAImage 3 1 AllChannelsLinear [firstPreviousRGBAValue, RGBAPixel {red = 1, green = 0, blue = 0, alpha = 255}, firstPreviousRGBAValue]

-- >>> encodeQOI testImg3 == QoiFile (QoiHeader {magic = MagicHeader, width = 3, height = 1, channels = RGBA, colorspace = AllChannelsLinear}) [QOI_OP_RUN {tag = TwoBitTag, rle = 1}, QOI_OP_DIFF {tag = TwoBitTag, dr = 1, dg = 0, db = 0}, QOI_OP_INDEX {tag = TwoBitTag, index = 53}] EndMarker
-- False
