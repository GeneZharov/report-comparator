import Data.Extraction


main = fromPhotos False "/root/p/zdrav/addr/samples/spb" >>= print . length
