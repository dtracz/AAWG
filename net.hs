
import Foreign
import Foreign.C
import Foreign.Storable

data Frame = Frame 
        { label :: CInt
        , prob  :: Float
        , left  :: Float
        , botom :: Float
        , right :: Float
        , top   :: Float
        } deriving (Show)

instance Storable MyStruct where
    alignment _ = 8
    sizeOf _    = 24
    peek ptr    = MyStruct
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr 4
        <*> peekByteOff ptr 8
        <*> peekByteOff ptr 12
        <*> peekByteOff ptr 16
        <*> peekByteOff ptr 20
    poke ptr (MyStruct d c i f) = do
        pokeByteOff ptr 0  label
        pokeByteOff ptr 4  prob
        pokeByteOff ptr 8  left
        pokeByteOff ptr 12 botom
        pokeByteOff ptr 16 right
        pokeByteOff ptr 20 top

