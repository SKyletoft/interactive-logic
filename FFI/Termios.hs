{-# LANGUAGE ForeignFunctionInterface #-}

{- As taken from: https://wiki.haskell.org/FFI_Introduction -}

module FFI.Termios where
import Foreign.C

foreign import ccall "setup_terminal" setupTerminal :: IO ()
foreign import ccall "deinit_terminal" deInitTerminal :: IO ()
