module Main where

import Graphics.UI.SDL as SDL

import Settings
import Resources

main :: IO ()
main = do
    -- Load game resources
    palette <- loadPalette

    -- Initialize SDL system
    SDL.init [InitAudio, InitVideo]

    screen <- SDL.setVideoMode scrWidth scrHeight scrBpp [SWSurface]
    _ <- SDL.setPalette screen [] palette 0 -- @todo not sure about palette's flags. Need to go deeper
    
    -- @todo no game loop yet. Just waiting for user input
    _ <- getChar
    return ()
