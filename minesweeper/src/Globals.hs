module Globals where

  border :: Float
  border = 100

  getScreenWidth :: Float
  getScreenWidth = 600 - border

  getScreenHeight :: Float
  getScreenHeight = 600

  getWidth :: Float
  getWidth = getScreenWidth - 4

  getHeight :: Float
  getHeight = getScreenHeight - border

  getBoardSize :: Int
  getBoardSize = 4

  getNumMines :: Int
  getNumMines = 16

  easyBoard :: Int
  easyBoard = 8

  easyMines :: Int
  easyMines = 10

  medBoard :: Int
  medBoard = 16

  medMines :: Int
  medMines = 40

  hardBoard :: Int
  hardBoard = 24

  hardMines :: Int 
  hardMines = 99
