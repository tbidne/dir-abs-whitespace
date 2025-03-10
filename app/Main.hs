{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Exception (Exception (displayException))
import Data.Foldable qualified as F
import Data.List qualified as L
import System.Directory.Internal qualified as IDir
import System.Directory.OsPath qualified as ODir
import System.IO.Error (modifyIOError)
import System.OsPath (OsPath, osp, (</>))
import System.OsPath qualified as OsP

main :: IO ()
main = do
  let a = OsP.pack [OsP.pathSeparator] </> ([osp|root|] </> relOsPath)
      b = (OsP.pack [OsP.pathSeparator] </> [osp|root|]) </> relOsPath

      -- or Show
      toStr :: OsPath -> String
      toStr =
        either
          (\ex -> error $ "exception: " ++ displayException ex)
          (\s -> "\"" ++ s ++ "\"")
          . OsP.decodeUtf

  -- OK / FAIL refers to whether the string was left intact (OK) or had
  -- trailing whitespace trimmed (FAIL) on windows.

  printPaths
    [ -- relative paths: OK
      -- "a/rel/path   "
      ("Rel:", Left $ toStr relOsPath),
      ("Rel normalise: ", Left $ toStr $ OsP.normalise relOsPath),
      ("Rel simplify: ", Left $ toStr $ IDir.simplify relOsPath),
      ("", Left ""),
      -- combine abs and rel path w/ different associativity: OK
      -- "/root/a/rel/path   "
      -- "/root/a/rel/path   "
      ("/ </> (root </> a/rel/path   ): ", Left $ toStr a),
      ("/root </> a/rel/path   :", Left $ toStr b),
      ("", Left ""),
      -- absolute paths: OK
      -- "/a/rel/path   "
      ("Abs:", Left $ toStr absOsPath),
      ("Abs normalise: ", Left $ toStr $ OsP.normalise absOsPath),
      ("Abs simplify: ", Left $ toStr $ IDir.simplify absOsPath),
      ("", Left ""),
      -- Manual Dir.Internal.prependCurrentDirectory: OK
      -- ".../a/rel/path   "
      ( "Manual prepend:",
        Right $
          toStr <$> (</> relOsPath) <$> IDir.getCurrentDirectoryInternal
      ),
      -- Copied Dir.prependCurrentDirectory: OK (somehow)
      -- ".../a/rel/path   "
      ( "My prepend:",
        Right $
          toStr
            <$> myPrependCurrentDirectory relOsPath
      ),
      -- Dir.Internal.prependCurrentDirectory: FAIL
      -- ".../a/rel/path"
      ( "Dir prepend:",
        Right $
          toStr
            <$> IDir.prependCurrentDirectory relOsPath
      ),
      -- Dir.makeAbsolute: FAIL
      -- ".../a/rel/path"
      ("Abs:", Right $ toStr <$> ODir.makeAbsolute relOsPath)
    ]
  where
    printPaths :: [(String, Either String (IO String))] -> IO ()
    printPaths paths = do
      let maxLen = F.foldl' (\acc (hdr, _) -> max acc (length hdr)) 0 paths
          padMax = padN maxLen

      F.for_ paths $ \(header, ep) -> case ep of
        Left p -> putStrLn $ padMax header ++ p
        Right io -> putStrLn . (padMax header ++) =<< io

    padN :: Int -> String -> String
    padN m s =
      let x = L.replicate (m - length s) ' '
       in s ++ x

relOsPath :: OsPath
relOsPath = [osp|a|] </> [osp|rel|] </> [osp|path   |]

absOsPath :: OsPath
absOsPath = OsP.pack [OsP.pathSeparator] </> relOsPath

myPrependCurrentDirectory :: OsPath -> IO OsPath
myPrependCurrentDirectory path
  | OsP.isRelative path =
      ( (`IDir.ioeAddLocation` "myPrependCurrentDirectory")
          . (`IDir.ioeSetOsPath` path)
      )
        `modifyIOError` do
          (</> path) <$> IDir.getCurrentDirectoryInternal
  | otherwise = pure path
