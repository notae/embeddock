{-# OPTIONS -Wall #-}


import Control.Lens (_1,  _2, (^.), Lens', (.~), (&))
import Control.Monad
import Data.List (isInfixOf, isPrefixOf)
import Data.String.Utils (replace)
import Language.Haskell.Lexer (lexerPass0, Token(..), Pos, PosToken)
import System.Environment
import System.FilePath (splitFileName, splitExtension, (</>), (<.>))
import System.IO
import System.Process (runInteractiveCommand)
import Text.Printf

import qualified Embeddock.Option.Value as Opt

strOfToken :: Lens' PosToken String
strOfToken = _2 . _2
posOfToken :: Lens' PosToken Pos
posOfToken = _2 . _1
tokOfToken :: Lens' PosToken Token
tokOfToken = _1



toksToStr :: [PosToken] -> String
toksToStr = concat . map (^. strOfToken)


main :: IO ()
main = do
  -- the file to be processed.
  src <- readFile Opt.inputFilePath
  let
    embedKey :: String
    runhaskellArgs :: [String]

    (embedKey,runhaskellArgs) = ("$", [])

    openKey, closeKey :: String
    openKey = findFree "Open" 1 "Sesami"
    closeKey = findFree "Close" 1 "Sesami"

    findFree tag n tag' =
      let cand = tag ++ show (n::Integer) ++ tag' in
        if cand `isInfixOf` src then findFree tag (3*n) tag'
                                else cand

    parsedSrc :: [PosToken]
    parsedSrc =
      filter (not . isEmbedPragma) $
      lexerPass0 src

    isEmbedPragma :: PosToken -> Bool
    isEmbedPragma (tok, (_, str))
      | tok == NestedComment
        && "{-#" `isPrefixOf` str
        && "OPTIONS_GHC" `isInfixOf` str
        && "embeddock" `isInfixOf` str
                    = True
      | otherwise   = False

    (srcDir, srcFn) = splitFileName Opt.inputFilePath
    (srcFnBody, srcExt) = splitExtension srcFn

    isEmbedLoop = (head srcFn == '.') &&
                  ("_embeddock." `isInfixOf` srcFn)

    runnerFn = srcDir </> ("." ++ srcFnBody ++ "_embeddock") <.> srcExt


    destContent :: String
    destContent = toksToStr parsedSrc

    quotedMain = unlines $ "main = do" :map mkPrinter parsedSrc

    mkPrinter :: PosToken -> String
    mkPrinter tok = ("  putStr $ " ++ ) $
        replace openKey  "\"++(" $
        replace closeKey ")++\"" $
        show $
        seedEmbed tok ^. strOfToken
      where
        embeds :: [String]
        embeds = findEmbed tok

        seedEmbed :: PosToken -> PosToken
        seedEmbed tok = tok & strOfToken .~ newStr
          where
            newStr = foldl
              (\str' e -> replace (printf "%s(%s)" embedKey e)
                                  (printf "%s%s%s" openKey e closeKey)
                                  str')
              (tok ^. strOfToken) embeds


    findEmbed :: PosToken -> [String]
    findEmbed tok = go str
      where
        str = tok ^. strOfToken

        go []  = []
        go xss@(_:xs) = try embedKey xss `mplus` go xs

        try [] []       = []
        try _  []       = []
        try (k:ey) (x:xs)
          | k==x        = try ey xs
          | otherwise   = []
        try [] ('(':xs) = tryParen (1::Int) xs ""
        try _ _         = []

        tryParen n [] buf
          | n <= 0    = [reverse $ drop 1 buf]
          | otherwise = []

        tryParen n (x:xs) buf
          | n <= 0    = [reverse $ drop 1 buf]
          | otherwise = let next '(' = n+1
                            next ')' = n-1
                            next _   = n
                        in tryParen (next x) xs (x:buf)



  when (not isEmbedLoop) $ do

    -- generate a modified source file with the quoter and original bindings in scope
    writeFile runnerFn $ destContent ++ "\n" ++ quotedMain

    -- let the file print out the new file
    (_, hOut, hErr, _) <- runInteractiveCommand $
      printf "runhaskell %s %s" (unwords runhaskellArgs) runnerFn

    -- mirror the standard error output
    hGetContents hErr >>= hPutStr stderr

    -- print out the standard output
    hGetContents hOut >>=
      (if Opt.outputFilePath /= "" then writeFile Opt.outputFilePath
                         else putStrLn )
