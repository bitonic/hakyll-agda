{-# LANGUAGE ViewPatterns #-}
-- Parts of the code (specifically parts of `pairPositions' and `groupLiterate')
-- are taken from the Agda.Interaction.Highlighting.HTML module of Agda, see
-- <http://code.haskell.org/Agda/LICENSE> for the license and the copyright
-- information for that code.
module Hakyll.Web.Agda
    ( markdownAgda
    , pandocAgdaCompilerWith
    , pandocAgdaCompiler
    ) where

import           Agda.Interaction.FindFile (findFile, SourceFile(..))
import           Agda.Interaction.Highlighting.Precise
import qualified Agda.Interaction.Imports as Imp
import           Agda.Interaction.Options
import           Agda.Syntax.Abstract.Name (toTopLevelModuleName)
import           Agda.Syntax.Common
import           Agda.Syntax.Concrete.Name (TopLevelModuleName)
import           Agda.TypeChecking.Errors
import           Agda.TypeChecking.Monad (TCM)
import qualified Agda.TypeChecking.Monad as TCM
import           Agda.Utils.FileName
import qualified Agda.Utils.IO.UTF8 as UTF8
import           Control.Monad.Except (catchError, throwError)
import           Control.Monad.IO.Class (liftIO)
import           Data.Char (isSpace)
import           Data.Function (on)
import qualified Data.IntMap as IntMap
import           Data.List (groupBy, isInfixOf, isPrefixOf, tails)
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Web.Pandoc
import           System.Directory (getCurrentDirectory, setCurrentDirectory, canonicalizePath, setCurrentDirectory)
import           System.Exit (exitFailure)
import           System.FilePath (dropFileName, splitExtension)
import           Text.Pandoc (readMarkdown, ReaderOptions, WriterOptions)
import qualified Text.Pandoc as Pandoc
import           Text.XHtml.Strict
import qualified Data.Text as T

checkFile :: SourceFile -> TCM TopLevelModuleName
checkFile file = do
    TCM.resetState
    info <- Imp.sourceInfo file
    toTopLevelModuleName . TCM.iModuleName . fst <$>
      Imp.typeCheckMain file Imp.TypeCheck info

getModule :: TopLevelModuleName -> TCM (HighlightingInfo, Text)
getModule m = do
    Just mi <- TCM.getVisitedModule m
    f <- findFile m
    s <- liftIO . UTF8.readTextFile . filePath . srcFilePath $ f
    return (TCM.iHighlighting (TCM.miInterface mi), s)

pairPositions :: HighlightingInfo -> String -> [(Integer, String, Aspects)]
pairPositions info contents =
    map (\cs@((mi, (pos, _)) : _) -> (toInteger pos, map (snd . snd) cs, maybe mempty id mi)) $
    groupBy ((==) `on` fst) $
    map (\(pos, c) -> (IntMap.lookup pos infoMap, (pos, c))) $
    zip [1..] $
    contents
  where
    infoMap = toMap (decompress info)

-- TODO make these more accurate
beginCode :: String -> Bool
beginCode s = "\\begin{code}" `isInfixOf` s

endCode :: String -> Bool
endCode s = "\\end{code}" `isInfixOf` s

infixEnd :: Eq a => [a] -> [a] -> [a]
infixEnd i s = head [drop (length i) s' | s' <- tails s, i `isPrefixOf` s']

stripBegin :: (Integer, String, Aspects) -> (Integer, String, Aspects)
stripBegin (i, s, mi) = (i, cut (dropWhile (== ' ') (infixEnd "\\begin{code}" s)), mi)
  where
    cut ('\n' : s') = s'
    cut s'          = s'

groupLiterate
    :: [(Integer, String, Aspects)]
    -> [Either String [(Integer, String, Aspects)]]
groupLiterate contents =
    let (com, rest) = span (notCode beginCode) contents
    in Left ("\n\n" ++ concat [s | (_, s, _) <- com] ++ "\n\n") : go rest
  where
    go []         = []
    go (be : mis) =
        let be'@(_, s, _) = stripBegin be
            (code, rest)  = span (notCode endCode) mis
        in if "\\end{code}" `isInfixOf` s
           then -- We simply ignore empty code blocks
                groupLiterate mis
           else Right (be' : code) :
                -- If there's nothing between \end{code} and \begin{code}, we
                -- start consuming code again.
                case rest of
                    []                                  -> error "malformed file"
                    ((_, beginCode -> True, _) : code') -> go code'
                    (_                         : com  ) -> groupLiterate com

    notCode f (_, s, _) = not (f s)

annotate :: TopLevelModuleName -> Integer -> Aspects -> Html -> Html
annotate m pos mi = anchor ! attributes
  where
    attributes = [name (show pos)] ++
                 fromMaybe [] (definitionSite mi >>= link) ++
                 (case classes of [] -> []; cs -> [theclass (unwords cs)])

    classes = maybe [] noteClasses (note mi) ++
              otherAspectClasses (otherAspects mi) ++
              maybe [] aspectClasses (aspect mi)

    aspectClasses (Name mKind op) =
        let kindClass = maybe [] ((: []) . showKind) mKind

            showKind (Constructor Inductive)   = "InductiveConstructor"
            showKind (Constructor CoInductive) = "CoinductiveConstructor"
            showKind k                         = show k

            opClass = if op then ["Operator"] else []
        in kindClass ++ opClass
    aspectClasses a = [show a]

    otherAspectClasses = map show . Set.toList

    -- Notes are not included.
    noteClasses _ = []

    link defSite = if defSiteModule defSite == m
      then Just [href ("#" ++ show (defSitePos defSite))]
      else Nothing

toMarkdown :: String
           -> TopLevelModuleName -> [Either String [(Integer, String, Aspects)]]
           -> String
toMarkdown classpr m contents =
    concat [ case c of
                  Left s   -> s
                  Right cs ->
                      let h = pre . tag "code" . mconcat $
                              [ (annotate m pos mi (stringToHtml s))
                              | (pos, s, mi) <- cs ]
                      in  renderHtmlFragment (h ! [theclass classpr])
           | c <- contents ]

convert :: String -> TopLevelModuleName -> TCM String
convert classpr m =
    do (info, contents) <- getModule m
       return . toMarkdown classpr m . groupLiterate . pairPositions info . TL.unpack $ contents

markdownAgda :: CommandLineOptions -> String -> SourceFile -> IO String
markdownAgda opts classpr file =
    do let check = do
               TCM.setCommandLineOptions opts
               checkFile file >>= convert classpr
       r <- TCM.runTCMTop $ check `catchError` \err -> do
                s <- prettyError err
                liftIO (putStrLn s)
                throwError err
       case r of
           Right s -> return (dropWhile isSpace s)
           Left _  -> exitFailure

isAgda :: Item a -> Bool
isAgda i = ex == ".lagda"
  where
    ex = snd . splitExtension . toFilePath . itemIdentifier $ i

saveDir :: IO a -> IO a
saveDir m = do
    origDir <- getCurrentDirectory
    m <* setCurrentDirectory origDir

pandocAgdaCompilerWith :: ReaderOptions -> WriterOptions -> CommandLineOptions
                       -> Compiler (Item String)
pandocAgdaCompilerWith ropt wopt aopt = do
    i <- getResourceBody
    if isAgda i
      then cached cacheName $ do
        fp <- getResourceFilePath
        -- TODO get rid of the unsafePerformIO, and have a more solid
        -- way of getting the absolute path
        unsafeCompiler $ saveDir $ do
             -- We set to the directory of the file, we assume that
             -- the agda files are in one flat directory which might
             -- not be not the one where Hakyll is ran in.
             abfp <- canonicalizePath fp
             setCurrentDirectory (dropFileName abfp)
             s <- markdownAgda aopt "Agda" (SourceFile $ mkAbsolute abfp)
             let i' = i {itemBody = T.pack s}
             case Pandoc.runPure (traverse (readMarkdown ropt) i') of
               Left err -> fail $ "pandocAgdaCompilerWith: Pandoc failed with error " ++ show err
               Right i'' -> return $ writePandocWith wopt i''
      else pandocCompilerWith ropt wopt
  where
    cacheName = "LiterateAgda.pandocAgdaCompilerWith"

pandocAgdaCompiler :: Compiler (Item String)
pandocAgdaCompiler =
    pandocAgdaCompilerWith defaultHakyllReaderOptions defaultHakyllWriterOptions
                           defaultOptions
