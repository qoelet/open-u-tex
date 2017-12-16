{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module TeX where

import           Data.String.Interpolate (i)
import           Data.Text (Text)
import qualified Data.Text as T
import           Dhall (Generic, Interpret, auto, input)

data Student = Student {
  fullName :: Text
, studentId :: Text
, email :: Text
} deriving (Eq, Show, Generic)

instance Interpret Student

getStudentInfo :: IO Student
getStudentInfo = input auto "./.student"

mkTeX
  :: Student
  -> String
  -> String
  -> Int
  -> String
mkTeX Student{..} m a q = texTemplate
  where
    questionsTeX = concatMap (\x -> "\\section{Question " ++ show x ++ "}\n") [1 .. q]
    texTemplate = [i|
\\documentclass{article}
\\renewcommand{\\familydefault}{\\sfdefault}

\\title{#{m} #{a}}
\\author{#{T.unpack fullName} (#{T.unpack studentId}) \\ #{T.unpack email}}

\\usepackage{amsmath}
\\usepackage{comment}
\\usepackage{fancyhdr}
\\usepackage{forest}
\\usepackage{lastpage}
\\usepackage{graphicx}

\\fancyfoot[C]{Page \\thepage\\ of \\pageref{LastPage} / #{T.unpack fullName} (#{T.unpack studentId})}
\\pagestyle{fancy}

\\begin{document}
\\pagenumbering{gobble}
\\maketitle
\\newpage
\\pagenumbering{arabic}
\\renewcommand{\\thesection}{}
\\renewcommand\\thesubsection{\\alph{subsection})}
\\renewcommand\\thesubsubsection{\\roman{subsubsection})}
\\makeatletter
\\def\\@seccntformat#1{\\csname #1ignore\\expandafter\\endcsname\\csname the#1\\endcsname\\quad}
\\let\\sectionignore\\@gobbletwo
\\let\\latex@numberline\\numberline
\\def\\numberline#1{\\if\\relax#1\\relax\\else\\latex@numberline{#1}\\fi}
\\makeatother

#{questionsTeX}

\\end{document}
    |]
