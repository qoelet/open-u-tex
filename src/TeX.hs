{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module TeX where

import           Data.String.Interpolate (i)
import           Data.Text (Text)
import qualified Data.Text as T
import           Dhall (Generic, Interpret, Natural, auto, input)

data Student = Student {
  fullName :: Text
, studentId :: Text
, email :: Text
} deriving (Eq, Show, Generic)

instance Interpret Student

getStudentInfo :: IO Student
getStudentInfo = input auto "./.student"

mkTeX :: Student -> String -> String -> Natural -> String
mkTeX Student{..} moduleName assignmentId questions
  = texTemplate
  where
    questionsTeX
      = concatMap
        (\x -> "\\section{Question " ++ show x ++ "}\n\\pagebreak\n")
        [1 .. questions]
    texTemplate = [i|
% Preamble

\\documentclass{article}
\\renewcommand{\\familydefault}{\\sfdefault}

\\title{#{moduleName} #{assignmentId}}
\\author{#{T.unpack fullName} (#{T.unpack studentId}) \\ #{T.unpack email}}

\\usepackage{amsmath}
\\usepackage{comment}
\\usepackage{fancyhdr}
\\usepackage{forest}
\\usepackage{gensymb}
\\usepackage{graphicx}
\\usepackage{lastpage}
\\usepackage{mathtools}
\\usepackage{tikz}

% Additional features

\\begin{comment}
  Additional tikz setup for drawing force diagrams
  From: http://www.texample.net/tikz/examples/free-body-diagrams/
\\end{comment}
\\usetikzlibrary{scopes}

\\begin{comment}
  This gives us "Page x of k" as instructed at:
  https://tex.stackexchange.com/questions/151989/add-page-number-with-total-page-number-on-each-page
\\end{comment}
\\fancyfoot[C]{Page \\thepage\\ of \\pageref{LastPage} / #{T.unpack fullName} (#{T.unpack studentId})}
\\pagestyle{fancy}
\\begin{comment}
  This allows us to specify equation numbering on an ad-hoc basis.
  From: https://tex.stackexchange.com/questions/42726/align-but-show-one-equation-number-at-the-end
\\end{comment}
\\newcommand\\numberthis{\\addtocounter{equation}{1}\\tag{\\theequation}}
\\newcommand\\comb[2][^n]{\\prescript{#1\\mkern-0.5mu}{}C_{#2}}
\\begin{comment}
  Combinatorial notation
  From: https://tex.stackexchange.com/questions/107125/is-there-a-command-to-write-the-form-of-a-combination-or-permutation
\\end{comment}

% Body

\\begin{document}
\\pagenumbering{gobble}
\\maketitle
\\newpage
\\pagenumbering{arabic}
\\begin{comment}
  We want numbering to match the TMA format.
  Some helpful info here:
  https://tex.stackexchange.com/questions/3177/how-to-change-the-numbering-of-part-chapter-section-to-alphabetical-r
\\end{comment}
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
